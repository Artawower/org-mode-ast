import { AstBuilder } from 'ast-builder';
import { BracketHandler } from 'bracket-handler';
import { tokenize } from 'tokenizer';
import {
  Headline,
  NodeType,
  OrgData,
  Token,
  OrgText,
  TokenType,
  OrgRoot,
  UniversalOrgNode,
  OrgBold,
  Section,
  OrgCrossed,
  OrgCheckbox,
  WithValue,
  List,
  ListItem,
  OrgItalic,
  OrgInlineCode,
} from './types';
// import 'jsonify-console';

class Parser {
  // TODO: add common token iterator class
  constructor(private bracketHandler: BracketHandler, private astBuilder: AstBuilder) {}

  // TODO: master init from separated class
  private tokens: Token[];
  private token: Token;
  private tokenPosition: number;

  private bracketsStackPositions: Array<{ childIndex: number; node: OrgData }> = [];

  get isLastToken(): boolean {
    return this.tokenPosition === this.tokens.length - 1;
  }

  get isNewLine(): boolean {
    return this.token.value.endsWith('\n');
  }

  get insideList(): boolean {
    return this.checkIfInsideList();
  }

  private checkIfInsideList(node?: OrgData): boolean {
    node ||= this.astBuilder.lastNode;

    if (node.type === NodeType.ListItem || node.type === NodeType.List) {
      return true;
    }
    if (node.parent) {
      return this.checkIfInsideList(node.parent);
    }
    return false;
  }

  public parse(text: string): OrgData {
    this.tokens = tokenize(text);
    this.buildTree();

    return this.astBuilder.nodeTree;
  }

  private buildTree(): void {
    this.tokens.forEach((token, i) => {
      this.token = token;
      this.tokenPosition = i;
      this.handleToken();
    });
  }

  private tokensHandlers: { [key: string]: () => OrgData } = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.handleBracket(),
    [TokenType.Operator]: () => this.handleOperator(),
  };

  private handleToken(): void {
    const handler = this.tokensHandlers[this.token.type];
    if (!handler) {
      // TODO: error class
      throw new Error('No handler for token type: ' + this.token.type);
    }
    const orgData = handler();
    if (!orgData) {
      const m = `Handler for token ${this.token.type} returned undefined`;
      throw new Error(m);
    }

    this.astBuilder.preserveLastPositionSnapshot(orgData);
    this.appendLengthToParentNodes(this.astBuilder.lastPos, this.astBuilder.lastNode?.parent);

    const lineBreak = this.isNewLine || this.isLastToken;
    if (lineBreak) {
      this.clearBracketsPairs();
    }
    // NOT A TOKEN! FIND PARENT HEADLINE WHEN WE ARE INSIDE HEADLINE
    if (this.isNewLine && this.astBuilder.insideHeadline) {
      this.astBuilder.initNewSection();
      this.astBuilder.insideHeadline = false;
    }
    // this.nodeStack.push(orgData);
  }

  private handleHeadline(): OrgData {
    this.astBuilder.insideHeadline = true;
    const end = this.astBuilder.lastPos + this.token.value.length;
    const orgData: Headline = {
      type: NodeType.Headline,
      level: this.token.value.trim().length,
      start: this.astBuilder.lastPos,
      end,
      children: [{ type: NodeType.Operator, value: this.token.value, start: this.astBuilder.lastPos, end }],
    };
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleText(): OrgData {
    const orgData: OrgText = {
      type: NodeType.Text,
      value: this.token.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.token.value.length,
    };
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private clearBracketsPairs(): void {
    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const neighbors = (bracket.node.parent as OrgRoot).children;
      let childIndex = bracket.childIndex + childIndexOffset;
      const leftChild = neighbors[childIndex - 1];
      const rightChild = neighbors[childIndex + 1];

      // Offset after position changed
      bracket.node.type = NodeType.Text;

      if (leftChild?.type === NodeType.Text) {
        neighbors.splice(childIndex - 1, 1);
        bracket.node.start = leftChild.start;
        (bracket.node as OrgText).value = (leftChild as OrgText).value + (bracket.node as OrgText).value;
        childIndexOffset--;
        childIndex--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex + 1, 1);
        bracket.node.end = rightChild.end;
        (bracket.node as OrgText).value += (rightChild as OrgText).value;
        childIndexOffset--;
      }
    });

    this.bracketsStackPositions = [];
  }

  private handleBracket(): OrgData {
    const orgData: OrgData = {
      type: NodeType.Unresolved,
      value: this.token.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.token.value.length,
    };
    this.astBuilder.attachToTree(orgData);

    // TODO: master method for find last bracket from end
    const nodeAfterPairBracketClosed = this.tryHandlePairBracket(orgData);
    if (nodeAfterPairBracketClosed) {
      // console.log('We found bracket mathing');
      // We found pair bracket matching
      // this.attachToTree(nodeAfterPairBracketClosed);
      return nodeAfterPairBracketClosed;
    }

    this.bracketsStackPositions.push({ childIndex: (orgData.parent as OrgRoot).children.length - 1, node: orgData });

    return orgData;
  }

  private handleOperator(): OrgData {
    // TODO: need to create common handler for such operators
    // this.attachToTree()
    const orgData = this.buildOrgDataForOperator(this.token.value);
    if (!orgData) {
      throw new Error(`Couldn't handle opereator ${this.token.value}`);
    }
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private buildOrgDataForOperator(operator: string): OrgData {
    if (operator === '- ') {
      const orgData = this.handleListItem();
      return orgData;
    }
  }

  private handleListItem(): OrgData {
    if (!this.insideList) {
      const isOrdered = this.token.value?.[0] === '1';
      this.createEmptyList(isOrdered);
    }
    this.createNewListItem();

    const orgData: OrgData = {
      type: NodeType.Operator,
      value: this.token.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.token.value.length,
    };

    return orgData;
  }

  private createEmptyList(ordered: boolean): OrgData {
    const list: List = {
      type: NodeType.List,
      start: 0,
      end: 0,
      ordered,
      children: [],
    };
    this.astBuilder.attachToTree(list);
    this.astBuilder.saveLastNode(list);
    return list;
  }

  private createNewListItem(): void {
    const orgData: ListItem = {
      type: NodeType.ListItem,
      start: this.astBuilder.lastPos,
      end: 0,
      children: [],
    };
    this.astBuilder.attachToTree(orgData);
    this.astBuilder.saveLastNode(orgData);
  }

  private readonly textFormattersNodeTypeMap: {
    [key: string]: NodeType.Bold | NodeType.Crossed | NodeType.Italic | NodeType.InlineCode;
  } = {
    '*': NodeType.Bold,
    '/': NodeType.Italic,
    '+': NodeType.Crossed,
    '=': NodeType.InlineCode,
  };

  private tryHandlePairBracket(o: OrgData): OrgData {
    if (this.bracketsStackPositions.length === 0) {
      return;
    }

    const reversedBracketsStack = this.bracketsStackPositions.slice().reverse();

    // TODO: master expose this logic as composition entity
    const pairToDetect = this.getOpenedBracket(this.token.value);

    const foundPairIndex = reversedBracketsStack.findIndex((r) => (r.node as UniversalOrgNode).value === pairToDetect);

    const found = foundPairIndex !== -1;

    if (!found) {
      return;
    }

    const pair = reversedBracketsStack[foundPairIndex];
    pair.node.type = NodeType.Operator;

    o.type = NodeType.Operator;

    const realChildren = (pair.node.parent as UniversalOrgNode).children as OrgData[];
    const updatedChildren = realChildren.slice(0, pair.childIndex);

    const nestedChildren = this.mergeUnresolvedNodes(realChildren.slice(pair.childIndex, realChildren.length));
    const isCheckBox = this.isNodesCheckbox(nestedChildren as Array<OrgData & WithValue>);
    const checked = (nestedChildren[1] as WithValue)?.value?.toLowerCase() === 'x';

    const orgData: OrgBold | OrgCrossed | OrgCheckbox | OrgItalic | OrgInlineCode = isCheckBox
      ? ({
          type: NodeType.Checkbox,
          start: pair.node.start,
          end: o.end,
          checked,
          value: this.getRawValueFromNodes(nestedChildren as WithValue[]),
          children: [],
          parent: o.parent,
        } as OrgCheckbox)
      : {
          type: this.textFormattersNodeTypeMap[pairToDetect],
          start: pair.node.start,
          end: o.end,
          children: nestedChildren,
          parent: o.parent,
        };

    updatedChildren.push(orgData);
    this.astBuilder.lastNode = orgData;
    (pair.node.parent as UniversalOrgNode).children = updatedChildren;

    if (isCheckBox && pair.node.parent.type === NodeType.Headline) {
      (pair.node.parent as Headline).checked = checked;
    }

    reversedBracketsStack.splice(foundPairIndex, 1);
    this.bracketsStackPositions = reversedBracketsStack.reverse();

    return orgData;
  }

  private getOpenedBracket(openedBracket: string): string {
    if (openedBracket === ']') {
      return '[';
    }
    return openedBracket;
  }

  private isNodesCheckbox(nodes: Array<OrgData & WithValue>): boolean {
    return (
      nodes.length === 3 &&
      nodes[0]?.value === '[' &&
      (nodes[1]?.value === ' ' || nodes[1]?.value.toLowerCase() === 'x') &&
      nodes[2]?.value === ']'
    );
  }

  private mergeUnresolvedNodes(nodes: OrgData[]): OrgData[] {
    const mergedNodes: OrgData[] = [];
    nodes.forEach((n) => {
      const lastNode = mergedNodes[mergedNodes.length - 1];

      if (n.type === NodeType.Unresolved) {
        (n as OrgData).type = NodeType.Text;
      }

      if (!lastNode) {
        mergedNodes.push(n);
        return;
      }
      if (lastNode.type === NodeType.Text && n.type === NodeType.Text) {
        lastNode.end = n.end;
        (lastNode as OrgText).value += (n as OrgText).value;
        return;
      }
      mergedNodes.push(n);
    });
    return mergedNodes;
  }

  private getRawValueFromNodes(nodes: WithValue[]): string {
    // TODO: nested nodes!
    return nodes.map((n) => n?.value).join('');
  }

  private appendLengthToParentNodes(length: number, node?: OrgData): void {
    if (!node || !length) {
      return;
    }

    node.end = length;
    if (!node.parent) {
      return;
    }
    this.appendLengthToParentNodes(length, node.parent);
  }
}

export function parse(text: string): OrgData {
  const astBuilder = new AstBuilder();
  const bracketHandler = new BracketHandler();
  const parser = new Parser(bracketHandler, astBuilder);
  return parser.parse(text);
}
