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
} from './types';
// import 'jsonify-console';

class Parser {
  constructor() {}

  // TODO: master init from separated class
  private nodeTree: OrgData;
  public lastNode: OrgData;
  private tokens: Token[];
  private token: Token;
  private tokenPosition: number;
  private lastSection: Section;
  private insideHeadline: boolean = false;

  private bracketsStackPositions: Array<{ childIndex: number; node: OrgData }> = [];

  private lastPos: number = 0;

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
    node ||= this.lastNode;

    if (node.type === NodeType.ListItem || node.type === NodeType.List) {
      return true;
    }
    if (node.parent) {
      return this.checkIfInsideList(node.parent);
    }
    return false;
  }

  private isLastTokenTypeEqual(type: TokenType): boolean {
    return this.tokens[this.tokenPosition - 1]?.type === type;
  }

  public parse(text: string): OrgData {
    this.tokens = tokenize(text);
    this.initRootNode();
    this.buildTree();

    return this.nodeTree;
  }

  private initRootNode(): void {
    this.nodeTree = {
      type: NodeType.Root,
      start: 0,
      end: 0,
      children: [],
    };
    this.preserveLastPositionSnapshot(this.nodeTree);
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

    this.preserveLastPositionSnapshot(orgData);
    this.appendLengthToParentNodes(this.lastPos, this.lastNode?.parent);

    const lineBreak = this.isNewLine || this.isLastToken;
    if (lineBreak) {
      this.clearBracketsPairs();
    }
    // NOT A TOKEN! FIND PARENT HEADLINE WHEN WE ARE INSIDE HEADLINE
    if (this.isNewLine && this.insideHeadline) {
      this.initNewSection();
      this.insideHeadline = false;
    }
    // this.nodeStack.push(orgData);
  }

  /*
   * Create new nested section
   */
  private initNewSection(): void {
    const headline = this.lastNode.parent as Headline;
    const section: Section = {
      type: NodeType.Section,
      start: headline.end,
      end: headline.end,
      children: [],
      parent: headline?.parent,
    };

    headline.section = section;
    this.lastSection = section;
  }

  private handleHeadline(): OrgData {
    this.insideHeadline = true;
    const end = this.lastPos + this.token.value.length;
    const orgData: Headline = {
      type: NodeType.Headline,
      level: this.token.value.trim().length,
      start: this.lastPos,
      end,
      children: [{ type: NodeType.Operator, value: this.token.value, start: this.lastPos, end }],
    };
    this.attachToTree(orgData);
    return orgData;
  }

  private handleText(): OrgData {
    const orgData: OrgText = {
      type: NodeType.Text,
      value: this.token.value,
      start: this.lastPos,
      end: this.lastPos + this.token.value.length,
    };
    this.attachToTree(orgData);
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
      start: this.lastPos,
      end: this.lastPos + this.token.value.length,
    };
    this.attachToTree(orgData);

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

  // TODO: master  rewrite as map of methods
  // private operatorHandlers: { [key: string]: () => OrgData } = {
  //   '- ':
  // }

  private handleOperator(): OrgData {
    // TODO: need to create common handler for such operators
    if (this.token.value === '- ') {
      const orgData = this.handleListItem();
      this.attachToTree(orgData);
      return orgData;
    }
    // this.attachToTree()
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
      start: this.lastPos,
      end: this.lastPos + this.token.value.length,
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
    this.attachToTree(list);
    this.saveLastNode(list);
    return list;
  }

  private createNewListItem(): void {
    const orgData: ListItem = {
      type: NodeType.ListItem,
      start: this.lastPos,
      end: 0,
      children: [],
    };
    this.attachToTree(orgData);
    this.saveLastNode(orgData);
  }

  private readonly charNodeTypeMap: { [key: string]: NodeType.Bold | NodeType.Crossed } = {
    '*': NodeType.Bold,
    // '/': NodeType.Italic,
    '+': NodeType.Crossed,
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

    const orgData: OrgBold | OrgCrossed | OrgCheckbox | OrgItalic = isCheckBox
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
    this.lastNode = orgData;
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

  private attachToTree(orgData: OrgData): void {
    const parentNode = this.findParentForNodeType(orgData);
    (parentNode as OrgRoot).children.push(orgData);
    orgData.parent = parentNode;
  }

  private findParentForNodeType(srcNode: OrgData, dstNode?: OrgData): OrgData {
    if (!this.insideHeadline && this.lastSection) {
      return this.lastSection as any;
    }

    dstNode ||= this.lastNode;

    if (!dstNode) {
      throw new Error(`Something wen wrong, couldn't find parent`);
    }

    if (dstNode.type === NodeType.Root) {
      return dstNode;
    }

    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    const isTargetNodeHeadline = dstNode.type === NodeType.Headline;

    if (isSourceNodeHeadline && isTargetNodeHeadline && (<Headline>srcNode).level > (<Headline>dstNode).level) {
      return (dstNode as Headline).section;
    }

    const isSrcListItem = srcNode.type === NodeType.ListItem;
    const isTargetList = dstNode.type === NodeType.List;

    if (isSrcListItem) {
      return isTargetList ? dstNode : this.findParentForNodeType(srcNode, dstNode.parent);
    }

    if (
      !isSourceNodeHeadline &&
      [NodeType.Root, NodeType.Headline, NodeType.Section, NodeType.Checkbox, NodeType.ListItem].includes(dstNode.type)
    ) {
      return dstNode;
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  private preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  private saveLastNode(orgData: OrgData): void {
    this.lastNode = orgData;
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
  const parser = new Parser();
  return parser.parse(text);
}
