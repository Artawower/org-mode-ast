import { tokenize } from 'tokenizer';
import {
  Headline,
  NodeType,
  OrgData,
  Token,
  Text,
  TokenType,
  OrgRoot,
  UniversalOrgNode,
  OrgBold,
  Unresolved,
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

  private bracketsStackPositions: Array<{ childIndex: number; node: OrgData }> = [];

  private lastPos: number = 0;

  get isLastToken(): boolean {
    return this.tokenPosition === this.tokens.length - 1;
  }

  get isNewLine(): boolean {
    return this.token.value.endsWith('\n');
  }

  public parse(text: string): OrgData {
    this.tokens = tokenize(text);
    this.initRootNode();
    this.buildTree();

    // let i = 0;
    // let n = this.nodeTree.parent;
    // while (n && i < 10) {
    //   // console.log(JSON.stringify(n, null, 2));
    //   n = n?.parent;
    // }

    // console.log(this.nodeTree);
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
    // this.lastNode = this.nodeTree;
    // this.lastHeadingNode = this.nodeTree;
    // this.nodeStack = [this.nodeTree];
  }

  // private getNthFromEndNode(pos: number): OrgData {
  //   return this.nodeStack?.slice(-pos)?.[0];
  // }

  // get lastNode(): OrgData {
  //   return this.lastNode;
  // }

  private buildTree(): void {
    this.tokens.forEach((token, i) => {
      this.token = token;
      // console.log('🦄: [line 67][parser.ts] [35mthis.token: ', this.token);
      this.tokenPosition = i;
      this.handleToken();
    });
  }

  private tokensHandlers: { [key: string]: () => OrgData } = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.handleBracket(),
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

    if (this.isNewLine || this.isLastToken) {
      this.clearBracketsPairs();
    }
    // this.nodeStack.push(orgData);
  }

  private handleHeadline(): OrgData {
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

  private nodesMayContainText = [NodeType.Headline, NodeType.Root];

  private handleText(): OrgData {
    const orgData: Text = {
      type: NodeType.Text,
      value: this.token.value,
      start: this.lastPos,
      end: this.lastPos + this.token.value.length,
    };
    // if (this.nodesMayContainText.includes(this.lastNode.type)) {
    //   (this.lastNode as Headline).children.push(orgData);
    //   return orgData;
    // }
    this.attachToTree(orgData);
    return orgData;
  }

  private clearBracketsPairs(): void {
    // TODO: clear brackets pairs. We didn't found closed bracket

    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const neighbors = (bracket.node.parent as OrgRoot).children;
      const childIndex = bracket.childIndex + childIndexOffset;
      const leftChild = neighbors[childIndex - 1];
      const rightChild = neighbors[childIndex + 1];
      // Offset after position changed
      bracket.node.type = NodeType.Text;

      if (leftChild?.type === NodeType.Text) {
        neighbors.splice(childIndex - 1, 1);
        bracket.node.start = leftChild.start;
        (bracket.node as Text).value = (leftChild as Text).value + (bracket.node as Text).value;
        childIndexOffset--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex, 1);
        bracket.node.end = rightChild.end;
        (bracket.node as Text).value += (rightChild as Text).value;
        childIndexOffset--;
      }
    });
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
    if (this.tryHandlePairBracket(orgData)) {
      console.log('We found bracket mathing');
      // We found bold matching
      return orgData;
    }

    this.bracketsStackPositions.push({ childIndex: (orgData.parent as OrgRoot).children.length - 1, node: orgData });

    return orgData;
  }

  private tryHandlePairBracket(o: OrgData): boolean {
    // console.log('Length of bracket stack');

    // console.log(this.bracketsStackPositions.length);
    if (this.bracketsStackPositions.length === 0) {
      return false;
    }

    const reversedBracketsStack = this.bracketsStackPositions.slice().reverse();

    const pairToDetect = this.token.value === ']' ? '[' : this.token.value;

    const foundPairIndex = reversedBracketsStack.findIndex((r) => (r.node as UniversalOrgNode).value === pairToDetect);

    // console.log('🦄: [line 188][parser.ts] [35mfoundPairIndex: ', foundPairIndex);

    if (foundPairIndex !== -1) {
      const pair = reversedBracketsStack[foundPairIndex];
      pair.node.type = NodeType.Operator;
      // console.log(pair.node);
      console.log('-----');

      o.type = NodeType.Operator;
      const realChildren = (pair.node.parent as UniversalOrgNode).children as OrgData[];
      const updatedChildren = realChildren.slice(0, pair.childIndex);
      console.log(
        '🦄: [line 217][parser.ts] [35mrealChildren.slice(pair.childIndex, realChildren.length): ',
        realChildren.slice(pair.childIndex, realChildren.length)
      );
      const nestedChildren = this.mergeUnresolvedNodes(realChildren.slice(pair.childIndex, realChildren.length));
      // console.log('🦄: [line 206][parser.ts] [35mnestedChildren: ', nestedChildren);

      const orgData: OrgBold = {
        type: NodeType.Bold,
        start: pair.node.start,
        end: o.end,
        children: nestedChildren,
      };

      updatedChildren.push(orgData);
      this.lastNode = orgData;
      (pair.node.parent as UniversalOrgNode).children = updatedChildren;

      // console.log('🦄: [line 204][parser.ts] [35mrealChildren: ', realChildren);
      // console.log('Pair found! Need to do something with it');
      reversedBracketsStack.splice(foundPairIndex);
      this.bracketsStackPositions = reversedBracketsStack.reverse();
    }

    return foundPairIndex !== -1;
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
        (lastNode as Text).value += (n as Text).value;
        return;
      }
      mergedNodes.push(n);
    });
    return mergedNodes;
  }

  private attachToTree(orgData: OrgData): void {
    const parentNode = this.findParentForNodeType(orgData);
    (parentNode as OrgRoot).children.push(orgData);
    orgData.parent = parentNode;
  }

  private findParentForNodeType(srcNode: OrgData, dstNode?: OrgData): OrgData {
    dstNode ||= this.lastNode;

    if (!dstNode) {
      return;
    }

    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    const isTargetNodeHeadline = dstNode.type === NodeType.Headline;

    if (
      isSourceNodeHeadline &&
      ((isTargetNodeHeadline && (<Headline>srcNode).level < (<Headline>dstNode).level) ||
        dstNode.type === NodeType.Root)
    ) {
      return dstNode;
    }

    if (!isSourceNodeHeadline && [NodeType.Root, NodeType.Headline].includes(dstNode.type)) {
      return dstNode;
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  private preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    this.lastNode = orgData;
  }

  private appendLengthToParentNodes(length: number, node?: OrgData): void {
    if (!node) {
      return;
    }

    node.end = length;
    if (!node.parent) {
      return;
    }
    this.appendLengthToParentNodes(length, node.parent);
  }

  private clearNodeStack(): void {
    this.lastNode = this.nodeTree;
  }
}

export function parse(text: string): OrgData {
  const parser = new Parser();
  return parser.parse(text);
}
