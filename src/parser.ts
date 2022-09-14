import { tokenize } from 'tokenizer';
import { Headline, NodeType, OrgData, Token, Text } from './types';
import 'jsonify-console';

class Parser {
  constructor() {}

  // TODO: master init from separated class
  private nodeTree: OrgData;
  public lastNode: OrgData;
  private tokens: Token[];
  private lastPos: number = 0;

  public parse(text: string): OrgData {
    this.tokens = tokenize(text);
    // console.log('🦄: [line 16][parser.ts] [35mthis.tokens: ', this.tokens);
    this.initRootNode();
    this.buildTree();

    let i = 0;
    let n = this.nodeTree.parent;
    while (n && i < 10) {
      // console.log(JSON.stringify(n, null, 2));
      n = n?.parent;
    }

    console.log(this.nodeTree);
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
    this.tokens.forEach((token) => {
      this.handleToken(token);
    });
  }

  private tokensHandlers: { [key: string]: (token: Token) => OrgData } = {
    [NodeType.Headline]: (token: Token) => this.handleHeadline(token),
    [NodeType.Text]: (token: Token) => this.handleText(token),
  };

  private handleToken(token: Token): void {
    const handler = this.tokensHandlers[token.type];
    if (!handler) {
      // TODO: error class
      throw new Error('No handler for token type: ' + token.type);
    }
    const orgData = handler(token);
    if (!orgData) {
      throw new Error('Something strange :<');
    }
    this.preserveLastPositionSnapshot(orgData);
    this.appendLengthToParentNodes(this.lastPos, this.lastNode?.parent);
    // this.nodeStack.push(orgData);
  }

  private handleHeadline(token: Token): OrgData {
    const end = this.lastPos + token.value.length;
    const orgData: Headline = {
      type: NodeType.Headline,
      level: token.value.trim().length,
      start: this.lastPos,
      end,
      children: [{ type: NodeType.Operator, value: token.value, start: this.lastPos, end }],
    };
    (this.lastNode as any).children.push(orgData);
    return orgData;
  }

  private nodesMayContainText = [NodeType.Headline, NodeType.Root];

  private handleText(token: Token): OrgData {
    // console.log('🦄: [line 92][parser.ts] [35mthis.lastNode.type: ', this.lastNode.type);
    if (this.nodesMayContainText.includes(this.lastNode.type)) {
      const orgData: Text = {
        type: NodeType.Text,
        value: token.value,
        start: this.lastPos,
        end: this.lastPos + token.value.length,
      };
      (this.lastNode as Headline).children.push(orgData);
      return orgData;
    }
  }

  private preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    if (this.lastNode?.type !== NodeType.Operator) {
      orgData.parent = this.lastNode;
    } else {
      orgData.parent = this.lastNode.parent;
    }
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
