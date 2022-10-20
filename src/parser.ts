import { AstBuilder } from 'ast-builder';
import { BracketHandler } from 'bracket-handler';
import { TokenIterator } from 'token-iterator';
import { Tokenizer } from 'tokenizer';
import { Headline, NodeType, OrgData, OrgText, TokenType, List, ListItem } from './types';
// import 'jsonify-console';

class Parser {
  // TODO: add common token iterator class
  constructor(
    private tokenIterator: TokenIterator,
    private bracketHandler: BracketHandler,
    private astBuilder: AstBuilder
  ) {}

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

  public parse(): OrgData {
    this.buildTree();

    return this.astBuilder.nodeTree;
  }

  private buildTree(): void {
    this.tokenIterator.forEach(() => {
      this.handleToken();
    });
  }

  private tokensHandlers: { [key: string]: () => OrgData } = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.bracketHandler.handle(),
    [TokenType.Operator]: () => this.handleOperator(),
  };

  private handleToken(): void {
    const handler = this.tokensHandlers[this.tokenIterator.type];
    if (!handler) {
      // TODO: error class
      throw new Error('No handler for token type: ' + this.tokenIterator.type);
    }
    const orgData = handler();
    if (!orgData) {
      const m = `Handler for token ${this.tokenIterator.type} returned undefined`;
      throw new Error(m);
    }

    this.astBuilder.preserveLastPositionSnapshot(orgData);
    this.astBuilder.appendLengthToParentNodes(this.astBuilder.lastPos, this.astBuilder.lastNode?.parent);

    const lineBreak = this.tokenIterator.isNewLine || this.tokenIterator.isLastToken;
    if (lineBreak) {
      this.bracketHandler.clearBracketsPairs();
    }
    // NOT A TOKEN! FIND PARENT HEADLINE WHEN WE ARE INSIDE HEADLINE
    if (this.tokenIterator.isNewLine && this.astBuilder.insideHeadline) {
      this.astBuilder.initNewSection();
      this.astBuilder.insideHeadline = false;
    }
    // this.nodeStack.push(orgData);
  }

  private handleHeadline(): OrgData {
    this.astBuilder.insideHeadline = true;
    const end = this.astBuilder.lastPos + this.tokenIterator.value.length;
    const orgData: Headline = {
      type: NodeType.Headline,
      level: this.tokenIterator.value.trim().length,
      start: this.astBuilder.lastPos,
      end,
      children: [{ type: NodeType.Operator, value: this.tokenIterator.value, start: this.astBuilder.lastPos, end }],
    };
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleText(): OrgData {
    const orgData: OrgText = {
      type: NodeType.Text,
      value: this.tokenIterator.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.value.length,
    };
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleOperator(): OrgData {
    // TODO: need to create common handler for such operators
    // this.attachToTree()
    const orgData = this.buildOrgDataForOperator(this.tokenIterator.value);
    if (!orgData) {
      throw new Error(`Couldn't handle opereator ${this.tokenIterator.value}`);
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
      const isOrdered = this.tokenIterator.value?.[0] === '1';
      this.createEmptyList(isOrdered);
    }
    this.createNewListItem();

    const orgData: OrgData = {
      type: NodeType.Operator,
      value: this.tokenIterator.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.value.length,
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
}

export function parse(text: string): OrgData {
  const tokenizer = new Tokenizer(text);
  const tokenIterator = new TokenIterator(tokenizer);
  const astBuilder = new AstBuilder();
  const bracketHandler = new BracketHandler(astBuilder, tokenIterator);
  const parser = new Parser(tokenIterator, bracketHandler, astBuilder);
  return parser.parse();
}
