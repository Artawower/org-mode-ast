import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { BracketHandler } from 'bracket-handler';
import { HandlerNotFoundError } from 'errors';
import { ListHandler } from 'list-handler';
import { TokenIterator } from 'token-iterator';
import { Tokenizer } from 'tokenizer';
import { Headline, NodeType, OrgData, OrgIndent, OrgText, TokenType, WithValue } from './types';

class Parser {
  constructor(
    private ctx: AstContext,
    private tokenIterator: TokenIterator,
    private astBuilder: AstBuilder,
    private bracketHandler: BracketHandler,
    private listHandler: ListHandler
  ) {}

  public parse(): OrgData {
    this.buildTree();

    return this.astBuilder.nodeTree;
  }

  private buildTree(): void {
    this.tokenIterator.forEach(() => {
      this.handleToken();
    });
  }

  private tokensHandlers: { [key: string]: () => OrgData | void } = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.bracketHandler.handle(),
    [TokenType.Operator]: () => this.handleOperator(),
    [TokenType.Indent]: () => this.handleIndent(),
  };

  private handleToken(): void {
    const handler = this.tokensHandlers[this.tokenIterator.type];
    if (!handler) {
      throw new HandlerNotFoundError(this.tokenIterator.type);
    }
    const orgData = handler();
    if (!orgData) {
      // TODO: check by token, only indent could return empty value
      const m = `Handler for token ${this.tokenIterator.type} returned undefined`;
      console.warn(m);
      return;
      // throw new Error(m);
    }

    this.astBuilder.preserveLastPositionSnapshot(orgData);
    this.astBuilder.appendLengthToParentNodes(this.astBuilder.lastPos, this.astBuilder.lastNode?.parent);

    const lineBreak = this.tokenIterator.isNewLine || this.tokenIterator.isLastToken;
    if (lineBreak) {
      this.bracketHandler.clearBracketsPairs();
      this.ctx.insideListItem = false;
    }
    if (this.tokenIterator.isNewLine && this.ctx.insideHeadline) {
      this.astBuilder.getLastSessionOrCreate();
      this.ctx.insideHeadline = false;
    }
  }

  private handleHeadline(): OrgData {
    this.ctx.insideHeadline = true;
    const end = this.astBuilder.lastPos + this.tokenIterator.currentValue.length;
    const orgData: Headline = {
      type: NodeType.Headline,
      level: this.tokenIterator.currentValue.trim().length,
      start: this.astBuilder.lastPos,
      end,
      children: [
        { type: NodeType.Operator, value: this.tokenIterator.currentValue, start: this.astBuilder.lastPos, end },
      ],
    };
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleText(): OrgData {
    // TODO: master TOKEN SHOULD BE SELF SUFFICIENT, AND CONTAIN SUCH METHODS
    // FOR DETERMINE TOKEN STATE (space, new line, additional information about token properties)
    const lastTokenWasNewLine = (this.astBuilder.lastNode as WithValue).value?.endsWith('\n');

    const orgData: OrgText = {
      type: NodeType.Text,
      value: this.tokenIterator.currentValue,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.currentValue.length,
    };

    this.astBuilder.attachToTree(orgData);

    if (lastTokenWasNewLine && this.astBuilder.lastNode.type !== NodeType.Indent) {
      this.ctx.exitList();
    }

    return orgData;
  }

  private handleOperator(): OrgData {
    const orgData = this.buildOrgDataForOperator(this.tokenIterator.currentValue);
    if (!orgData) {
      throw new Error(`Couldn't handle opereator ${this.tokenIterator.currentValue}`);
    }
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleIndent(): OrgIndent {
    const indentNode = this.astBuilder.createIndentNode();

    if (this.astBuilder.isListOperator(this.tokenIterator.nextToken.value)) {
      this.ctx.nextIndentNode = indentNode;
      this.astBuilder.increaseLastPosition(this.tokenIterator.currentValue);
      return;
    }
    this.astBuilder.getLastSessionOrCreate();
    this.astBuilder.attachToTree(indentNode);
    return indentNode;
  }

  private buildOrgDataForOperator(operator: string): OrgData {
    if (this.astBuilder.isListOperator(operator)) {
      const orgData = this.listHandler.handle();
      return orgData;
    }
  }
}

export function parse(text: string): OrgData {
  const ctx = new AstContext();
  const tokenizer = new Tokenizer(text);
  const tokenIterator = new TokenIterator(tokenizer);
  const astBuilder = new AstBuilder(ctx, tokenIterator);
  const bracketHandler = new BracketHandler(astBuilder, tokenIterator);
  const listHandler = new ListHandler(ctx, astBuilder, tokenIterator);
  const parser = new Parser(ctx, tokenIterator, astBuilder, bracketHandler, listHandler);
  return parser.parse();
}
