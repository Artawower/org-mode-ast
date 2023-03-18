import {
  HandlerNotFoundError,
  NodeType,
  OrgNode,
  ParserConfiguration,
  TokenType,
  UnsupportedOperator,
} from '../models/index.js';
import { TokenIterator, Tokenizer } from '../tokenizer/index.js';
import { AstBuilder } from './ast-builder.js';
import { AstContext } from './ast-context.js';
import { ColonHandler } from './handlers/colon.handler.js';
import {
  BlockHandler,
  BracketHandler,
  CommentHandler,
  KeywordHandler,
  ListHandler,
  PropertiesHandler,
  HorizontalRuleHandler,
  LatexEnvironmentHandler,
  TableHandler,
} from './handlers/index.js';
import { parserConfiguration } from './parser.configuration.js';

class Parser {
  constructor(
    private configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly tokenIterator: TokenIterator,
    private readonly astBuilder: AstBuilder,
    private readonly bracketHandler: BracketHandler,
    private readonly listHandler: ListHandler,
    private readonly commentHandler: CommentHandler,
    private readonly keywordHandler: KeywordHandler,
    private readonly horizontalRuleHandler: HorizontalRuleHandler,
    private readonly latexEnvironmentHandler: LatexEnvironmentHandler,
    private readonly colonHandler: ColonHandler,
    private readonly tableHandler: TableHandler
  ) {}

  public parse(): OrgNode {
    this.buildTree();
    return this.astBuilder.nodeTree;
  }

  private buildTree(): void {
    this.tokenIterator.forEach(() => {
      this.handleToken();
    });
    this.handleEndOfLine();
    this.handleEndOfFile();
  }

  /*
   * Handlers who can hold the parsing process and force attach
   * next tokens to the handle operation
   */
  private holdOnHandlers = [this.colonHandler, this.keywordHandler];

  private readonly tokensHandlers = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.bracketHandler.handle(),
    [TokenType.Operator]: () => this.handleOperator(),
    [TokenType.Indent]: () => this.handleIndent(),
    [TokenType.NewLine]: () => this.handleNewLine(),
    [TokenType.Keyword]: () => this.keywordHandler.handle(),
    [TokenType.HorizontalRule]: () => this.horizontalRuleHandler.handle(),
    [TokenType.LatexBracket]: () => this.latexEnvironmentHandler.handle(),
    [TokenType.LatexEnvironmentKeyword]: () =>
      this.latexEnvironmentHandler.handle(),
    // TODO: master make same for other keys
    [CommentHandler.tokenType]: () => this.commentHandler.handle(),
    [TokenType.TableOperator]: () => this.tableHandler.handle(),
  } satisfies Record<string, () => OrgNode | void>;

  private handleToken(): void {
    const handler =
      this.getOnHoldHandler() ?? this.tokensHandlers[this.tokenIterator.type];

    if (!handler) {
      throw new HandlerNotFoundError(this.tokenIterator.type);
    }
    const orgData = handler();

    if (!orgData) {
      this.handleEmptyHandlerValue();
      return;
    }

    // TODO: master move to OrgNode class
    this.astBuilder.preserveLastPositionSnapshot(orgData);

    const lineBreak =
      this.tokenIterator.token?.isType(TokenType.NewLine) ||
      this.tokenIterator.isLastToken;
    if (lineBreak) {
      this.bracketHandler.handleEndOfLine();
      this.ctx.insideListItem = false;
    }
    if (
      this.tokenIterator.token?.isType(TokenType.NewLine) &&
      this.ctx.insideHeadline
    ) {
      this.astBuilder.getLastSectionOrCreate();
      this.ctx.insideHeadline = false;
      this.ctx.insideKeyword = false;
    }
  }

  private getOnHoldHandler(): () => OrgNode | void {
    const foundHandler = this.holdOnHandlers.find((h) => h.onHold);
    if (foundHandler) {
      return () => foundHandler.handleHolded();
    }
  }

  private handleEmptyHandlerValue(): void {
    // console.info(new HandlerDidNotReturnValue(this.tokenIterator.token));
  }

  private handleHeadline(): OrgNode {
    this.ctx.insideHeadline = true;
    const headlineNode = this.astBuilder.createHeadline();
    this.astBuilder.attachToTree(headlineNode);
    return headlineNode;
  }

  private handleText(): OrgNode {
    // TODO: master think if it's necessary
    if (this.astBuilder.lastNode.is(NodeType.Text)) {
      this.astBuilder.lastNode.appendValue(this.tokenIterator.currentValue);
      return;
    }

    const textNode = this.astBuilder.createText();

    this.astBuilder.attachToTree(textNode);

    const lastTokenWasNewLine = this.astBuilder.lastNode.value?.endsWith('\n');
    if (
      lastTokenWasNewLine &&
      this.astBuilder.lastNode.type !== NodeType.Indent
    ) {
      this.ctx.exitList();
    }

    return textNode;
  }

  private handleOperator(): OrgNode {
    const orgData = this.buildOrgDataForOperator(
      this.tokenIterator.currentValue
    );
    if (!orgData) {
      throw new UnsupportedOperator(this.tokenIterator.currentValue);
    }
    this.astBuilder.attachToTree(orgData);
    return orgData;
  }

  private handleIndent(): OrgNode {
    const indentNode = this.astBuilder.createIndentNode();

    if (this.astBuilder.isListOperator(this.tokenIterator.nextToken?.value)) {
      this.ctx.nextIndentNode = indentNode;
      this.astBuilder.increaseLastPosition(this.tokenIterator.currentValue);
      return;
    }
    this.astBuilder.getLastSectionOrCreate();
    this.astBuilder.attachToTree(indentNode);
    return indentNode;
  }

  private handleNewLine(): OrgNode {
    this.handleEndOfLine();
    const newLineNode = this.astBuilder.createNewLineNode();
    this.astBuilder.attachToTree(newLineNode);
    return newLineNode;
  }

  private handleEndOfLine(): void {
    // this.bracketHandler.handleEndOfLine();
    this.tableHandler.handleNewLine();
    this.keywordHandler.handleEndOfLine();
    this.colonHandler.handleNewLine();
  }

  private handleEndOfFile(): void {
    this.latexEnvironmentHandler.handleEndOfFile();
  }

  private buildOrgDataForOperator(operator: string): OrgNode {
    // TODO: master move this check to list handler!
    if (this.listHandler.isListTagOperator(operator)) {
      const orgNode = this.listHandler.handleListTag();
      return orgNode;
    }

    if (this.astBuilder.isListOperator(operator)) {
      const orgNode = this.listHandler.handle();
      return orgNode;
    }

    if (this.colonHandler.isColonOperator(operator)) {
      return this.colonHandler.handle();
    }

    return this.astBuilder.createTextNode(this.tokenIterator.currentValue);
  }
}

export function parse(
  text: string,
  configuration: ParserConfiguration = {}
): OrgNode {
  configuration = {
    ...parserConfiguration,
    ...configuration,
  };
  const ctx = new AstContext();
  const tokenizer = new Tokenizer(text, configuration);
  const tokenIterator = new TokenIterator(tokenizer);
  const astBuilder = new AstBuilder(ctx, tokenIterator);
  const horizontalRuleHandler = new HorizontalRuleHandler(
    configuration,
    astBuilder,
    tokenIterator
  );
  const commentHandler = new CommentHandler(astBuilder, tokenIterator);
  const bracketHandler = new BracketHandler(astBuilder, tokenIterator);
  const blockHandler = new BlockHandler(ctx, astBuilder, tokenIterator);
  const listHandler = new ListHandler(ctx, astBuilder, tokenIterator);
  const propertiesHandler = new PropertiesHandler(
    ctx,
    astBuilder,
    tokenIterator
  );
  const colonHandler = new ColonHandler(
    configuration,
    ctx,
    astBuilder,
    tokenIterator
  );
  const keywordHandler = new KeywordHandler(
    configuration,
    ctx,
    astBuilder,
    tokenIterator,
    blockHandler,
    propertiesHandler
  );
  const latexEnvironmentHandler = new LatexEnvironmentHandler(
    configuration,
    astBuilder,
    tokenIterator
  );
  const tableHandler = new TableHandler(
    configuration,
    astBuilder,
    tokenIterator
  );

  const parser = new Parser(
    configuration,
    ctx,
    tokenIterator,
    astBuilder,
    bracketHandler,
    listHandler,
    commentHandler,
    keywordHandler,
    horizontalRuleHandler,
    latexEnvironmentHandler,
    colonHandler,
    tableHandler
  );
  return parser.parse();
}
