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
  PairedSequencesHandler,
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
    private readonly ctx: AstContext,
    private readonly tokenIterator: TokenIterator,
    private readonly astBuilder: AstBuilder,
    private readonly pairedSequencesHandler: PairedSequencesHandler,
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
      // this.ctx.storeLastNewLineInfo(this.tokenIterator.currentValue);
    });
    this.handleEndOfExpressions();
    this.handleEndOfFile();
  }

  /*
   * Handlers who can hold the parsing process and force attach
   * next tokens to the handle operation
   */
  private holdOnHandlers = [
    this.colonHandler,
    this.keywordHandler,
    this.commentHandler,
  ];

  private readonly tokensHandlers = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.pairedSequencesHandler.handle(),
    [TokenType.OpenMarkup]: () => this.pairedSequencesHandler.handle(),
    [TokenType.CloseMarkup]: () => this.pairedSequencesHandler.handle(),
    [TokenType.Operator]: () => this.handleOperator(),
    [TokenType.Indent]: () => this.handleIndent(),
    [TokenType.NewLine]: () => this.handleNewLine(),
    [TokenType.Entity]: () => this.handleEntity(),
    [TokenType.Keyword]: () => this.keywordHandler.handle(),
    [TokenType.Link]: () => this.handleRawLink(),
    [TokenType.HorizontalRule]: () => this.horizontalRuleHandler.handle(),
    [TokenType.LatexBracket]: () => this.latexEnvironmentHandler.handle(),
    [TokenType.LatexEnvironmentKeyword]: () =>
      this.latexEnvironmentHandler.handle(),
    // TODO: master make same for other keys
    [CommentHandler.tokenType]: () => this.commentHandler.handle(),
    [TokenType.TableOperator]: () => this.handleTableOperator(),
    [TokenType.TableDelimiter]: () => this.tableHandler.handleDelimiter(),
  } satisfies Record<string, () => OrgNode | void>;

  private handleToken(): void {
    this.listHandler.checkListEnded();
    const handler =
      this.getOnHoldHandler() ?? this.tokensHandlers[this.tokenIterator.type];

    if (!handler) {
      throw new HandlerNotFoundError(this.tokenIterator.type);
    }
    const orgData = handler();

    if (!orgData) {
      return;
    }

    // TODO: master move to OrgNode class
    this.astBuilder.preserveLastPositionSnapshot(orgData);

    const lineBreak =
      this.tokenIterator.token?.isType(TokenType.NewLine) ||
      this.tokenIterator.isLastToken;
    if (lineBreak) {
      this.pairedSequencesHandler.handleEndOfLine();
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

  private handleHeadline(): OrgNode {
    this.ctx.exitList();
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

    this.astBuilder.checkContext();

    return textNode;
  }

  private handleOperator(): OrgNode {
    const orgData = this.buildOrgDataForOperator(
      this.tokenIterator.currentValue
    );
    if (!orgData) {
      return;
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
    this.handleEndOfExpressions();
    const newLineNode = this.astBuilder.createNewLineNode();
    this.astBuilder.attachToTree(newLineNode);
    return newLineNode;
  }

  private handleRawLink(): OrgNode {
    const rawLinkNode = this.astBuilder.createRawLinkNode(
      this.tokenIterator.currentValue
    );
    this.astBuilder.attachToTree(rawLinkNode);
    return rawLinkNode;
  }

  private handleEntity(): OrgNode {
    const entityNode = this.astBuilder.createEntity();
    this.astBuilder.attachToTree(entityNode);
    return entityNode;
  }

  private handleTableOperator(): OrgNode {
    if (!this.tableHandler.isTableLine()) {
      return;
    }
    this.pairedSequencesHandler.handleEndOfLine();
    return this.tableHandler.handle();
  }

  private handleEndOfExpressions(): void {
    if (this.astBuilder.lastNode.is(NodeType.NewLine)) {
      this.ctx.exitList();
    }
    this.commentHandler.handleNewLine();
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

    if (this.pairedSequencesHandler.isListDelimiterOperator()) {
      return this.astBuilder.createOperatorNode(
        this.tokenIterator.currentValue
      );
    }
    return this.astBuilder.upsertText();
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
  const colonHandler = new ColonHandler(
    configuration,
    ctx,
    astBuilder,
    tokenIterator
  );

  const pairedSequencesHandler = new PairedSequencesHandler(
    configuration,
    ctx,
    astBuilder,
    tokenIterator,
    colonHandler
  );
  const blockHandler = new BlockHandler(ctx, astBuilder, tokenIterator);
  const listHandler = new ListHandler(ctx, astBuilder, tokenIterator);
  const propertiesHandler = new PropertiesHandler(
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
    ctx,
    astBuilder,
    tokenIterator
  );
  const tableHandler = new TableHandler(
    configuration,
    astBuilder,
    tokenIterator
  );

  const parser = new Parser(
    ctx,
    tokenIterator,
    astBuilder,
    pairedSequencesHandler,
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
