import {
  HandlerDidNotReturnValue,
  HandlerNotFoundError,
  NodeType,
  OrgNode,
  ParserConfiguration,
  TokenType,
  UnsupportedOperator,
} from 'models';
import { TokenIterator, Tokenizer } from 'tokenizer';
import { AstBuilder } from './ast-builder';
import { AstContext } from './ast-context';
import {
  BlockHandler,
  BracketHandler,
  CommentHandler,
  ListHandler,
  PropertiesHandler,
} from './handlers';
import { parserConfiguration } from './parser.configuration';

class Parser {
  constructor(
    private configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly tokenIterator: TokenIterator,
    private readonly astBuilder: AstBuilder,
    private readonly bracketHandler: BracketHandler,
    private readonly listHandler: ListHandler,
    private readonly blockHandler: BlockHandler,
    private readonly commentHandler: CommentHandler,
    private readonly propertiesHandler: PropertiesHandler
  ) {}

  public parse(): OrgNode {
    this.buildTree();
    return this.astBuilder.nodeTree;
  }

  private buildTree(): void {
    this.tokenIterator.forEach(() => {
      this.handleToken();
    });
  }

  private tokensHandlers = {
    [TokenType.Headline]: () => this.handleHeadline(),
    [TokenType.Text]: () => this.handleText(),
    [TokenType.Bracket]: () => this.bracketHandler.handle(),
    [TokenType.Operator]: () => this.handleOperator(),
    [TokenType.Indent]: () => this.handleIndent(),
    [TokenType.NewLine]: () => this.handleNewLine(),
    [TokenType.Keyword]: () => this.handleKeyword(),
    // TODO: master make same for other keys
    [CommentHandler.tokenType]: () => this.commentHandler.handle(),
  } satisfies Record<string, () => OrgNode | void>;

  private handleToken(): void {
    const handler = this.tokensHandlers[this.tokenIterator.type];
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
    // this.astBuilder.appendLengthToParentNodes(this.astBuilder.lastPos, this.astBuilder.lastNode?.parent);

    const lineBreak =
      this.tokenIterator.token?.isType(TokenType.NewLine) ||
      this.tokenIterator.isLastToken;
    if (lineBreak) {
      this.bracketHandler.clearBracketsPairs();
      this.ctx.insideListItem = false;
    }
    if (
      this.tokenIterator.token?.isType(TokenType.NewLine) &&
      this.ctx.insideHeadline
    ) {
      this.astBuilder.getLastSectionOrCreate();
      this.ctx.insideHeadline = false;
    }
  }

  private handleEmptyHandlerValue(): void {
    const tokenWithPotentialUndefinedResult = [TokenType.Indent];
    if (tokenWithPotentialUndefinedResult.includes(this.tokenIterator.type)) {
      return;
    }
    throw new HandlerDidNotReturnValue(this.tokenIterator.token);
  }

  private handleHeadline(): OrgNode {
    this.ctx.insideHeadline = true;
    const headlineNode = this.astBuilder.createHeadline();
    this.astBuilder.attachToTree(headlineNode);
    return headlineNode;
  }

  private handleText(): OrgNode {
    const lastTokenWasNewLine = this.astBuilder.lastNode.value?.endsWith('\n');

    const textNode = this.astBuilder.createText();

    this.astBuilder.attachToTree(textNode);

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

    if (this.astBuilder.isListOperator(this.tokenIterator.nextToken.value)) {
      this.ctx.nextIndentNode = indentNode;
      this.astBuilder.increaseLastPosition(this.tokenIterator.currentValue);
      return;
    }
    this.astBuilder.getLastSectionOrCreate();
    this.astBuilder.attachToTree(indentNode);
    return indentNode;
  }

  private handleNewLine(): OrgNode {
    this.bracketHandler.handleNewLine();
    const newLineNode = this.astBuilder.createNewLineNode();
    this.astBuilder.attachToTree(newLineNode);
    return newLineNode;
  }

  // TODO: master separated handler
  private handleKeyword(): OrgNode {
    if (this.blockHandler.isBlockKeyword(this.tokenIterator.currentValue)) {
      return this.blockHandler.handle();
    }
    if (this.propertiesHandler.isPropertyKeyword()) {
      return this.propertiesHandler.handle();
    }
    if (this.isTodoKeyword(this.tokenIterator.currentValue)) {
      return this.handleTodoKeyword();
    }
    if (this.propertiesHandler.isBlockPropertyKeyword()) {
      return this.blockHandler.handleBlockProperty();
    }
    const textNode = this.astBuilder.createText();
    const createdKeyword = this.astBuilder.createKeyword(textNode);
    this.astBuilder.attachToTree(createdKeyword);
    return createdKeyword;
  }

  private isTodoKeyword(keyword: string): boolean {
    return this.configuration.todoKeywords.includes(keyword);
  }

  private handleTodoKeyword(): OrgNode {
    const todoKeywordNode = this.astBuilder.createTodoKeywordNode();
    this.astBuilder.attachToTree(todoKeywordNode);
    return todoKeywordNode;
  }

  private buildOrgDataForOperator(operator: string): OrgNode {
    if (this.astBuilder.isListOperator(operator)) {
      const orgNode = this.listHandler.handle();
      return orgNode;
    }

    // TODO also check is not a tag and opened propery drawer
    if (this.astBuilder.isPropertyOperator(operator)) {
      return this.astBuilder.createUnresolvedNode();
    }
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
  const commentHandler = new CommentHandler(astBuilder, tokenIterator);
  const bracketHandler = new BracketHandler(astBuilder, tokenIterator);
  const blockHandler = new BlockHandler(ctx, astBuilder, tokenIterator);
  const listHandler = new ListHandler(ctx, astBuilder, tokenIterator);
  const propertiesHandler = new PropertiesHandler(
    ctx,
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
    blockHandler,
    commentHandler,
    propertiesHandler
  );
  return parser.parse();
}
