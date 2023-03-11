import {
  NodeType,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
  TokenType,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { AstContext } from '../ast-context.js';
import { TokenIterator } from '../../tokenizer/index.js';
import { BlockHandler } from './block.handler.js';
import { PropertiesHandler } from './properties.handler.js';

export class KeywordHandler implements OrgHandler {
  #lastKeyword: OrgNode;

  readonly #htmlKeyword = '#+html:';

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator,
    private readonly blockHandler: BlockHandler,
    private readonly propertiesHandler: PropertiesHandler
  ) {}

  get onHold(): boolean {
    if (this.tokenIterator.token.isType(TokenType.NewLine)) {
      this.resetLastStoredKeyword();
    }
    return (
      this.#lastKeyword?.children.first?.value?.toLowerCase() ===
      this.#htmlKeyword
    );
  }

  get #isHtmlKeyword(): boolean {
    return this.tokenIterator.currentValue?.toLowerCase() === this.#htmlKeyword;
  }

  public handle(): OrgNode {
    // TODO: master tmp hack. Need to check correct token value inside tokenizer
    if (this.incorrectLatexEnvironmentKeyword()) {
      return;
    }
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
    const textNode = this.astBuilder.createTextNode(
      this.tokenIterator.currentValue
    );
    const createdKeyword = this.astBuilder.createKeywordNode(textNode);
    this.#lastKeyword = createdKeyword;
    if (
      this.#isHtmlKeyword &&
      !this.astBuilder.lastNode.parent?.is(NodeType.InlineHtml)
    ) {
      const inlineHtml = this.astBuilder.createInlineHtmlNode();
      this.astBuilder.attachToTree(inlineHtml);
      this.astBuilder.preserveLastPositionSnapshot(inlineHtml);
    }
    console.log(
      '✎: [line 62][keyword.handler.ts] this.astBuilder.lastNode: ',
      this.astBuilder.lastNode
    );
    this.astBuilder.attachToTree(createdKeyword);
    return createdKeyword;
  }

  private incorrectLatexEnvironmentKeyword(): boolean {
    if (this.tokenIterator.currentValue !== '\\') {
      return;
    }

    if (this.astBuilder.lastNode.is(NodeType.Text)) {
      this.astBuilder.lastNode.appendValue(this.tokenIterator.currentValue);
      return true;
    }

    const textNode = this.astBuilder.createTextNode(
      this.tokenIterator.currentValue
    );

    this.astBuilder.attachToTree(textNode);
    this.astBuilder.preserveLastPositionSnapshot(textNode);
    return true;
  }

  private isTodoKeyword(keyword: string): boolean {
    return this.configuration.todoKeywords.includes(keyword);
  }

  private handleTodoKeyword(): OrgNode {
    const todoKeywordNode = this.astBuilder.createTodoKeywordNode();
    this.astBuilder.attachToTree(todoKeywordNode);
    return todoKeywordNode;
  }

  public handleEndOfLine(): void {
    this.#lastKeyword?.calculateNodeProperties();
    this.resetLastStoredKeyword();
  }

  private resetLastStoredKeyword(): void {
    this.#lastKeyword = null;
  }

  public handleHolded(): OrgNode {
    // console.log(
    //   '✎: [line 120]',
    //   `"${this.astBuilder.lastNode?.value}" - ${this.astBuilder.lastNode?.type} curr: "${this.tokenIterator.currentValue}"`
    // );
    if (!this.astBuilder.lastNode.is(NodeType.Text)) {
      const lastParent = this.astBuilder.lastNode;
      const orgTextNode = this.astBuilder.createTextNode(
        this.tokenIterator.currentValue
      );
      lastParent.addChild(orgTextNode);
      return orgTextNode;
    }
    this.astBuilder.lastNode.appendValue(this.tokenIterator.currentValue);
  }
}
