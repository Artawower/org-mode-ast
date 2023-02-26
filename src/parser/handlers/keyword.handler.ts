import {
  OrgHandler,
  OrgNode,
  ParserConfiguration,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { AstContext } from '../ast-context.js';
import { TokenIterator } from '../../tokenizer/index.js';
import { BlockHandler } from './block.handler.js';
import { PropertiesHandler } from './properties.handler.js';

export class KeywordHandler implements OrgHandler {
  private lastKeyword: OrgNode;

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator,
    private readonly blockHandler: BlockHandler,
    private readonly propertiesHandler: PropertiesHandler
  ) {}

  public handle(): OrgNode {
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
    const createdKeyword = this.astBuilder.createKeywordNode(textNode);
    this.lastKeyword = createdKeyword;
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

  public handleEndOfLine(): void {
    this.lastKeyword?.calculateNodeProperties();
    this.resetLastStoredKeyword();
  }

  private resetLastStoredKeyword(): void {
    this.lastKeyword = null;
  }
}
