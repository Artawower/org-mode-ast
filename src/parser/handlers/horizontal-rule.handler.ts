import {
  OrgHandler,
  OrgNode,
  ParserConfiguration,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class HorizontalRuleHandler implements OrgHandler {
  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly astBuilder: AstBuilder,
    private readonly tookenInterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const node = this.astBuilder.createHorizontalRuleNode(
      this.tookenInterator.currentValue
    );
    this.astBuilder.attachToTree(node);
    return node;
  }
}
