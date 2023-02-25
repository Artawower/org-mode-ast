import { OrgHandler, OrgNode, ParserConfiguration } from 'models';
import { AstBuilder } from 'parser/ast-builder';
import { TokenIterator } from 'tokenizer';

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
