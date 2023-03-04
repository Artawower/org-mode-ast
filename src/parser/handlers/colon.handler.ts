import {
  NodeType,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
  TokenType,
} from 'models';
import { AstBuilder } from 'parser/ast-builder';
import { TokenIterator } from 'tokenizer';

export class ColonHandler implements OrgHandler {
  readonly #fixedWidthOperator = ': ';
  readonly #colonOperator = ':';
  readonly #colonOperators = [this.#fixedWidthOperator, this.#colonOperator];

  #lastFixedWidthNode: OrgNode;

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {}

  get isFixedWidthOperator(): boolean {
    return this.tokenIterator.currentValue === this.#fixedWidthOperator;
  }

  get holdOn(): boolean {
    if (this.tokenIterator.token.isType(TokenType.NewLine)) {
      this.#lastFixedWidthNode = null;
      return;
    }
    return !!this.#lastFixedWidthNode;
  }

  // TODO also check is not a tag and opened propery drawer
  public handle(): OrgNode {
    if (this.isFixedWidthOperator) {
      return this.createFixedWidthNode();
    }

    if (this.isColonOperator(this.tokenIterator.currentValue)) {
      const unresolvedNode = this.astBuilder.createUnresolvedNode();
      return unresolvedNode;
    }

    this.appendFixedWidthContent();
  }

  public isColonOperator(operator: string): boolean {
    return this.#colonOperators.includes(operator);
  }

  private appendFixedWidthContent(): void {
    if (!this.#lastFixedWidthNode.children.last.is(NodeType.Text)) {
      const textNode = this.astBuilder.createText();
      this.#lastFixedWidthNode.addChild(textNode);
      this.astBuilder.lastNode = textNode;
      return;
    }
    const lastTextNode = this.#lastFixedWidthNode.children.last;
    lastTextNode.appendValue(this.tokenIterator.currentValue);
  }

  private createFixedWidthNode(): OrgNode {
    this.#lastFixedWidthNode = this.getOrCreateFixedWidthNode();
    const operator = this.astBuilder.createOperatorNode(
      this.tokenIterator.currentValue
    );
    this.#lastFixedWidthNode.addChild(operator);
    return this.#lastFixedWidthNode;
  }

  private getOrCreateFixedWidthNode(): OrgNode {
    if (this.astBuilder.lastNode.is(NodeType.FixedWidth)) {
      return this.astBuilder.lastNode;
    }
    if (this.astBuilder.lastNode.parent?.is(NodeType.FixedWidth)) {
      return this.astBuilder.lastNode.parent;
    }
    return this.astBuilder.createFixedWidthNode();
  }

  public handleNewLine(): void {
    this.#lastFixedWidthNode = null;
  }
}
