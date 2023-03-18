import {
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
  TokenType,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder';
import { AstContext } from '../ast-context';
import { TokenIterator } from '../../tokenizer/index.js';

export class ColonHandler implements OrgHandler {
  readonly #fixedWidthOperator = ': ';
  readonly #colonOperator = ':';
  readonly #colonOperators = [this.#fixedWidthOperator, this.#colonOperator];
  readonly #potentialTagOperators = new OrgChildrenList();

  #lastFixedWidthNode: OrgNode;

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {}

  get isFixedWidthOperator(): boolean {
    return this.tokenIterator.currentValue === this.#fixedWidthOperator;
  }

  get onHold(): boolean {
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

    const operatorNode = this.astBuilder.createOperatorNode(
      this.tokenIterator.currentValue
    );

    console.log(
      '✎: [line 51][colon.handler.ts] this.astBuilder.lastNode: ',
      this.astBuilder.lastNode
    );
    if (this.ctx.insideHeadline || this.ctx.insideKeyword) {
      this.#potentialTagOperators.push(operatorNode);
    }

    return operatorNode;
  }

  public handleHolded(): OrgNode {
    if (!this.isColonOperator(this.tokenIterator.currentValue)) {
      this.appendFixedWidthContent();
    }
    return;
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
    this.#buildTags();
  }

  #buildTags(): void {
    if (this.#potentialTagOperators.isEmpty) {
      return;
    }

    const firstColonOperator = this.#potentialTagOperators.first;
    const lastColonOperator = this.#potentialTagOperators.last;
    console.log(
      '✎: [line 118][colon.handler.ts] lastColonOperator: ',
      lastColonOperator
    );
    const isLastNodeEol =
      !lastColonOperator.next || lastColonOperator.is(NodeType.NewLine);
    const realParent = firstColonOperator.parent;

    const nodesBetweenColons = realParent.children.getNodesBetweenPairs(
      firstColonOperator,
      lastColonOperator,
      true
    );

    if (!isLastNodeEol) {
      // console.log(
      //   '✎: [line 130][colon.handler.ts] nodesBetweenColons: ',
      //   nodesBetweenColons
      // );
      this.#makeTextFromHeadlineColons(nodesBetweenColons);
      // console.log(
      //   '✎: [line 131][colon.handler.ts] nodesBetweenColons: ',
      //   nodesBetweenColons
      // );
      // this.astBuilder.lastNode = nodesBetweenColons.first?.prev;
      // this.astBuilder.attachToTree(nodesBetweenColons.first);
      return;
    }

    // TODO: master last node should be set automatically when remove children!
    this.astBuilder.lastNode =
      firstColonOperator.prev ?? firstColonOperator.parent;
    realParent.removeChildren(nodesBetweenColons);

    const tagListNode = this.astBuilder.createTagListNode();

    tagListNode.addChildren(nodesBetweenColons);

    this.#potentialTagOperators.clear();
    this.astBuilder.attachToTree(tagListNode);

    this.#potentialTagOperators.clear();
  }

  #makeTextFromHeadlineColons(nodesBetweenColons: OrgChildrenList): void {
    console.log(
      '✎: [line 150][colon.handler.ts] nodesBetweenColons.first: ',
      nodesBetweenColons.first
    );
    this.astBuilder.mergeNeighborsNodesWithSameType(
      nodesBetweenColons.first,
      NodeType.Operator,
      NodeType.Text
    );
    console.log(
      '✎: [line 155][colon.handler.ts] nodesBetweenColons.first: ',
      nodesBetweenColons.first
    );
    console.log(
      '✎: [line 172][colon.handler.ts] nodesBetweenColons: ',
      nodesBetweenColons
    );
    this.#potentialTagOperators.clear();
  }
}
