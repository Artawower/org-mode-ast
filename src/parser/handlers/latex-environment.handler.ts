import {
  NodeType,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';
import { AstContext } from 'parser/ast-context.js';

export class LatexEnvironmentHandler implements OrgHandler {
  readonly #latexOpenedBracket = '{';
  readonly #latexClosedBracket = '}';

  readonly #latexEnvironmentBlocks = {
    begin: '\\begin',
    end: '\\end',
  };

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {}

  get isBeginLatexEnvironmentKeyword(): boolean {
    return (
      this.tokenIterator.currentValue === this.#latexEnvironmentBlocks.begin
    );
  }

  get isEndLatexEnvironmentKeyword(): boolean {
    return this.tokenIterator.currentValue === this.#latexEnvironmentBlocks.end;
  }

  public handle(): OrgNode {
    const isLatexBracket = this.#isLatexBrackets();
    if (isLatexBracket) {
      return this.#handleLatexBracket();
    }

    return this.#handleLatexEnvironmentKeyword();
  }

  #handleLatexBracket(): OrgNode {
    const isOpenedBracket =
      this.tokenIterator.currentValue === this.#latexOpenedBracket;
    const orgNode = isOpenedBracket
      ? this.#handleOpenedLatexBracket()
      : this.#handleClosedLatexBracket();

    if (!orgNode && !isOpenedBracket) {
      const textNode = this.astBuilder.attachToTree(
        this.astBuilder.createTextNode(this.tokenIterator.currentValue)
      );
      return textNode;
    }

    if (!orgNode) {
      return;
    }

    if (!isOpenedBracket && this.ctx.endLatexEnvironmentKeyword) {
      this.astBuilder.attachToTree(orgNode);
      const mergedLatexEnvironmentNode =
        this.#mergeLatexEnvironmentNodes(orgNode);

      this.astBuilder.attachToTree(mergedLatexEnvironmentNode);

      this.ctx.endLatexEnvironmentKeyword = undefined;
      return mergedLatexEnvironmentNode;
    }

    this.astBuilder.attachToTree(orgNode);
    const mergedNode = this.#tryMergeInconsistentLatexNodes(orgNode);
    return mergedNode ?? orgNode;
  }

  #tryMergeInconsistentLatexNodes(latexNameNode: OrgNode): OrgNode {
    const isOpenedBracket =
      this.tokenIterator.currentValue === this.#latexOpenedBracket;

    if (isOpenedBracket) {
      return;
    }
    if (
      !this.ctx.beginLatexEnvironmentKeyword &&
      latexNameNode.prev &&
      latexNameNode.prev.isNot(NodeType.Unresolved)
    ) {
      // TODO: master here!!
      this.astBuilder.mergeNeighborsNodesWithSameType(
        this.ctx.beginLatexEnvironmentKeyword
      );
      return latexNameNode;
    }
  }

  #handleOpenedLatexBracket(): OrgNode {
    if (this.ctx.beginLatexBracket) {
      this.#mergePreviousLatexBracket();
    }

    this.ctx.beginLatexBracket = this.astBuilder.createUnresolvedNode();
    return this.ctx.beginLatexBracket;
  }

  #mergePreviousLatexBracket(): void {
    this.ctx.beginLatexBracket.type = NodeType.Text;
    this.astBuilder.mergeNeighborsNodesWithSameType(
      this.ctx.beginLatexBracket,
      NodeType.Text
    );
    this.ctx.beginLatexBracket = undefined;
  }

  #handleClosedLatexBracket(): OrgNode {
    if (!this.ctx.beginLatexBracket) {
      const node = this.astBuilder.createTextNode(
        this.tokenIterator.currentValue
      );
      return node;
      // this.astBuilder.mergeNeighborsNodesWithSameType(node);
      // return this.astBuilder.lastNode;
    }
    const closedBracket = this.astBuilder.createUnresolvedNode(
      this.tokenIterator.currentValue
    );
    const realParent = this.ctx.beginLatexBracket.parent;
    if (!realParent) {
      return;
    }
    const nodesBetweenBrackets = realParent.children.getNodesBetweenPairs(
      this.ctx.beginLatexBracket,
      null,
      true
    );
    this.astBuilder.lastNode =
      this.ctx.beginLatexBracket.prev ?? this.ctx.beginLatexBracket.parent;

    realParent.removeChildren(nodesBetweenBrackets);
    nodesBetweenBrackets.push(closedBracket);
    const rawValue = this.astBuilder.getRawValueFromNodes(nodesBetweenBrackets);
    const textNode = this.astBuilder.createTextNode(rawValue);
    this.ctx.beginLatexBracket = null;
    return textNode;
  }

  #mergeLatexEnvironmentNodes(latexNameNode: OrgNode): OrgNode {
    const realParent = this.ctx.beginLatexEnvironmentKeyword.parent;
    const nodesBetweenLatexEnvironmentKeywords =
      realParent.children.getNodesBetweenPairs(
        this.ctx.beginLatexEnvironmentKeyword,
        latexNameNode,
        true
      );
    const rawValue = this.astBuilder.getRawValueFromNodes(
      nodesBetweenLatexEnvironmentKeywords
    );
    const latexEnvironmentNode =
      this.astBuilder.createLatexEnvironmentNode(rawValue);

    // TODO: master this logic should be moved to ast builder. When we remove nodes,
    // we need to change last node
    this.astBuilder.lastNode =
      nodesBetweenLatexEnvironmentKeywords.first.prev ||
      nodesBetweenLatexEnvironmentKeywords.first.parent;

    realParent.removeChildren(nodesBetweenLatexEnvironmentKeywords);

    return latexEnvironmentNode;
  }

  #handleLatexEnvironmentKeyword(): OrgNode {
    const orgNode = this.astBuilder.createTextNode(
      this.tokenIterator.currentValue
    );

    if (this.isBeginLatexEnvironmentKeyword) {
      this.ctx.beginLatexEnvironmentKeyword = orgNode;
    }

    if (
      this.ctx.beginLatexEnvironmentKeyword &&
      this.isEndLatexEnvironmentKeyword
    ) {
      this.ctx.endLatexEnvironmentKeyword = orgNode;
    }

    this.astBuilder.attachToTree(orgNode);
    return orgNode;
  }

  #isLatexBrackets(): boolean {
    const isClosedLatexBracket =
      this.tokenIterator.currentValue === this.#latexClosedBracket;
    const isOpenedLatexBracket =
      this.astBuilder.lastNode?.value === '\\begin' &&
      this.tokenIterator.currentValue === this.#latexOpenedBracket;
    return isClosedLatexBracket || isOpenedLatexBracket;
  }

  public isLatexEnvironmentKeyword(keyword: string): boolean {
    return Object.values(this.#latexEnvironmentBlocks).includes(keyword);
  }

  public handleEndOfFile(): void {
    if (this.ctx.beginLatexEnvironmentKeyword) {
      this.astBuilder.mergeNeighborsNodesWithSameType(
        this.ctx.beginLatexEnvironmentKeyword
      );
      return;
    }
    if (this.ctx.beginLatexBracket) {
      this.astBuilder.mergeNeighborsNodesWithSameType(
        this.ctx.beginLatexBracket
      );
    }
  }
}
