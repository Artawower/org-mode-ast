import {
  NodeType,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class LatexEnvironmentHandler implements OrgHandler {
  readonly #latexOpenedBracket = '{';
  readonly #latexClosedBracket = '}';
  readonly #latexBrackets: string[] = [
    this.#latexOpenedBracket,
    this.#latexClosedBracket,
  ];

  readonly #latexEnvironmentBlocks = {
    begin: '\\begin',
    end: '\\end',
  };

  #beginLatexEnvironmentKeyword: OrgNode;
  #endLatexEnvironmentKeyword: OrgNode;

  #beginLatexBracket: OrgNode;

  constructor(
    private readonly configuration: ParserConfiguration,
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
    if (this.#isLatexBrackets()) {
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

    if (!isOpenedBracket && this.#endLatexEnvironmentKeyword) {
      const mergedLatexEnvironmentNode =
        this.#mergeLatexEnvironmentNodes(orgNode);
      this.astBuilder.attachToTree(mergedLatexEnvironmentNode);
      this.#endLatexEnvironmentKeyword = undefined;
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
    if (!this.#beginLatexEnvironmentKeyword && latexNameNode.prev) {
      this.astBuilder.mergeNeighborsNodesWithSameType(latexNameNode.prev);
      return latexNameNode.prev;
    }
  }

  #handleOpenedLatexBracket(): OrgNode {
    if (this.#beginLatexBracket) {
      this.#mergePreviousLatexBracket();
    }
    this.#beginLatexBracket = this.astBuilder.createUnresolvedNode();
    return this.#beginLatexBracket;
  }

  #mergePreviousLatexBracket(): void {
    this.#beginLatexBracket.type = NodeType.Text;
    this.astBuilder.mergeNeighborsNodesWithSameType(
      this.#beginLatexBracket,
      NodeType.Text
    );
    this.#beginLatexBracket = undefined;
  }

  #handleClosedLatexBracket(): OrgNode {
    if (!this.#beginLatexBracket) {
      return this.astBuilder.createTextNode(this.tokenIterator.currentValue);
    }
    const closedBracket = this.astBuilder.createUnresolvedNode(
      this.tokenIterator.currentValue
    );
    const realParent = this.#beginLatexBracket.parent;
    const nodesBetweenBrackets = realParent.children.getNodesBetweenPairs(
      this.#beginLatexBracket,
      null,
      true
    );
    this.astBuilder.lastNode =
      this.#beginLatexBracket.prev ?? this.#beginLatexBracket.parent;

    realParent.removeChildren(nodesBetweenBrackets);
    nodesBetweenBrackets.push(closedBracket);
    const rawValue = this.astBuilder.getRawValueFromNodes(nodesBetweenBrackets);
    const textNode = this.astBuilder.createTextNode(rawValue);

    this.#beginLatexBracket = null;
    return textNode;
  }

  #mergeLatexEnvironmentNodes(latexNameNode: OrgNode): OrgNode {
    const realParent = this.#beginLatexEnvironmentKeyword.parent;
    const nodesBetweenLatexEnvironmentKeywords =
      realParent.children.getNodesBetweenPairs(
        this.#beginLatexEnvironmentKeyword,
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
      this.#beginLatexEnvironmentKeyword = orgNode;
    }

    if (
      this.#beginLatexEnvironmentKeyword &&
      this.isEndLatexEnvironmentKeyword
    ) {
      this.#endLatexEnvironmentKeyword = orgNode;
    }

    this.astBuilder.attachToTree(orgNode);
    return orgNode;
  }

  #isLatexBrackets(): boolean {
    return this.#latexBrackets.includes(this.tokenIterator.currentValue);
  }

  public isLatexEnvironmentKeyword(keyword: string): boolean {
    return Object.values(this.#latexEnvironmentBlocks).includes(keyword);
  }

  public handleEndOfFile(): void {
    if (this.#beginLatexEnvironmentKeyword) {
      this.astBuilder.mergeNeighborsNodesWithSameType(
        this.#beginLatexEnvironmentKeyword,
        NodeType.Text
      );
      return;
    }
    if (this.#beginLatexBracket) {
      this.astBuilder.mergeNeighborsNodesWithSameType(
        this.#beginLatexBracket,
        NodeType.Text
      );
    }
  }
}
