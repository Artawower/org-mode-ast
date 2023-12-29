import {
  Block,
  BlockPosition,
  BlockType,
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
  SrcBlockMetaInfo,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { AstContext } from '../ast-context.js';
import { TokenIterator } from '../../tokenizer/index.js';

// TODO: master this class should be refactored!
export class BlockHandler implements OrgHandler {
  private blockTypeToNode: {
    [key: string]: Block;
  } = {
    src: NodeType.SrcBlock,
    quote: NodeType.QuoteBlock,
    html: NodeType.HtmlBlock,
    export: NodeType.ExportBlock,
    comment: NodeType.CommentBlock,
    example: NodeType.ExampleBlock,
  };

  private blockHanderls: {
    [key: string]: (pos: BlockPosition, type: string) => OrgNode;
  } = {
    src: (pos, type) => this.handleRawBlock(pos, type),
    quote: (pos, type) => this.handleBlockWithFormat(pos, type),
    html: (pos, type) => this.handleRawBlock(pos, type),
    export: (pos, type) => this.handleRawBlock(pos, type),
    comment: (pos, type) => this.handleBlockWithFormat(pos, type),
    example: (pos, type) => this.handleRawBlock(pos, type),
  };

  constructor(
    private ctx: AstContext,
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const [blockPosition, blockType] = this.determineBlockType();
    const fallbackHandler = (pos: BlockPosition, type: string) =>
      this.handleBlockWithFormat(pos, type);
    const blockHandler =
      this.blockHanderls[blockType.toLowerCase()] ?? fallbackHandler;

    const node = blockHandler(blockPosition, blockType);
    return node;
  }

  public isBlockKeyword(keyword: string): boolean {
    return ['#+begin_', '#+end_'].some((prefix) =>
      keyword.toLowerCase().startsWith(prefix)
    );
  }

  /**
   * Handling blocks that don't need nested formatting
   */
  private handleRawBlock(position: BlockPosition, type: BlockType): OrgNode {
    const keywordTextNode = this.astBuilder.createText();
    const keywordNode = this.astBuilder.createKeywordNode(keywordTextNode);

    if (position === 'begin') {
      this.ctx.srcBlockBegin = keywordNode;
      this.astBuilder.attachToTree(keywordNode);
      return keywordNode;
    }

    if (position === 'end' && this.ctx.srcBlockBegin) {
      this.mergeNodesBetweenBlockKeywords(this.ctx.srcBlockBegin, type, true);
      this.ctx.resetSrcBlockInfo();
      this.ctx.resetLatexEnvironmentInfo();
    }

    this.astBuilder.attachToTree(keywordNode);
    return keywordNode;
  }

  private mergeNodesBetweenBlockKeywords(
    blockBeginNode: OrgNode,
    type: BlockType,
    bodyAsText: boolean
  ): void {
    const parentNode = blockBeginNode?.parent;

    const blockNodes = parentNode.children.getNodesBetweenPairs(
      blockBeginNode,
      null,
      true
    );

    // note: include indent before block begin node
    if (blockBeginNode?.prev?.is(NodeType.Indent)) {
      blockNodes.unshift(blockBeginNode.prev);
    }

    parentNode.removeChildren(blockNodes);

    const nestedBlockNodes = this.buildNestedBlockNodes(blockNodes, bodyAsText);

    const metaInfo = this.getBlockHeaderMetaInfo(nestedBlockNodes.first);

    const rawBlock = this.astBuilder.createBlockNode(
      this.blockTypeToNode[type.toLowerCase()],
      metaInfo
    );

    rawBlock.setChildren(nestedBlockNodes);
    parentNode.addChild(rawBlock);
    this.ctx.exitSectionByRange(rawBlock.start, rawBlock.end);
    this.ctx.exitNestedListInRanges(rawBlock.start, rawBlock.end);
  }

  private buildNestedBlockNodes(
    blockNodes: OrgChildrenList,
    bodyAsText: boolean
  ): OrgChildrenList {
    const nestedBlocks = new OrgChildrenList();
    const buffer = new OrgChildrenList();

    blockNodes.forEach((node, _, last) => {
      const isHeaderInit =
        !nestedBlocks.first?.is(NodeType.BlockHeader) &&
        node.is(NodeType.NewLine);

      if (isHeaderInit) {
        const blockHeaderNode = this.buildHeaderNode(buffer);
        nestedBlocks.push(blockHeaderNode);
        nestedBlocks.push(node);
        buffer.clear();
        return;
      }
      buffer.push(node);
    });

    const lastNewLine = buffer.last?.is(NodeType.NewLine) ? buffer.pop() : null;
    const additionalFooterChildren = buffer.last?.is(NodeType.Indent)
      ? [buffer.pop()]
      : null;

    const nestedBodyNodes = this.getBlockBodyNodes(buffer, bodyAsText);
    // const value = this.astBuilder.getRawValueFromNodes(buffer);

    if (nestedBodyNodes) {
      // const blockBodyText = this.astBuilder.createTextNode(value);
      const blockBodyNode =
        this.astBuilder.createBlockBodyNode(nestedBodyNodes);
      nestedBlocks.push(blockBodyNode);
    }

    if (lastNewLine) {
      nestedBlocks.push(lastNewLine);
    }

    const blockFooterNode = this.astBuilder.createBlockFooterNode(
      additionalFooterChildren
    );

    nestedBlocks.push(blockFooterNode);

    this.astBuilder.lastNode = blockFooterNode;
    return nestedBlocks;
  }

  private getBlockBodyNodes(
    nodes: OrgChildrenList,
    bodyAsText: boolean
  ): OrgChildrenList | OrgNode[] {
    if (!bodyAsText) {
      return nodes;
    }
    const value = this.astBuilder.getRawValueFromNodes(nodes);
    if (value) {
      return [this.astBuilder.createTextNode(value)];
    }
  }

  // TODO: master refactor 😭
  private buildHeaderNode(nodes: OrgChildrenList): OrgNode {
    const headerNodes = new OrgChildrenList();

    nodes.forEach((node) => {
      const isKeyword = node.is(NodeType.Keyword);
      const isSrcBlockLanguage =
        node.children?.length === 2 &&
        node.is(NodeType.Keyword) &&
        node.children.last.is(NodeType.Text);

      if (isSrcBlockLanguage) {
        node.children.last.type = NodeType.SrcLanguage;
        node.setProperties({
          language: node.children.last.value.trim(),
        });
      }
      const isEmpty = this.isHeaderNodesEmpty(headerNodes);

      if (isEmpty && (node.is(NodeType.Indent) || isKeyword)) {
        headerNodes.push(node);
        return;
      }
      const isLastNodeBlockProperty = headerNodes.last?.is(
        NodeType.BlockProperty
      );
      const isText = node.is(NodeType.Text);

      if (node.is(NodeType.BlockProperty)) {
        headerNodes.push(node);
        return;
      }

      if (isLastNodeBlockProperty && isText) {
        headerNodes.last?.addChild(node);
        headerNodes.last.setProperties({
          [headerNodes.last.children.first.value.slice(1)]: node.value.trim(),
        });
      }
    });
    return this.astBuilder.createBlockHeaderNode(headerNodes);
  }

  private isHeaderNodesEmpty(nodes: OrgChildrenList): boolean {
    return (
      nodes.isEmpty || (nodes.length === 1 && nodes.first.is(NodeType.Indent))
    );
  }

  private getBlockHeaderMetaInfo(headerNode: OrgNode): SrcBlockMetaInfo {
    let metaInfo: SrcBlockMetaInfo = {};

    headerNode.children.forEach((node) => {
      if (node.properties) {
        metaInfo = { ...metaInfo, ...node.properties };
      }
    });

    return metaInfo;
  }

  private handleBlockWithFormat(
    position: BlockPosition,
    type: BlockType
  ): OrgNode {
    const keywordTextNode = this.astBuilder.createText();
    const keywordNode = this.astBuilder.createKeywordNode(keywordTextNode);

    if (position === 'begin') {
      this.ctx.blockBegin = keywordNode;
      this.astBuilder.attachToTree(keywordNode);
      return keywordNode;
    }

    if (position === 'end' && this.ctx.blockBegin) {
      this.mergeNodesBetweenBlockKeywords(this.ctx.blockBegin, type, false);
      this.ctx.resetQuoteBlockInfo();
    }

    this.astBuilder.attachToTree(keywordNode);
    return keywordNode;
  }

  private determineBlockType(): [BlockPosition, BlockType] {
    const [pos, type] = this.tokenIterator.currentValue.split('_');
    return [
      pos.slice(2, pos.length).toLowerCase() as BlockPosition,
      type.trim(),
    ];
  }

  public handleBlockProperty(): OrgNode {
    const textNode = this.astBuilder.createTextNode(
      this.tokenIterator.currentValue
    );
    const blockPropertyNode = this.astBuilder.createBlockPropertyNode(textNode);
    this.astBuilder.attachToTree(blockPropertyNode);
    return blockPropertyNode;
  }
}
