import {
  BlockPosition,
  BlockType,
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
  SrcBlockMetaInfo,
} from 'models';
import { AstBuilder } from 'parser/ast-builder';
import { AstContext } from 'parser/ast-context';
import { TokenIterator } from 'tokenizer';

// TODO: master this class should be refactored!
export class BlockHandler implements OrgHandler {
  private blockTypeToNode: {
    [key: string]: NodeType.HtmlBlock | NodeType.SrcBlock | NodeType.QuoteBlock;
  } = {
    src: NodeType.SrcBlock,
    quote: NodeType.QuoteBlock,
    html: NodeType.HtmlBlock,
  };

  private blockHanderls: {
    [key: string]: (pos: BlockPosition, type: string) => OrgNode;
  } = {
    src: (pos, type) => this.handleRawBlock(pos, type),
    html: (pos, type) => this.handleRawBlock(pos, type),
    quote: (pos, type) => this.handleQuoteBlock(pos, type),
  };

  constructor(
    private ctx: AstContext,
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const [blockPosition, blockType] = this.determineBlockType();
    const blockHandler = this.blockHanderls[blockType.toLowerCase()];

    if (!blockHandler) {
      throw new Error(`Block type ${blockType} is not supported`);
    }

    return blockHandler(blockPosition, blockType);
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
    const keywordNode = this.astBuilder.createKeyword();

    if (position === 'begin') {
      this.ctx.srcBlockBegin = keywordNode;
      this.astBuilder.attachToTree(keywordNode);
      return keywordNode;
    }

    if (position === 'end' && this.ctx.srcBlockBegin) {
      this.mergeNodesBetweenBlockKeywords(this.ctx.srcBlockBegin, type, true);
      this.ctx.resetSrcBlockInfo();
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

    parentNode.removeChildren(blockNodes);

    const nestedBlockNodes = this.buildNestedBlockNodes(blockNodes, bodyAsText);

    const metaInfo = this.getBlockHeaderMetaInfo(nestedBlockNodes.first);

    const rawBlock = this.astBuilder.createBlockNode(
      this.blockTypeToNode[type.toLowerCase()],
      metaInfo
    );

    rawBlock.setChildren(nestedBlockNodes);
    parentNode.addChild(rawBlock);
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

    const blockFooterNode = this.astBuilder.createBlockFooterNode();

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

  private buildHeaderNode(nodes: OrgChildrenList): OrgNode {
    const headerNodes = new OrgChildrenList();

    nodes.forEach((node) => {
      const isKeyword = node.is(NodeType.Keyword);
      if (isKeyword && headerNodes.isEmpty) {
        headerNodes.push(node);
        return;
      }
      const isLastNodeBlockProperty = headerNodes.last?.is(
        NodeType.BlockProperty
      );
      const isText = node.is(NodeType.Text);

      if (headerNodes.last?.is(NodeType.Keyword) && isText) {
        headerNodes.push(this.astBuilder.createBlockLanguageNode(node.value));
        return;
      }

      if (isKeyword) {
        headerNodes.push(this.astBuilder.createBlockPropertyNode(node));
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

  private getBlockHeaderMetaInfo(headerNode: OrgNode): SrcBlockMetaInfo {
    let metaInfo: SrcBlockMetaInfo = {};

    headerNode.children.forEach((node) => {
      if (node.properties) {
        metaInfo = { ...metaInfo, ...node.properties };
      }
    });

    return metaInfo;
  }

  private handleQuoteBlock(position: BlockPosition, type: BlockType): OrgNode {
    const keywordNode = this.astBuilder.createKeyword();

    if (position === 'begin') {
      this.ctx.quoteBlockBegin = keywordNode;
      this.astBuilder.attachToTree(keywordNode);
      return keywordNode;
    }

    if (position === 'end' && this.ctx.quoteBlockBegin) {
      this.mergeNodesBetweenBlockKeywords(
        this.ctx.quoteBlockBegin,
        type,
        false
      );
      this.ctx.resetQuoteBlockInfo();
    }

    this.astBuilder.attachToTree(keywordNode);
    return keywordNode;
  }

  private determineBlockType(): [BlockPosition, BlockType] {
    const [pos, type] = this.tokenIterator.currentValue.split('_');
    return [pos.slice(2, pos.length).toLowerCase() as BlockPosition, type];
  }
}
