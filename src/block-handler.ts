import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { OrgHandler } from 'internal.types';
import { OrgChildrenList } from 'org-children-list';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { BlockPosition, BlockType, NodeType, SrcBlockMetaInfo } from 'types';

// TODO: master this class should be refactored!
export class BlockHandler implements OrgHandler {
  constructor(
    private ctx: AstContext,
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  private blockHanderls: {
    [key: string]: (pos: BlockPosition, type: string) => OrgNode;
  } = {
    src: (pos, type) => this.handleRawBlock(pos, type),
    html: (pos, type) => this.handleRawBlock(pos, type),
  };

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
      this.mergeNodesBetweenRawBlockKeywords(type);
      this.ctx.resetSrcBlockInfo();
    }

    this.astBuilder.attachToTree(keywordNode);
    return keywordNode;
  }

  private mergeNodesBetweenRawBlockKeywords(type: BlockType): void {
    const parentNode = this.ctx.srcBlockBegin.parent;
    const blockParent = this.ctx.srcBlockBegin.parent;

    const blockNodes = blockParent.children.getNodesBetweenPairs(
      this.ctx.srcBlockBegin,
      null,
      true
    );

    blockParent.removeChildren(blockNodes);

    const nestedBlockNodes = this.buildNestedBlockNodes(blockNodes);

    const metaInfo = this.getBlockHeaderMetaInfo(nestedBlockNodes.first);

    const rawBlock = this.astBuilder.createBlockNode(
      type.toLowerCase() === 'src' ? NodeType.SrcBlock : NodeType.HtmlBlock,
      metaInfo
    );

    rawBlock.setChildren(nestedBlockNodes);
    parentNode.addChild(rawBlock);
  }

  private buildNestedBlockNodes(blockNodes: OrgChildrenList): OrgChildrenList {
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

    const value = this.astBuilder.getRawValueFromNodes(buffer);

    if (value) {
      const blockBodyText = this.astBuilder.createTextNode(value);
      const blockBodyNode = this.astBuilder.createBlockBodyNode([
        blockBodyText,
      ]);
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

  /** Collection information about block headline
   *
   * @params node - node to collect information about
   * return list of headline nodes and next node to end of headline
   */
  // private collectHeadline(node: OrgNode): {
  //   headerChildren: OrgNode[];
  //   nextNode: OrgNode;
  //   metaInfo: SrcBlockMetaInfo;
  // } {
  //   const headerChildren: OrgNode[] = [];
  //   const metaInfo: { [key: string]: string } = {};

  //   while (node) {
  //     const value = node?.value;

  //     if (
  //       headerChildren.length === 1 &&
  //       !value?.trim().startsWith(':') &&
  //       value.trim()
  //     ) {
  //       metaInfo.language = value?.trim();
  //     }

  //     const lastPotentialKeyword = headerChildren[headerChildren.length - 1];

  //     if (lastPotentialKeyword?.value.startsWith(':') && value) {
  //       metaInfo[lastPotentialKeyword.value.slice(1)] = value.trim();
  //     }

  //     headerChildren.push(node);

  //     if (node.type === NodeType.NewLine) {
  //       return {
  //         headerChildren,
  //         nextNode: node.next,
  //         metaInfo,
  //       };
  //     }
  //     node = node.next;
  //   }
  // }

  private determineBlockType(): [BlockPosition, BlockType] {
    const [pos, type] = this.tokenIterator.currentValue.split('_');
    return [pos.slice(2, pos.length).toLowerCase() as BlockPosition, type];
  }
}
