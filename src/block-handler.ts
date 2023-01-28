import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { BlockPosition, BlockType, NodeType, OrgStruct, SrcBlockMetaInfo } from 'types';

// TODO: master this class should be refactored!
export class BlockHandler implements OrgHandler {
  constructor(private ctx: AstContext, private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  private blockHanderls: { [key: string]: (pos: BlockPosition, type: string) => OrgNode<OrgStruct> } = {
    src: (pos, type) => this.handleRawBlock(pos, type),
    html: (pos, type) => this.handleRawBlock(pos, type),
  };

  public handle(): OrgNode<OrgStruct> {
    const [blockPosition, blockType] = this.determineBlockType();
    const blockHandler = this.blockHanderls[blockType.toLowerCase()];

    if (!blockHandler) {
      throw new Error(`Block type ${blockType} is not supported`);
    }

    // console.log('✎: [line 25][block-handler.ts] blockPosition: ', blockPosition);
    return blockHandler(blockPosition, blockType);
  }

  public isBlockKeyword(keyword: string): boolean {
    return ['#+begin_', '#+end_'].some((prefix) => keyword.toLowerCase().startsWith(prefix));
  }

  /**
   * Handling blocks that don't need nested formatting
   */
  private handleRawBlock(position: BlockPosition, type: BlockType): OrgNode<OrgStruct> {
    const keywordNode = this.astBuilder.createKeyword();

    if (position.toLowerCase() === 'begin') {
      this.ctx.srcBlockBegin = keywordNode;
      this.astBuilder.attachToTree(keywordNode);
      this.ctx.srcBlockChildIndex = keywordNode.parent.children.length - 1;
      // console.log('✎: [line 40][block-handler.ts] this.ctx.srcBlockChildIndex: ', this.ctx.srcBlockChildIndex);
      return keywordNode;
    }

    if (position.toLowerCase() === 'end' && this.ctx.srcBlockBegin) {
      this.mergeNodesBetweenRawBlockKeywords(type);
      this.ctx.resetSrcBlockInfo();
    }

    this.astBuilder.attachToTree(keywordNode);
    return keywordNode;
  }

  private mergeNodesBetweenRawBlockKeywords(type: BlockType): void {
    let start: number;
    let end = this.ctx.srcBlockBegin.end;
    let value = '';
    const parentNode = this.ctx.srcBlockBegin.parent;

    let lastNewLine: OrgNode;

    const headlineData = this.collectHeadline(this.ctx.srcBlockBegin);
    const { headerChildren, metaInfo } = headlineData;
    let { nextNode } = headlineData;

    if (nextNode) {
      start = nextNode.start;
    }

    while (nextNode) {
      end = nextNode.end;

      if (!nextNode.next) {
        lastNewLine = nextNode;
        break;
      }

      value += this.astBuilder.getRawValueFromNode(nextNode);
      nextNode = nextNode.next;
    }

    const rawBlock = this.astBuilder.createBlockNode(
      type.toLowerCase() === 'src' ? NodeType.SrcBlock : NodeType.HtmlBlock,
      this.ctx.srcBlockBegin.start,
      end,
      this.ctx.srcBlockBegin.prev,
      null,
      metaInfo
    );

    const headerNewLineNode = headerChildren.pop();
    const blockHeader = this.astBuilder.createBlockHeaderNode(rawBlock, headerChildren);
    rawBlock.setChildren([blockHeader, headerNewLineNode]);

    if (value) {
      const orgText = this.astBuilder.createTextNode(start, value);
      const blockBody = this.astBuilder.createBlockBodyNode(rawBlock, [orgText]);
      rawBlock.addChild(blockBody);
      lastNewLine.setPrev(blockBody);
      rawBlock.addChild(lastNewLine);
    }

    const blockFooterNode = this.astBuilder.createBlockFooterNode(rawBlock, [], rawBlock.lastChild?.end);

    rawBlock.addChild(blockFooterNode);

    rawBlock.parent = parentNode;
    rawBlock.setPrev(this.ctx.srcBlockBegin.prev);
    parentNode.children.splice(this.ctx.srcBlockChildIndex);
    parentNode.addChild(rawBlock);
    this.astBuilder.lastNode = blockFooterNode;
    // console.log('✎: [line 55][block-handler.ts] value: ', value);
  }

  /** Collection information about block headline
   *
   * @params node - node to collect information about
   * return list of headline nodes and next node to end of headline
   */
  private collectHeadline(node: OrgNode<OrgStruct>): {
    headerChildren: OrgNode<OrgStruct>[];
    nextNode: OrgNode<OrgStruct>;
    metaInfo: SrcBlockMetaInfo;
  } {
    const headerChildren: OrgNode<OrgStruct>[] = [];
    const metaInfo: { [key: string]: string } = {};

    while (node) {
      const value = node?.value;

      if (headerChildren.length === 1 && !value?.trim().startsWith(':') && value.trim()) {
        metaInfo.language = value?.trim();
      }

      const lastPotentialKeyword = headerChildren[headerChildren.length - 1];

      if (lastPotentialKeyword?.value.startsWith(':') && value) {
        metaInfo[lastPotentialKeyword.value.slice(1)] = value.trim();
      }

      headerChildren.push(node);

      if (node.type === NodeType.NewLine) {
        return {
          headerChildren,
          nextNode: node.next,
          metaInfo,
        };
      }
      node = node.next;
    }
  }

  private determineBlockType(): [BlockPosition, BlockType] {
    const [pos, type] = this.tokenIterator.currentValue.split('_');
    return [pos.slice(2, pos.length) as BlockPosition, type];
  }
}
