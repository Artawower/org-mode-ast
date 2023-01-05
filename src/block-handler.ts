import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { OrgHandler } from 'internal.types';
import { TokenIterator } from 'token-iterator';
import {
  BlockPosition,
  NodeType,
  OrgData,
  OrgNewLine,
  SrcBlockMetaInfo,
  WithChildren,
  WithNeighbors,
  WithParent,
  WithValue,
} from 'types';

// TODO: master this class should be refactored!
export class BlockHandler implements OrgHandler {
  constructor(private ctx: AstContext, private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  private blockHanderls: { [key: string]: (pos: BlockPosition) => OrgData } = {
    src: (pos) => this.handleSrcBlock(pos),
  };

  public handle(): OrgData {
    const [blockPosition, blockType] = this.determineBlockType();
    const blockHandler = this.blockHanderls[blockType.toLowerCase()];

    if (!blockHandler) {
      throw new Error(`Block type ${blockType} is not supported`);
    }

    return blockHandler(blockPosition);
  }

  public isBlockKeyword(keyword: string): boolean {
    return ['#+BEGIN_', '#+END_'].some((prefix) => keyword.startsWith(prefix));
  }

  private handleSrcBlock(position: BlockPosition): OrgData {
    const keyword = this.astBuilder.createKeyword();

    if (position.toLowerCase() === 'begin') {
      this.ctx.srcBlockBegin = keyword;
      this.astBuilder.attachToTree(keyword);
      this.ctx.srcBlockChildIndex = (keyword.parent as WithChildren).children.length - 1;
      return keyword;
    }

    if (position.toLowerCase() === 'end' && this.ctx.srcBlockBegin) {
      this.mergeNodesAfterSrcBlock();
      this.ctx.resetSrcBlockInfo();
    }

    this.astBuilder.attachToTree(keyword);
    return keyword;
  }

  private mergeNodesAfterSrcBlock(): void {
    let start: number;
    let end = this.ctx.srcBlockBegin.end;
    let value = '';
    const parentNode = this.ctx.srcBlockBegin.parent;

    let lastNewLine: OrgNewLine;

    let { headerChildren, nextNode, metaInfo } = this.collectHeadline(this.ctx.srcBlockBegin);

    if (nextNode) {
      start = nextNode.start;
    }

    while (nextNode) {
      end = nextNode.end;

      if (!(nextNode as WithNeighbors).next) {
        lastNewLine = nextNode as OrgNewLine;
        break;
      }

      value += this.astBuilder.getRawValueFromNode(nextNode);
      nextNode = (nextNode as WithNeighbors).next;
    }

    const srcBlock = this.astBuilder.createSrcBlockNode(
      this.ctx.srcBlockBegin.start,
      end,
      this.ctx.srcBlockBegin.prev,
      null,
      metaInfo
    );

    const headerNewLineNode = headerChildren.pop();
    const blockHeader = this.astBuilder.createBlockHeaderNode(srcBlock, headerChildren);
    srcBlock.children = [blockHeader, headerNewLineNode];

    if (value) {
      const orgText = this.astBuilder.createTextNode(start, value);
      const blockBody = this.astBuilder.createBlockBodyNode(srcBlock, [orgText]);
      srcBlock.children.push(blockBody);

      (lastNewLine as WithParent).parent = srcBlock as unknown as OrgData;
      (lastNewLine as WithNeighbors).prev = blockBody;

      srcBlock.children.push(lastNewLine);
    }

    const blockFooter = this.astBuilder.createBlockFooterNode(
      srcBlock,
      [],
      srcBlock.children[srcBlock.children.length - 1].end
    );
    srcBlock.children.push(blockFooter);

    srcBlock.parent = parentNode;
    srcBlock.prev = this.ctx.srcBlockBegin.prev;
    (parentNode as unknown as WithChildren).children.splice(this.ctx.srcBlockChildIndex);
    (parentNode as unknown as WithChildren).children.push(srcBlock as unknown as OrgData);
    this.astBuilder.lastNode = blockFooter;
  }

  /** Collection information about block headline
   *
   * @params node - node to collect information about
   * return list of headline nodes and next node to end of headline
   */
  private collectHeadline(node: OrgData): {
    headerChildren: OrgData[];
    nextNode: OrgData;
    metaInfo: SrcBlockMetaInfo;
  } {
    const headerChildren: OrgData[] = [];
    const metaInfo: { [key: string]: string } = {};

    while (node) {
      const value = (node as WithValue)?.value;

      if (headerChildren.length === 1 && !value?.trim().startsWith(':') && value.trim()) {
        metaInfo.language = value?.trim();
      }

      const lastPotentialKeyword = headerChildren[headerChildren.length - 1];

      if ((lastPotentialKeyword as WithValue)?.value.startsWith(':') && value) {
        metaInfo[(lastPotentialKeyword as WithValue).value.slice(1)] = value.trim();
      }

      headerChildren.push(node);

      if (node.type === NodeType.NewLine) {
        return {
          headerChildren,
          nextNode: (node as WithNeighbors).next,
          metaInfo,
        };
      }
      node = (node as WithNeighbors).next;
    }
  }

  private determineBlockType(): [BlockPosition, string] {
    const [pos, type] = this.tokenIterator.currentValue.split('_');
    return [pos.slice(2, pos.length) as BlockPosition, type];
  }
}
