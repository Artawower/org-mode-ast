import { AstContext } from 'ast-context';
import { TokenIterator } from 'token-iterator';
import {
  Headline,
  NodeType,
  OrgData,
  OrgText,
  Section,
  WithValue,
  WithChildren,
  PartialUniversalOrgNode,
  List,
  OrgIndent,
  TokenType,
  OrgNewLine,
  OrgKeyword,
  WithNeighbors,
  OrgSrcBlock,
  Unresolved,
  OrgBlockHeader,
  OrgBlockBody,
  OrgBlockFooter,
} from 'types';

export class AstBuilder {
  public lastNode: OrgData;
  public lastPos: number = 0;

  private lastSection: Section;

  #nodeTree: OrgData;

  get nodeTree(): OrgData {
    return this.#nodeTree;
  }

  constructor(private ctx: AstContext, private tokenIterator: TokenIterator) {
    this.initRootNode();
  }

  public increaseLastPosition(byText: string): void {
    this.lastPos += byText.length;
  }

  private initRootNode(): void {
    this.#nodeTree = {
      type: NodeType.Root,
      start: 0,
      end: 0,
      children: [],
    };
    this.preserveLastPositionSnapshot(this.nodeTree);
  }

  public attachToTree(orgData: OrgData): void {
    const parentNode = this.findParentForNodeType(orgData);
    const parentWithChildren = parentNode as WithChildren;
    const prevNeighbor = parentWithChildren.children[parentWithChildren.children.length - 1] as WithNeighbors & OrgData;
    (orgData as WithNeighbors).prev = prevNeighbor;
    if (prevNeighbor) {
      prevNeighbor.next = orgData;
    }

    (parentNode as WithChildren).children.push(orgData);
    orgData.parent = parentNode;
  }

  private findFirstParentNodeWithType(...type: NodeType[]): OrgData {
    let node = this.lastNode;
    while (node) {
      if (type.includes(node.type)) {
        return node;
      }
      node = node.parent;
    }
  }

  private isNotListIndentInsideSection(srcNode: OrgData, _dstNode: OrgData): OrgData {
    if (srcNode.type === NodeType.Indent && !this.isListOperator(this.tokenIterator.currentValue) && this.lastSection) {
      return this.lastSection;
    }
  }

  private isParentAlreadyExist(srcNode: OrgData, _dstNode: OrgData): OrgData {
    if (srcNode.parent) {
      return srcNode.parent;
    }
  }

  private isNodeAfterListWithSameLevel(_srcNode: OrgData, _dstNode: OrgData): OrgData {
    const exitFromTopList = !!this.ctx.topLevelList;
    const notIndentAfterNewLine = !this.ctx.nextIndentNode && this.tokenIterator.prevToken?.isType(TokenType.NewLine);

    if (notIndentAfterNewLine && exitFromTopList && !this.isListOperator(this.tokenIterator.currentValue)) {
      const parent = this.ctx.topLevelList.parent;
      return parent;
    }
  }

  private isDestinationRootNode(_srcNode: OrgData, dstNode: OrgData): OrgData {
    if (dstNode.type === NodeType.Root) {
      return dstNode;
    }
  }

  private isInsideList(srcNode: OrgData): OrgData {
    const isNestedList = (this.lastSection?.parent?.parent as List)?.level < (srcNode as List).level;

    if (
      !this.ctx.insideHeadline &&
      this.lastSection &&
      srcNode.type !== NodeType.ListItem &&
      !this.ctx.insideListItem &&
      (this.lastSection.parent.type !== NodeType.ListItem || isNestedList)
    ) {
      return this.lastSection as any;
    }
  }

  private isNestedHeadline(srcNode: OrgData, dstNode: OrgData): OrgData {
    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    const isTargetNodeHeadline = dstNode.type === NodeType.Headline;

    if (isSourceNodeHeadline && isTargetNodeHeadline && (<Headline>srcNode).level > (<Headline>dstNode).level) {
      return (dstNode as Headline).section;
    }
  }

  // TODO: remove
  // private isNestedListItem(srcNode: OrgData, dstNode: OrgData): OrgData {
  //   const isSrcListItem = srcNode.type === NodeType.ListItem;
  //   const isTargetList = dstNode.type === NodeType.List;

  //   if (isSrcListItem) {
  //     return isTargetList ? dstNode : this.findParentForNodeType(srcNode, dstNode.parent);
  //   }
  // }

  private isCommonDestinationAndSrcNotHeadline(srcNode: OrgData, dstNode: OrgData): OrgData {
    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    if (
      !isSourceNodeHeadline &&
      [
        NodeType.Root,
        NodeType.Headline,
        NodeType.Section,
        NodeType.Checkbox,
        NodeType.List,
        NodeType.ListItem,
      ].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  private isKeyword(srcNode: OrgData, dstNode: OrgData): OrgData {
    if (
      srcNode.type === NodeType.Keyword &&
      [NodeType.SrcBlock, NodeType.BlockFooter, NodeType.BlockHeader, NodeType.BlockBody].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  // private isUnresolvedNode(srcNode: OrgData, dstNode: OrgData): OrgData {
  //   if (srcNode.type === NodeType.Unresolved) {
  //     return this.findParentForNodeType(srcNode, dstNode.parent);
  //   }
  // }

  private findParentForNodeType(srcNode: PartialUniversalOrgNode, dstNode?: OrgData): OrgData {
    dstNode = dstNode || this.lastNode;
    // if (srcNode.type === NodeType.Unresolved) {
    //   console.log('✎: [line 166][ast-builder.ts] dstNode: ', srcNode, dstNode, dstNode.parent);
    // }
    console.log('✎: [line 166][ast-builder.ts] dstNode: ', srcNode, dstNode, dstNode.parent);

    if (!dstNode) {
      throw new Error(`Something went wrong, couldn't find last node`);
    }

    // TODO: need to combine some functions for less complexity
    const parentMatchers = [
      this.isParentAlreadyExist,
      this.isDestinationRootNode,
      // this.isUnresolvedNode,
      this.isNotListIndentInsideSection,
      this.isKeyword,
      this.isNodeAfterListWithSameLevel,
      this.isInsideList,
      this.isNestedHeadline,
      this.isCommonDestinationAndSrcNotHeadline,
    ];

    for (const matcher of parentMatchers) {
      const parent = matcher.bind(this)(srcNode, dstNode);
      if (parent) {
        return parent;
      }
    }

    if (!dstNode.parent) {
      throw new Error(`Something went wrong, couldn't find last node, ${srcNode.type}, prev node: ${dstNode.type}`);
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  public createKeyword(): OrgKeyword {
    return {
      type: NodeType.Keyword,
      start: this.lastPos,
      end: this.lastPos + this.tokenIterator.currentValue.length,
      value: this.tokenIterator.currentValue,
    };
  }

  public preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  public saveLastNode(orgData: OrgData): void {
    this.lastNode = orgData;
  }

  public appendLengthToParentNodes(length: number, node?: PartialUniversalOrgNode): void {
    // console.log('✎: [line 216][ast-builder.ts] node: ', node);
    if (!node || !length) {
      return;
    }

    // NOTE: Nodes with section have range bounded by the new line
    if (!node.section) {
      node.end = length;
    }
    if (!node.parent) {
      return;
    }
    this.appendLengthToParentNodes(length, node.parent);
  }

  /*
   * Create new nested section
   */
  public getLastSessionOrCreate(parentNode?: PartialUniversalOrgNode): void {
    const nodeWithSection =
      parentNode ||
      (this.findFirstParentNodeWithType(
        NodeType.Headline,
        NodeType.Section,
        NodeType.ListItem
      ) as PartialUniversalOrgNode);
    const parentSectionAlreadyExists = nodeWithSection?.section || nodeWithSection?.type === NodeType.Section;

    if (parentSectionAlreadyExists || !nodeWithSection) {
      return;
    }

    const section: Section = {
      type: NodeType.Section,
      start: nodeWithSection.end,
      end: nodeWithSection.end,
      children: [],
      parent: nodeWithSection as OrgData,
    };

    nodeWithSection.section = section;
    this.lastSection = section;
  }

  public exitSection(): void {
    this.lastSection = null;
  }

  public mergeUnresolvedNodes(nodes: OrgData[], newType?: NodeType): OrgData[] {
    const mergedNodes: OrgData[] = [];
    nodes.forEach((n) => {
      const lastNode = mergedNodes[mergedNodes.length - 1];

      if (n.type === NodeType.Unresolved) {
        (n as PartialUniversalOrgNode).type = newType || NodeType.Text;
      }

      if (!lastNode) {
        mergedNodes.push(n);
        return;
      }
      if (lastNode.type === NodeType.Text && n.type === NodeType.Text) {
        lastNode.end = n.end;
        (lastNode as OrgText).value += (n as OrgText).value;
        return;
      }
      mergedNodes.push(n);
    });
    return mergedNodes;
  }

  // Section of helpers function. Consider moving them to separate class

  public isNodesCheckbox(nodes: Array<OrgData & WithValue>): boolean {
    if (nodes.length !== 3) {
      return false;
    }
    const [openBracket, checkbox, closeBracket] = nodes;
    const potentialCheckboxValues = [' ', 'x', '-'];

    return (
      openBracket?.value === '[' &&
      potentialCheckboxValues.includes(checkbox?.value.toLocaleLowerCase()) &&
      closeBracket?.value === ']'
    );
  }

  public getRawValueFromNode(node: PartialUniversalOrgNode): string {
    if ((node as WithValue).value) {
      return (node as WithValue).value;
    }
    if ((node as WithChildren).children) {
      return (node as WithChildren).children.map((n) => this.getRawValueFromNode(n)).join('');
    }
  }

  public parentNodeExist(node: OrgData, types: NodeType | NodeType[]): boolean {
    if (!Array.isArray(types)) {
      types = [types];
    }

    if (!node.parent) {
      return false;
    }
    if (types.includes(node.parent.type)) {
      return true;
    }
    return this.parentNodeExist(node.parent, types);
  }

  public getRawValueFromNodes(nodes: WithValue[]): string {
    return nodes.map((n) => n?.value || this.getRawValueFromNodes([n])).join('');
  }

  public createIndentNode(start?: number, val?: string): OrgIndent {
    start = start || this.lastPos;
    val = val || this.tokenIterator.currentValue;
    const end = start + val.length;

    return {
      type: NodeType.Indent,
      start,
      end,
      value: this.tokenIterator.currentValue,
    };
  }

  public createNewLineNode(): OrgNewLine {
    return {
      type: NodeType.NewLine,
      start: this.lastPos,
      end: this.lastPos + 1,
      value: this.tokenIterator.currentValue,
    };
  }

  public createSrcBlockNode(
    start: number,
    end: number,
    prev: OrgData,
    language?: string,
    properties?: { [key: string]: string }
  ): OrgSrcBlock {
    return {
      type: NodeType.SrcBlock,
      start,
      end,
      prev,
      language,
      properties,
      children: [],
    };
  }

  public createTextNode(start: number, value: string): OrgText {
    return {
      type: NodeType.Text,
      start,
      end: start + value.length,
      value,
    };
  }

  public createUnresolvedNode(): Unresolved {
    return {
      type: NodeType.Unresolved,
      start: this.lastPos,
      end: this.lastPos + this.tokenIterator.currentValue.length,
      value: this.tokenIterator.currentValue,
    };
  }

  public isListOperator(tokenValue: string): boolean {
    // NOTE: + ,- , 1), 1. - strings indicated list operator
    // https://regex101.com/r/4qq9Ob/1
    const listOperatorsRegexp = /^((\-|\+) )|([1-9][0-9]*((\)|\.)) )$/;
    return !!listOperatorsRegexp.exec(tokenValue);
  }

  public createBlockHeaderNode(parent: OrgData, children: PartialUniversalOrgNode[]): OrgBlockHeader {
    const blockHeader: OrgBlockHeader = {
      type: NodeType.BlockHeader,
      start: children[0].start,
      end: children[children.length - 1].end,
      children: children as OrgData[],
      parent,
    };
    blockHeader.children.forEach((n) => (n.parent = blockHeader));
    return blockHeader;
  }

  public createBlockFooterNode(parent: OrgData, children?: PartialUniversalOrgNode[], start?: number): OrgBlockFooter {
    const blockFooter: OrgBlockFooter = {
      type: NodeType.BlockFooter,
      start: children?.[0]?.start || start,
      end: children?.[children.length - 1]?.end,
      children: (children as OrgData[]) || [],
      parent,
    };

    blockFooter.children.forEach((c) => (c.parent = blockFooter));
    return blockFooter;
  }

  public createBlockBodyNode(parent: OrgData, children: PartialUniversalOrgNode[]): OrgBlockBody {
    const blockBody: OrgBlockBody = {
      type: NodeType.BlockBody,
      start: children[0].start,
      end: children[children.length - 1].end,
      children: children as OrgData[],
      parent,
    };

    blockBody.children.forEach((n) => (n.parent = blockBody));
    return blockBody;
  }

  public isPropertyOperator(tokenValue: string): boolean {
    return tokenValue === ':';
  }
}
