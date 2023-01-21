import { AstContext } from 'ast-context';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import {
  Headline,
  NodeType,
  OrgStruct,
  Text,
  Section,
  WithValue,
  PartialUniversalOrgStruct,
  List,
  Indent,
  TokenType,
  NewLine,
  Keyword,
  SrcBlock,
  Unresolved,
  BlockHeader,
  BlockBody,
  BlockFooter,
  Operator,
  Comment,
  Date,
  Checkbox,
} from 'types';

export class AstBuilder {
  public lastNode: OrgNode;
  public lastPos = 0;

  private lastSection: OrgNode<Section>;

  #nodeTree: OrgNode;

  get nodeTree(): OrgNode {
    return this.#nodeTree;
  }

  constructor(private ctx: AstContext, private tokenIterator: TokenIterator) {
    this.initRootNode();
  }

  public increaseLastPosition(byText: string): void {
    this.lastPos += byText.length;
  }

  private initRootNode(): void {
    this.#nodeTree = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
      children: [],
    });

    this.preserveLastPositionSnapshot(this.nodeTree);
  }

  public attachToTree(orgData: OrgNode): void {
    const parentNode = this.findParentForNodeType(orgData);
    const parentWithChildren = parentNode;
    const prevNeighbor = parentWithChildren.lastChild;

    orgData.setPrev(prevNeighbor);

    if (prevNeighbor) {
      prevNeighbor.setNext(orgData);
    }

    parentNode.addChild(orgData);
  }

  private findFirstParentNodeWithType(...type: NodeType[]): OrgNode {
    let node = this.lastNode;
    while (node) {
      if (type.includes(node.type)) {
        return node;
      }
      node = node.parent;
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private isNotListIndentInsideSection(srcNode: OrgStruct, _dstNode: OrgStruct): OrgNode<Section> {
    if (srcNode.type === NodeType.Indent && !this.isListOperator(this.tokenIterator.currentValue) && this.lastSection) {
      return this.lastSection;
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private isParentAlreadyExist(srcNode: OrgNode<OrgStruct>, _dstNode: OrgNode<OrgStruct>): OrgNode<OrgStruct> {
    if (srcNode.parent) {
      return srcNode.parent;
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private isNodeAfterListWithSameLevel(_srcNode: OrgStruct, _dstNode: OrgStruct): OrgNode<OrgStruct> {
    const exitFromTopList = !!this.ctx.topLevelList;
    const notIndentAfterNewLine = !this.ctx.nextIndentNode && this.tokenIterator.prevToken?.isType(TokenType.NewLine);

    if (notIndentAfterNewLine && exitFromTopList && !this.isListOperator(this.tokenIterator.currentValue)) {
      const parent = this.ctx.topLevelList.parent;
      return parent;
    }
  }

  private isDestinationRootNode(_srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    if (dstNode.type === NodeType.Root) {
      return dstNode;
    }
  }

  private isInsideList(srcNode: OrgNode): OrgNode {
    const isNestedList =
      this.lastSection?.parent?.parent.is(NodeType.List) && this.lastSection?.parent?.parent?.level < srcNode.level;

    if (
      !this.ctx.insideHeadline &&
      this.lastSection &&
      srcNode.type !== NodeType.ListItem &&
      !this.ctx.insideListItem &&
      (this.lastSection.parent.type !== NodeType.ListItem || isNestedList)
    ) {
      return this.lastSection;
    }
  }

  private isNestedHeadline(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
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

  private isCommonDestinationAndSrcNotHeadline(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    if (
      !isSourceNodeHeadline &&
      [NodeType.Root, NodeType.Headline, NodeType.Section, NodeType.List, NodeType.ListItem].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  private isKeyword(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    if (
      srcNode.type === NodeType.Keyword &&
      [NodeType.SrcBlock, NodeType.BlockFooter, NodeType.BlockHeader, NodeType.BlockBody].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  private isCommentParent(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    if ([NodeType.Text].includes(srcNode.type) && dstNode.type === NodeType.Comment) {
      return dstNode;
    }
  }

  private findParentForNodeType(srcNode: OrgNode, dstNode?: OrgNode): OrgNode {
    dstNode = dstNode || this.lastNode;

    if (!dstNode) {
      throw new Error(`Something went wrong, couldn't find last node`);
    }

    // TODO: need to combine some functions for less complexity
    const parentMatchers = [
      this.isParentAlreadyExist,
      this.isDestinationRootNode,
      this.isCommentParent,
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

  public createHeadline(): OrgNode<Headline> {
    const end = this.lastPos + this.tokenIterator.currentValue.length;

    const headline = new OrgNode<Headline>({
      type: NodeType.Headline,
      level: this.tokenIterator.currentValue.trim().length,
      start: this.lastPos,
      end,
    });
    const operatorNode = new OrgNode<Operator>({
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
      start: this.lastPos,
      end,
    });
    headline.addChild(operatorNode);
    return headline;
  }

  public createKeyword(): OrgNode<Keyword> {
    const keyword: Keyword = {
      type: NodeType.Keyword,
      start: this.lastPos,
      end: this.lastPos + this.tokenIterator.currentValue.length,
      value: this.tokenIterator.currentValue,
    };

    return new OrgNode(keyword);
  }

  public createText(): OrgNode {
    const orgText: Text = {
      type: NodeType.Text,
      value: this.tokenIterator.currentValue,
      start: this.lastPos,
      end: this.lastPos + this.tokenIterator.currentValue.length,
    };
    return new OrgNode(orgText);
  }

  public preserveLastPositionSnapshot(orgData: OrgNode): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  public saveLastNode(orgData: OrgNode): void {
    this.lastNode = orgData;
  }

  public appendLengthToParentNodes(length: number, node?: OrgNode): void {
    // console.log('✎: [line 274][ast-builder.ts] node: ', node);
    if (!node || !length) {
      return;
    }

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
  public getLastSectionOrCreate(parentNode?: OrgNode): void {
    const nodeWithSection =
      parentNode || this.findFirstParentNodeWithType(NodeType.Headline, NodeType.Section, NodeType.ListItem);
    const parentSectionAlreadyExists = nodeWithSection?.section || nodeWithSection?.type === NodeType.Section;

    if (parentSectionAlreadyExists || !nodeWithSection) {
      return;
    }

    const section: Section = {
      type: NodeType.Section,
      start: nodeWithSection.end,
      end: nodeWithSection.end,
      children: [],
    };

    const sectionNode = new OrgNode<Section>(section);
    nodeWithSection.setSection(sectionNode);
    this.lastSection = sectionNode;
  }

  public exitSection(): void {
    this.lastSection = null;
  }

  public mergeUnresolvedNodes(nodes: OrgNode<OrgStruct>[], newType?: NodeType): OrgNode<OrgStruct>[] {
    const mergedNodes: OrgNode<OrgStruct>[] = [];
    nodes.forEach((n) => {
      const lastNode = mergedNodes[mergedNodes.length - 1];

      if (n.type === NodeType.Unresolved) {
        n.type = newType || NodeType.Text;
      }

      if (!lastNode) {
        mergedNodes.push(n);
        return;
      }
      if (lastNode.type === NodeType.Text && n.type === NodeType.Text) {
        lastNode.end = n.end;
        this.lastNode.appendValue(n.value);
        return;
      }
      mergedNodes.push(n);
    });
    return mergedNodes;
  }

  public createCheckboxNode(start: number, end: number, value: string, checked: boolean): OrgNode<Checkbox> {
    const checkBoxData: Checkbox = {
      type: NodeType.Checkbox,
      start,
      end,
      checked,
      value,
    };
    return new OrgNode<Checkbox>(checkBoxData);
  }

  // Section of helpers function. Consider moving them to separate class

  public isNodesCheckbox(nodes: OrgNode[]): boolean {
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

  public getRawValueFromNode(node: OrgNode<OrgStruct>): string {
    if (node.value) {
      return node.value;
    }
    if (node.children) {
      return node.children.map((n) => this.getRawValueFromNode(n)).join('');
    }
  }

  public parentNodeExist(node: OrgNode, types: NodeType | NodeType[]): boolean {
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

  public createIndentNode(start?: number, val?: string): OrgNode<Indent> {
    start = start || this.lastPos;
    val = val || this.tokenIterator.currentValue;
    const end = start + val.length;

    const indent: Indent = {
      type: NodeType.Indent,
      start,
      end,
      value: this.tokenIterator.currentValue,
    };

    return new OrgNode<Indent>(indent);
  }

  public createComment(): OrgNode<Comment> {
    const end = this.lastPos + this.tokenIterator.currentValue.length;
    const comment: Comment = {
      type: NodeType.Comment,
      start: this.lastPos,
      end,
    };
    const commentNode = new OrgNode<Comment>(comment);

    const operatorNode = new OrgNode<Operator>({
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
      start: this.lastPos,
      end,
    });

    commentNode.addChild(operatorNode);

    return commentNode;
  }

  public createNewLineNode(): OrgNode<NewLine> {
    const newLine: NewLine = {
      type: NodeType.NewLine,
      start: this.lastPos,
      end: this.lastPos + 1,
      value: this.tokenIterator.currentValue,
    };
    return new OrgNode<NewLine>(newLine);
  }

  public createSrcBlockNode(
    start: number,
    end: number,
    prev: OrgNode<OrgStruct>,
    language?: string,
    properties?: { [key: string]: string }
  ): OrgNode<SrcBlock> {
    const srcBlock: SrcBlock = {
      type: NodeType.SrcBlock,
      start,
      end,
      language,
      properties,
      children: [],
    };

    const srcBlockNode = new OrgNode<SrcBlock>(srcBlock);
    srcBlockNode.setPrev(prev);
    return srcBlockNode;
  }

  public createTextNode(start: number, value: string): OrgNode<Text> {
    const text: Text = {
      type: NodeType.Text,
      start,
      end: start + value.length,
      value,
    };

    return new OrgNode<Text>(text);
  }

  public createUnresolvedNode(): OrgNode<Unresolved> {
    const unresolved: Unresolved = {
      type: NodeType.Unresolved,
      start: this.lastPos,
      end: this.lastPos + this.tokenIterator.currentValue.length,
      value: this.tokenIterator.currentValue,
    };

    return new OrgNode<Unresolved>(unresolved);
  }

  public createDateNode(openBracket: OrgNode, dateTextNode: OrgNode, closeBracket: OrgNode): OrgNode<Date> {
    const date: Date = {
      type: NodeType.Date,
      start: openBracket.start,
      end: closeBracket.end,
    };

    const dateNode = new OrgNode<Date>(date);
    dateNode.addChild(openBracket);
    dateNode.addChild(dateTextNode);
    dateNode.addChild(closeBracket);

    return dateNode;
  }

  public isListOperator(tokenValue: string): boolean {
    // NOTE: + ,- , 1), 1. - strings indicated list operator
    // https://regex101.com/r/4qq9Ob/1
    const listOperatorsRegexp = /^((-|\+) )|([1-9][0-9]*((\)|\.)) )$/;
    return !!listOperatorsRegexp.exec(tokenValue);
  }

  public createBlockHeaderNode(parent: OrgNode, children: OrgNode[]): OrgNode<BlockHeader> {
    return this.createBlockNode<BlockHeader>(NodeType.BlockHeader, parent, children);
  }

  public createBlockFooterNode(parent: OrgNode<OrgStruct>, children?: OrgNode[], start?: number): OrgNode<BlockFooter> {
    return this.createBlockNode<BlockFooter>(NodeType.BlockFooter, parent, children, start);
  }

  public createBlockBodyNode(parent: OrgNode, children: OrgNode[]): OrgNode<BlockBody> {
    return this.createBlockNode<BlockBody>(NodeType.BlockBody, parent, children);
  }

  private createBlockNode<T = OrgStruct>(
    type: NodeType,
    parent: OrgNode<OrgStruct>,
    children: OrgNode[],
    start?: number
  ): OrgNode<T> {
    const block: PartialUniversalOrgStruct = {
      type,
      start: children[0]?.start ?? start,
      end: children[children.length - 1]?.end ?? start,
    };
    const blockHeaderNode = new OrgNode<T>(block);
    blockHeaderNode.addChildren(children);
    // parent.addChild(blockHeaderNode);
    return blockHeaderNode;
  }

  public createList(ordered: boolean, level = 0): OrgNode<List> {
    const list: List = {
      type: NodeType.List,
      start: this.lastNode.end,
      end: this.lastNode.end,
      level,
      ordered,
      children: [],
    };

    return new OrgNode<List>(list);
  }

  public isPropertyOperator(tokenValue: string): boolean {
    return tokenValue === ':';
  }
}
