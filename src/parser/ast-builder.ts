import {
  NodeType,
  OrgChildrenList,
  OrgNode,
  OrgStruct,
  TokenType,
} from 'models';
import { TokenIterator } from 'tokenizer';
import { AstContext } from './ast-context';

export class AstBuilder {
  public lastNode: OrgNode;
  public lastPos = 0;

  private lastSection: OrgNode;

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
      children: [],
    });

    this.preserveLastPositionSnapshot(this.nodeTree);
  }

  public attachToTree(orgData: OrgNode): void {
    const parentNode = this.findParentForNodeType(orgData);
    parentNode.addChild(orgData);
    // console.log(this.#nodeTree.toString());
    // console.log('----------------');
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
  private isNotListIndentInsideSection(
    srcNode: OrgStruct,
    _dstNode: OrgStruct
  ): OrgNode {
    if (
      srcNode.type === NodeType.Indent &&
      !this.isListOperator(this.tokenIterator.currentValue) &&
      this.lastSection
    ) {
      return this.lastSection;
    }
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private isParentAlreadyExist(srcNode: OrgNode, _dstNode: OrgNode): OrgNode {
    if (srcNode.parent) {
      return srcNode.parent;
    }
  }

  private isNodeAfterListWithSameLevel(
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _srcNode: OrgStruct,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _dstNode: OrgStruct
  ): OrgNode {
    const exitFromTopList = !!this.ctx.topLevelList;
    const notIndentAfterNewLine =
      !this.ctx.nextIndentNode &&
      this.tokenIterator.prevToken?.isType(TokenType.NewLine);

    if (
      notIndentAfterNewLine &&
      exitFromTopList &&
      !this.isListOperator(this.tokenIterator.currentValue)
    ) {
      const parent = this.ctx.topLevelList.parent;
      return parent;
    }
  }

  private isDestinationRootNode(
    _srcNode: OrgStruct,
    dstNode: OrgStruct
  ): OrgStruct {
    if (dstNode.type === NodeType.Root) {
      return dstNode;
    }
  }

  private isInsideList(srcNode: OrgNode): OrgNode {
    const isNestedList =
      this.lastSection?.parent?.parent.is(NodeType.List) &&
      this.lastSection?.parent?.parent?.level < srcNode.level;

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

    if (
      isSourceNodeHeadline &&
      isTargetNodeHeadline &&
      srcNode.level > dstNode.level
    ) {
      return dstNode.section;
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

  private isCommonDestinationAndSrcNotHeadline(
    srcNode: OrgStruct,
    dstNode: OrgStruct
  ): OrgStruct {
    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    if (
      !isSourceNodeHeadline &&
      [
        NodeType.Root,
        NodeType.Headline,
        NodeType.Section,
        NodeType.List,
        NodeType.ListItem,
      ].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  private isKeyword(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    if (
      srcNode.type === NodeType.Keyword &&
      [
        NodeType.SrcBlock,
        NodeType.BlockFooter,
        NodeType.BlockHeader,
        NodeType.BlockBody,
      ].includes(dstNode.type)
    ) {
      return dstNode;
    }
  }

  private isCommentParent(srcNode: OrgStruct, dstNode: OrgStruct): OrgStruct {
    if (
      [NodeType.Text].includes(srcNode.type) &&
      dstNode.type === NodeType.Comment
    ) {
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
      throw new Error(
        `Something went wrong, couldn't find last node, ${srcNode.type}, prev node: ${dstNode.type}`
      );
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  public createTitleNode(): OrgNode {
    return new OrgNode({
      type: NodeType.Title,
    });
  }

  public createHeadline(): OrgNode {
    const headline = new OrgNode({
      type: NodeType.Headline,
      level: this.tokenIterator.currentValue.trim().length,
    });
    headline.setTitle(this.createTitleNode());
    const operatorNode = new OrgNode({
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
    });
    headline.title.addChild(operatorNode);
    return headline;
  }

  public createKeyword(): OrgNode {
    return new OrgNode({
      type: NodeType.Keyword,
      value: this.tokenIterator.currentValue,
    });
  }

  public createText(): OrgNode {
    return new OrgNode({
      type: NodeType.Text,
      value: this.tokenIterator.currentValue,
    });
  }

  public preserveLastPositionSnapshot(orgData: OrgNode): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  public saveLastNode(orgData: OrgNode): void {
    this.lastNode = orgData;
  }

  /*
   * Create new nested section
   */
  public getLastSectionOrCreate(parentNode?: OrgNode): void {
    const nodeWithSection =
      parentNode ||
      this.findFirstParentNodeWithType(
        NodeType.Headline,
        NodeType.Section,
        NodeType.ListItem
      );
    const parentSectionAlreadyExists =
      nodeWithSection?.section || nodeWithSection?.type === NodeType.Section;

    if (parentSectionAlreadyExists || !nodeWithSection) {
      return;
    }

    const sectionNode = new OrgNode({
      type: NodeType.Section,
    });
    nodeWithSection.setSection(sectionNode);
    this.lastSection = sectionNode;
  }

  public exitSection(): void {
    this.lastSection = null;
  }

  public mergeUnresolvedNodes(nodes: OrgNode[], newType?: NodeType): OrgNode[] {
    const mergedNodes: OrgNode[] = [];
    const prev = nodes[0].prev;

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
        lastNode.appendValue(n.value);
        return;
      }
      mergedNodes.push(n);
    });
    mergedNodes[0].setPrev(prev);
    return mergedNodes;
  }

  public createCheckboxNode(value: string, checked: boolean): OrgNode {
    return new OrgNode({
      type: NodeType.Checkbox,
      checked,
      value,
    });
  }

  // Section of helpers function. Consider moving them to separate class

  public isNodesCheckbox(nodes: OrgChildrenList): boolean {
    if (nodes.length !== 3) {
      return false;
    }
    const potentialCheckboxValues = [' ', 'x', '-'];

    return (
      nodes.first?.value === '[' &&
      potentialCheckboxValues.includes(
        nodes.get(1)?.value?.toLocaleLowerCase()
      ) &&
      nodes.last?.value === ']'
    );
  }

  public getRawValueFromNode(node: OrgNode): string {
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

  public getRawValueFromNodes(nodes: OrgChildrenList | OrgNode): string {
    if (nodes instanceof OrgChildrenList) {
      return nodes
        .map((n) => n?.value || this.getRawValueFromNodes(n))
        .join('');
    }
    return nodes?.value || this.getRawValueFromNodes(nodes.children);
  }

  public createIndentNode(val?: string): OrgNode {
    return new OrgNode({
      type: NodeType.Indent,
      value: val || this.tokenIterator.currentValue,
    });
  }

  public createComment(): OrgNode {
    const commentNode = new OrgNode({
      type: NodeType.Comment,
    });

    const operatorNode = new OrgNode({
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
    });

    commentNode.addChild(operatorNode);

    return commentNode;
  }

  public createNewLineNode(): OrgNode {
    return new OrgNode({
      type: NodeType.NewLine,
      value: this.tokenIterator.currentValue,
    });
  }

  public createBlockNode(
    type: NodeType.HtmlBlock | NodeType.SrcBlock | NodeType.QuoteBlock,
    properties?: { [key: string]: string }
  ): OrgNode {
    const srcBlockNode = new OrgNode({
      type,
      properties,
    });
    return srcBlockNode;
  }

  public createTextNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.Text,
      value,
    });
  }

  public createUnresolvedNode(value?: string): OrgNode {
    return new OrgNode({
      type: NodeType.Unresolved,
      value: value ?? this.tokenIterator.currentValue,
    });
  }

  public createBlockPropertyNode(child?: OrgNode): OrgNode {
    const blockProperty = new OrgNode({
      type: NodeType.BlockProperty,
    });
    blockProperty.addChild(child);
    return blockProperty;
  }

  public createBlockLanguageNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.BlockLanguage,
      properties: { language: value.slice(1).trim() },
      value,
    });
  }

  public createLinkNode(): OrgNode {
    const linkNode = new OrgNode({
      type: NodeType.Link,
    });
    // linkNode.setChildren(bracketedNodes);
    return linkNode;
  }

  public createDateNode(): // openBracket: OrgNode,
  // dateTextNode: OrgNode,
  // closeBracket: OrgNode
  OrgNode {
    const dateNode = new OrgNode({
      type: NodeType.Date,
    });
    // dateNode.addChild(openBracket);
    // dateNode.addChild(dateTextNode);
    // dateNode.addChild(closeBracket);

    return dateNode;
  }

  public isListOperator(tokenValue: string): boolean {
    // NOTE: + ,- , 1), 1. - strings indicated list operator
    // https://regex101.com/r/4qq9Ob/1
    const listOperatorsRegexp = /^((-|\+) )|([1-9][0-9]*((\)|\.)) )$/;
    return !!listOperatorsRegexp.exec(tokenValue);
  }

  public createBlockHeaderNode(children: OrgChildrenList | OrgNode[]): OrgNode {
    return this.createBlockSubNode(NodeType.BlockHeader, children);
  }

  public createBlockFooterNode(
    children?: OrgChildrenList | OrgNode[]
  ): OrgNode {
    return this.createBlockSubNode(NodeType.BlockFooter, children);
  }

  public createBlockBodyNode(children: OrgChildrenList | OrgNode[]): OrgNode {
    return this.createBlockSubNode(NodeType.BlockBody, children);
  }

  private createBlockSubNode(
    type: NodeType,
    children: OrgChildrenList | OrgNode[]
  ): OrgNode {
    const blockHeaderNode = new OrgNode({ type });
    blockHeaderNode.addChildren(children);
    return blockHeaderNode;
  }

  public createList(ordered: boolean, level = 0): OrgNode {
    return new OrgNode({
      type: NodeType.List,
      level,
      ordered,
      children: [],
    });
  }

  public isPropertyOperator(tokenValue: string): boolean {
    return tokenValue === ':';
  }

  /**
   * Merge neighbors when they have same type, or one of them Unresolved
   */
  public mergeNeighborsNodesWithSameType(node?: OrgNode): void {
    if (!node) {
      return;
    }
    let currentNode = node;

    while (currentNode) {
      if (this.couldBeMergedIntoText(currentNode)) {
        currentNode.type = NodeType.Text;
      }
      if (
        this.couldBeMergedIntoText(currentNode) &&
        this.couldBeMergedIntoText(currentNode.prev)
      ) {
        const prev = currentNode.prev;
        prev.appendValue(currentNode.value);
        prev.parent?.removeNode(currentNode);
      }
      currentNode = currentNode.next;
    }
  }

  private couldBeMergedIntoText(node?: OrgNode): boolean {
    return (
      node?.type === NodeType.Text ||
      node?.type === NodeType.Unresolved ||
      (node?.type === NodeType.Operator &&
        ['<', '>'].includes(node.value) &&
        !node.parent.is(NodeType.Date))
    );
  }
}
