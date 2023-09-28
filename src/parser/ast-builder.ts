import {
  Block,
  NodeType,
  OrgChildrenList,
  OrgNode,
  TokenType,
} from '../models/index.js';
import { TokenIterator } from '../tokenizer/index.js';
import { AstContext } from './ast-context.js';

export class AstBuilder {
  public lastNode: OrgNode;
  public lastPos = 0;

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

  public attachToTree(orgNode: OrgNode): OrgNode {
    const parentNode = this.findParentForNodeType(orgNode);
    parentNode.addChild(orgNode);
    return orgNode;
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
    srcNode: OrgNode,
    _dstNode: OrgNode
  ): OrgNode {
    if (
      srcNode.type === NodeType.Indent &&
      !this.isListOperator(this.tokenIterator.currentValue) &&
      this.ctx.lastSection
    ) {
      return this.ctx.lastSection;
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
    _srcNode: OrgNode,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _dstNode: OrgNode
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

  private isDestinationRootNode(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      dstNode.type === NodeType.Root &&
      !srcNode.is(NodeType.TableCell, NodeType.TableRow)
    ) {
      return dstNode;
    }
  }

  private isPropertyKeyVal(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    const maxPropertyChildren = 2;
    if (
      srcNode.is(NodeType.Text) &&
      dstNode.is(NodeType.Property) &&
      dstNode.children.length < maxPropertyChildren
    ) {
      return dstNode;
    }
  }

  // TODO: master rename
  private isInsideList(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    const isNestedList =
      this.ctx.lastSection?.parent?.parent.is(NodeType.List) &&
      this.ctx.lastSection?.parent?.parent?.level < srcNode.level;

    const isParentMatched = dstNode.parent?.is(
      NodeType.List,
      NodeType.ListItem,
      NodeType.Root
    );

    if (
      isParentMatched &&
      !this.ctx.insideHeadline &&
      this.ctx.lastSection &&
      srcNode.type !== NodeType.ListItem &&
      !this.ctx.insideListItem &&
      (this.ctx.lastSection.parent.type !== NodeType.ListItem || isNestedList)
    ) {
      return this.ctx.lastSection;
    }
  }

  private isNestedHeadline(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
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

  private isCommonDestinationAndSrcNotHeadline(
    srcNode: OrgNode,
    dstNode: OrgNode
  ): OrgNode {
    const isAvailableSrcNode = srcNode.isNot(
      NodeType.Headline,
      NodeType.TableRow,
      NodeType.TableCell
    );

    if (!isAvailableSrcNode) {
      return;
    }

    const isHeadlineSection =
      dstNode.is(NodeType.Section) && dstNode.parent.is(NodeType.Headline);

    const isListItemSection =
      dstNode.is(NodeType.Section) &&
      dstNode.parent.is(NodeType.ListItem) &&
      this.lastNode.isNot(NodeType.NewLine);

    if (
      dstNode.is(NodeType.Root, NodeType.Headline) ||
      isHeadlineSection ||
      isListItemSection
    ) {
      return dstNode;
    }
  }

  private isKeyword(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
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

  private isCommentParent(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      [NodeType.Text].includes(srcNode.type) &&
      dstNode.type === NodeType.Comment
    ) {
      return dstNode;
    }
  }

  private isPropertyDrawer(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      srcNode.is(NodeType.PropertyDrawer) &&
      dstNode.is(NodeType.Root, NodeType.Headline)
    ) {
      return dstNode;
    }
  }

  private isPartOfKeyword(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (dstNode.isNot(NodeType.Keyword)) {
      return;
    }
    if (srcNode.is(NodeType.TagList)) {
      return dstNode;
    }
    if (srcNode.is(NodeType.Text) && dstNode.children.length < 2) {
      return dstNode;
    }
  }

  private isPartOfBlockProperty(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      srcNode.is(NodeType.Text) &&
      dstNode.is(NodeType.BlockProperty) &&
      dstNode.children.length < 2
    ) {
      return dstNode;
    }
  }

  private isPartOfPropertyKeyword(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      (srcNode.is(NodeType.BlockProperty),
      dstNode.is(NodeType.Keyword) &&
        dstNode.children.first.isEqual('#+property:'))
    ) {
      return dstNode;
    }
  }

  private isInlineHtml(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      srcNode.is(NodeType.Keyword, NodeType.NewLine) &&
      dstNode.is(NodeType.InlineHtml)
    ) {
      return dstNode;
    }
  }

  private isCellSrc(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (srcNode.is(NodeType.TableCell) && dstNode.is(NodeType.TableRow)) {
      return dstNode;
    }
  }

  private isPartOfTable(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    const isNewLineBetweenRows =
      srcNode.is(NodeType.NewLine) &&
      (!this.tokenIterator.nextToken ||
        this.tokenIterator.nextToken.isType(
          TokenType.TableOperator,
          TokenType.Indent,
          TokenType.TableDelimiter
        ));
    if (
      dstNode.is(NodeType.Table) &&
      (srcNode.is(NodeType.TableRow, NodeType.Indent) || isNewLineBetweenRows)
    ) {
      return dstNode;
    }
  }

  private isCellDst(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (!dstNode.is(NodeType.TableCell)) {
      return;
    }

    const isMatched = srcNode.is(
      NodeType.Text,
      NodeType.Bold,
      NodeType.Italic,
      NodeType.Crossed,
      NodeType.Link,
      NodeType.Date,
      NodeType.DateRange,
      NodeType.InlineCode,
      NodeType.Verbatim,
      NodeType.TodoKeyword,
      NodeType.LinkName,
      NodeType.LinkUrl,
      NodeType.Operator,
      NodeType.Unresolved
    );

    if (isMatched) {
      return dstNode;
    }
  }

  private isPartOfSection(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    if (
      this.ctx.insideHeadline ||
      dstNode.isNot(NodeType.Title) ||
      dstNode?.parent?.isNot(NodeType.Headline)
    ) {
      return;
    }

    return dstNode.parent.section;
  }

  private isInsideListItemTitle(srcNode: OrgNode, dstNode: OrgNode): OrgNode {
    const isListItemTitle =
      dstNode.is(NodeType.Title) && dstNode.parent.is(NodeType.ListItem);
    const isSrcAllowed = srcNode.isNot(
      NodeType.Headline,
      NodeType.Section,
      NodeType.Title
    );
    const isTitleEnded = dstNode.children?.last?.is(NodeType.NewLine);

    if (!isListItemTitle || !isSrcAllowed || isTitleEnded) {
      return;
    }
    return dstNode;
  }

  private readonly parentMatchers = [
    this.isParentAlreadyExist,
    this.isCellSrc,
    this.isPartOfSection,
    this.isPartOfTable,
    this.isPropertyDrawer,
    this.isCellDst,
    this.isInlineHtml,
    this.isPartOfPropertyKeyword,
    this.isPartOfKeyword,
    this.isPartOfBlockProperty,
    this.isDestinationRootNode,
    this.isPropertyKeyVal,
    this.isCommentParent,
    // this.isUnresolvedNode,
    this.isNotListIndentInsideSection,
    this.isKeyword,
    this.isNodeAfterListWithSameLevel,
    this.isInsideList,
    this.isNestedHeadline,
    this.isInsideListItemTitle,
    this.isCommonDestinationAndSrcNotHeadline,
  ];

  private findParentForNodeType(srcNode: OrgNode, dstNode?: OrgNode): OrgNode {
    dstNode = dstNode || this.lastNode;
    if (!dstNode) {
      throw new Error(`Something went wrong, couldn't find last node`);
    }

    // TODO: need to combine some functions for less complexity
    for (const matcher of this.parentMatchers) {
      const parent = matcher.bind(this)(srcNode, dstNode);
      if (parent) {
        return parent;
      }
    }

    if (!dstNode.parent) {
      console.log(
        `✎: [ast-builder.ts][${new Date().toString()}] node tree`,
        this.nodeTree.toString()
      );
      console.log(
        `✎: [ast-builder.ts][${new Date().toString()}] current pos`,
        this.tokenIterator.token.start
      );
      throw new Error(
        `Something went wrong, couldn't find parent for:
   [${srcNode.type}: ${srcNode.rawValue}](${this.tokenIterator.token.start}:${this.tokenIterator.token.end}), prev node: [${dstNode.type}: ${dstNode.rawValue}](${dstNode.start}:${dstNode.end})`
      );
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  public createTitleNode(): OrgNode {
    return new OrgNode({
      type: NodeType.Title,
    });
  }

  public createTodoKeywordNode(): OrgNode {
    return new OrgNode({
      type: NodeType.TodoKeyword,
      value: this.tokenIterator.currentValue,
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

  public createKeywordNode(...children: OrgNode[]): OrgNode {
    const keywordNode = new OrgNode({
      type: NodeType.Keyword,
    });
    keywordNode.addChildren(children);
    return keywordNode;
  }

  // TODO: master delete. Redundant.
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
    this.ctx.lastSection = sectionNode;
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
      return nodes.map((n) => n.rawValue).join('');
    }
    return nodes.rawValue;
  }

  public createIndentNode(val?: string): OrgNode {
    return new OrgNode({
      type: NodeType.Indent,
      value: val || this.tokenIterator.currentValue,
    });
  }

  public createRawLinkNode(val: string): OrgNode {
    return new OrgNode({
      type: NodeType.RawLink,
      value: val,
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
    type: Block,
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

  public createInlineHtmlNode(): OrgNode {
    return new OrgNode({
      type: NodeType.InlineHtml,
    });
  }

  public createTagListNode(): OrgNode {
    return new OrgNode({
      type: NodeType.TagList,
    });
  }

  public createDateRangeNode(): OrgNode {
    return new OrgNode({
      type: NodeType.DateRange,
    });
  }

  public createPriorityNode(): OrgNode {
    return new OrgNode({
      type: NodeType.Priority,
    });
  }

  public createBlockPropertyNode(child?: OrgNode): OrgNode {
    const blockProperty = new OrgNode({
      type: NodeType.BlockProperty,
    });
    blockProperty.addChild(child);
    return blockProperty;
  }

  public createPropertyDrawerNode(): OrgNode {
    return new OrgNode({
      type: NodeType.PropertyDrawer,
    });
  }

  public createHorizontalRuleNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.HorizontalRule,
      value,
    });
  }

  public createPropertyNode(
    val?: string,
    children?: OrgNode[] | OrgChildrenList
  ): OrgNode {
    const propertyNode = new OrgNode({
      type: NodeType.Property,
    });
    if (children) {
      propertyNode.addChildren(children);
      return propertyNode;
    }
    const textNode = new OrgNode({
      type: NodeType.Text,
      value: val || this.tokenIterator.currentValue,
    });
    propertyNode.addChild(textNode);
    return propertyNode;
  }

  public createFixedWidthNode(): OrgNode {
    return new OrgNode({
      type: NodeType.FixedWidth,
    });
  }

  public createOperatorNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.Operator,
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

  // TODO: master move to tools
  public isListOperator(tokenValue: string): boolean {
    // NOTE: + ,- , 1), 1. - strings indicated list operator
    // https://regex101.com/r/4qq9Ob/1
    const listOperatorsRegexp = /^((-|\+) )|([1-9][0-9]*((\)|\.)) )$/;
    return !!listOperatorsRegexp.exec(tokenValue);
  }

  public createBlockHeaderNode(children: OrgChildrenList | OrgNode[]): OrgNode {
    return this.createBlockSubNode(NodeType.BlockHeader, children);
  }

  public createProgressNode(): OrgNode {
    return new OrgNode({
      type: NodeType.Progress,
    });
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

  public createLatexEnvironmentNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.LatexEnvironment,
      value,
    });
  }

  public createTableCellNode(): OrgNode {
    return new OrgNode({
      type: NodeType.TableCell,
    });
  }

  public createTableRowNode(): OrgNode {
    return new OrgNode({
      type: NodeType.TableRow,
    });
  }

  public createTableNode(): OrgNode {
    return new OrgNode({
      type: NodeType.Table,
    });
  }

  public createTableDelimiterNode(value: string): OrgNode {
    return new OrgNode({
      type: NodeType.TableDelimiter,
      value,
    });
  }

  public checkContext(): void {
    this.checkExitList();
  }

  private checkExitList(): void {
    const lastTokenWasNewLine = this.lastNode.value?.endsWith('\n');
    if (lastTokenWasNewLine && this.lastNode.isNot(NodeType.Indent)) {
      this.ctx.exitList();
    }
  }

  /**
   * Merge neighbors when they have same type, or one of them Unresolved
   */
  public mergeNeighborsNodesWithSameType(
    node?: OrgNode,
    ...mergeableTypes: NodeType[]
  ): void {
    if (!node) {
      return;
    }
    let currentNode = node;

    while (currentNode) {
      if (this.couldBeMergedIntoText(currentNode, ...mergeableTypes)) {
        currentNode.type = NodeType.Text;
      }
      if (
        this.couldBeMergedIntoText(currentNode, ...mergeableTypes) &&
        this.couldBeMergedIntoText(currentNode.prev, ...mergeableTypes)
        // TODO: master check it
        // currentNode.value
      ) {
        const prev = currentNode.prev;
        prev.appendValue(currentNode.rawValue);
        prev.parent?.removeNode(currentNode);
        this.lastNode = prev;
      } else {
        this.lastNode = currentNode;
      }
      currentNode = currentNode.next;
    }
  }

  private couldBeMergedIntoText(
    node?: OrgNode,
    ...mergeableTypes: NodeType[]
  ): boolean {
    if (!node) {
      return;
    }
    if (mergeableTypes?.length) {
      return mergeableTypes.includes(node?.type);
    }
    return (
      node.is(NodeType.Text, NodeType.Unresolved) ||
      (node.is(NodeType.Operator) &&
        ['<', '>'].includes(node.value) &&
        !node.parent.is(NodeType.Date))
    );
  }
}
