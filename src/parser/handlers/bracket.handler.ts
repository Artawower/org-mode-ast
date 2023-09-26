import {
  LinkType,
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
  linkTypes,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';
import { isNumber } from '../../tools/index.js';
import { AstContext } from 'parser/ast-context.js';

export class BracketHandler implements OrgHandler {
  readonly #dateRangeDelimiter = '--';
  readonly #priorityValueRegexp = /#([\w\d]{1})$/;
  readonly #unresolvedNodes = new OrgChildrenList();

  // readonly #urlRegexp =
  //   /(\b(https?|ftp|file):\/\/[-A-Z0-9+&@#\/%?=~_|!:,.;]*[-A-Z0-9+&@#\/%=~_|])/gi;

  // TODO: master add configuration object for parser
  private readonly listProgressSeparator = '/';

  private readonly closedOpenedBrackets = {
    ']': '[',
    '>': '<',
  };

  private readonly bracketsWithRawContent = ['$', '$$'];

  private readonly textFormattersNodeTypeMap: {
    [key: string]:
      | NodeType.Bold
      | NodeType.Crossed
      | NodeType.Italic
      | NodeType.InlineCode
      | NodeType.LatexFragment
      | NodeType.Verbatim;
  } = {
    '*': NodeType.Bold,
    '/': NodeType.Italic,
    '+': NodeType.Crossed,
    '=': NodeType.Verbatim,
    '~': NodeType.InlineCode,
    $: NodeType.LatexFragment,
    $$: NodeType.LatexFragment,
  };

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly ctx: AstContext,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const unresolvedNode = this.astBuilder.createUnresolvedNode();
    this.astBuilder.attachToTree(unresolvedNode);

    const closedBracketNode = this.tryHandlePairBracket(unresolvedNode);

    if (closedBracketNode) {
      this.removeFormattingInsideInlineCode(closedBracketNode);
      this.storeUnresolvedNode(closedBracketNode);
      this.addMetaInfo(closedBracketNode);
      return closedBracketNode;
    }

    this.ctx.bracketsStack.push(unresolvedNode);

    return unresolvedNode;
  }

  private storeUnresolvedNode(orgNode: OrgNode): void {
    this.#unresolvedNodes.push(orgNode);
  }

  private normalizeUnresolvedNode(node: OrgNode) {
    if (node.type === NodeType.Unresolved) {
      // NOTE: master when node has unresolved type it could have
      // children and value at the same time
      const rawValue = node.rawValue + (node.value ?? '');
      node.removeChildren(node.children);
      node.setValue(rawValue);
      node.type = NodeType.Text;
    }
  }

  // TODO: refactor this method, so complex!
  private tryHandlePairBracket(closedBracket: OrgNode): OrgNode {
    if (this.ctx.bracketsStack.isEmpty || this.isOpenedBracket(closedBracket)) {
      return;
    }

    const openedBracketSymbol = this.getOpenedBracket(
      this.tokenIterator.currentValue
    );

    const openedBracket = this.ctx.bracketsStack.findLast(
      (bracketNode) => bracketNode.value === openedBracketSymbol
    );

    if (!openedBracket) {
      // TODO: master should i make it as text?
      return;
    }

    openedBracket.type = NodeType.Operator;
    closedBracket.type = NodeType.Operator;

    const realParent = openedBracket.parent;

    const potentialBracketNodes = realParent.children.getNodesBetweenPairs(
      openedBracket,
      closedBracket,
      true
    );

    const orgNode = this.handleBracketSequence(potentialBracketNodes);

    if (orgNode) {
      this.normalizeBracketedNodes(orgNode, potentialBracketNodes);
    }

    return orgNode;
  }

  private normalizeBracketedNodes(
    bracketNodeParent: OrgNode,
    bracketNodes: OrgChildrenList
  ): void {
    const realParent = bracketNodes.first.parent;

    this.ctx.bracketsStack.removeNodes(bracketNodes);
    realParent.removeChildren(bracketNodes);

    const shouldHaveRawContent = this.bracketsWithRawContent.includes(
      bracketNodes.first.value
    );
    if (shouldHaveRawContent) {
      const rawValue = this.astBuilder.getRawValueFromNodes(
        bracketNodes.slice(1, -1)
      );
      const textNode = this.astBuilder.createTextNode(rawValue);
      bracketNodes = new OrgChildrenList(
        bracketNodes.first,
        textNode,
        bracketNodes.last
      );
    }

    bracketNodes = this.normalizeOrgLinkUrl(bracketNodeParent, bracketNodes);

    bracketNodeParent.addChildren(bracketNodes);
    realParent.addChild(bracketNodeParent);
    this.astBuilder.mergeNeighborsNodesWithSameType(bracketNodes.first);
  }

  private addMetaInfo(orgNode: OrgNode): void {
    if (orgNode.is(NodeType.Link)) {
      orgNode.updateMeta({
        linkType: this.determineLinkType(
          orgNode.children.get(1).children.get(1)
        ),
      });
    }
  }

  private determineLinkType(orgNode: OrgNode): LinkType {
    const link = orgNode.value;
    const imgType = this.configuration.imgExtensions.find((it) =>
      link.endsWith(it)
    );

    if (imgType) {
      return 'image';
    }

    const linkType = linkTypes.find((lt) => link.startsWith(`${lt}:`));
    if (linkType) {
      return linkType;
    }
    if (link.match(this.configuration.httpLinkRegexp)) {
      return 'network';
    }
    return 'raw';
  }

  private normalizeOrgLinkUrl(
    parent: OrgNode,
    childrenNodes: OrgChildrenList
  ): OrgChildrenList {
    if (parent.isNot(NodeType.Link)) {
      return childrenNodes;
    }
    // NOTE: Geting raw value from url link
    // need to extract raw link inside tokenizer
    const linkUrlNode = childrenNodes.get(1);

    if (linkUrlNode.length > 3) {
      const firstChild = linkUrlNode.children.first;
      const parent = firstChild.parent;
      const lastChild = linkUrlNode.children.last;
      const rawValue = this.astBuilder.getRawValueFromNodes(
        linkUrlNode.children.slice(1, -1)
      );
      const textNode = this.astBuilder.createTextNode(rawValue);
      parent.removeChildren(parent.children);
      parent.addChildren([firstChild, textNode, lastChild]);
    }
    return childrenNodes;
  }

  private readonly bracketedNodesHandler: Array<
    (bracketedNodes: OrgChildrenList) => OrgNode
  > = [
    this.handleChecboxBrackets.bind(this),
    this.handleDateBrackets.bind(this),
    this.handlePriorityBrackets.bind(this),
    this.handleListProgressBrackets.bind(this),
    this.handleFormatBrackets.bind(this),
    this.handleLinkBrackets.bind(this),
  ];

  private handleBracketSequence(bracketedNodes: OrgChildrenList): OrgNode {
    if (bracketedNodes.isEmpty) {
      return;
    }
    for (const bracketsHandler of this.bracketedNodesHandler) {
      const result = bracketsHandler(bracketedNodes);
      if (result) {
        return result;
      }
    }
  }

  private handleChecboxBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    const checkboxParent = bracketedNodes.first.parent;
    const isCheckBox = this.astBuilder.isNodesCheckbox(bracketedNodes);
    if (!isCheckBox) {
      return;
    }
    const checked = bracketedNodes.get(1)?.value?.toLowerCase() === 'x';
    const rawValue = this.astBuilder.getRawValueFromNodes(bracketedNodes);
    const checkBoxNode = this.astBuilder.createCheckboxNode(rawValue, checked);

    // checkBoxNode.addChildren(bracketedNodes);

    if (checkboxParent.type === NodeType.Headline) {
      checkboxParent.checked = checked;
    }
    return checkBoxNode;
  }

  private handleLinkBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    const isOutsideLink = this.isLinkBracketNodes(bracketedNodes);
    const isInsideLink = this.isLinkBracketNodes(
      bracketedNodes.get(1)?.children
    );

    if (isOutsideLink && !isInsideLink) {
      return new OrgNode({ type: NodeType.Unresolved });
    }

    const isLinkBracketNode = isOutsideLink && isInsideLink;

    if (!isLinkBracketNode) {
      return;
    }

    bracketedNodes.get(1).type = NodeType.LinkUrl;
    const nameExist = !!bracketedNodes.get(3);
    if (nameExist) {
      bracketedNodes.get(2).type = NodeType.LinkName;
    }

    const orgLinkNode = this.astBuilder.createLinkNode();
    return orgLinkNode;
  }

  private handleListProgressBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    if (bracketedNodes.length !== 5) {
      return;
    }
    const isOpenedProgressBracket = bracketedNodes.first.value === '[';
    const isClosedProgressBracket = bracketedNodes.last.value === ']';
    const hasDoneProgress = isNumber(bracketedNodes.get(1).value);
    const hasSeporator =
      bracketedNodes.get(2).value === this.listProgressSeparator;
    const hasDealProgress = isNumber(bracketedNodes.get(3).value);

    const isProgressBrackets =
      isOpenedProgressBracket &&
      isClosedProgressBracket &&
      hasDoneProgress &&
      hasSeporator &&
      hasDealProgress;

    if (isProgressBrackets) {
      return this.astBuilder.createProgressNode();
    }
  }

  private handlePriorityBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    // TODO: master check if priority inside headline and there no was previous priority
    const isInsideHeadline = bracketedNodes.first?.parent?.parent?.is(
      NodeType.Headline
    );
    const isPreviousNodeOperator = bracketedNodes.first?.prev?.is(
      NodeType.Operator
    );

    if (
      bracketedNodes.length !== 3 ||
      !isInsideHeadline ||
      !isPreviousNodeOperator
    ) {
      return;
    }
    const isOpenedPriorityBracket = bracketedNodes.first.value === '[';
    const isClosedPriorityBracket = bracketedNodes.last.value === ']';
    const priorityValue = bracketedNodes.get(1).value;
    const isPriorityValue = this.#priorityValueRegexp.test(priorityValue);

    const isPriorityBrackets =
      isOpenedPriorityBracket && isClosedPriorityBracket && isPriorityValue;

    if (!isPriorityBrackets) {
      return;
    }
    return this.astBuilder.createPriorityNode();
  }

  private handleFormatBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    const type = this.textFormattersNodeTypeMap[bracketedNodes.last.value];

    if (!type) {
      return;
    }

    const orgNode = new OrgNode({ type });
    // orgNode.addChildren(bracketedNodes);
    return orgNode;
  }

  private isLinkBracketNodes(nodes: OrgChildrenList): boolean {
    if (!nodes) {
      return false;
    }

    const leftBracket = nodes.first;
    const rightBracket = nodes.last;
    const isLinkBracket =
      leftBracket.type === NodeType.Operator &&
      leftBracket.value === '[' &&
      rightBracket.type === NodeType.Operator &&
      rightBracket.value === ']';

    return isLinkBracket;
  }

  private handleDateBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    if (!this.isDate(this.astBuilder.getRawValueFromNodes(bracketedNodes))) {
      return;
    }

    this.tryCreateRangeDate(bracketedNodes.first.prev);
    // TODO: check date range (previous node)
    const dateNode = this.astBuilder.createDateNode();

    return dateNode;
  }

  private tryCreateRangeDate(dateDelimiterNode?: OrgNode): void {
    if (
      !this.isDateDelimiterNode(dateDelimiterNode) ||
      !dateDelimiterNode?.prev.is(NodeType.Date)
    ) {
      return;
    }
    const dateRangeNode = this.astBuilder.createDateRangeNode();
    const parentNode = dateDelimiterNode.parent;
    const dateRangeChildren = parentNode.children.getNodesBetweenPairs(
      dateDelimiterNode.prev,
      null,
      true
    );
    parentNode.removeChildren(dateRangeChildren);
    dateRangeNode.addChildren(dateRangeChildren);
    parentNode.addChild(dateRangeNode);
  }

  private isDateDelimiterNode(node?: OrgNode): boolean {
    return node?.value === this.#dateRangeDelimiter;
  }

  // TODO: extract date date property by groups
  private isDate(text: string): boolean {
    return !!text?.match(this.configuration.dateRegexp);
  }

  private isOpenedBracket(bracketNode: OrgNode): boolean {
    return !!Object.values(this.closedOpenedBrackets).find(
      (b) => b === bracketNode.value
    );
  }

  private removeFormattingInsideInlineCode(orgNode: OrgNode): void {
    const isBlockHasRawText = orgNode.is(
      NodeType.InlineCode,
      NodeType.Verbatim
    );
    const hasExtraChild =
      orgNode.children.length !== 1 ||
      !orgNode.children?.get(1)?.is(NodeType.Text);

    if (!isBlockHasRawText || !hasExtraChild) {
      return;
    }

    const firstChildNode = orgNode.children.first;
    const lastChildNode = orgNode.children.last;

    const unmergedChildren = orgNode.children.getNodesBetweenPairs(
      firstChildNode,
      lastChildNode
    );
    const rawValue = this.astBuilder.getRawValueFromNodes(unmergedChildren);

    const newTextNode = this.astBuilder.createTextNode(rawValue);
    orgNode.setChildren([firstChildNode, newTextNode, lastChildNode]);
  }

  private getOpenedBracket(openedBracket: string): string {
    return this.closedOpenedBrackets[openedBracket] ?? openedBracket;
  }

  // TODO: master handle new line
  public handleEndOfLine(): void {
    // NOTE: another handler could unattached node from the parent.
    // It means that this node already has found pair.
    // return;
    const filteredBrackets = this.ctx.bracketsStack.filter((b) => b.parent);
    // const t = Array.from(filteredBrackets);
    // const u = Array.from(this.#unresolvedNodes.get(0).children);
    this.astBuilder.mergeNeighborsNodesWithSameType(filteredBrackets.first);
    this.#unresolvedNodes.forEach((n) => {
      if (!n.parent) {
        return;
      }
      if (n.is(NodeType.Unresolved)) {
        this.normalizeUnresolvedNode(n);
        this.astBuilder.mergeNeighborsNodesWithSameType(n);
      }
    });
    this.#unresolvedNodes.clear();
    this.ctx.bracketsStack.clear();
  }
}
