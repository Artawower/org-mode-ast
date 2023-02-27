import {
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';
import { isNumber } from '../../tools/index.js';

export class BracketHandler implements OrgHandler {
  private bracketsStack: OrgChildrenList = new OrgChildrenList();

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

  // NOTE: https://regex101.com/r/IPfgId/1
  // TODO: master make regexp for time with shifting
  private readonly dateRegex =
    /\d{4}-\d{2}-\d{2} (Mon|Tue|Wed|Thu|Fri|Sat|Sun)( \d{2}:\d{2})?$/;

  constructor(
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const unresolvedNode = this.astBuilder.createUnresolvedNode();
    this.astBuilder.attachToTree(unresolvedNode);

    const closedBracketNode = this.tryHandlePairBracket(unresolvedNode);

    if (closedBracketNode) {
      this.removeFormattingInsideInlineCode(closedBracketNode);
      return closedBracketNode;
    }

    this.bracketsStack.push(unresolvedNode);

    return unresolvedNode;
  }

  public handleEndOfLine(): void {}

  // TODO: refactor this method, so complex!
  private tryHandlePairBracket(closedBracket: OrgNode): OrgNode {
    if (this.bracketsStack.isEmpty || this.isOpenedBracket(closedBracket)) {
      return;
    }

    const closedBracketSymbol = this.getOpenedBracket(
      this.tokenIterator.currentValue
    );
    const openedBracket = this.bracketsStack.findLast(
      (bracketNode) => bracketNode.value === closedBracketSymbol
    );

    if (!openedBracket) {
      return;
    }

    openedBracket.type = NodeType.Operator;
    closedBracket.type = NodeType.Operator;

    const realParent = openedBracket.parent;

    const potentialBraketNodes = realParent.children.getNodesBetweenPairs(
      openedBracket,
      closedBracket,
      true
    );

    const orgNode = this.handleBracketSequence(potentialBraketNodes);

    if (orgNode) {
      this.normalizeBracketedNodes(orgNode, potentialBraketNodes);
    }

    return orgNode;
  }

  private normalizeBracketedNodes(
    bracketNodeParent: OrgNode,
    bracketNodes: OrgChildrenList
  ): void {
    const realParent = bracketNodes.first.parent;

    this.bracketsStack.removeNodes(bracketNodes);
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

    bracketNodeParent.addChildren(bracketNodes);
    realParent.addChild(bracketNodeParent);
    this.astBuilder.mergeNeighborsNodesWithSameType(bracketNodes.first);
  }

  private readonly bracketedNodesHandler: Array<
    (bracketedNodes: OrgChildrenList) => OrgNode
  > = [
    this.handleChecboxBrackets.bind(this),
    this.handleDateBrackets.bind(this),
    this.handleLinkBrackets.bind(this),
    this.handleListProgressBrackets.bind(this),
    this.handleFormatBrackets.bind(this),
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

    return (
      (nodes?.length === 4 || nodes?.length === 3) &&
      leftBracket.type === NodeType.Operator &&
      leftBracket.value === '[' &&
      rightBracket.type === NodeType.Operator &&
      rightBracket.value === ']'
    );
  }

  private handleDateBrackets(bracketedNodes: OrgChildrenList): OrgNode {
    if (
      bracketedNodes.length !== 3 ||
      !this.isDate(bracketedNodes.get(1).value)
    ) {
      return;
    }

    const dateNode = this.astBuilder.createDateNode();

    return dateNode;
  }

  private isDate(text: string): boolean {
    return !!text?.match(this.dateRegex);
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
  public clearBracketsPairs(): void {
    // NOTE: another handler could unattached node from the parent.
    // It means that this node already has found pair.
    const filteredBrackets = this.bracketsStack.filter((b) => b.parent);
    this.astBuilder.mergeNeighborsNodesWithSameType(filteredBrackets.first);
    this.bracketsStack.clear();
  }
}
