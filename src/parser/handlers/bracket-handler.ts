import { NodeType, OrgChildrenList, OrgHandler, OrgNode } from 'models';
import { AstBuilder } from 'parser/ast-builder';
import { TokenIterator } from 'tokenizer';

export class BracketHandler implements OrgHandler {
  private bracketsStack: OrgChildrenList = new OrgChildrenList();

  private readonly closedOpenedBrackets = {
    ']': '[',
    '>': '<',
  };

  private readonly textFormattersNodeTypeMap: {
    [key: string]:
      | NodeType.Bold
      | NodeType.Crossed
      | NodeType.Italic
      | NodeType.InlineCode
      | NodeType.Verbatim;
  } = {
    '*': NodeType.Bold,
    '/': NodeType.Italic,
    '+': NodeType.Crossed,
    '=': NodeType.Verbatim,
    '~': NodeType.InlineCode,
  };

  // NOTE: https://regex101.com/r/IPfgId/1
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
    console.log(
      '✎: [line 43][bracket-handler.ts] closedBracketNode: ',
      closedBracketNode
    );

    if (closedBracketNode) {
      this.removeFormattingInsideInlineCode(closedBracketNode);
      return closedBracketNode;
    }

    this.bracketsStack.push(unresolvedNode);

    return unresolvedNode;
  }

  public handleNewLine(): void {}

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
      this.bracketsStack.removeNodes(potentialBraketNodes);
      realParent.removeChildren(potentialBraketNodes);
      orgNode.addChildren(potentialBraketNodes);
      realParent.addChild(orgNode);
      this.astBuilder.mergeNeighborsNodesWithSameType(
        potentialBraketNodes.first
      );
    }

    return orgNode;
  }

  private readonly bracketedNodesHandler: Array<
    (bracketedNodes: OrgChildrenList) => OrgNode
  > = [
    this.handleChecboxBrackets.bind(this),
    this.handleDateBrackets.bind(this),
    this.handleLinkBrackets.bind(this),
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
    const isInlineCode = orgNode.is(NodeType.InlineCode);
    const hasExtraChild =
      orgNode.children.length !== 1 ||
      !orgNode.children?.get(1)?.is(NodeType.Text);

    if (!isInlineCode || !hasExtraChild) {
      return;
    }

    const firstChildNode = orgNode.children.first;
    const lastChildNode = orgNode.children.last;

    const unmergedChildren = orgNode.children.getNodesBetweenPairs(
      firstChildNode,
      lastChildNode
    );
    const rawValue = this.astBuilder.getRawValueFromNodes(unmergedChildren);

    // orgNode.removeChildren(unmergedChildren);
    const newTextNode = this.astBuilder.createTextNode(rawValue);
    // orgNode.removeChildren(orgNode.children);

    // orgNode.children.first.addNextNode(newTextNode);
    orgNode.setChildren([firstChildNode, newTextNode, lastChildNode]);

    // const nestedNode = orgNode.children.get(1);

    // if (nestedNode?.type !== NodeType.InlineCode) {
    //   return;
    // }

    // nestedNode.type = NodeType.Text;
    // nestedNode.setValue(this.astBuilder.getRawValueFromNode(nestedNode));
    // nestedNode.setChildren(undefined);
  }

  private getOpenedBracket(openedBracket: string): string {
    return this.closedOpenedBrackets[openedBracket] ?? openedBracket;
  }

  // TODO: master handle new line
  public clearBracketsPairs(): void {
    this.astBuilder.mergeNeighborsNodesWithSameType(this.bracketsStack.first);
    this.bracketsStack.clear();
  }
}
