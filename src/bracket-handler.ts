import { AstBuilder } from 'ast-builder';
import type { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { NodeType, OrgStruct, InlineCode, Text, PartialUniversalOrgStruct, Checkbox, Link } from 'types';

export class BracketHandler implements OrgHandler {
  private bracketsStackPositions: Array<{ childIndex: number; node: OrgNode<OrgStruct> }> = [];

  private readonly closedOpenedBrackets = {
    ']': '[',
    '>': '<',
  };

  private readonly textFormattersNodeTypeMap: {
    [key: string]: NodeType.Bold | NodeType.Crossed | NodeType.Italic | NodeType.InlineCode;
  } = {
    '*': NodeType.Bold,
    '/': NodeType.Italic,
    '+': NodeType.Crossed,
    '=': NodeType.InlineCode,
  };

  // NOTE: https://regex101.com/r/IPfgId/1
  private readonly dateRegex = /\d{4}-\d{2}-\d{2} (Mon|Tue|Wed|Thu|Fri|Sat|Sun)( \d{2}:\d{2})?$/;

  constructor(private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  public handle(): OrgNode<OrgStruct> {
    const unresolved: OrgStruct = {
      type: NodeType.Unresolved,
      value: this.tokenIterator.currentValue,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.currentValue.length,
    };
    const unresolvedNode = new OrgNode<OrgStruct>(unresolved);
    this.astBuilder.attachToTree(unresolvedNode);

    const nodeAfterPairBracketClosed = this.tryHandlePairBracket(unresolvedNode);
    // console.log('✎: [line 40][bracket-handler.ts] nodeAfterPairBracketClosed: ', nodeAfterPairBracketClosed);
    if (nodeAfterPairBracketClosed) {
      this.tryRemoveNestedInlineCode(nodeAfterPairBracketClosed);
      return nodeAfterPairBracketClosed;
    }

    // if (!this.isOpenedBracket(unresolvedNode)) {
    //   return unresolvedNode;
    // }

    this.bracketsStackPositions.push({
      childIndex: unresolvedNode.parent.children.length - 1,
      node: unresolvedNode,
    });

    return unresolvedNode;
  }

  public handleNewLine(): void {
    // if (!this.bracketsStackPositions.length) {
    //   return;
    // }
    // const parent = this.bracketsStackPositions[0].node.parent;
    // parent.type = NodeType.Text;
    // parent.value = this.astBuilder.getRawValueFromNodes(this.bracketsStackPositions.map((sp) => sp.node));
    // parent.removeChildren();
  }

  // TODO: refactor this method, so complex!
  private tryHandlePairBracket(o: OrgNode<OrgStruct>): OrgNode<OrgStruct> {
    // TODO: master don't check for opened bracket
    if (this.bracketsStackPositions.length === 0 || this.isOpenedBracket(o)) {
      return;
    }

    const reversedBracketsStack = this.bracketsStackPositions.slice().reverse();
    const pairToDetect = this.getOpenedBracket(this.tokenIterator.currentValue);
    const foundPairIndex = reversedBracketsStack.findIndex((r) => r.node.value === pairToDetect);

    const found = foundPairIndex !== -1;

    if (!found) {
      return;
    }

    const openedBracket = reversedBracketsStack[foundPairIndex];
    openedBracket.node.type = NodeType.Operator;

    o.type = NodeType.Operator;

    const realParent = openedBracket.node.parent;
    const prevSibling = openedBracket.node.prev;
    const realChildren = realParent.children;
    const updatedChildren = realChildren.slice(0, openedBracket.childIndex);
    const realBracketedNodeParent = openedBracket.node.parent;

    const nestedChildren = this.astBuilder.mergeUnresolvedNodes(
      realChildren.slice(openedBracket.childIndex, realChildren.length)
    );

    const orgNode = this.handleBracketSequence(nestedChildren, this.textFormattersNodeTypeMap[pairToDetect]);

    // if (!orgNode) {
    //   return;
    // }
    orgNode.setParent(realParent);
    orgNode.setPrev(prevSibling);
    prevSibling?.setNext(orgNode);

    updatedChildren.push(orgNode);
    this.astBuilder.lastNode = orgNode;
    realBracketedNodeParent.setChildren(updatedChildren);

    reversedBracketsStack.splice(foundPairIndex, 1);

    this.bracketsStackPositions = reversedBracketsStack.reverse();

    if (orgNode.type === NodeType.InlineCode) {
      // TODO: master think!
      this.removeNestedFormattingForInlineCode(orgNode as OrgNode<InlineCode>);
    }
    return orgNode;
  }

  private readonly bracketedNodesHandler: Array<
    (
      openedBracket: OrgNode<OrgStruct>,
      closedBracket: OrgNode<OrgStruct>,
      bracketedNodes: OrgNode<OrgStruct>[]
    ) => OrgNode<OrgStruct>
  > = [this.handleChecboxBrackets.bind(this), this.handleDateBrackets.bind(this), this.handleLinkBrackets.bind(this)];

  private handleBracketSequence(bracketedNodes: OrgNode<OrgStruct>[], type?: NodeType): OrgNode {
    const openedBracket = bracketedNodes[0];
    const closedBracket = bracketedNodes[bracketedNodes.length - 1];

    for (const bracketsHandler of this.bracketedNodesHandler) {
      const result = bracketsHandler(openedBracket, closedBracket, bracketedNodes);
      if (result) {
        return result;
      }
    }

    // NOTE: not a formatted text
    // console.log('✎: [line 141][bracket-handler.ts] type: ', type);
    // if (!type) {
    //   return;
    // }

    const formattedNode: PartialUniversalOrgStruct = {
      type,
      start: openedBracket.start,
      end: closedBracket.end,
    };
    // console.log('✎: [line 140][bracket-handler.ts] formattedNode: ', formattedNode);
    const orgNode = new OrgNode(formattedNode);
    orgNode.addChildren(bracketedNodes);
    return orgNode;
  }

  private handleChecboxBrackets(
    openedBracket: OrgNode<OrgStruct>,
    closedBracket: OrgNode<OrgStruct>,
    bracketedNodes: OrgNode<OrgStruct>[]
  ): OrgNode<Checkbox> {
    const checkboxParent = openedBracket.parent;
    const isCheckBox = this.astBuilder.isNodesCheckbox(bracketedNodes);
    if (!isCheckBox) {
      return;
    }
    const checked = bracketedNodes[1]?.value?.toLowerCase() === 'x';
    const rawValue = this.astBuilder.getRawValueFromNodes(bracketedNodes);
    const checkBoxNode = this.astBuilder.createCheckboxNode(openedBracket.start, closedBracket.end, rawValue, checked);
    checkBoxNode.addChildren(bracketedNodes);

    if (checkboxParent.type === NodeType.Headline) {
      checkboxParent.checked = checked;
    }
    return checkBoxNode;
  }

  private handleLinkBrackets(
    openedBracket: OrgNode<OrgStruct>,
    closedBracket: OrgNode<OrgStruct>,
    bracketedNodes: OrgNode<OrgStruct>[]
  ): OrgNode<Link> {
    const isLinkBracketNode =
      this.isLinkBracketNodes(bracketedNodes) && this.isLinkBracketNodes(bracketedNodes[1]?.children);

    if (!isLinkBracketNode) {
      return;
    }

    bracketedNodes[1].type = NodeType.LinkUrl;
    const nameExist = !!bracketedNodes[3];
    if (nameExist) {
      bracketedNodes[2].type = NodeType.LinkName;
    }

    const orgLinkNode = this.astBuilder.createLinkNode(openedBracket.start, closedBracket.end, bracketedNodes);
    return orgLinkNode;
  }

  private isLinkBracketNodes(nodes: OrgNode[]): boolean {
    if (!nodes) {
      return false;
    }
    const leftBracket = nodes[0];
    const rightBracket = nodes.slice(-1)?.[0];

    return (
      (nodes?.length === 4 || nodes?.length === 3) &&
      leftBracket.type === NodeType.Operator &&
      leftBracket.value === '[' &&
      rightBracket.type === NodeType.Operator &&
      rightBracket.value === ']'
    );
  }

  private handleDateBrackets(
    openedBracket: OrgNode<OrgStruct>,
    closedBracket: OrgNode<OrgStruct>,
    bracketedNodes: OrgNode<OrgStruct>[]
  ): OrgNode {
    if (bracketedNodes.length !== 3 || !this.isDate(bracketedNodes[1].value)) {
      return;
      // this.tryMergeNotDate(bracketedNodes);
    }

    const dateNode = this.astBuilder.createDateNode(openedBracket, bracketedNodes[1], closedBracket);

    return dateNode;
  }

  private tryMergeNotDate(bracketedNodes: OrgNode<OrgStruct>[]): OrgNode {
    if (bracketedNodes[0].value !== '<' || bracketedNodes[bracketedNodes.length - 1].value !== '>') {
      return;
    }
    const parent = bracketedNodes[0].parent;
    // DEBUG: here is incorrect call, need to merge nodes
    const value = this.astBuilder.getRawValueFromNodes(bracketedNodes);

    const textNode = this.astBuilder.createTextNode(bracketedNodes[0].start, value);
    textNode.parent = parent;
    return textNode;
  }

  private isDate(text: string): boolean {
    return !!text?.match(this.dateRegex);
  }

  private isOpenedBracket(bracketNode: OrgNode<OrgStruct>): boolean {
    return !!Object.values(this.closedOpenedBrackets).find((b) => b === bracketNode.value);
  }

  private tryRemoveNestedInlineCode(orgNode: OrgNode<OrgStruct>): void {
    // TODO: master move to node impl
    if (![NodeType.Bold, NodeType.Crossed, NodeType.Italic].includes(orgNode.type)) {
      return;
    }

    const nestedNode = orgNode.children[1];

    if (nestedNode?.type !== NodeType.InlineCode) {
      return;
    }

    nestedNode.type = NodeType.Text;
    nestedNode.setValue(this.astBuilder.getRawValueFromNode(nestedNode));
    nestedNode.setChildren(undefined);
  }

  private removeNestedFormattingForInlineCode(inlineCodeNode: OrgNode<InlineCode>): OrgNode<InlineCode> {
    const value = this.astBuilder.getRawValueFromNode(inlineCodeNode.children[1]);

    // TODO: master move to ast builder
    inlineCodeNode.children[1] = new OrgNode<Text>({
      value,
      start: inlineCodeNode.children[1].start,
      end: inlineCodeNode.children[1].start + value.length,
      type: NodeType.Text,
    });

    return inlineCodeNode;
  }

  private getOpenedBracket(openedBracket: string): string {
    return this.closedOpenedBrackets[openedBracket] ?? openedBracket;
  }

  // TODO: master handle new line
  public clearBracketsPairs(): void {
    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const currentNode = bracket.node;
      currentNode.type = NodeType.Text;
      const children = currentNode.parent.children;
      let childIndex = bracket.childIndex + childIndexOffset;

      if (currentNode.prev?.type === NodeType.Text) {
        currentNode.prependValue(currentNode.prev.value);
        currentNode.start = currentNode.prev.start;
        currentNode.setPrev(currentNode.prev.prev);
        currentNode.prev?.setNext(currentNode);
        children.splice(childIndex - 1, 1);
        --childIndexOffset;
        --childIndex;
      }

      if (currentNode.next?.type === NodeType.Text) {
        currentNode.appendValue(currentNode.next.value);
        currentNode.end = currentNode.next.end;
        currentNode.setNext(currentNode.next.next);
        currentNode.next?.setPrev(currentNode);
        children.splice(childIndex + 1, 1);
        --childIndexOffset;
      }
    });
    // this.astBuilder.mergeNeighborsNodesWithSameType(this.bracketsStackPositions[0]?.node);

    this.bracketsStackPositions = [];
  }
}
