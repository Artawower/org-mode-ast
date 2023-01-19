import { AstBuilder } from 'ast-builder';
import type { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { NodeType, OrgStruct, InlineCode, OrgRoot, Text, PartialUniversalOrgStruct, Operator, Checkbox } from 'types';

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
    if (nodeAfterPairBracketClosed) {
      this.tryRemoveNestedInlineCode(nodeAfterPairBracketClosed);
      return nodeAfterPairBracketClosed;
    }

    this.bracketsStackPositions.push({
      childIndex: unresolvedNode.parent.children.length - 1,
      node: unresolvedNode,
    });

    return unresolvedNode;
  }

  // TODO: master place for handle dates!
  // TODO: refactor this method, so complex!
  private tryHandlePairBracket(o: OrgNode<OrgStruct>): OrgNode<OrgStruct> {
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

    const realChildren = openedBracket.node.parent.children;
    const updatedChildren = realChildren.slice(0, openedBracket.childIndex);
    const realBracketedNodeParent = openedBracket.node.parent;

    const nestedChildren = this.astBuilder.mergeUnresolvedNodes(
      realChildren.slice(openedBracket.childIndex, realChildren.length)
    );

    const orgNode = this.handleBracketSequence(nestedChildren, this.textFormattersNodeTypeMap[pairToDetect]);
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
  > = [this.handleChecboxBrackets.bind(this), this.handleDateBrackets.bind(this)];

  private handleBracketSequence(bracketedNodes: OrgNode<OrgStruct>[], type?: NodeType): OrgNode {
    const openedBracket = bracketedNodes[0];
    const closedBracket = bracketedNodes[bracketedNodes.length - 1];

    for (const bracketsHandler of this.bracketedNodesHandler) {
      const result = bracketsHandler(openedBracket, closedBracket, bracketedNodes);
      if (result) {
        return result;
      }
    }

    const formattedNode: PartialUniversalOrgStruct = {
      type,
      start: openedBracket.start,
      end: closedBracket.end,
      // children: nodesBetweenBrackets,
    };
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

  private handleDateBrackets(
    openedBracket: OrgNode<OrgStruct>,
    closedBracket: OrgNode<OrgStruct>,
    bracketedNodes: OrgNode<OrgStruct>[]
  ): OrgNode<Date> {
    return null;
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

  public clearBracketsPairs(): void {
    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const neighbors = (bracket.node.parent as OrgRoot).children;
      let childIndex = bracket.childIndex + childIndexOffset;
      const leftChild = neighbors[childIndex - 1];
      const rightChild = neighbors[childIndex + 1];

      // Offset after position changed
      bracket.node.type = NodeType.Text;

      if (leftChild?.type === NodeType.Text) {
        neighbors.splice(childIndex - 1, 1);
        bracket.node.start = leftChild.start;
        bracket.node.setValue(leftChild.value + bracket.node.value);
        childIndexOffset--;
        childIndex--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex + 1, 1);
        bracket.node.end = rightChild.end;
        bracket.node.setValue(rightChild.value);
        childIndexOffset--;
      }
    });

    this.bracketsStackPositions = [];
  }
}
