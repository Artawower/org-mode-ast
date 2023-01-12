import { AstBuilder } from 'ast-builder';
import type { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import {
  Headline,
  NodeType,
  Checkbox,
  OrgStruct,
  InlineCode,
  OrgRoot,
  Text,
  PartialUniversalOrgNode,
  WithValue,
} from 'types';

export class BracketHandler implements OrgHandler {
  private bracketsStackPositions: Array<{ childIndex: number; node: OrgNode<OrgStruct> }> = [];

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

  private tryHandlePairBracket(o: OrgNode<OrgStruct>): OrgNode<OrgStruct> {
    if (this.bracketsStackPositions.length === 0) {
      return;
    }

    const reversedBracketsStack = this.bracketsStackPositions.slice().reverse();
    const pairToDetect = this.getOpenedBracket(this.tokenIterator.currentValue);
    const foundPairIndex = reversedBracketsStack.findIndex((r) => r.node.value === pairToDetect);

    const found = foundPairIndex !== -1;

    if (!found) {
      return;
    }

    const pair = reversedBracketsStack[foundPairIndex];
    pair.node.type = NodeType.Operator;

    o.type = NodeType.Operator;

    const realChildren = pair.node.parent.children;
    const updatedChildren = realChildren.slice(0, pair.childIndex);

    const nestedChildren = this.astBuilder.mergeUnresolvedNodes(
      realChildren.slice(pair.childIndex, realChildren.length)
    );
    const isCheckBox = this.astBuilder.isNodesCheckbox(nestedChildren);
    const checked = nestedChildren[1]?.value?.toLowerCase() === 'x';

    const orgData = isCheckBox
      ? ({
          type: NodeType.Checkbox,
          start: pair.node.start,
          end: o.end,
          checked,
          value: this.astBuilder.getRawValueFromNodes(nestedChildren),
          children: [],
        } as Checkbox)
      : ({
          type: this.textFormattersNodeTypeMap[pairToDetect],
          start: pair.node.start,
          end: o.end,
          children: nestedChildren,
        } as PartialUniversalOrgNode);

    const orgNode = new OrgNode(orgData);

    updatedChildren.push(orgNode);
    this.astBuilder.lastNode = orgNode;
    pair.node.parent.setChildren(updatedChildren);

    if (isCheckBox && pair.node.parent.type === NodeType.Headline) {
      pair.node.parent.checked = checked;
    }

    reversedBracketsStack.splice(foundPairIndex, 1);
    this.bracketsStackPositions = reversedBracketsStack.reverse();

    if (orgData.type === NodeType.InlineCode) {
      // TODO: master think!
      this.removeNestedFormattingForInlineCode(orgNode as OrgNode<InlineCode>);
    }
    return orgNode;
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
    if (openedBracket === ']') {
      return '[';
    }
    return openedBracket;
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
