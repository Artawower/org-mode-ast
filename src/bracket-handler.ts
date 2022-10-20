import { AstBuilder } from 'ast-builder';
import type { OrgHandler } from 'internal.types';
import { TokenIterator } from 'token-iterator';
import {
  Headline,
  NodeType,
  OrgBold,
  OrgCheckbox,
  OrgCrossed,
  OrgData,
  OrgInlineCode,
  OrgItalic,
  OrgRoot,
  OrgText,
  UniversalOrgNode,
  WithValue,
} from 'types';

export class BracketHandler implements OrgHandler {
  private bracketsStackPositions: Array<{ childIndex: number; node: OrgData }> = [];

  private readonly textFormattersNodeTypeMap: {
    [key: string]: NodeType.Bold | NodeType.Crossed | NodeType.Italic | NodeType.InlineCode;
  } = {
    '*': NodeType.Bold,
    '/': NodeType.Italic,
    '+': NodeType.Crossed,
    '=': NodeType.InlineCode,
  };

  constructor(private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  public handle(): OrgData {
    const orgData: OrgData = {
      type: NodeType.Unresolved,
      value: this.tokenIterator.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.value.length,
    };
    this.astBuilder.attachToTree(orgData);

    // TODO: master method for find last bracket from end
    const nodeAfterPairBracketClosed = this.tryHandlePairBracket(orgData);
    if (nodeAfterPairBracketClosed) {
      // console.log('We found bracket mathing');
      // We found pair bracket matching
      // this.attachToTree(nodeAfterPairBracketClosed);
      return nodeAfterPairBracketClosed;
    }

    this.bracketsStackPositions.push({ childIndex: (orgData.parent as OrgRoot).children.length - 1, node: orgData });

    return orgData;
  }

  private tryHandlePairBracket(o: OrgData): OrgData {
    if (this.bracketsStackPositions.length === 0) {
      return;
    }

    const reversedBracketsStack = this.bracketsStackPositions.slice().reverse();

    // TODO: master expose this logic as composition entity
    const pairToDetect = this.getOpenedBracket(this.tokenIterator.value);

    const foundPairIndex = reversedBracketsStack.findIndex((r) => (r.node as UniversalOrgNode).value === pairToDetect);

    const found = foundPairIndex !== -1;

    if (!found) {
      return;
    }

    const pair = reversedBracketsStack[foundPairIndex];
    pair.node.type = NodeType.Operator;

    o.type = NodeType.Operator;

    const realChildren = (pair.node.parent as UniversalOrgNode).children as OrgData[];
    const updatedChildren = realChildren.slice(0, pair.childIndex);

    const nestedChildren = this.astBuilder.mergeUnresolvedNodes(
      realChildren.slice(pair.childIndex, realChildren.length)
    );
    const isCheckBox = this.astBuilder.isNodesCheckbox(nestedChildren as Array<OrgData & WithValue>);
    const checked = (nestedChildren[1] as WithValue)?.value?.toLowerCase() === 'x';

    const orgData: OrgBold | OrgCrossed | OrgCheckbox | OrgItalic | OrgInlineCode = isCheckBox
      ? ({
          type: NodeType.Checkbox,
          start: pair.node.start,
          end: o.end,
          checked,
          value: this.astBuilder.getRawValueFromNodes(nestedChildren as WithValue[]),
          children: [],
          parent: o.parent,
        } as OrgCheckbox)
      : {
          type: this.textFormattersNodeTypeMap[pairToDetect],
          start: pair.node.start,
          end: o.end,
          children: nestedChildren,
          parent: o.parent,
        };

    updatedChildren.push(orgData);
    this.astBuilder.lastNode = orgData;
    (pair.node.parent as UniversalOrgNode).children = updatedChildren;

    if (isCheckBox && pair.node.parent.type === NodeType.Headline) {
      (pair.node.parent as Headline).checked = checked;
    }

    reversedBracketsStack.splice(foundPairIndex, 1);
    this.bracketsStackPositions = reversedBracketsStack.reverse();

    return orgData;
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
        (bracket.node as OrgText).value = (leftChild as OrgText).value + (bracket.node as OrgText).value;
        childIndexOffset--;
        childIndex--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex + 1, 1);
        bracket.node.end = rightChild.end;
        (bracket.node as OrgText).value += (rightChild as OrgText).value;
        childIndexOffset--;
      }
    });

    this.bracketsStackPositions = [];
  }
}
