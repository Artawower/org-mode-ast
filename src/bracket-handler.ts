import { AstBuilder } from 'ast-builder';
import type { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { prettyTreePrint } from 'tools';
import { NodeType, OrgStruct, InlineCode, Text, PartialUniversalOrgStruct, Date, Checkbox, Link } from 'types';

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

    const formattedNode: PartialUniversalOrgStruct = {
      type,
      start: openedBracket.start,
      end: closedBracket.end,
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

  private handleLinkBrackets(
    openedBracket: OrgNode<OrgStruct>,
    closedBracket: OrgNode<OrgStruct>,
    bracketedNodes: OrgNode<OrgStruct>[]
  ): OrgNode<Link> {
    const isLinkBracketNode =
      this.isLinkBracketNodes(bracketedNodes) && this.isLinkBracketNodes(bracketedNodes[1]?.children);
    console.log('✎: [line 156][bracket-handler.ts] bracketedNodes: ', bracketedNodes);
    // console.log(
    //   '✎: [line 155][bracket-handler.ts] isLinkBracketNode: ',
    //   isLinkBracketNode,
    //   openedBracket.start,
    //   closedBracket.end
    // );

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
  ): OrgNode<Date> {
    const dateParent = openedBracket.parent;
    if (bracketedNodes.length !== 3 || !this.isDate(bracketedNodes[1].value)) {
      return;
    }

    const dateNode = this.astBuilder.createDateNode(openedBracket, bracketedNodes[1], closedBracket);
    dateNode.setParent(dateParent);

    return dateNode;
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

  public clearBracketsPairs(): void {
    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const neighbors = bracket.node.parent.children;
      let childIndex = bracket.childIndex + childIndexOffset;
      const leftChild = neighbors[childIndex - 1];
      const rightChild = neighbors[childIndex + 1];

      // Offset after position changed
      bracket.node.type = NodeType.Text;

      if (leftChild?.type === NodeType.Text) {
        neighbors.splice(childIndex - 1, 1);
        bracket.node.start = leftChild.start;
        bracket.node.prependValue(leftChild.value);
        childIndexOffset--;
        childIndex--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex + 1, 1);
        bracket.node.end = rightChild.end;
        bracket.node.appendValue(rightChild.value);
        childIndexOffset--;
      }
    });

    this.bracketsStackPositions = [];
  }
}
