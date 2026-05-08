import { NodeType, OrgNode } from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';

const CLOCK_DURATION_RE = /^(\s*=>\s+)(\d+:\d{2})$/;

export class ClockHandler {
  constructor(private readonly astBuilder: AstBuilder) {}

  public handleToken(): OrgNode {
    const node = this.astBuilder.createNodeFromToken(NodeType.ClockKeyword);
    this.astBuilder.attachToTree(node);
    return node;
  }

  public handleNewLine(): void {
    const lineNodes = this.collectClockLineNodes();
    if (!lineNodes) {
      return;
    }
    this.rewrapAsClock(lineNodes);
  }

  private collectClockLineNodes(): OrgNode[] | null {
    const lastNode = this.astBuilder.lastNode;
    const clockKeyword = this.findClockKeywordInLine(lastNode);
    if (!clockKeyword) {
      return null;
    }
    return this.collectLineNodes(clockKeyword);
  }

  private findClockKeywordInLine(node: OrgNode): OrgNode | null {
    let current = node;
    while (current) {
      if (current.is(NodeType.ClockKeyword)) {
        return current;
      }
      if (current.is(NodeType.NewLine)) {
        return null;
      }
      current = current.prev;
    }
    return null;
  }

  private collectLineNodes(clockKeyword: OrgNode): OrgNode[] {
    const nodes: OrgNode[] = [];
    let current: OrgNode = clockKeyword;
    while (current && !current.is(NodeType.NewLine)) {
      nodes.push(current);
      current = current.next;
    }
    return nodes;
  }

  private rewrapAsClock(lineNodes: OrgNode[]): void {
    const parent = lineNodes[0].parent;
    if (!parent) {
      return;
    }

    const allChildren = [...parent.children];
    const clockStart = allChildren.indexOf(lineNodes[0]);
    const before = allChildren.slice(0, clockStart);
    const after = allChildren.slice(clockStart + lineNodes.length);

    parent.removeChildren(allChildren);

    const clockNode = new OrgNode({ type: NodeType.Clock });
    clockNode.addChildren(this.buildClockChildren(lineNodes));

    before.forEach((child) => parent.addChild(child));
    parent.addChild(clockNode);
    after.forEach((child) => parent.addChild(child));

    this.astBuilder.saveLastNode(clockNode.children?.last ?? clockNode);
  }

  private buildClockChildren(nodes: OrgNode[]): OrgNode[] {
    const durationStart = this.findDurationStartIndex(nodes);
    if (durationStart === -1) {
      return nodes;
    }
    const durationNodes = nodes.slice(durationStart);
    const durationValue = durationNodes.map((n) => n.value ?? '').join('');
    const mergedNode = new OrgNode({
      type: NodeType.Text,
      value: durationValue,
      start: durationNodes[0].start,
      end: durationNodes[durationNodes.length - 1].end,
    });
    return [
      ...nodes.slice(0, durationStart),
      ...this.splitDurationNode(mergedNode),
    ];
  }

  private findDurationStartIndex(nodes: OrgNode[]): number {
    const lastDateIndex = this.findLastDateNodeIndex(nodes);
    if (lastDateIndex === -1) {
      return -1;
    }
    const afterDateNodes = nodes.slice(lastDateIndex + 1);
    const combined = afterDateNodes.map((n) => n.value ?? '').join('');
    return CLOCK_DURATION_RE.test(combined) ? lastDateIndex + 1 : -1;
  }

  private findLastDateNodeIndex(nodes: OrgNode[]): number {
    for (let i = nodes.length - 1; i >= 0; i--) {
      if (nodes[i].is(NodeType.Date, NodeType.DateRange)) {
        return i;
      }
    }
    return -1;
  }

  private splitDurationNode(textNode: OrgNode): OrgNode[] {
    const match = CLOCK_DURATION_RE.exec(textNode.value);
    if (!match) {
      return [textNode];
    }
    const [, operatorValue, durationValue] = match;
    const operatorEnd = textNode.start + operatorValue.length;

    const durationNode = new OrgNode({ type: NodeType.ClockDuration });
    durationNode.addChildren([
      new OrgNode({
        type: NodeType.Operator,
        value: operatorValue,
        start: textNode.start,
        end: operatorEnd,
      }),
      new OrgNode({
        type: NodeType.Text,
        value: durationValue,
        start: operatorEnd,
        end: textNode.end,
      }),
    ]);
    return [durationNode];
  }
}
