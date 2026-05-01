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

    const lineNodeSet = new Set(lineNodes);
    const remainingChildren = [...parent.children].filter(
      (n) => !lineNodeSet.has(n)
    );

    parent.removeChildren([...parent.children]);

    const clockNode = new OrgNode({ type: NodeType.Clock });
    clockNode.addChildren(this.buildClockChildren(lineNodes));
    parent.addChild(clockNode);

    remainingChildren.forEach((child) => parent.addChild(child));
    this.astBuilder.saveLastNode(clockNode.children?.last ?? clockNode);
  }

  private buildClockChildren(nodes: OrgNode[]): OrgNode[] {
    return nodes.flatMap((node) =>
      node.is(NodeType.Text) && CLOCK_DURATION_RE.test(node.value)
        ? this.splitDurationNode(node)
        : [node]
    );
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
