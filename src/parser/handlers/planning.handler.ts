import { NodeType, OrgNode } from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';

export class PlanningHandler {
  constructor(private readonly astBuilder: AstBuilder) {}

  public handleToken(): OrgNode {
    const node = this.astBuilder.createNodeFromToken(NodeType.PlanningKeyword);
    this.astBuilder.attachToTree(node);
    return node;
  }

  public handleNewLine(): void {
    const section = this.findFirstLineHeadlineSection();
    if (!section) {
      return;
    }
    const lineNodes = this.collectFirstLineNodes(section);
    if (!this.isPlanningLine(lineNodes)) {
      return;
    }
    this.rewrapSection(section, lineNodes);
  }

  private findFirstLineHeadlineSection(): OrgNode | null {
    let node = this.astBuilder.lastNode;
    while (node) {
      if (node.is(NodeType.Section) && node.parent?.is(NodeType.Headline)) {
        return this.isSectionFirstLine(node) ? node : null;
      }
      node = node.parent;
    }
    return null;
  }

  private isSectionFirstLine(section: OrgNode): boolean {
    return !section.children?.find((n) => n.is(NodeType.NewLine));
  }

  private collectFirstLineNodes(section: OrgNode): OrgNode[] {
    const children = [...section.children];
    const newLineIndex = children.findIndex((n) => n.is(NodeType.NewLine));
    return newLineIndex === -1 ? children : children.slice(0, newLineIndex + 1);
  }

  private isPlanningLine(nodes: OrgNode[]): boolean {
    return nodes.some((n) => n.is(NodeType.PlanningKeyword));
  }

  private rewrapSection(section: OrgNode, lineNodes: OrgNode[]): void {
    const lineNodeSet = new Set(lineNodes);
    const remainingChildren = [...section.children].filter(
      (n) => !lineNodeSet.has(n)
    );

    section.removeChildren([...section.children]);

    const planningNode = new OrgNode({ type: NodeType.Planning });
    planningNode.addChildren(lineNodes);
    section.addChild(planningNode);

    remainingChildren.forEach((child) => section.addChild(child));

    this.updateLastNode(planningNode);
  }

  private updateLastNode(planningNode: OrgNode): void {
    const lastChild = planningNode.children?.last;
    if (lastChild) {
      this.astBuilder.saveLastNode(lastChild);
    }
  }
}
