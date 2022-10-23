import { Headline, NodeType, OrgData, OrgRoot, OrgText, Section, WithValue, WithChildren, OrgInlineCode } from 'types';

export class AstBuilder {
  public lastNode: OrgData;
  public lastPos: number = 0;
  public insideHeadline: boolean = false;

  #nodeTree: OrgData;

  get nodeTree(): OrgData {
    return this.#nodeTree;
  }

  private lastSection: Section;

  constructor() {
    this.initRootNode();
  }

  private initRootNode(): void {
    this.#nodeTree = {
      type: NodeType.Root,
      start: 0,
      end: 0,
      children: [],
    };
    this.preserveLastPositionSnapshot(this.nodeTree);
  }

  public attachToTree(orgData: OrgData): void {
    const parentNode = this.findParentForNodeType(orgData);
    (parentNode as OrgRoot).children.push(orgData);
    orgData.parent = parentNode;
  }

  private findParentForNodeType(srcNode: OrgData, dstNode?: OrgData): OrgData {
    if (!this.insideHeadline && this.lastSection) {
      return this.lastSection as any;
    }

    dstNode ||= this.lastNode;

    if (!dstNode) {
      throw new Error(`Something wen wrong, couldn't find parent`);
    }

    if (dstNode.type === NodeType.Root) {
      return dstNode;
    }

    const isSourceNodeHeadline = srcNode.type === NodeType.Headline;
    const isTargetNodeHeadline = dstNode.type === NodeType.Headline;

    if (isSourceNodeHeadline && isTargetNodeHeadline && (<Headline>srcNode).level > (<Headline>dstNode).level) {
      return (dstNode as Headline).section;
    }

    const isSrcListItem = srcNode.type === NodeType.ListItem;
    const isTargetList = dstNode.type === NodeType.List;

    if (isSrcListItem) {
      return isTargetList ? dstNode : this.findParentForNodeType(srcNode, dstNode.parent);
    }

    if (
      !isSourceNodeHeadline &&
      [NodeType.Root, NodeType.Headline, NodeType.Section, NodeType.Checkbox, NodeType.ListItem].includes(dstNode.type)
    ) {
      return dstNode;
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  public preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  public saveLastNode(orgData: OrgData): void {
    this.lastNode = orgData;
  }

  public appendLengthToParentNodes(length: number, node?: OrgData): void {
    if (!node || !length) {
      return;
    }

    node.end = length;
    if (!node.parent) {
      return;
    }
    this.appendLengthToParentNodes(length, node.parent);
  }

  /*
   * Create new nested section
   */
  public initNewSection(): void {
    const headline = this.lastNode.parent as Headline;
    const section: Section = {
      type: NodeType.Section,
      start: headline.end,
      end: headline.end,
      children: [],
      parent: headline?.parent,
    };

    headline.section = section;
    this.lastSection = section;
  }

  public mergeUnresolvedNodes(nodes: OrgData[]): OrgData[] {
    const mergedNodes: OrgData[] = [];
    nodes.forEach((n) => {
      const lastNode = mergedNodes[mergedNodes.length - 1];

      if (n.type === NodeType.Unresolved) {
        (n as OrgData).type = NodeType.Text;
      }

      if (!lastNode) {
        mergedNodes.push(n);
        return;
      }
      if (lastNode.type === NodeType.Text && n.type === NodeType.Text) {
        lastNode.end = n.end;
        (lastNode as OrgText).value += (n as OrgText).value;
        return;
      }
      mergedNodes.push(n);
    });
    return mergedNodes;
  }

  // Section of helpers function. Consider moving them to separate class

  public isNodesCheckbox(nodes: Array<OrgData & WithValue>): boolean {
    return (
      nodes.length === 3 &&
      nodes[0]?.value === '[' &&
      (nodes[1]?.value === ' ' || nodes[1]?.value.toLowerCase() === 'x') &&
      nodes[2]?.value === ']'
    );
  }

  public getRawValueFromNode(node: OrgData): string {
    if ((node as WithValue).value) {
      return (node as WithValue).value;
    }
    if ((node as WithChildren).children) {
      return (node as WithChildren).children.map((n) => this.getRawValueFromNode(n)).join('');
    }
  }

  public parentNodeExist(node: OrgData, types: NodeType | NodeType[]): boolean {
    if (!Array.isArray(types)) {
      types = [types];
    }

    console.log('🦄: [line 161][ast-builder.ts] [35mnode.parent: ', node.parent);
    if (!node.parent) {
      return false;
    }
    if (types.includes(node.parent.type)) {
      return true;
    }
    return this.parentNodeExist(node.parent, types);
  }

  public getRawValueFromNodes(nodes: WithValue[]): string {
    // TODO: nested nodes!
    return nodes.map((n) => n?.value).join('');
  }
}
