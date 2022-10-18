import { Headline, NodeType, OrgData, OrgRoot, Section } from 'types';

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

  private appendLengthToParentNodes(length: number, node?: OrgData): void {
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
}
