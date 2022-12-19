import { AstContext } from 'ast-context';
import { TokenIterator } from 'token-iterator';
import {
  Headline,
  NodeType,
  OrgData,
  OrgRoot,
  OrgText,
  Section,
  WithValue,
  WithChildren,
  UniversalOrgNode,
  List,
  OrgIndent,
} from 'types';

export class AstBuilder {
  public lastNode: OrgData;
  public lastPos: number = 0;

  // TODO: move inside context container
  // this flag is necessary to prevent the tree from moving backward to find the parent
  public insideHeadline: boolean = null;
  public insideListItem: boolean = false;
  // TODO: master move to list handler

  private lastSection: Section;
  #nodeTree: OrgData;

  get nodeTree(): OrgData {
    return this.#nodeTree;
  }

  constructor(private ctx: AstContext, private tokenIterator: TokenIterator) {
    this.initRootNode();
  }

  public increaseLastPosition(byText: string): void {
    this.lastPos += byText.length;
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
    console.log('✎: [line 53][ast-builder.ts] orgData: ', orgData);
    const parentNode = this.findParentForNodeType(orgData);
    console.log('parent node: ', parentNode);

    (parentNode as OrgRoot).children.push(orgData);
    orgData.parent = parentNode;
  }

  private findFirstParentNodeWithType(...type: NodeType[]): OrgData {
    let node = this.lastNode;
    while (node) {
      if (type.includes(node.type)) {
        return node;
      }
      node = node.parent;
    }
  }

  // TODO: this method really need refactoring
  private findParentForNodeType(srcNode: UniversalOrgNode, dstNode?: OrgData): OrgData {
    if (srcNode.parent) {
      return srcNode.parent;
    }

    if (srcNode.type === NodeType.Indent && !this.isListOperator(this.tokenIterator.currentValue) && this.lastSection) {
      return this.lastSection;
    }

    const exitFromTopList = !!this.ctx.topLevelList;
    const notIndentAfterNewLine =
      !this.ctx.nextIndentNode && this.tokenIterator.isTokenNewLine(this.tokenIterator.prevToken);

    if (notIndentAfterNewLine && exitFromTopList && !this.isListOperator(this.tokenIterator.currentValue)) {
      console.log('AMMA OPERATOR FOR LIST BLA!: ', (srcNode as any).value, '\n');

      const parent = this.ctx.topLevelList.parent;
      // this.ctx.exitList();
      return parent;
    }

    dstNode ||= this.lastNode;

    const isNestedList = (this.lastSection?.parent?.parent as List)?.level < (srcNode as List).level;

    if (
      !this.insideHeadline &&
      this.lastSection &&
      srcNode.type !== NodeType.ListItem &&
      !this.insideListItem &&
      (this.lastSection.parent.type !== NodeType.ListItem || isNestedList)
    ) {
      return this.lastSection as any;
    }

    if (!dstNode) {
      throw new Error(`Something went wrong, couldn't find parent`);
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

    if (srcNode.type === NodeType.Indent) {
      console.log('INDENT!?');
    }

    if (
      !isSourceNodeHeadline &&
      [
        NodeType.Root,
        NodeType.Headline,
        NodeType.Section,
        NodeType.Checkbox,
        NodeType.List,
        NodeType.ListItem,
      ].includes(dstNode.type)
    ) {
      return dstNode;
    }

    return this.findParentForNodeType(srcNode, dstNode.parent);
  }

  private findListParent(node: OrgData): OrgData {
    while (node) {
      if (node.type === NodeType.List) {
        return node.parent;
      }
      node = node.parent;
    }
    return;
  }

  public preserveLastPositionSnapshot(orgData: OrgData): void {
    this.lastPos = orgData.end;
    this.saveLastNode(orgData);
  }

  public saveLastNode(orgData: OrgData): void {
    this.lastNode = orgData;
  }

  public appendLengthToParentNodes(length: number, node?: UniversalOrgNode): void {
    if (!node || !length) {
      return;
    }

    // NOTE: Nodes with section have range bounded by the new line
    if (!node.section) {
      node.end = length;
    }
    if (!node.parent) {
      return;
    }
    this.appendLengthToParentNodes(length, node.parent);
  }

  /*
   * Create new nested section
   */
  public getLastSessionOrCreate(parentNode?: UniversalOrgNode): void {
    const nodeWithSection =
      parentNode ||
      (this.findFirstParentNodeWithType(NodeType.Headline, NodeType.Section, NodeType.ListItem) as UniversalOrgNode);
    const parentSectionAlreadyExists = nodeWithSection?.section || nodeWithSection?.type === NodeType.Section;

    console.log('✎: [line 179][ast-builder.ts] parentSectionAlreadyExists: ', parentSectionAlreadyExists);
    if (parentSectionAlreadyExists || !nodeWithSection) {
      return;
    }

    const section: Section = {
      type: NodeType.Section,
      start: nodeWithSection.end,
      end: nodeWithSection.end,
      children: [],
      parent: nodeWithSection as OrgData,
    };

    nodeWithSection.section = section;
    this.lastSection = section;
  }

  public exitSection(): void {
    this.lastSection = null;
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
    // TODO: master SPREAD OPERATOR
    return (
      nodes.length === 3 &&
      nodes[0]?.value === '[' &&
      (nodes[1]?.value === ' ' || nodes[1]?.value.toLowerCase() === 'x') &&
      nodes[2]?.value === ']'
    );
  }

  public getRawValueFromNode(node: UniversalOrgNode): string {
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

  public createIndentNode(start?: number, val?: string): OrgIndent {
    start = start || this.lastPos;
    val = val || this.tokenIterator.currentValue;
    const end = start + val.length;

    return {
      type: NodeType.Indent,
      start,
      end,
      value: this.tokenIterator.currentValue,
    };
  }

  public readonly listOperators = ['- ', '+ '];

  // TODO: operator matcher?
  public isListOperator(token: string): boolean {
    return this.listOperators.includes(token);
  }
}
