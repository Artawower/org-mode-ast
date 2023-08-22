import { prettyTreePrint } from '../tools/index.js';
import { NodeAlreadyHaveParent, NodeCouldNotHaveChildren } from './errors.js';
import { OrgChildrenList } from './org-children-list.js';
import { BlockProperties, MetaInfo, NodeType, OrgStruct } from './types.js';

export class OrgNode {
  public type!: NodeType;
  public safeCheckMode = false;

  value: string;

  public parent?: OrgNode;
  public start = 0;
  public end = 0;
  public meta: MetaInfo;

  public children?: OrgChildrenList;

  private _prev?: OrgNode;
  get prev() {
    return this._prev;
  }

  get childrenList(): OrgNode[] {
    return this.children?.asArray() || [];
  }

  get lastChild(): OrgNode {
    return this.children?.last;
  }

  private _next?: OrgNode;
  get next() {
    return this._next;
  }

  // TODO: add getter with generic type checker
  private _level?: number;
  get level(): number {
    return this._level;
  }

  /**
   * Some nodes, like headlines and list items, could have a title and section content
   */
  private _title?: OrgNode;
  get title(): OrgNode {
    return this._title;
  }

  private _section?: OrgNode;
  get section(): OrgNode {
    return this._section;
  }

  private _ordered?: boolean;
  get ordered(): boolean {
    return this._ordered;
  }

  private _properties?: BlockProperties;
  get properties(): BlockProperties {
    return this._properties;
  }

  private _checked?: boolean;
  get checked(): boolean {
    return this._checked;
  }

  set checked(checked: boolean) {
    this._checked = checked;
  }

  get length(): number {
    return this.end - this.start;
  }

  /** Return raw text value from node, included nested nodes */
  get rawValue(): string {
    let val = '';
    if (this.title?.children?.length) {
      val += this.getRawValueFromNodes(this.title.children);
    }
    if (this.section?.children?.length) {
      val += this.getRawValueFromNodes(this.section.children);
    }
    if (this.children?.length) {
      val += this.getRawValueFromNodes(this.children);
    }
    if (this.value) {
      val += this.value;
    }
    return val;
  }

  get cleanValue(): string {
    if (this.value) {
      return this.value;
    }
    const titleValue = this.title?.cleanValue ?? '';
    const sectionValue = this.section?.cleanValue ?? '';
    const childrenValue =
      this.children
        ?.map((n) => {
          if (n.is(NodeType.Operator, NodeType.NewLine)) {
            return '';
          }
          return n.cleanValue;
        })
        ?.join('') ?? '';

    return `${titleValue}${sectionValue}${childrenValue}`;
  }

  private getRawValueFromNodes(nodes: OrgChildrenList): string {
    return nodes
      .map((n) => {
        return n.rawValue;
      })
      .join('');
  }

  constructor(nodeData: OrgStruct) {
    this.type = nodeData.type;
    if (nodeData.section) {
      this._section = new OrgNode(nodeData.section);
    }
    this.value = nodeData.value;
    this._level = nodeData.level;
    this._title = nodeData.title;
    this._ordered = nodeData.ordered;
    this._properties = nodeData.properties;
    this._checked = nodeData.checked;
    this.addRawChildren(nodeData.children);
  }

  private initPositions(): void {
    if (this.end) {
      const diff = this.end - this.start;
      this.end = diff;
      this.start = 0;
      return;
    }
    if (this.value) {
      this.end = this.start + this.value?.length ?? 0;
      return;
    }
  }

  public setParent(parent: OrgNode) {
    this.parent = parent;
  }

  public setPrev(prev: OrgNode) {
    this._prev = prev;
  }

  public setNext(next: OrgNode) {
    this._next = next;
  }

  public updateMeta(partialInfo: MetaInfo): void {
    this.meta = { ...(this.meta ?? {}), ...partialInfo };
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  public setProperties(properties: { [key: string]: any }) {
    this._properties = properties;
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  public mergeProperties(properties: { [key: string]: any }) {
    this._properties = { ...(this._properties ?? {}), ...properties };
  }

  private validateAddedChild(child: OrgNode): void {
    if (this.is(NodeType.Text)) {
      throw new NodeCouldNotHaveChildren(child);
    }
    if (child.parent) {
      throw new NodeAlreadyHaveParent(child);
    }
  }

  public addChild(child: OrgNode): OrgNode {
    if (this.title) {
      this.title.addChild(child);
      return;
    }
    this.validateAddedChild(child);
    child.setParent(this);
    const subtreeLength = child.parent?.children?.last?.end ?? child.parent.end;
    if (!this.children) {
      this.children = new OrgChildrenList();
    }
    child.setPrev(this.lastChild);
    child.setNext(null);
    this.lastChild?.setNext(child);
    this.children.push(child);

    child.initPositions();
    child.recalculateParentEnd(child.length);
    if (subtreeLength) {
      child.recalculateForEachNestedNodes(subtreeLength);
    }
    child.calculateNodeProperties();
    return child;
  }

  public setChildren(children?: OrgChildrenList | OrgNode[]) {
    this.removeChildren(this.children);
    this.children = new OrgChildrenList();

    children?.forEach((child: OrgNode) => {
      this.addChild(child);
    });
  }

  public resetChildren(): void {
    const diff = this.end - this.children?.last.end;
    this.end -= diff;
    this.recalculateParentEnd(-diff);
    this.recalculatePositionsForNeighbors(-diff);
    this.children = undefined;
  }

  public removeChildren(nodes?: OrgNode[] | OrgChildrenList): void {
    nodes?.forEach((node: OrgNode) => this.removeNode(node));
  }

  public removeNode(node: OrgNode): void {
    const realNodeDiff = node.length;
    node.recalculateParentEnd(-realNodeDiff);
    node.recalculatePositionsForNeighbors(-realNodeDiff);

    node.setParent(null);
    node.prev?.setNext(node.next);
    node.next?.setPrev(node.prev);

    // NOTE: balance range of removed nodes
    node.rebalanceRanges();

    this.children.removeNodes([node]);
  }

  private rebalanceRanges(respectNeighbors?: boolean): void {
    const diff = this.start;
    if (!diff) {
      return;
    }
    this.initPositions();
    if (respectNeighbors) {
      this.start += this.prev?.end ?? this.parent?.start ?? 0;
      this.end += this.prev?.end ?? this.parent?.start ?? 0;
    }
    this.forEachNestedChildren((node) => {
      node.rebalanceRanges(true);
    });
  }

  private recalculateParentEnd(diff: number): void {
    if (!this.parent || !diff) {
      return;
    }
    this.parent.end += diff;
    this.parent.recalculateParentEnd(diff);
    this.parent.recalculatePositionsForNeighbors(diff);
  }

  private recalculateForEachNestedNodes(diff: number): void {
    let currentNode = this as OrgNode;
    while (currentNode) {
      currentNode.end += diff;
      currentNode.start += diff;
      currentNode.title?.recalculateForEachNestedNodes(diff);
      currentNode.children?.first?.recalculateForEachNestedNodes(diff);
      currentNode = currentNode.next;
    }
  }

  // TODO: master create special abstraction to change buffer properties
  // and finding parent nodes
  // REFACTOR
  /**
   * Go through all children and update meta information about current nodes.
   * Including properties, list counter, etc
   */
  public calculateNodeProperties(): void {
    const collectedProperties = {};
    if (this.is(NodeType.PropertyDrawer)) {
      this.children?.forEach((child: OrgNode) => {
        if (child.is(NodeType.Property) && child.children?.length > 1) {
          const key = child.children.first.value.replaceAll(':', '');
          const value = child.children?.last.value;
          collectedProperties[key.toLowerCase()] = value;
        }
      });
      this.setPropertyForNode(
        collectedProperties,
        NodeType.Headline,
        NodeType.Root
      );
    }

    if (
      this.is(NodeType.Keyword) &&
      this.children.first.isEqual('#+property:') &&
      this.children.get(1)
    ) {
      const key = this.children.get(1).value.trim();
      const value = this.getRawValueFromNodes(this.children.slice(2));
      collectedProperties[key] = value;
      this.setPropertyForNode(
        collectedProperties,
        NodeType.Headline,
        NodeType.Root
      );
    }
  }

  private setPropertyForNode(
    properties: { [key: string]: string },
    ...parentType: NodeType[]
  ): void {
    const nodeWithProperties = this.findParent(...parentType);
    nodeWithProperties.mergeProperties(properties);
  }

  // TODO: master move this method into separated abstraction
  public findParent(...parentType: NodeType[]): OrgNode {
    if (this.is(...parentType)) {
      return this;
    }
    return this.parent.findParent(...parentType);
  }

  private recalculatePositionsForNeighbors(diff: number): void {
    let currentNode = this.next;

    while (currentNode) {
      currentNode.start += diff;
      currentNode.end += diff;
      if (currentNode.children?.first) {
        currentNode.children.first.end += diff;
        currentNode.children.first.start += diff;
        currentNode.children?.first?.recalculatePositionsForNeighbors(diff);
      }
      currentNode = currentNode.next;
    }
  }

  public setTitle(title: OrgNode): OrgNode {
    title.setParent(this);
    const subtreeLength = title.parent?.children?.last?.end ?? title.parent.end;

    this._title = title;
    title.initPositions();
    this.end += title.length;
    title.parent?.recalculateParentEnd(title.length);
    // child.recalculatePositionsForNeighbors(child.length);
    if (subtreeLength) {
      title.recalculateForEachNestedNodes(subtreeLength);
    }
    return title;
  }

  public setSection(section: OrgNode): OrgNode {
    section.setParent(this as OrgNode);
    const subtreeLength =
      section.parent?.title?.children.last.end ??
      section.parent?.children?.last?.end ??
      section.parent.end;
    this._section = section;

    section.initPositions();
    this.end += section.length;
    section.parent?.recalculateParentEnd(section.length);
    // child.recalculatePositionsForNeighbors(child.length);
    if (subtreeLength) {
      section.recalculateForEachNestedNodes(subtreeLength);
    }
    return section;
  }

  public addRawChildren(children: OrgStruct[]): void {
    children?.forEach((child) => this.addChild(new OrgNode(child)));
  }

  public addChildren(children?: OrgNode[] | OrgChildrenList): void {
    children?.forEach((child: OrgNode) => this.addChild(child));
  }

  public setValue(value: string): void {
    const lengthDiff = (value?.length ?? 0) - this.length;
    this.value = value;
    this.end += lengthDiff;
    this.recalculateParentEnd(lengthDiff);
    this.recalculatePositionsForNeighbors(lengthDiff);
  }

  // TODO: update start/end positions for this operations
  public appendValue(value: string): void {
    this.value ??= '';
    this.value += value;
    this.end += value.length;
    this.recalculateParentEnd(value.length);
    this.recalculatePositionsForNeighbors(value.length);
  }

  public prependValue(value: string): void {
    this.value = value + this.value;
    this.end += value.length;
    this.recalculateParentEnd(value.length);
  }

  public addNextNode(node: OrgNode): void {
    node.setPrev(this);
    this.setNext(node);
    this.next?.setPrev(node);
    node.setParent(this.parent);
    node.recalculateParentEnd(node.length);
    node.recalculatePositionsForNeighbors(node.length);
  }

  public is(...nodeType: NodeType[]): boolean {
    return nodeType.some((t) => t === this.type);
  }

  public isNot(...nodeType: NodeType[]): boolean {
    return !this.is(...nodeType);
  }

  public toString(offset?: number): string {
    return prettyTreePrint(this);
  }

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  public toJson(): any {
    const sharedDictKeys: Array<keyof this> = ['value', 'properties', 'meta'];

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const data = sharedDictKeys.reduce<Record<keyof this, any>>(
      (acc, key) => {
        if (this[key]) {
          acc[key] = this[key];
        }
        return acc;
      },
      {
        type: this.type,
        start: this.start,
        end: this.end,
      } as Record<keyof this, any>
    );

    if (this.children) {
      data.children = this.children.map((child) => child.toJson());
    }
    if (this.section) {
      data.section = this.section.toJson();
    }

    if (this.title) {
      data.title = this.title.toJson();
    }

    return data;
  }

  private forEachNestedChildren(callback: (node: OrgNode) => void): void {
    let node = this.children?.first;

    while (node) {
      callback(node);
      node.children?.first?.forEachNestedChildren(callback);
      node = node.next;
    }
  }

  /**
   * Check if value equals to the given value.
   * Both params will be trimmed and lowercased.
   */
  public isEqual(value: string): boolean {
    return this.value.trim().toLowerCase() === value.trim().toLowerCase();
  }
}
