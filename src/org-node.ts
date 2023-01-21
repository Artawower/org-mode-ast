import { BlockProperties, NodeType, OrgStruct, PartialUniversalOrgStruct, Section } from 'types';

export class OrgNode<T = OrgStruct> {
  public type!: NodeType;
  public safeCheckMode = false;

  #value: string;

  get value() {
    this.safetyCheck(
      'value',
      NodeType.Text,
      NodeType.Unresolved,
      NodeType.Operator,
      NodeType.Indent,
      NodeType.NewLine,
      NodeType.Keyword
    );
    return this.#value;
  }

  public parent?: OrgNode;
  public start!: number;
  public end!: number;

  get unsafe(): OrgNode {
    return { ...this, safeCheckMode: false } as OrgNode;
  }
  #children?: OrgNode[];
  get children(): OrgNode[] {
    return this.#children || [];
  }

  #prev?: OrgNode;
  get prev() {
    return this.#prev;
  }

  get lastChild(): OrgNode {
    return this.#children?.[this.#children.length - 1];
  }

  #next?: OrgNode;
  get next() {
    return this.#next;
  }

  // TODO: add getter with generic type checker
  #level?: number;
  get level(): number {
    this.safetyCheck('level', NodeType.List, NodeType.Unresolved, NodeType.Headline);
    return this.#level;
  }

  #section?: OrgNode<Section>;
  get section(): OrgNode<Section> {
    this.safetyCheck('section', NodeType.List, NodeType.Headline, NodeType.Root, NodeType.ListItem);
    return this.#section;
  }

  #ordered?: boolean;
  get ordered(): boolean {
    this.safetyCheck('ordered', NodeType.List);
    return this.#ordered;
  }

  #properties?: BlockProperties;
  get properties(): BlockProperties {
    this.safetyCheck('properties', NodeType.SrcBlock, NodeType.ListItem);
    return this.#properties;
  }

  #checked?: boolean;
  get checked(): boolean {
    this.safetyCheck('checked', NodeType.List, NodeType.Headline);
    return this.#checked;
  }

  set checked(checked: boolean) {
    this.safetyCheck('checked', NodeType.List, NodeType.Headline);
    this.#checked = checked;
  }

  constructor(nodeData: PartialUniversalOrgStruct) {
    this.type = nodeData.type;
    if (nodeData.section) {
      this.#section = new OrgNode(nodeData.section);
    }
    this.#value = nodeData.value;
    this.#level = nodeData.level;
    this.#ordered = nodeData.ordered;
    this.#properties = nodeData.properties;
    this.#checked = nodeData.checked;
    this.addRawChildren(nodeData.children);

    // this.setOptionalAttribute('value', nodeData);
    // this.setOptionalAttribute('level', nodeData);
    // this.setOptionalAttribute('children', nodeData);
    // this.setOptionalAttribute('ordered', nodeData);
    // this.setOptionalAttribute('properties', nodeData);
    // this.setOptionalAttribute('checked', nodeData);
    this.start = nodeData.start;
    this.end = nodeData.end || 0;
  }

  // private setOptionalAttribute(propertyName: keyof this, nodeData: PartialUniversalOrgNode) {
  //   const value = nodeData[propertyName as string];
  //   // if (value != null) {
  //   this[`#${propertyName as string}`] = value;
  //   // console.log('VAAAAL: ', this.#value);
  //   // } else {
  //   //   delete this[propertyName];
  //   // }
  // }

  private safetyCheck(propertyName: keyof this, ...availableTypes: NodeType[]): void | never {
    if (this.safeCheckMode && !availableTypes.includes(this.type)) {
      throw new Error(`${propertyName as string} property doesn't available for type ${this.type}`);
    }
  }

  get objectTree(): T {
    return {
      type: this.type,
      value: this.value,
      children: this.#children.map((child) => child.objectTree),
      section: this.section,
      start: this.start,
      end: this.end,
    } as T;
  }

  public setParent(parent: OrgNode) {
    this.parent = parent;
  }

  public setPrev(prev: OrgNode) {
    this.#prev = prev;
  }

  public setNext(next: OrgNode) {
    this.#next = next;
  }

  // TODO: master need to add range recheck after child added
  public addChild<T = OrgStruct>(child: OrgNode<T>): OrgNode<T> {
    child.setParent(this as OrgNode);
    if (!this.#children) {
      this.#children = [];
    }
    this.#children.push(child as OrgNode);
    return child;
  }

  public setChildren(children?: OrgNode[]) {
    children?.forEach((child) => child.setParent(this as OrgNode));
    this.#children = children;
  }

  public setSection(section: OrgNode<Section>) {
    section.setParent(this as OrgNode);
    this.#section = section;
  }

  public addRawChildren(children: PartialUniversalOrgStruct[]) {
    children?.forEach((child) => this.addChild(new OrgNode(child)));
  }

  public addChildren<T = OrgStruct>(children: OrgNode<T>[]) {
    children.forEach((child) => this.addChild(child));
  }

  public setValue(value: string): void {
    this.safetyCheck('value', NodeType.Text);
    this.#value = value;
  }

  public appendValue(value: string): void {
    this.#value += value;
  }

  public prependValue(value: string): void {
    this.#value = value + this.#value;
  }

  public is(nodeType: NodeType): boolean {
    return this.type === nodeType;
  }
}
