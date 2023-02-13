import { OrgNode } from 'models';

export class AstContext {
  #nextIndentNode: OrgNode;
  #nestedLists: OrgNode[] = [];

  public insideHeadline: boolean = null;
  public insideListItem = false;

  // TODO: master move to block handler
  public srcBlockBegin: OrgNode = null;
  public quoteBlockBegin: OrgNode = null;

  get lastParentList(): OrgNode {
    if (!this.#nestedLists.length) {
      return null;
    }
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get lastListItem(): OrgNode {
    return this.#nestedLists[this.#nestedLists.length - 1]?.children?.[0];
  }

  get lastList(): OrgNode {
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get listLevel(): number {
    return (this.#nextIndentNode?.value.length || 0) / 2;
  }

  get nextIndentNode(): OrgNode {
    return this.#nextIndentNode;
  }

  set nextIndentNode(node: OrgNode) {
    this.#nextIndentNode = node;
  }

  get nestedLists(): OrgNode[] {
    return this.#nestedLists;
  }

  get topLevelList(): OrgNode {
    return this.#nestedLists[0];
  }

  public setupNewParentListByLevel(): OrgNode {
    this.#nestedLists = this.#nestedLists.filter(
      (l) => l.level <= this.listLevel
    );
    return this.lastList;
  }

  public resetIndent(): void {
    this.#nextIndentNode = null;
  }

  public addNestedList(listNode: OrgNode): void {
    this.#nestedLists.push(listNode);
  }

  public exitLastList(): void {
    this.#nestedLists.pop();
  }

  public exitList(): void {
    this.#nestedLists = [];
  }

  public resetSrcBlockInfo(): void {
    this.srcBlockBegin = null;
  }

  public resetQuoteBlockInfo(): void {
    this.quoteBlockBegin = null;
  }
}
