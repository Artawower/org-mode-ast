import { List, ListItem, OrgIndent } from 'types';

export class AstContext {
  #nextIndentNode: OrgIndent;
  #nestedLists: List[] = [];

  get lastParentList(): List {
    if (!this.#nestedLists.length) {
      return null;
    }
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get lastListItem(): ListItem {
    return this.#nestedLists[this.#nestedLists.length - 1]?.children?.[0] as ListItem;
  }

  get lastList(): List {
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get listLevel(): number {
    return (this.#nextIndentNode?.value.length || 0) / 2;
  }

  get nextIndentNode(): OrgIndent {
    return this.#nextIndentNode;
  }

  set nextIndentNode(node: OrgIndent) {
    this.#nextIndentNode = node;
  }

  get nestedLists(): List[] {
    return this.#nestedLists;
  }

  get topLevelList(): List {
    return this.#nestedLists[0];
  }

  public setupNewParentListByLevel(): List {
    this.#nestedLists = this.#nestedLists.filter((l) => l.level <= this.listLevel);
    return this.lastList;
  }

  public resetIndent(): void {
    this.#nextIndentNode = null;
  }

  public addNestedList(list: List): void {
    this.#nestedLists.push(list);
  }

  public exitLastList(): void {
    this.#nestedLists.pop();
  }

  public exitList(): void {
    this.#nestedLists = [];
  }

  constructor() {}
}
