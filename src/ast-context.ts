import { OrgNode } from 'org-node';
import { List, ListItem, Indent, Keyword } from 'types';

export class AstContext {
  #nextIndentNode: OrgNode<Indent>;
  #nestedLists: OrgNode<List>[] = [];

  public insideHeadline: boolean = null;
  public insideListItem = false;

  public srcBlockBegin: OrgNode<Keyword> = null;
  public srcBlockChildIndex: number = null;

  get lastParentList(): OrgNode<List> {
    if (!this.#nestedLists.length) {
      return null;
    }
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get lastListItem(): OrgNode<ListItem> {
    return this.#nestedLists[this.#nestedLists.length - 1]?.children?.[0] as OrgNode<ListItem>;
  }

  get lastList(): OrgNode<List> {
    return this.#nestedLists[this.#nestedLists.length - 1];
  }

  get listLevel(): number {
    return (this.#nextIndentNode?.value.length || 0) / 2;
  }

  get nextIndentNode(): OrgNode<Indent> {
    return this.#nextIndentNode;
  }

  set nextIndentNode(node: OrgNode<Indent>) {
    this.#nextIndentNode = node;
  }

  get nestedLists(): OrgNode<List>[] {
    return this.#nestedLists;
  }

  get topLevelList(): OrgNode<List> {
    return this.#nestedLists[0];
  }

  public setupNewParentListByLevel(): OrgNode<List> {
    this.#nestedLists = this.#nestedLists.filter((l) => l.level <= this.listLevel);
    return this.lastList;
  }

  public resetIndent(): void {
    this.#nextIndentNode = null;
  }

  public addNestedList(listNode: OrgNode<List>): void {
    this.#nestedLists.push(listNode);
  }

  public exitLastList(): void {
    this.#nestedLists.pop();
  }

  public exitList(): void {
    console.log('✎: [line 68][ast-context.ts] EXIT: ', 'EXIT');
    this.#nestedLists = [];
  }

  public resetSrcBlockInfo(): void {
    this.srcBlockBegin = null;
    this.srcBlockChildIndex = null;
  }
}
