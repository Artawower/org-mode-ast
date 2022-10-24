import { AstBuilder } from 'ast-builder';
import { OrgHandler } from 'internal.types';
import { TokenIterator } from 'token-iterator';
import { List, ListItem, NodeType, OrgData } from 'types';

export class ListHandler implements OrgHandler {
  constructor(private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  get insideList(): boolean {
    return this.checkIfInsideList();
  }

  public handle() {
    if (!this.insideList) {
      const isOrdered = this.tokenIterator.value?.[0] === '1';
      this.createEmptyList(isOrdered);
    }
    this.createNewListItem();

    const orgData: OrgData = {
      type: NodeType.Operator,
      value: this.tokenIterator.value,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.value.length,
    };

    return orgData;
  }

  private createEmptyList(ordered: boolean): OrgData {
    const list: List = {
      type: NodeType.List,
      start: 0,
      end: 0,
      ordered,
      children: [],
    };
    this.astBuilder.attachToTree(list);
    this.astBuilder.saveLastNode(list);
    return list;
  }

  private createNewListItem(): void {
    const orgData: ListItem = {
      type: NodeType.ListItem,
      start: this.astBuilder.lastPos,
      end: 0,
      children: [],
    };
    this.astBuilder.attachToTree(orgData);
    this.astBuilder.saveLastNode(orgData);
  }

  private checkIfInsideList(node?: OrgData): boolean {
    node ||= this.astBuilder.lastNode;

    if (node.type === NodeType.ListItem || node.type === NodeType.List) {
      return true;
    }
    if (node.parent) {
      return this.checkIfInsideList(node.parent);
    }
    return false;
  }
}
