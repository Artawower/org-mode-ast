import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { OrgHandler } from 'internal.types';
import { TokenIterator } from 'token-iterator';
import { List, ListItem, NodeType, OrgData } from 'types';

export class ListHandler implements OrgHandler {
  constructor(private ctx: AstContext, private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  public handle(): OrgData {
    const isNestedList = this.ctx.setupNewParentListByLevel();
    const isSameLevelList = this.ctx.lastList?.level === this.ctx.listLevel;

    if (isNestedList && !isSameLevelList) {
      this.astBuilder.getLastSessionOrCreate(this.ctx.lastListItem);
    }

    if (!this.ctx.nestedLists.length || (isNestedList && !isSameLevelList)) {
      const isOrdered = this.tokenIterator.currentValue?.[0] === '1';
      this.createEmptyList(isOrdered, this.ctx.listLevel);
    }

    this.createNewListItem();

    if (this.ctx.nextIndentNode) {
      this.astBuilder.attachToTree(this.ctx.nextIndentNode);
      this.ctx.resetIndent();
    }

    const orgData: OrgData = {
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.currentValue.length,
    };

    return orgData;
  }

  private createEmptyList(ordered: boolean, level: number = 0): OrgData {
    const list: List = {
      type: NodeType.List,
      start: this.astBuilder.lastNode.end,
      end: this.astBuilder.lastNode.end,
      level,
      ordered,
      children: [],
    };
    this.astBuilder.attachToTree(list);
    this.astBuilder.saveLastNode(list);
    this.ctx.addNestedList(list);
    return list;
  }

  private createNewListItem(): void {
    const start = this.ctx.nextIndentNode ? this.ctx.nextIndentNode.start : this.astBuilder.lastPos;
    const orgData: ListItem = {
      type: NodeType.ListItem,
      start,
      end: 0,
      parent: this.ctx.lastParentList,
      children: [],
    };
    this.astBuilder.attachToTree(orgData);
    this.astBuilder.saveLastNode(orgData);
    this.ctx.insideListItem = true;
  }
}
