import { AstBuilder } from 'ast-builder';
import { AstContext } from 'ast-context';
import { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { List, ListItem, NodeType, OrgStruct } from 'types';

export class ListHandler implements OrgHandler {
  constructor(private ctx: AstContext, private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  public handle(): OrgNode {
    const isNestedList = this.ctx.setupNewParentListByLevel();
    const isSameLevelList = this.ctx.lastList?.level === this.ctx.listLevel;

    if (isNestedList && !isSameLevelList) {
      this.astBuilder.getLastSectionOrCreate(this.ctx.lastListItem);
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

    const orgData: OrgStruct = {
      type: NodeType.Operator,
      value: this.tokenIterator.currentValue,
      start: this.astBuilder.lastPos,
      end: this.astBuilder.lastPos + this.tokenIterator.currentValue.length,
    };

    return new OrgNode(orgData);
  }

  private createEmptyList(ordered: boolean, level = 0): OrgNode<List> {
    console.log('✎: [line 44][list-handler.ts] level: ', level);
    const listNode = this.astBuilder.createList(ordered, level);
    this.astBuilder.attachToTree(listNode);
    this.astBuilder.saveLastNode(listNode);
    this.ctx.addNestedList(listNode);
    return listNode;
  }

  private createNewListItem(): void {
    const start = this.ctx.nextIndentNode ? this.ctx.nextIndentNode.start : this.astBuilder.lastPos;
    const listItem: ListItem = {
      type: NodeType.ListItem,
      start,
      end: 0,
      children: [],
    };
    const listItemNode = new OrgNode<ListItem>(listItem);
    listItemNode.parent = this.ctx.lastList;

    this.astBuilder.attachToTree(listItemNode);
    this.astBuilder.saveLastNode(listItemNode);
    // this.ctx.lastParentList.addChild(listItemNode);
    this.ctx.insideListItem = true;
  }
}
