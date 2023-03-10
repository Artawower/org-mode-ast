import {
  NodeType,
  OrgHandler,
  OrgNode,
  OrgStruct,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { AstContext } from '../ast-context.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class ListHandler implements OrgHandler {
  readonly #listTagOperator = '::';

  constructor(
    private ctx: AstContext,
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

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
    };

    return new OrgNode(orgData);
  }

  private createEmptyList(ordered: boolean, level = 0): OrgNode {
    const listNode = this.astBuilder.createList(ordered, level);
    this.astBuilder.attachToTree(listNode);
    this.astBuilder.saveLastNode(listNode);
    this.ctx.addNestedList(listNode);
    return listNode;
  }

  private createNewListItem(): void {
    const listTitleNode = new OrgNode({
      type: NodeType.Title,
    });

    const listItemNode = new OrgNode({
      type: NodeType.ListItem,
    });

    listItemNode.setTitle(listTitleNode);

    this.ctx.lastList.addChild(listItemNode);

    // this.astBuilder.attachToTree(listItemNode);
    this.astBuilder.saveLastNode(listTitleNode);
    this.ctx.insideListItem = true;
  }

  public isListTagOperator(operator: string): boolean {
    return this.ctx.insideListItem && operator === this.#listTagOperator;
  }

  public handleListTag(): OrgNode {
    const listTagNode = this.astBuilder.createOperatorNode(
      this.tokenIterator.currentValue
    );

    this.astBuilder.lastNode.type = NodeType.ListTag;

    return listTagNode;
  }
}
