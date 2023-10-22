import {
  NodeType,
  OrgChildrenList,
  OrgHandler,
  OrgNode,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { AstContext } from '../ast-context.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class PropertiesHandler implements OrgHandler {
  private propertiesBeginNode: OrgNode;

  private readonly propertiesBegin = ':properties:';
  private readonly propertiesEnd = ':end:';

  constructor(
    private ctx: AstContext,
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public isPropertyKeyword(): boolean {
    const keyword = this.tokenIterator.currentValue;
    return keyword.startsWith(':') && keyword.trim().endsWith(':');
  }

  public isBlockPropertyKeyword(): boolean {
    const keyword = this.tokenIterator.currentValue;
    return keyword.startsWith(':') && !keyword.trim().endsWith(':');
  }

  public handle(): OrgNode {
    const keywordNode = this.astBuilder.createPropertyNode();
    this.astBuilder.attachToTree(keywordNode);

    const rawKeywordName = this.tokenIterator.currentValue.toLowerCase();
    const isPropertyStartKeyword = rawKeywordName === this.propertiesBegin;

    if (isPropertyStartKeyword) {
      this.propertiesBeginNode = keywordNode;
      return keywordNode;
    }

    const isPropertyCloseKeyword = rawKeywordName === this.propertiesEnd;

    if (!isPropertyCloseKeyword || !this.propertiesBeginNode) {
      return keywordNode;
    }

    const propertyDrawerChildren =
      this.propertiesBeginNode.parent.children.getNodesBetweenPairs(
        this.propertiesBeginNode,
        null,
        true
      );

    const propertyDrawerNode = this.tryHandleEndOfProperties(
      propertyDrawerChildren
    );

    if (!propertyDrawerNode) {
      return keywordNode;
    }

    this.astBuilder.attachToTree(propertyDrawerNode);
    // propertyDrawerNode.calculateNodeProperties();

    return propertyDrawerNode;
  }

  public tryHandleEndOfProperties(
    propertyDrawerChildren: OrgChildrenList
  ): OrgNode {
    const firstChild = propertyDrawerChildren.first;

    const parentIsRoot = firstChild.parent.is(NodeType.Root);
    const parentIsHeadline =
      firstChild.parent.is(NodeType.Section) &&
      firstChild.parent.parent.is(NodeType.Headline);

    const isFirstHeadlineOrRootChild =
      !firstChild?.prev && (parentIsRoot || parentIsHeadline);

    if (!isFirstHeadlineOrRootChild) {
      return null;
    }

    const propertyDrawerNode = this.astBuilder.createPropertyDrawerNode();

    this.astBuilder.lastNode = firstChild.prev ?? firstChild.parent;
    firstChild.parent.removeChildren(propertyDrawerChildren);
    propertyDrawerNode.addChildren(propertyDrawerChildren);

    return propertyDrawerNode;
  }
}
