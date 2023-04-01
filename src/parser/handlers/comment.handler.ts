import { OrgHandler, OrgNode, TokenType } from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class CommentHandler implements OrgHandler {
  public static readonly tokenType = TokenType.Comment;
  #lastCommentNode: OrgNode;

  get onHold(): boolean {
    return !!this.#lastCommentNode;
  }

  constructor(
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    this.#lastCommentNode = this.astBuilder.createComment();
    this.astBuilder.attachToTree(this.#lastCommentNode);
    return this.#lastCommentNode;
  }

  public handleNewLine(): void {
    this.#lastCommentNode = null;
  }

  public handleHolded(): void {
    if (!this.#lastCommentNode) {
      return;
    }
    const textNode = this.#lastCommentNode.children.get(1);
    if (!textNode) {
      this.#lastCommentNode.addChild(
        this.astBuilder.createTextNode(this.tokenIterator.currentValue)
      );
      return;
    }
    textNode.appendValue(this.tokenIterator.currentValue);
  }
}
