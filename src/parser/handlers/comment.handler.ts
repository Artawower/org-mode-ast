import { OrgHandler, OrgNode, TokenType } from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class CommentHandler implements OrgHandler {
  public static readonly tokenType = TokenType.Comment;

  constructor(
    private astBuilder: AstBuilder,
    private tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    const commentNode = this.astBuilder.createComment();
    this.astBuilder.attachToTree(commentNode);
    return commentNode;
  }
}
