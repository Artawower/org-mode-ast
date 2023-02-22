import { OrgHandler, OrgNode, TokenType } from 'models';
import { AstBuilder } from 'parser/ast-builder';
import { TokenIterator } from 'tokenizer';

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
