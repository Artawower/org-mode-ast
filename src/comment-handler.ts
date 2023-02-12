import { AstBuilder } from 'ast-builder';
import { OrgHandler } from 'internal.types';
import { OrgNode } from 'org-node';
import { TokenIterator } from 'token-iterator';
import { TokenType } from 'types';

export class CommentHandler implements OrgHandler {
  public static readonly tokenType = TokenType.Comment;

  constructor(private astBuilder: AstBuilder, private tokenIterator: TokenIterator) {}

  public handle(): OrgNode {
    const commentNode = this.astBuilder.createComment();
    this.astBuilder.attachToTree(commentNode);
    return commentNode;
  }
}
