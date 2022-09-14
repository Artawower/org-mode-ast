export enum NodeType {
  Root = 'root',
  Operator = 'operator',
  Headline = 'headline',
  Text = 'text',
  TodoKeyword = 'todoKeyword',
}

export enum TokenType {
  Headline = 'headline',
  Text = 'text',
  Keyword = 'keyword',
  Bracket = 'bracket',
  Comment = 'comment',
  Operator = 'operator',
}

export interface Token {
  type: TokenType;
  value: string;
}

interface WithRange {
  start: number;
  end: number;
}

interface WithChildren {
  children: OrgData[];
}

interface WithParent {
  parent?: OrgData;
}

export interface Headline extends WithRange, WithChildren, WithParent {
  type: NodeType.Headline;
  level: number;
}

export interface Operator extends WithRange, WithParent {
  type: NodeType.Operator;
  value: string;
}

export interface OrgRoot extends WithRange, WithChildren, WithParent {
  type: NodeType.Root;
}

export interface Text extends WithRange, WithParent {
  type: NodeType.Text;
  value: string;
}

export type OrgData = Headline | OrgRoot | Operator | Text;
export type OrgNode = Headline | OrgRoot;

export interface Node {
  type: NodeType;
  children: OrgData[];
}
