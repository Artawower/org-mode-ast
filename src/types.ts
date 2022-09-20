export enum NodeType {
  Root = 'root',
  Headline = 'headline',
  Text = 'text',
  TodoKeyword = 'todoKeyword',
  Unresolved = 'unresolved',
  Operator = 'operator',
  Bold = 'bold',
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

// Special type for temporary nodes. Should not be exist after parsing.
// Probably could be replaced by simple text...
export interface Unresolved extends WithRange, WithParent {
  type: NodeType.Unresolved;
  value: string;
}

export interface OrgBold extends WithRange, WithChildren, WithParent {
  type: NodeType.Bold;
}

export type OrgData = Headline | OrgRoot | Operator | Text | Unresolved | OrgBold;

// TODO: delete
export type OrgNode = Headline | OrgRoot;

export interface UniversalOrgNode extends Headline, OrgRoot, Operator, Text, Unresolved {
  type: any;
}

export interface Node {
  type: NodeType;
  children: OrgData[];
}
