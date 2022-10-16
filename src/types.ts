export enum NodeType {
  Root = 'root',
  Headline = 'headline',
  Text = 'text',
  TodoKeyword = 'todoKeyword',
  Unresolved = 'unresolved',
  Operator = 'operator',
  Bold = 'bold',
  Section = 'section',
  Crossed = 'crossed',
  Checkbox = 'checkbox',
  List = 'list',
  ListItem = 'listItem',
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

export interface WithValue {
  value: string;
}

export interface WithCheckStatus {
  checked: boolean;
}

export interface Section extends WithRange, WithChildren, WithParent {
  type: NodeType.Section;
}

export interface Headline extends WithRange, WithChildren, WithParent {
  type: NodeType.Headline;
  level: number;
  section?: Section;
  checked?: boolean;
}

export interface Operator extends WithRange, WithParent, WithValue {
  type: NodeType.Operator;
}

export interface OrgRoot extends WithRange, WithChildren, WithParent {
  type: NodeType.Root;
}

export interface OrgText extends WithRange, WithParent, WithValue {
  type: NodeType.Text;
}

export interface List extends WithRange, WithChildren, WithParent {
  type: NodeType.List;
  ordered: boolean;
}

export interface ListItem extends WithRange, WithChildren, WithParent {
  type: NodeType.ListItem;
}

// Special type for temporary nodes. Should not be exist after parsing.
// Probably could be replaced by simple text...
export interface Unresolved extends WithRange, WithParent, WithValue {
  type: NodeType.Unresolved;
}

export interface OrgBold extends WithRange, WithChildren, WithParent {
  type: NodeType.Bold;
}

export interface OrgCrossed extends WithRange, WithChildren, WithParent {
  type: NodeType.Crossed;
}

export interface OrgCheckbox extends WithRange, WithParent, WithCheckStatus {
  type: NodeType.Checkbox;
}

export type OrgData =
  | Headline
  | OrgRoot
  | Operator
  | OrgText
  | Unresolved
  | OrgBold
  | OrgCrossed
  | Section
  | List
  | ListItem
  | OrgCheckbox;

// TODO: delete
export type OrgNode = Headline | OrgRoot;

export interface UniversalOrgNode extends Headline, OrgRoot, Operator, OrgText, Unresolved {
  type: any;
}

export interface Node {
  type: NodeType;
  children: OrgData[];
}
