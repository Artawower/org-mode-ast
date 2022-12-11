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
  Italic = 'italic',
  InlineCode = 'inlineCode',
  // TODO: need to think about this type, could be redindand...
  Indent = 'indent',
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

export interface WithRange {
  start: number;
  end: number;
}

export interface WithChildren {
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

export interface WithSection {
  section?: Section;
}

export interface Section extends WithRange, WithChildren, WithParent {
  type: NodeType.Section;
}

export interface Headline extends WithRange, WithChildren, WithParent, WithSection {
  type: NodeType.Headline;
  level: number;
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
  level: number;
}

export interface ListItem extends WithRange, WithChildren, WithParent, WithSection {
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

export interface OrgItalic extends WithRange, WithChildren, WithParent {
  type: NodeType.Italic;
}

export interface OrgInlineCode extends WithRange, WithChildren, WithParent {
  type: NodeType.InlineCode;
}

export type OrgData =
  | Headline
  | OrgRoot
  | Operator
  | OrgText
  | Unresolved
  | OrgBold
  | OrgCrossed
  | OrgItalic
  | Section
  | List
  | ListItem
  | OrgCheckbox
  | OrgInlineCode;

// TODO: delete
export type OrgNode = Headline | OrgRoot;

// export interface UniversalOrgNode extends Headline, OrgRoot, Operator, OrgText, Unresolved, ListItem {
//   type: any;
// }

// export type UniversalOrgNode = Headline | OrgRoot | Operator | OrgText | Unresolved | ListItem | Section | List;

// interface AllMergedOrgNodeInterfaces extends Headline, OrgRoot, Operator, OrgText, Unresolved, ListItem, List {
//   type: any;
// }

// export interface UniversalOrgNode extends Partial<AllMergedOrgNodeInterfaces> {
//   type: any;
// }

type OrgNodeProperties = WithChildren & WithSection & WithValue & WithParent & WithRange;

export interface UniversalOrgNode extends Partial<OrgNodeProperties> {
  type: any;
}

export interface Node {
  type: NodeType;
  children: OrgData[];
}
