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
  Indent = 'indent',
}

export interface RawToken {
  type: TokenType;
  value: string;
}

export class Token {
  public end!: number;
  public value!: string;
  public type!: TokenType;

  constructor(token: RawToken, public readonly start: number) {
    this.end = start + token.value.length;
    this.type = token.type;
    this.value = token.value;
  }

  get isNewLine(): boolean {
    return this.value.endsWith('\n');
  }

  public appendText(text: string): void {
    this.value += text;
    this.end += text.length;
  }

  public setType(t: TokenType): void {
    this.type = t;
  }
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

export interface OrgIndent extends WithRange, WithValue, WithParent {
  type: NodeType.Indent;
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
  | OrgInlineCode
  | OrgIndent;

type OrgNodeProperties = WithChildren & WithSection & WithValue & WithParent & WithRange;

export interface PartialUniversalOrgNode extends Partial<OrgNodeProperties> {
  type: any;
}

export interface Node {
  type: NodeType;
  children: OrgData[];
}
