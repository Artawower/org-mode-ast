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
  Indent = 'indent',
  NewLine = 'newLine',

  SrcBlock = 'srcBlock',
  BlockHeader = 'blockHeader',
  BlockFooter = 'blockFooter',
  BlockBody = 'blockBody',
  QuoteBlock = 'quoteBlock',
  HtmlBlock = 'htmlBlock',
  Keyword = 'keyword',
}

export enum TokenType {
  Headline = 'headline',
  Text = 'text',
  NewLine = 'newLine',
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

  get isBlank(): boolean {
    return this.value.trim().length === 0;
  }

  public appendText(text: string): void {
    this.value += text;
    this.end += text.length;
  }

  public setType(t: TokenType): void {
    this.type = t;
  }

  public isType(t: TokenType): boolean {
    return this.type === t;
  }
}

// TODO: master make nodes as classes

export interface WithRange {
  start: number;
  end: number;
}

export interface WithChildren {
  children: OrgData[];
}

export interface WithParent {
  parent?: OrgData;
}

export interface WithNeighbors {
  prev?: OrgData;
  next?: OrgData;
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

export interface Headline extends WithRange, WithChildren, WithParent, WithSection, WithNeighbors {
  type: NodeType.Headline;
  level: number;
  checked?: boolean;
}

export interface Operator extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.Operator;
}

export interface OrgRoot extends WithRange, WithChildren, WithParent {
  type: NodeType.Root;
}

export interface OrgText extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.Text;
}

export interface List extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.List;
  ordered: boolean;
  level: number;
}

export interface ListItem extends WithRange, WithChildren, WithParent, WithSection, WithNeighbors {
  type: NodeType.ListItem;
}

// Special type for temporary nodes. Should not be exist after parsing.
// Probably could be replaced by simple text...
export interface Unresolved extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.Unresolved;
}

export interface OrgBold extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.Bold;
}

export interface OrgCrossed extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.Crossed;
}

export interface OrgCheckbox extends WithRange, WithParent, WithCheckStatus, WithNeighbors {
  type: NodeType.Checkbox;
}

export interface OrgItalic extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.Italic;
}

export interface OrgInlineCode extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.InlineCode;
}

export interface OrgIndent extends WithRange, WithValue, WithParent, WithNeighbors {
  type: NodeType.Indent;
}

export interface OrgNewLine extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.NewLine;
}

export interface OrgBlockHeader extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.BlockHeader;
}

export interface OrgBlockBody extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.BlockBody;
}

export interface OrgBlockFooter extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.BlockFooter;
}

export interface OrgSrcBlock extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.SrcBlock;
  language?: string;
  properties?: { [key: string]: string };
}

export interface SrcBlockMetaInfo {
  language?: string;
  tangle?: string;
  [key: string]: string | undefined;
}

export interface OrgKeyword extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.Keyword;
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
  | OrgIndent
  | OrgNewLine
  | OrgKeyword
  | OrgSrcBlock
  | OrgBlockHeader
  | OrgBlockBody
  | OrgBlockFooter;

type OrgNodeProperties = WithChildren & WithSection & WithValue & WithParent & WithRange & WithNeighbors;

export interface PartialUniversalOrgNode extends Partial<OrgNodeProperties> {
  type: any;
}

export interface Node {
  type: NodeType;
  children: OrgData[];
}

export interface ParserConfiguration {
  todoKeywords?: string[];
}

export type BlockPosition = 'begin' | 'end';
