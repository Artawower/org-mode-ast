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
  Comment = 'comment',

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
  Date = 'date',
  ActiveDate = 'activeDate',
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
  public next?: Token;
  public prev?: Token;

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

  public setNextToken(token: Token): void {
    this.next = token;
    token.setPrevToken(this);
  }

  public setPrevToken(token: Token): void {
    this.prev = token;
  }
}

// TODO: master make nodes as classes

export interface WithRange {
  start: number;
  end: number;
}

export interface WithChildren {
  children: OrgStruct[];
}

export interface WithParent {
  parent?: OrgStruct;
}

export interface WithNeighbors {
  prev?: OrgStruct;
  next?: OrgStruct;
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

export interface Text extends WithRange, WithParent, WithValue, WithNeighbors {
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

export interface Checkbox extends WithRange, WithParent, WithCheckStatus, WithNeighbors {
  type: NodeType.Checkbox;
}

export interface OrgItalic extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.Italic;
}

export interface InlineCode extends WithRange, WithChildren, WithParent, WithNeighbors {
  type: NodeType.InlineCode;
}

export interface Indent extends WithRange, WithValue, WithParent, WithNeighbors {
  type: NodeType.Indent;
}

export interface NewLine extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.NewLine;
}

export interface BlockHeader extends WithRange, WithParent, WithNeighbors {
  type: NodeType.BlockHeader;
}

export interface BlockBody extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.BlockBody;
}

export interface BlockFooter extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.BlockFooter;
}

export type BlockProperties = { [key: string]: string };

export interface SrcBlock extends WithRange, WithParent, WithNeighbors, WithChildren {
  type: NodeType.SrcBlock;
  language?: string;
  properties?: BlockProperties;
}

export interface Comment extends WithRange, WithParent {
  type: NodeType.Comment;
}

export interface SrcBlockMetaInfo {
  language?: string;
  tangle?: string;
  [key: string]: string | undefined;
}

export interface Keyword extends WithRange, WithParent, WithValue, WithNeighbors {
  type: NodeType.Keyword;
}

export type OrgStruct =
  | Headline
  | OrgRoot
  | Operator
  | Text
  | Unresolved
  | OrgBold
  | OrgCrossed
  | OrgItalic
  | Section
  | List
  | ListItem
  | Checkbox
  | InlineCode
  | Indent
  | NewLine
  | Keyword
  | SrcBlock
  | BlockHeader
  | BlockBody
  | BlockFooter
  | Comment;

type DistributiveOmit<T, K extends keyof T> = T extends unknown ? Omit<T, K> : never;

type UnionToIntersection<U> = (U extends any ? (k: U) => void : never) extends (k: infer I) => void ? I : never;

type NodeProperties = UnionToIntersection<DistributiveOmit<OrgStruct, 'type'>>;

export interface PartialUniversalOrgNode extends Partial<NodeProperties> {
  type: NodeType;
}

export interface Node {
  type: NodeType;
  children: OrgStruct[];
}

export interface ParserConfiguration {
  todoKeywords?: string[];
}

export type BlockPosition = 'begin' | 'end';
