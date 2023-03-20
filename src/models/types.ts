import { OrgNode } from './org-node.js';

export enum NodeType {
  Root = 'root',

  Headline = 'headline',
  Text = 'text',

  Unresolved = 'unresolved',

  Operator = 'operator',
  Checkbox = 'checkbox',

  Indent = 'indent',
  NewLine = 'newLine',

  Date = 'date',
  DateRange = 'dateRange',

  Comment = 'comment',
  TodoKeyword = 'todoKeyword',

  TagList = 'tagList',

  List = 'list',
  ListItem = 'listItem',
  Progress = 'progress',
  ListTag = 'listTag',

  Bold = 'bold',
  Crossed = 'crossed',
  InlineCode = 'inlineCode',
  Verbatim = 'verbatim',
  Italic = 'italic',

  LatexFragment = 'latexFragment',
  LatexEnvironment = 'latexEnvironment',

  SrcBlock = 'srcBlock',
  BlockHeader = 'blockHeader',
  PropertyDrawer = 'propertyDrawer',
  Property = 'property',
  BlockProperty = 'blockProperty',
  BlockLanguage = 'blockLanguage',
  BlockFooter = 'blockFooter',
  BlockBody = 'blockBody',
  QuoteBlock = 'quoteBlock',
  ExampleBlock = 'exampleBlock',
  HtmlBlock = 'htmlBlock',
  ExportBlock = 'exportBlock',
  CommentBlock = 'commentBlock',
  HeaderArg = 'headerArg',

  InlineHtml = 'inlineHtml',

  FixedWidth = 'fixedWidth',
  Priority = 'priority',

  Keyword = 'keyword',
  Link = 'link',
  LinkUrl = 'linkUrl',
  LinkName = 'linkName',

  HorizontalRule = 'horizontalRule',

  Section = 'section',

  // Workaround for structures that have section + title
  Title = 'title',

  Table = 'table',
  TableCell = 'tableCell',
  TableRow = 'tableRow',
}

export type Block =
  | NodeType.SrcBlock
  | NodeType.QuoteBlock
  | NodeType.HtmlBlock
  | NodeType.ExportBlock
  | NodeType.ExampleBlock
  | NodeType.CommentBlock;

export enum TokenType {
  Headline = 'headline',
  Text = 'text',
  NewLine = 'newLine',
  Keyword = 'keyword',
  LatexEnvironmentKeyword = 'latexEnvironmentKeyword',
  Bracket = 'bracket',
  LatexBracket = 'latexBracket',
  Comment = 'comment',
  Operator = 'operator',
  Indent = 'indent',
  HorizontalRule = 'horizontalRule',
  TableOperator = 'tableOperator',
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

export type BlockProperties = { [key: string]: string };
// Special type for temporary nodes. Should not be exist after parsing.
// Probably could be replaced by simple text...

export interface OrgStruct {
  type: NodeType;
  start?: number;
  end?: number;
  value?: string;
  children?: OrgStruct[];
  properties?: BlockProperties;
  checked?: boolean;
  priority?: string;
  section?: OrgStruct;
  level?: number;
  ordered?: boolean;
  language?: string;
  title?: OrgNode;
}

export interface SrcBlockMetaInfo {
  language?: string;
  tangle?: string;
  [key: string]: string | undefined;
}

// TODO: meta info should be collected via special wrapper
export interface MetaInfo {
  id?: string;
  title?: string;
  description?: string;
  images?: string[];
  tags?: string[];
  headings?: [];
  category?: string;
  [key: string]: string | string[] | undefined | boolean;
}

export interface ParserConfiguration {
  todoKeywords?: string[];
}

export type BlockPosition = 'begin' | 'end';

export type BlockType = 'src' | 'html' | 'quote' | string;
