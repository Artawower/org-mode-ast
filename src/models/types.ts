import { OrgNode } from './org-node';

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
  Verbatim = 'verbatim',
  Indent = 'indent',
  NewLine = 'newLine',
  Comment = 'comment',
  Date = 'date',
  DateRange = 'dateRange',
  SrcBlock = 'srcBlock',
  Progress = 'progress',

  BlockHeader = 'blockHeader',
  BlockProperty = 'blockProperty',
  BlockLanguage = 'blockLanguage',
  BlockFooter = 'blockFooter',
  BlockBody = 'blockBody',
  QuoteBlock = 'quoteBlock',
  HtmlBlock = 'htmlBlock',

  Keyword = 'keyword',
  Link = 'link',
  LinkUrl = 'linkUrl',
  LinkName = 'linkName',

  // Workaround for structures that have section + title
  Title = 'title',
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

export interface ParserConfiguration {
  todoKeywords?: string[];
}

export type BlockPosition = 'begin' | 'end';

export type BlockType = 'src' | 'html' | 'quote' | string;
