import { ParserConfiguration, RawToken, Token, TokenType } from 'types';

export class Tokenizer {
  private readonly delimiter = ' ';
  private readonly brackets = ['=', '+', '[', ']', '/', '*'];
  private readonly listItemCloseSymbols = [')', '.'];
  private readonly keywordPrefix = '#+';
  private readonly blockKeywords = ['begin_src', 'end_src'];

  private begin = 0;
  private end = 0;

  // TODO: make it as linked list
  private tokens: Token[] = [];
  private point = 0;
  private tokenAggregators: { [key: string]: (c: string) => void };

  constructor(private text: string, public readonly todoKeywords = ['TODO', 'DONE', 'HOLD', 'CANCELED']) {
    this.initTokenAggregators();
  }

  private initTokenAggregators() {
    const commonAggregators = {
      '*': (c: string) => this.handleAsterisk(c),
      [this.delimiter]: (c: string) => this.handleDelimiter(c),
      '#': (c: string) => this.handleNumberSign(c),
      '-': (c: string) => this.handleDash(c),
      '+': (c: string) => this.handlePlus(c),
      ':': (c: string) => this.handleComma(c),
      '.': (c: string) => this.handlePoint(c),
      ')': (c: string) => this.handleParenthesis(c),
      '\n': (c: string) => this.handleNewLine(c),
    };
    const bracketAggregators = this.brackets.reduce((acc, c) => {
      acc[c] = (c: string) => this.handleBracket(c);
      return acc;
    }, {});

    this.tokenAggregators = {
      ...bracketAggregators,
      ...commonAggregators,
    };
  }

  get prevToken(): Token {
    return this.tokens?.[this.tokens.length - 1];
  }

  get nextChar(): string {
    return this.text[this.point + 1];
  }

  get prevChar(): string {
    return this.text[this.point - 1];
  }

  get isPrevTokenEndsWithSpace(): boolean {
    if (!this.prevToken) {
      return false;
    }
    return this.prevToken.value?.[this.prevToken.value.length - 1] === this.delimiter;
  }

  public tokenize(): Token[] {
    for (; this.point < this.text.length; this.point++) {
      const c = this.text[this.point];
      this.buildTokens(c);
    }

    return this.tokens;
  }

  // Get token by number. from the end of list
  private getTokenByNumFromEnd(pos: number): Token {
    return this.tokens[this.tokens.length - pos];
  }

  private handleAsterisk(c: string): void {
    if (
      (this.isDelimiter(this.nextChar) || this.isNextChar('*')) &&
      (!this.prevToken || this.prevToken.isType(TokenType.NewLine))
    ) {
      this.addToken({ type: TokenType.Headline, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Headline)) {
      this.appendPrevValue(c);
      return;
    }
    if (this.isDelimiter(this.nextChar) && this.isDelimiter(this.prevChar)) {
      this.upsertToken({ type: TokenType.Text, value: c });
      return;
    }
    this.handleBracket(c);
  }

  private handleDelimiter(c: string): void {
    if (!this.prevToken || this.prevToken?.isType(TokenType.NewLine)) {
      this.addToken({ type: TokenType.Indent, value: c });
      return;
    }
    if (
      this.isPrevToken(TokenType.Indent) ||
      (this.isPrevToken(TokenType.Operator) && this.prevToken.value.endsWith('-')) ||
      this.isListOperator(this.prevToken)
    ) {
      this.appendPrevValue(c);
      return;
    }
    if (this.isPrevToken(TokenType.Headline) && !this.isPrevTokenEndsWithSpace) {
      this.appendPrevValue(c);
      return;
    }
    if (this.shouldFormatterHasSpaceAtTheEnd()) {
      this.appendPrevValue(c);
      return;
    }
    this.appendTextToken(c);
  }

  private isListOperator(token: Token): boolean {
    const orderNumber = token?.value.slice(0, -1);
    const listCloseOperator = token?.value.slice(-1);
    return (
      token?.type === TokenType.Operator &&
      this.isValueNumber(orderNumber) &&
      this.listItemCloseSymbols.includes(listCloseOperator)
    );
  }

  private handleNumberSign(c: string): void {
    if (
      [' ', '+'].includes(this.nextChar) &&
      (!this.prevToken || this.isPrevToken(TokenType.NewLine, TokenType.Indent))
    ) {
      this.upsertToken({ type: TokenType.Comment, value: c });
      return;
    }
    this.appendTextToken(c);
  }

  private handleDash(c: string): void {
    if (this.prevToken && !this.prevToken.isType(TokenType.NewLine) && !this.prevToken.value.trim()) {
      this.upsertToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (!this.prevToken || this.prevToken.isType(TokenType.NewLine)) {
      this.addToken({ type: TokenType.Operator, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handlePlus(c: string): void {
    if (this.isNextChar(this.delimiter) && (!this.prevToken || this.prevToken.isType(TokenType.NewLine))) {
      this.addToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Comment)) {
      this.upsertToken({ type: TokenType.Operator, value: c }, true);
      return;
    }
    this.handleBracket(c);
  }

  private handleComma(c: string): void {
    if (this.prevToken?.value?.startsWith(':')) {
      this.upsertToken({ type: TokenType.Keyword, value: c }, true);
      return;
    }
    this.addToken({ type: TokenType.Operator, value: c });
  }

  private handlePoint(c: string): void {
    this.handleListOperatorOrText(c);
  }

  private handleListOperatorOrText(c: string): boolean {
    const wasNewLineOrStartDocument =
      !this.getTokenByNumFromEnd(3) || this.getTokenByNumFromEnd(2)?.isType(TokenType.NewLine);
    const wasSpace = this.getTokenByNumFromEnd(2)?.isBlank;
    if ((wasSpace || wasNewLineOrStartDocument) && this.isValueNumber(this.prevToken.value)) {
      this.upsertToken({ type: TokenType.Operator, value: c }, true);
      return;
    }
    this.handleText(c);
  }

  private handleParenthesis(c: string): void {
    this.handleListOperatorOrText(c);
  }

  private handleNewLine(c: string): void {
    this.addToken({ type: TokenType.NewLine, value: c });
  }

  private isValueNumber(value: string): boolean {
    return !!value.match(/^[0-9]+$/);
  }

  private handleBracket(c: string): void {
    this.addToken({ type: TokenType.Bracket, value: c });
  }

  private handleText(c: string): void {
    if (this.isPrevToken(TokenType.Comment)) {
      this.appendPrevValue(c);
      return;
    }
    this.appendTextToken(c);
    this.checkIsLastTextTokenKeyword();
  }

  private appendTextToken(c: string): void {
    if (this.prevToken?.isType(TokenType.NewLine)) {
      this.addToken({ type: TokenType.Text, value: c });
      return;
    }
    if (this.prevToken?.value.startsWith(':') && !this.isDelimiter(c)) {
      this.upsertToken({ type: TokenType.Keyword, value: c }, true);
      // this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private isBlockKeyword(value: string): boolean {
    return this.blockKeywords.includes(value.toLowerCase());
  }

  private checkIsLastTextTokenKeyword(): void {
    if (this.todoKeywords.find((t) => t === this.prevToken.value)) {
      this.prevToken.setType(TokenType.Keyword);
      return;
    }
    if (this.isBlockKeyword(this.prevToken.value)) {
      this.forceMergeLastTokens(2, TokenType.Keyword);
    }
  }

  /** Force merge last tokens
   *
   * @param count - count of tokens to merge
   * @param type - type of new token
   */
  private forceMergeLastTokens(count: number, type: TokenType): void {
    const tokens = this.tokens.splice(-count);
    const newToken = new Token(
      {
        type,
        value: tokens.map((t) => t.value).join(''),
      },
      tokens[0].start
    );
    this.tokens.push(newToken);
  }

  private formattersWithSpaceAtTheEnd = ['-', '+'];

  private shouldFormatterHasSpaceAtTheEnd(): boolean {
    if (!this.prevToken || this.prevToken.type !== TokenType.Operator) {
      return false;
    }
    return this.formattersWithSpaceAtTheEnd.includes(this.prevToken.value);
  }

  private buildTokens(c: string) {
    const tokenAggregator = this.tokenAggregators[c];
    if (tokenAggregator) {
      tokenAggregator(c);
      return;
    }
    this.handleText(c);
  }

  private addToken(token: RawToken) {
    this.begin = this.end;
    this.end = this.begin + token.value.length;
    const newToken = new Token(token, this.begin);
    this.tokens.push(newToken);
  }

  /**
   * Append value to previous token when types are equal
   * Or create new token with current type
   */
  private upsertToken(token: RawToken, force = false): void {
    if (this.prevToken?.type === token.type || force) {
      this.appendPrevValue(token.value);
      this.prevToken.setType(token.type);
      return;
    }
    this.addToken(token);
  }

  private appendPrevValue(c: string) {
    this.end += c.length;
    this.prevToken.appendText(c);
  }

  private isPrevToken(...tokens: TokenType[]): boolean {
    if (!this.prevToken) {
      return false;
    }
    return tokens.some((t) => t === this.prevToken.type);
  }

  private isNextChar(c: string): boolean {
    return this.nextChar === c;
  }

  private isDelimiter(char: string): boolean {
    return char === this.delimiter;
  }
}

export function tokenize(text: string, configuration?: ParserConfiguration): Token[] {
  const tokenizer = new Tokenizer(text, configuration?.todoKeywords);
  return tokenizer.tokenize();
}
