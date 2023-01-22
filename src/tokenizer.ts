import { ParserConfiguration, RawToken, Token, TokenType } from 'types';

export class Tokenizer {
  private readonly delimiter = ' ';
  private readonly pariBrackets = ['[', ']', '<', '>'];
  private readonly formatterBrackets = ['=', '+', '/', '*'];
  private readonly brackets: string[] = [...this.formatterBrackets, ...this.pariBrackets];
  private readonly listItemCloseSymbols = [')', '.'];
  private readonly keywordPrefix = '#+';
  private readonly blockKeywords = ['begin_src', 'end_src'];

  private begin = 0;
  private end = 0;

  // TODO: make it as linked list
  private point = 0;
  private tokenAggregators: { [key: string]: (c: string) => void };
  private firstToken: Token;
  private lastToken: Token;

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

  get nextChar(): string {
    return this.text[this.point + 1];
  }

  get prevChar(): string {
    return this.text[this.point - 1];
  }

  get isPrevTokenEndsWithSpace(): boolean {
    if (!this.lastToken) {
      return false;
    }
    return this.lastToken.value?.[this.lastToken.value.length - 1] === this.delimiter;
  }

  public tokenize(): Token {
    for (; this.point < this.text.length; this.point++) {
      const c = this.text[this.point];
      this.buildTokens(c);
    }

    return this.firstToken;
  }

  private handleAsterisk(c: string): void {
    if (
      (this.isDelimiter(this.nextChar) || this.isNextChar('*')) &&
      (!this.lastToken || this.lastToken.isType(TokenType.NewLine))
    ) {
      this.createToken({ type: TokenType.Headline, value: c });
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
    if (!this.lastToken || this.lastToken?.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Indent, value: c });
      return;
    }
    if (
      this.isPrevToken(TokenType.Indent) ||
      (this.isPrevToken(TokenType.Operator) && this.lastToken.value.endsWith('-')) ||
      this.isListOperator(this.lastToken)
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
      (!this.lastToken || this.isPrevToken(TokenType.NewLine, TokenType.Indent))
    ) {
      this.upsertToken({ type: TokenType.Comment, value: c });
      return;
    }
    this.appendTextToken(c);
  }

  private handleDash(c: string): void {
    if (this.lastToken && !this.lastToken.isType(TokenType.NewLine) && !this.lastToken.value.trim()) {
      this.upsertToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (!this.lastToken || this.lastToken.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handlePlus(c: string): void {
    if (this.isNextChar(this.delimiter) && (!this.lastToken || this.lastToken.isType(TokenType.NewLine))) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Comment)) {
      this.upsertToken({ type: TokenType.Operator, value: c }, true);
      return;
    }
    this.handleBracket(c);
  }

  private handleComma(c: string): void {
    if (this.lastToken?.value?.startsWith(':')) {
      this.upsertToken({ type: TokenType.Keyword, value: c }, true);
      return;
    }
    if (this.isTokenSeparator(this.lastToken)) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c }, true);
  }

  private handlePoint(c: string): void {
    this.handleListOperatorOrText(c);
  }

  private handleListOperatorOrText(c: string): boolean {
    const wasNewLineOrStartDocument = !this.lastToken?.prev?.prev || this.lastToken?.prev?.isType(TokenType.NewLine);
    const wasSpace = this.lastToken?.prev?.prev?.isBlank;
    if ((wasSpace || wasNewLineOrStartDocument) && this.isValueNumber(this.lastToken.value)) {
      this.upsertToken({ type: TokenType.Operator, value: c }, true);
      return;
    }
    this.handleText(c);
  }

  private handleParenthesis(c: string): void {
    this.handleListOperatorOrText(c);
  }

  private handleNewLine(c: string): void {
    this.createToken({ type: TokenType.NewLine, value: c });
  }

  private isValueNumber(value: string): boolean {
    return !!value.match(/^[0-9]+$/);
  }

  private handleBracket(c: string): void {
    if (this.lastToken && this.isFormatterBracket(this.lastToken.value) && c === this.lastToken.value) {
      this.upsertToken({ type: TokenType.Text, value: c }, true);
      this.checkLastTokensNeedMerge();
      return;
    }
    this.createToken({ type: TokenType.Bracket, value: c });
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
    if (this.lastToken?.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Text, value: c });
      return;
    }
    if (this.lastToken?.value.startsWith(':') && !this.isDelimiter(c)) {
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
    if (this.todoKeywords.find((t) => t === this.lastToken.value)) {
      this.lastToken.setType(TokenType.Keyword);
      return;
    }
    if (this.isBlockKeyword(this.lastToken.value)) {
      this.forceMergeLastTokens(2, TokenType.Keyword);
    }
  }

  /** Force merge last tokens
   *
   * @param count - count of tokens to merge
   * @param type - type of new token
   */
  private forceMergeLastTokens(count: number, type?: TokenType): void {
    let value = '';
    let start: number;
    let prevToken = this.lastToken;
    type = type || this.lastToken.type;

    while (count > 0) {
      --count;
      value = prevToken.value + value;
      start = prevToken.start;
      prevToken = prevToken?.prev;
    }

    const newToken = new Token(
      {
        type,
        value,
      },
      start
    );

    console.log('✎: [line 263][tokenizer.ts] prevToken: ', prevToken);
    if (prevToken) {
      prevToken.setNextToken(newToken);
      newToken.setPrevToken(prevToken);
      this.addToken(newToken);
    } else {
      this.lastToken = newToken;
      this.firstToken = newToken;
    }
  }

  private formattersWithSpaceAtTheEnd = ['-', '+'];

  private shouldFormatterHasSpaceAtTheEnd(): boolean {
    if (!this.lastToken || this.lastToken.type !== TokenType.Operator) {
      return false;
    }
    return this.formattersWithSpaceAtTheEnd.includes(this.lastToken.value);
  }

  private buildTokens(c: string) {
    const tokenAggregator = this.tokenAggregators[c];
    if (tokenAggregator) {
      tokenAggregator(c);
      return;
    }
    this.handleText(c);
  }

  private createToken(token: RawToken) {
    this.begin = this.end;
    this.end = this.begin + token.value.length;
    const newToken = new Token(token, this.begin);
    this.addToken(newToken);
  }

  private addToken(token: Token) {
    this.firstToken ??= token;
    this.lastToken?.setNextToken(token);
    this.lastToken = token;
  }

  /**
   * Append value to previous token when types are equal
   * Or create new token with current type
   */
  private upsertToken(token: RawToken, force = false): void {
    if (this.lastToken?.type === token.type || force) {
      this.appendPrevValue(token.value);
      this.lastToken.setType(token.type);
      return;
    }
    this.createToken(token);
  }

  private appendPrevValue(c: string) {
    this.end += c.length;
    this.lastToken.appendText(c);
  }

  private checkLastTokensNeedMerge(): void {
    if (this.lastToken?.type !== this.lastToken?.prev?.type) {
      return;
    }
    this.forceMergeLastTokens(2);
  }

  private isPrevToken(...tokens: TokenType[]): boolean {
    if (!this.lastToken) {
      return false;
    }
    return tokens.some((t) => t === this.lastToken.type);
  }

  private isNextChar(c: string): boolean {
    return this.nextChar === c;
  }

  private isDelimiter(char: string): boolean {
    return char === this.delimiter;
  }

  /*
   * Return true when value ends with space, or no value, or value is new line
   */
  private isTokenSeparator(token?: Token): boolean {
    return !token || token.isType(TokenType.NewLine) || this.isDelimiter(token.value.slice(-1));
  }

  private isFormatterBracket(c?: string): boolean {
    return this.formatterBrackets.includes(c);
  }
}

export function tokenize(text: string, configuration?: ParserConfiguration): Token {
  const tokenizer = new Tokenizer(text, configuration?.todoKeywords);
  return tokenizer.tokenize();
}
