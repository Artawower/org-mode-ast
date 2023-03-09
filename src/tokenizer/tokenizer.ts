import {
  ParserConfiguration,
  RawToken,
  Token,
  TokenType,
} from '../models/index.js';

// TODO: master This code is candidate number one to be refactored.
// Seriously, spend some time to make it more readable...
export class Tokenizer {
  // TODO: master move this settings to parser configuration
  private readonly delimiter = ' ';
  private readonly pairBrackets = ['[', ']', '<', '>'];
  private readonly formatterBrackets = ['=', '+', '/', '*', '~', '$'];
  private readonly horizontalRuleChar = '-';
  private readonly newLineChar = '\n';
  private readonly horizontalRuleMinLength = 5;
  private readonly latexEnvironmentBlocks = ['\\begin', '\\end'];
  private readonly keywordStartOperator = '#+';
  private readonly brackets: string[] = [
    ...this.formatterBrackets,
    ...this.pairBrackets,
  ];
  private readonly listItemCloseSymbols = [')', '.'];

  private begin = 0;
  private end = 0;

  // TODO: make linked list
  private point = 0;
  private tokenAggregators: { [key: string]: (c: string) => void };
  private firstToken: Token;
  private lastToken: Token;
  private todoKeywords: string[] = [];

  constructor(private text: string, configuration: ParserConfiguration) {
    this.todoKeywords = configuration.todoKeywords ?? [];
    this.initTokenAggregators();
  }

  private initTokenAggregators() {
    const commonAggregators = {
      '*': (c: string) => this.handleAsterisk(c),
      [this.delimiter]: (c: string) => this.handleDelimiter(c),
      '#': (c: string) => this.handleNumberSign(c),
      '-': (c: string) => this.handleDash(c),
      '+': (c: string) => this.handlePlus(c),
      ':': (c: string) => this.handleColon(c),
      '.': (c: string) => this.handlePoint(c),
      ')': (c: string) => this.handleParenthesis(c),
      '\n': (c: string) => this.handleNewLine(c),
      '\\': (c: string) => this.handleBackslash(c),
      '{': (c: string) => this.handleLatexBrackets(c),
      '}': (c: string) => this.handleLatexBrackets(c),
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
    return (
      this.lastToken.value?.[this.lastToken.value.length - 1] === this.delimiter
    );
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
    if (this.handleFixedWidthOperator(c)) {
      return;
    }

    if (this.handleSpaceAfterIncorrectFixedWidthOperator(c)) {
      return;
    }

    if (!this.lastToken || this.lastToken?.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Indent, value: c });
      return;
    }
    if (
      this.isPrevToken(TokenType.Indent) ||
      (this.isPrevToken(TokenType.Operator) &&
        this.lastToken.value.endsWith('-')) ||
      this.isListOperator(this.lastToken)
    ) {
      this.appendPrevValue(c);
      return;
    }
    if (
      this.isPrevToken(TokenType.Headline) &&
      !this.isPrevTokenEndsWithSpace
    ) {
      this.appendPrevValue(c);
      return;
    }

    if (this.shouldFormatterHasSpaceAtTheEnd()) {
      this.appendPrevValue(c);
      return;
    }

    // NOTE: for src block property

    if (
      this.lastToken.prev?.value === ':' &&
      this.lastToken.isType(TokenType.Text) &&
      !this.isEol(this.lastToken.prev?.prev)
    ) {
      this.forceMergeLastTokens(2, TokenType.Keyword);
      this.createToken({ type: TokenType.Text, value: c });
      return;
    }

    if (this.isPrevToken(TokenType.Keyword)) {
      this.createToken({ type: TokenType.Text, value: c });
      return;
    }
    this.appendTextToken(c);
  }

  private handleFixedWidthOperator(c: string): boolean {
    const lastTokenNewLineOrSpaceWithNewLine = this.isEolWithOptionalIndent(
      this.lastToken?.prev
    );
    if (
      !this.lastToken?.isType(TokenType.Operator) ||
      this.lastToken.value !== ':' ||
      !lastTokenNewLineOrSpaceWithNewLine
    ) {
      return;
    }
    this.upsertToken({ type: TokenType.Operator, value: c }, true);
    return true;
  }

  private handleSpaceAfterIncorrectFixedWidthOperator(c: string): boolean {
    if (this.lastToken?.value !== ':') {
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c }, true);

    this.lastToken;

    if (this.lastToken.prev?.isType(TokenType.Text)) {
      this.forceMergeLastTokens(2, TokenType.Text);
    }
    return true;
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
    if (
      this.lastToken &&
      !this.lastToken.isType(TokenType.NewLine) &&
      !this.lastToken.value.trim()
    ) {
      this.upsertToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (!this.lastToken || this.lastToken.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Operator) && this.lastToken.value === '-') {
      this.upsertToken({ type: TokenType.Text, value: c }, true);
      return;
    }
    if (this.isHorizontalRule(c)) {
      this.upsertToken({ type: TokenType.HorizontalRule, value: c }, true);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handlePlus(c: string): void {
    if (this.isNextChar(this.delimiter) && this.isEol(this.lastToken)) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Comment)) {
      this.upsertToken({ type: TokenType.Operator, value: c }, true);
      return;
    }
    this.handleBracket(c);
  }

  private handleColon(c: string): void {
    if (this.isPropertyKeyword(c)) {
      return;
    }
    if (
      this.isEol(this.lastToken?.prev) &&
      (this.lastToken?.value?.startsWith(':') ||
        this.lastToken?.isType(TokenType.Keyword))
    ) {
      this.upsertToken({ type: TokenType.Keyword, value: c }, true);
      return;
    }
    if (
      this.isTokenSeparator(this.lastToken) ||
      (this.lastToken.prev?.isType(TokenType.Operator) &&
        this.lastToken.prev?.value === ':')
    ) {
      this.createToken({ type: TokenType.Operator, value: c });
      return;
    }
    // If its keyword
    if (this.lastToken?.value.startsWith('#+')) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c }, true);
  }

  private isPropertyKeyword(c: string): boolean {
    const prev = this.lastToken?.prev;
    const isPrevText = this.lastToken?.isType(TokenType.Text);
    const isStartedFromColon = prev?.value === ':';
    const isPropertyStartedFromnewLine = this.isEol(prev?.prev);

    if (isPrevText && isStartedFromColon && isPropertyStartedFromnewLine) {
      this.createToken({ type: TokenType.Operator, value: c });
      this.forceMergeLastTokens(3, TokenType.Keyword);
      return true;
    }
  }

  // TODO: master REPLACE ALL checking like
  // !this.lastToken || this.lastToken.isType(TokenType.NewLine)
  // into this method
  private isEol(token?: Token): boolean {
    return !token || token?.isType(TokenType.NewLine);
  }

  private isEolWithOptionalIndent(token: Token): boolean {
    return (
      this.isEol(token) ||
      (token?.isType(TokenType.Indent) && this.isEol(token.prev))
    );
  }

  private handlePoint(c: string): void {
    this.handleListOperatorOrText(c);
  }

  private handleListOperatorOrText(c: string): boolean {
    const wasNewLineOrStartDocument =
      !this.lastToken?.prev?.prev ||
      this.lastToken?.prev?.isType(TokenType.NewLine);
    const wasSpace = this.lastToken?.prev?.prev?.isBlank;
    if (
      (wasSpace || wasNewLineOrStartDocument) &&
      this.isValueNumber(this.lastToken.value)
    ) {
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

  private handleBackslash(c: string): void {
    this.createToken({
      type: TokenType.Keyword,
      value: c,
    });
  }

  private isValueNumber(value: string): boolean {
    return !!value.match(/^[0-9]+$/);
  }

  private handleBracket(c: string): void {
    if (this.lastToken?.value === '$' && c === '$') {
      this.upsertToken({ type: TokenType.Bracket, value: c }, true);
      return;
    }
    if (
      this.lastToken &&
      this.isFormatterBracket(this.lastToken.value) &&
      c === this.lastToken.value
    ) {
      this.upsertToken({ type: TokenType.Text, value: c }, true);
      this.checkLastTokensNeedMerge();
      return;
    }
    this.createToken({ type: TokenType.Bracket, value: c });
  }

  private handleLatexBrackets(c: string): void {
    this.createToken({ type: TokenType.LatexBracket, value: c });
  }

  private handleText(c: string): void {
    if (this.isPrevToken(TokenType.Comment)) {
      this.appendPrevValue(c);
      return;
    }

    if (
      this.isPrevToken(TokenType.Operator) &&
      this.lastToken.value === this.keywordStartOperator
    ) {
      this.upsertToken({ type: TokenType.Keyword, value: c }, true);
      return;
    }

    // TODO: master check is code reachable
    if (this.isPrevToken(TokenType.Keyword)) {
      this.appendPrevValue(c);
      this.checkSpecificKeyword();
      return;
    }
    this.appendTextToken(c);
    this.checkIsLastTextTokenKeyword();
  }

  private checkSpecificKeyword(): void {
    const isLatexKeyword = this.latexEnvironmentBlocks.includes(
      this.lastToken.value
    );
    if (!isLatexKeyword) {
      return;
    }
    const prevTokenNewLine = this.isEol(this.lastToken.prev);

    if (prevTokenNewLine) {
      this.lastToken.setType(TokenType.LatexEnvironmentKeyword);
      return;
    }
    this.lastToken.setType(TokenType.Text);
    if (this.lastToken.prev?.isType(TokenType.Text)) {
      this.forceMergeLastTokens(2, TokenType.Text);
    }
  }

  private isHorizontalRule(c: string): boolean {
    const prevTokenNewLine =
      !this.lastToken.prev || this.lastToken.prev.isType(TokenType.NewLine);
    const isDash = c === '-';
    const consistOfDashes =
      this.lastToken.value.replaceAll('-', '').length === 0;
    const horizontalRuleLengthSatisfied =
      this.lastToken.value.length + 1 >= this.horizontalRuleMinLength;
    return (
      prevTokenNewLine &&
      isDash &&
      horizontalRuleLengthSatisfied &&
      consistOfDashes
    );
  }

  private appendTextToken(c: string): void {
    if (this.lastToken?.isType(TokenType.NewLine)) {
      this.createToken({ type: TokenType.Text, value: c });
      return;
    }
    // if (
    //   this.lastToken?.value[0] === ':' &&
    //   !this.isDelimiter(this.lastToken?.value[1]) &&
    //   !this.isDelimiter(c)
    // ) {
    //   this.upsertToken({ type: TokenType.Keyword, value: c }, true);
    //   return;
    // }
    if (
      this.isPrevToken(TokenType.Keyword) &&
      this.isBlockKeyword(this.lastToken?.value.slice(2)) &&
      !this.isDelimiter(c)
    ) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private isBlockKeyword(value: string): boolean {
    if (!value || value.endsWith(this.delimiter)) {
      return false;
    }
    const lowerCasedValue = value.toLowerCase();
    return (
      lowerCasedValue.startsWith('begin_') || lowerCasedValue.startsWith('end_')
    );
  }

  private checkIsLastTextTokenKeyword(): void {
    if (this.todoKeywords.find((t) => t === this.lastToken.value)) {
      this.lastToken.setType(TokenType.Keyword);
      return;
    }
    if (this.isBlockKeyword(this.lastToken.value)) {
      this.forceMergeLastTokens(2, TokenType.Keyword);
    }
    if (
      this.lastToken.value.startsWith(':') &&
      !this.lastToken.value.startsWith(': ')
    ) {
      this.lastToken.setType(TokenType.Keyword);
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
      this.satisfyPreviousResult(c);
      tokenAggregator(c);
      return;
    }
    this.handleText(c);
  }

  private satisfyPreviousResult(c: string) {
    this.checkLastTokenHorizontalRule(c);
  }

  private checkLastTokenHorizontalRule(c: string): void {
    if (
      !this.lastToken ||
      !this.lastToken.isType(TokenType.HorizontalRule) ||
      c === this.horizontalRuleChar ||
      c === this.newLineChar
    ) {
      return;
    }
    this.lastToken.setType(TokenType.Text);
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
    return (
      !token ||
      token.isType(TokenType.NewLine) ||
      this.isDelimiter(token.value.slice(-1))
    );
  }

  private isFormatterBracket(c?: string): boolean {
    return this.formatterBrackets.includes(c);
  }
}

export function tokenize(
  text: string,
  configuration?: ParserConfiguration
): Token {
  const tokenizer = new Tokenizer(text, configuration);
  return tokenizer.tokenize();
}
