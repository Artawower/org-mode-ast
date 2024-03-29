import {
  ParserConfiguration,
  RawToken,
  Token,
  TokenType,
} from '../models/index.js';
import { KNOWN_ENTITIES } from './entities.const.js';

// TODO: master This code is candidate number one to be refactored.
// Seriously, spend some time to make it more readable...
export class Tokenizer {
  // TODO: master move this settings to parser configuration
  private readonly delimiter = ' ';
  private readonly nonbreakingSpace = ' ';
  private readonly brackets = ['[', ']', '<', '>', '$'];
  private readonly markupOperators = ['=', '+', '/', '*', '~', '_'];
  private readonly charactersBeforeMarkup = [
    this.delimiter,
    this.nonbreakingSpace,
    ...this.markupOperators,
    '\n',
    '-',
    '(',
    '{',
    "'",
    '"',
  ];
  private readonly charactersAfterMarkup = [
    this.delimiter,
    this.nonbreakingSpace,
    ...this.markupOperators,
    '-',
    '.',
    ',',
    ';',
    ':',
    '!',
    '?',
    ',',
    ')',
    '}',
    '[',
    '"',
    '\\',
    '\n',
  ];
  private readonly horizontalRuleChar = '-';
  private readonly newLineChar = '\n';
  private readonly horizontalRuleMinLength = 5;
  private readonly latexEnvironmentBlocks = ['\\begin', '\\end'];
  private readonly keywordStartOperator = '#+';
  private readonly listItemCloseSymbols = [')', '.'];

  private begin = 0;
  private end = 0;

  // TODO: make linked list
  private point = 0;
  private tokenAggregators: { [key: string]: (c: string) => void };
  private firstToken: Token;
  private lastToken: Token;
  private todoKeywords: string[] = [];

  constructor(
    private readonly text: string,
    private readonly configuration: ParserConfiguration
  ) {
    this.todoKeywords = configuration.todoKeywords ?? [];
    this.initTokenAggregators();
  }

  private initTokenAggregators() {
    const commonAggregators = {
      '*': (c: string) => this.handleAsterisk(c),
      // TODO: master dynamic add all delimiters
      [this.delimiter]: (c: string) =>
        this.preserveTableDelimiter(this.handleDelimiter)(c),
      [this.nonbreakingSpace]: (c: string) =>
        this.preserveTableDelimiter(this.handleDelimiter)(c),
      '#': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleNumberSign))(
          c
        ),
      '/': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleSlash))(c),
      '-': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleDash))(c),
      '+': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handlePlus))(c),
      ':': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleColon))(c),
      '.': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handlePoint))(c),
      ')': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleParenthesis))(
          c
        ),
      '\n': (c: string) => this.handleNewLine(c),
      '\\': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handleBackslash))(c),
      '{': (c: string) =>
        this.preserveTableDelimiter(
          this.preserveLink(this.handleLatexBrackets)
        )(c),
      '}': (c: string) =>
        this.preserveTableDelimiter(
          this.preserveLink(this.handleLatexBrackets)
        )(c),
      '|': (c: string) =>
        this.preserveTableDelimiter(this.preserveLink(this.handlePipe))(c),
    };
    const bracketAggregators = this.brackets.reduce((acc, c) => {
      acc[c] = (c: string) => this.handleBracket(c);
      return acc;
    }, {});

    const markupAggregators = this.markupOperators.reduce((acc, c) => {
      acc[c] = (c: string) => this.handleMarkupOperator(c);
      return acc;
    }, {});

    this.tokenAggregators = {
      ...bracketAggregators,
      ...markupAggregators,
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
    this.verifyTokenType(this.lastToken);
    this.mergeLastPotentialTextTokens();

    return this.firstToken;
  }

  private preserveLink(originalFn: (c: string) => void): (c: string) => void {
    return (c: string) => {
      if (this.isPrevToken(TokenType.Link) && !this.isDelimiter(c)) {
        this.appendPrevValue(c);
        return;
      }
      originalFn.bind(this)(c);
    };
  }

  private preserveTableDelimiter(
    originalFn: (c: string) => void
  ): (c: string) => void {
    return (c: string) => {
      if (this.lastToken?.isType(TokenType.TableDelimiter)) {
        this.upsertToken({ type: TokenType.TableDelimiter, value: c });
        return;
      }
      originalFn.bind(this)(c);
    };
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
    this.handleMarkupOperator(c);
  }

  private handleDelimiter(c: string): void {
    this.verifyTokenType(this.lastToken);
    this.mergeLastPotentialTextTokens();

    if (this.handleFixedWidthOperator(c)) {
      return;
    }

    if (this.handleSpaceAfterKeyword(c)) {
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

    if (this.isLastTokenTodoKeyword) {
      this.upsertToken({ type: TokenType.Text, value: c });
      return;
    }

    if (this.isPrevToken(TokenType.Keyword)) {
      this.appendPrevValue(c);
      return;
    }

    this.appendTextToken(c);
  }

  private mergeLastPotentialTextTokens(): void {
    if (!this.lastToken?.isType(TokenType.Text)) {
      return;
    }

    if (this.isLastTokenWebLink()) {
      this.lastToken.type = TokenType.Link;
      return;
    }

    if (!this.lastToken.prev?.isType(TokenType.Text)) {
      return;
    }
    this.forceMergeLastTokens(2, TokenType.Text);
  }

  private isLastTokenWebLink(): boolean {
    if (this.lastToken.value === 'www' && this.nextChar === '.') {
      return true;
    }
    const isStartLink =
      this.lastToken.value === 'http:' || this.lastToken.value === 'https:';
    if (isStartLink && this.text[this.point] === '/' && this.nextChar === '/') {
      return true;
    }
  }

  private verifyTokenType(token: Token): void {
    if (
      !token ||
      !token?.isType(TokenType.Text) ||
      !this.configuration.httpLinkRegexp.test(token.value)
    ) {
      return;
    }
    token.type = TokenType.Link;
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

  private handleSpaceAfterKeyword(c: string): boolean {
    if (!this.isLastKeywordEnd) {
      return;
    }
    this.createToken({ type: TokenType.Text, value: c });
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

  private handleSlash(c: string): void {
    if (this.isOpenedMarkupOperator() || this.isClosedMarkupOperator()) {
      return this.handleMarkupOperator(c);
    }
    return this.createToken({ type: TokenType.Operator, value: c });
  }

  private handleDash(c: string): void {
    if (this.lastToken?.isType(TokenType.TableOperator)) {
      this.upsertToken({ type: TokenType.TableDelimiter, value: c }, true);
      return;
    }
    if (this.lastToken?.isType(TokenType.Link)) {
      this.appendPrevValue(c);
      return;
    }
    if (
      this.lastToken &&
      !this.lastToken.isType(TokenType.NewLine) &&
      !this.lastToken.value.trim() &&
      this.lastToken.prev?.isType(TokenType.NewLine)
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
    this.handleMarkupOperator(c);
  }

  private handleColon(c: string): void {
    if (this.isListTagOperator(c)) {
      return;
    }
    if (this.isNotListTagOperator(c)) {
      return;
    }
    if (this.isPropertyKeyword(c)) {
      return;
    }
    if (
      !this.isLastKeywordEnd &&
      this.isEol(this.lastToken?.prev) &&
      (this.lastToken?.value === ':' ||
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

  private isNotListTagOperator(c: string): boolean {
    if (
      this.lastToken?.value !== ':' ||
      c !== ':' ||
      this.isDelimiter(this.nextChar)
    ) {
      return;
    }

    this.upsertToken({ type: TokenType.Text, value: c }, true);

    if (this.lastToken.prev?.isType(TokenType.Text)) {
      this.forceMergeLastTokens(2, TokenType.Text);
    }

    return true;
  }

  private isListTagOperator(c: string): boolean {
    if (
      !this.lastToken?.isType(TokenType.Operator) ||
      this.lastToken.value !== ':' ||
      !this.isDelimiter(this.nextChar)
    ) {
      return;
    }

    this.upsertToken({ type: TokenType.Operator, value: c });
    return true;
  }

  private isPropertyKeyword(c: string): boolean {
    const prev = this.lastToken?.prev;
    const isPrevText = this.lastToken?.isType(TokenType.Text);
    const isLastColon = prev?.value === ':';
    const isPropertyStartedFromnewLine = this.isEol(prev?.prev);

    if (isPrevText && isLastColon && isPropertyStartedFromnewLine) {
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
      this.isValueNumber(this.lastToken?.value)
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
    this.mergeLastPotentialTextTokens();
    this.createToken({ type: TokenType.NewLine, value: c });
  }

  private handleBackslash(c: string): void {
    this.createToken({
      type: TokenType.Keyword,
      value: c,
    });
  }

  private isValueNumber(value: string): boolean {
    return !!value?.match(/^[0-9]+$/);
  }

  private handleMarkupOperator(c: string): void {
    this.mergeLastPotentialTextTokens();

    if (this.lastToken?.isType(TokenType.Keyword)) {
      return this.appendPrevValue(c);
    }
    if (['~', '/', '='].includes(c) && this.lastToken?.isType(TokenType.Link)) {
      return this.appendPrevValue(c);
    }

    if (this.isClosedMarkupOperator()) {
      return this.createToken({ type: TokenType.CloseMarkup, value: c });
    }
    if (this.isOpenedMarkupOperator()) {
      return this.createToken({ type: TokenType.OpenMarkup, value: c });
    }
    this.appendTextToken(c);
  }

  private handleBracket(c: string): void {
    this.mergeLastPotentialTextTokens();

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

  private isFormatterBracket(c?: string): boolean {
    return this.markupOperators.includes(c);
  }

  private handleLatexBrackets(c: string): void {
    if (this.isPrevToken(TokenType.Entity)) {
      return this.appendPrevValue(c);
    }
    this.createToken({ type: TokenType.LatexBracket, value: c });
  }

  private handlePipe(c: string): void {
    this.createToken({ type: TokenType.TableOperator, value: c });
  }

  private handleText(c: string): void {
    if (this.isLastKeywordEnd) {
      this.createToken({ type: TokenType.Text, value: c });
      return;
    }
    if (this.lastToken?.isType(TokenType.Link)) {
      this.appendPrevValue(c);
      return;
    }

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

  private isEntity(val: string): boolean {
    return KNOWN_ENTITIES.includes(val);
  }

  get isLastKeywordEnd(): boolean {
    return (
      this.lastToken?.isType(TokenType.Keyword) &&
      (this.lastToken.value.startsWith('#+') ||
        this.lastToken.value.startsWith(':')) &&
      this.lastToken.value.trim().length !== this.lastToken.value.length
    );
  }

  private checkSpecificKeyword(): void {
    const isEntity = this.isEntity(this.lastToken.value.slice(1));
    if (isEntity) {
      this.lastToken.setType(TokenType.Entity);
      return;
    }
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
    if (
      this.isPrevToken(TokenType.Keyword) &&
      this.isBlockKeyword(this.lastToken?.value.slice(2)) &&
      !this.isDelimiter(c)
    ) {
      this.appendPrevValue(c);
      return;
    }
    if (
      this.lastToken?.value.endsWith(this.delimiter) &&
      !this.isDelimiter(c)
    ) {
      this.createToken({ type: TokenType.Text, value: c });
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
    if (
      this.lastToken?.prev?.isType(TokenType.Headline) &&
      this.isLastTokenTodoKeyword
    ) {
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

  private get isLastTokenTodoKeyword(): boolean {
    return !!this.todoKeywords.find((t) => t === this.lastToken.value);
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
      this.lastToken = prevToken;
      this.addToken(newToken);
    } else {
      this.lastToken = newToken;
      this.firstToken = newToken;
    }
    this.lastToken = newToken;
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
    if (this.lastToken && (this.lastToken?.type === token.type || force)) {
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
    return this.isCharSeparator(token?.value);
  }

  private isCharSeparator(c: string): boolean {
    return !c || c === '\n' || this.isDelimiter(c.slice(-1));
  }

  private isOpenedMarkupOperator(): boolean {
    const isMarkupPre =
      !this.prevChar || this.charactersBeforeMarkup.includes(this.prevChar);
    const isNextCharPartOfContent = !this.isDelimiter(this.nextChar);
    return isMarkupPre && isNextCharPartOfContent;
  }

  private isClosedMarkupOperator(): boolean {
    if (!this.lastToken) {
      return false;
    }
    const isMarkupPost =
      !this.nextChar || this.charactersAfterMarkup.includes(this.nextChar);

    const isPrevCharPartOfContent =
      !this.isDelimiter(this.prevChar) && this.prevChar !== '\n';

    return isPrevCharPartOfContent && isMarkupPost;
  }
}

export function tokenize(
  text: string,
  configuration?: ParserConfiguration
): Token {
  const tokenizer = new Tokenizer(text, configuration);
  return tokenizer.tokenize();
}
