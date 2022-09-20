import { TokenType, Token } from 'types';

export class Tokenizer {
  private readonly delimiter = ' ';
  // TODO: check other todos keywords. Make them customizable
  private readonly todoKeywords = ['TODO', 'DONE', 'HOLD', 'CANCELED'];
  private readonly brackets = ['=', '+', '[', ']', '/', '*'];

  private tokens: Token[] = [];
  private point: number = 0;
  private tokenAggregators: { [key: string]: (c: string) => void };

  constructor(private text: string) {
    this.initTokenAggregators();
  }

  private initTokenAggregators() {
    const commonAggregators = {
      '*': (c: string) => this.handleAsterisk(c),
      [this.delimiter]: (c: string) => this.handleDelimiter(c),
      '#': (c: string) => this.handleNumberSign(c),
      '-': (c: string) => this.handleDash(c),
      '+': (c: string) => this.handlePlus(c),
      ':': (c: string) => this.handleCommon(c),
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

  get isPrevTokenIsNewLine(): boolean {
    if (!this.prevToken) {
      return true;
    }
    const prev = this.prevToken;
    return prev?.type === TokenType.Text && prev.value?.[prev.value.length - 1] === '\n';
  }

  get nextChar(): string {
    return this.text[this.point + 1];
  }

  get isPrevTokenEndsWithSpace(): boolean {
    if (!this.prevToken) {
      return false;
    }
    return this.prevToken.value?.[this.prevToken.value.length - 1] === this.delimiter;
  }

  tokenize(): Token[] {
    for (; this.point < this.text.length; this.point++) {
      const c = this.text[this.point];
      this.buildTokens(c);
    }

    return this.tokens;
  }

  private handleAsterisk(c: string): void {
    if ((this.isDelimiter(this.nextChar) || this.isNextChar('*')) && (!this.prevToken || this.isPrevTokenIsNewLine)) {
      this.tokens.push({ type: TokenType.Headline, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Headline)) {
      this.appendPrevValue(c);
      return;
    }
    if (this.isDelimiter(this.nextChar)) {
      this.upsertToken({ type: TokenType.Text, value: c });
      return;
    }
    this.handleBracket(c);
  }

  private handleDelimiter(c: string): void {
    if (this.isPrevToken(TokenType.Headline) && !this.isPrevTokenEndsWithSpace) {
      this.appendPrevValue(c);
      return;
    }
    if (this.shouldFormatterHasSpaceAtTheEnd()) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handleNumberSign(c: string): void {
    this.upsertToken({ type: TokenType.Comment, value: c });
  }

  private handleDash(c: string): void {
    if (this.isPrevTokenIsNewLine || !this.prevToken) {
      this.tokens.push({ type: TokenType.Operator, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handlePlus(c: string): void {
    if (this.isPrevTokenIsNewLine || !this.prevToken) {
      this.tokens.push({ type: TokenType.Operator, value: c });
      return;
    }
    this.handleBracket(c);
  }

  private handleCommon(c: string): void {
    this.tokens.push({ type: TokenType.Operator, value: c });
  }

  private handleBracket(c: string): void {
    this.tokens.push({ type: TokenType.Bracket, value: c });
  }

  private handleText(c: string): void {
    if (this.isPrevToken(TokenType.Comment)) {
      this.appendPrevValue(c);
      return;
    }
    if (this.isPrevTokenIsNewLine) {
      this.tokens.push({ type: TokenType.Text, value: c });
    } else {
      this.upsertToken({ type: TokenType.Text, value: c });
    }

    this.checkIsLastTextTokenKeyword();
  }

  private checkIsLastTextTokenKeyword(): void {
    if (this.todoKeywords.find((t) => t === this.prevToken.value)) {
      this.prevToken.type = TokenType.Keyword;
      return;
    }
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

  private upsertToken(token: Token) {
    if (this.prevToken?.type === token.type) {
      this.appendPrevValue(token.value);
      return;
    }
    this.tokens.push(token);
  }

  private appendPrevValue(c: string) {
    this.prevToken.value += c;
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

  private isTokenFromEndEqualTo() {}

  private isDelimiter(char: string): boolean {
    return char === this.delimiter;
  }
}

export function tokenize(text: string): Token[] {
  const tokenizer = new Tokenizer(text);
  return tokenizer.tokenize();
}
