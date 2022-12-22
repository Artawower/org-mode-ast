import { TokenType, Token, RawToken } from 'types';

// TODO: master DELETE BEGIN/END ??? think about it, its very redundant
export class Tokenizer {
  private readonly delimiter = ' ';
  // TODO: check other todos keywords. Make them customizable
  private readonly todoKeywords = ['TODO', 'DONE', 'HOLD', 'CANCELED'];
  private readonly brackets = ['=', '+', '[', ']', '/', '*'];
  private begin: number = 0;
  private end: number = 0;

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

  get prevChar(): string {
    return this.text[this.point - 1];
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
    if (this.isPrevTokenIsNewLine) {
      this.addToken({ type: TokenType.Indent, value: c });
      return;
    }
    if (
      this.isPrevToken(TokenType.Indent) ||
      (this.isPrevToken(TokenType.Operator) && this.prevToken.value.endsWith('-'))
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
    this.appendTextNode(c);
  }

  private handleNumberSign(c: string): void {
    this.upsertToken({ type: TokenType.Comment, value: c });
  }

  private handleDash(c: string): void {
    if (!this.isPrevTokenIsNewLine && this.prevToken && !this.prevToken.value.trim()) {
      this.upsertToken({ type: TokenType.Operator, value: c });
      return;
    }
    if (this.isPrevTokenIsNewLine || !this.prevToken) {
      this.addToken({ type: TokenType.Operator, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handlePlus(c: string): void {
    if (this.isNextChar(this.delimiter) && (this.isPrevTokenIsNewLine || !this.prevToken)) {
      this.addToken({ type: TokenType.Operator, value: c });
      return;
    }
    this.handleBracket(c);
  }

  private handleCommon(c: string): void {
    this.addToken({ type: TokenType.Operator, value: c });
  }

  private handleBracket(c: string): void {
    this.addToken({ type: TokenType.Bracket, value: c });
  }

  private handleText(c: string): void {
    if (this.isPrevToken(TokenType.Comment)) {
      this.appendPrevValue(c);
      return;
    }
    this.appendTextNode(c);
    this.checkIsLastTextTokenKeyword();
  }

  private appendTextNode(c: string): void {
    if (this.isPrevTokenIsNewLine) {
      this.addToken({ type: TokenType.Text, value: c });
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private checkIsLastTextTokenKeyword(): void {
    if (this.todoKeywords.find((t) => t === this.prevToken.value)) {
      this.prevToken.setType(TokenType.Keyword);
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

  private addToken(token: RawToken) {
    this.begin = this.end;
    this.end = this.begin + token.value.length;
    const newToken = new Token(token, this.begin);
    this.tokens.push(newToken);
  }

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

export function tokenize(text: string): Token[] {
  const tokenizer = new Tokenizer(text);
  return tokenizer.tokenize();
}
