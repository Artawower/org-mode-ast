import { TokenType, Token } from 'types';

class Tokenizer {
  private readonly delimiter = ' ';
  // TODO: check other todos keywords. Make them customizable
  private readonly todoKeywords = ['TODO', 'DONE', 'HOLD', 'CANCELED'];
  private readonly brackets = ['_', '=', '+', '[', ']', '/', '*'];

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

  get prevOperator(): Token {
    return this.tokens?.[this.tokens.length - 1];
  }

  get prevTokenIsNewLine(): boolean {
    const prev = this.prevOperator;
    return prev?.type === TokenType.Text && prev.value?.[prev.value.length - 1] === '\n';
  }

  tokenize(): Token[] {
    for (; this.point < this.text.length; this.point++) {
      const c = this.text[this.point];
      this.buildTokens(c);
    }
    console.log(this.tokens);

    return this.tokens;
  }

  private handleAsterisk(c: string): void {
    if (!this.prevOperator || this.prevTokenIsNewLine) {
      this.tokens.push({ type: TokenType.Headline, value: c });
      return;
    }
    if (this.isPrevToken(TokenType.Headline)) {
      this.appendPrevValue(c);
      return;
    }
    this.handleBracket(c);
  }

  private handleDelimiter(c: string): void {
    if (this.isPrevToken(TokenType.Headline)) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
  }

  private handleNumberSign(c: string): void {
    this.upsertToken({ type: TokenType.Comment, value: c });
  }

  private handleBracket(c: string): void {
    this.tokens.push({ type: TokenType.Bracket, value: c });
  }

  private handleText(c: string): void {
    if (this.isPrevToken(TokenType.Comment)) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: TokenType.Text, value: c });
    this.checkIsLastTextTokenKeyword();
  }

  private checkIsLastTextTokenKeyword(): void {
    if (this.todoKeywords.find((t) => t === this.prevOperator.value)) {
      this.prevOperator.type = TokenType.TodoKeyword;
    }
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
    if (this.prevOperator.type === token.type) {
      this.appendPrevValue(token.value);
      return;
    }
    this.tokens.push(token);
  }

  private appendPrevValue(c: string) {
    this.prevOperator.value += c;
  }

  private isPrevToken(...tokens: TokenType[]): boolean {
    return tokens.some((t) => t === this.prevOperator.type);
  }

  private isDelimiter(char: string): boolean {
    return char === this.delimiter;
  }
}

export function tokenize(text: string): Token[] {
  const tokenizer = new Tokenizer(text);
  return tokenizer.tokenize();
}
