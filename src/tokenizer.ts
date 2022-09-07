import { NodeType, Token } from 'types';

class Tokenizer {
  private delimiter = ' ';

  private tokens: Token[] = [];

  get prevOperator(): Token {
    return this.tokens?.[this.tokens.length - 1];
  }

  get prevTokenIsNewLine(): boolean {
    const prev = this.prevOperator;
    return prev?.type === NodeType.Text && prev.value?.[prev.value.length - 1] === '\n';
  }

  tokenize(text: string): Token[] {
    for (let i = 0; i < text.length; i++) {
      const c = text[i];
      this.buildTokens(c);
    }
    console.log(this.tokens);

    return this.tokens;
  }

  private tokenAggregators: { [key: string]: (c: string) => void } = {
    '*': (c: string) => this.handleAsterisk(c),
    [this.delimiter]: (c: string) => this.handleDelimiter(c),
  };

  private handleAsterisk(c: string): void {
    if (!this.prevOperator || this.prevTokenIsNewLine) {
      this.tokens.push({ type: NodeType.Headline, value: c });
      return;
    }
    this.appendPrevValue(c);
  }

  private handleDelimiter(c: string): void {
    if (this.isPrevToken(NodeType.Headline)) {
      this.appendPrevValue(c);
      return;
    }
    this.upsertToken({ type: NodeType.Text, value: c });
  }

  private handleText(c: string): void {
    this.upsertToken({ type: NodeType.Text, value: c });
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

  private isPrevToken(token: NodeType): boolean {
    return this.prevOperator.type === token;
  }

  private isDelimiter(char: string): boolean {
    return char === this.delimiter;
  }
}

export function tokenize(text: string): Token[] {
  const tokenizer = new Tokenizer();
  return tokenizer.tokenize(text);
}
