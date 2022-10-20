import { Tokenizer } from 'tokenizer';
import { Token, TokenType } from 'types';

export class TokenIterator {
  #token: Token;

  get token(): Token {
    return this.#token;
  }

  get type(): TokenType {
    return this.token?.type;
  }

  get value(): string {
    return this.token?.value;
  }

  private tokens: Token[];
  private tokenPosition: number = 0;

  get isLastToken(): boolean {
    return this.tokenPosition === this.tokens.length - 1;
  }

  get isNewLine(): boolean {
    return this.token.value.endsWith('\n');
  }

  constructor(private tokenizer: Tokenizer) {}

  public forEach(callback: (token: Token, i: number) => void): void {
    this.tokens = this.tokenizer.tokenize();

    this.tokens.forEach((token, i) => {
      this.#token = token;
      this.tokenPosition = i;
      callback(this.token, i);
    });
  }
}
