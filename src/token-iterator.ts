import { Tokenizer } from 'tokenizer';
import { Token, TokenType } from 'types';

export class TokenIterator {
  #token: Token;
  #begin = 0;
  #end = 0;

  get token(): Token {
    return this.#token;
  }

  get type(): TokenType {
    return this.token?.type;
  }

  get currentValue(): string {
    return this.token?.value;
  }

  get prevToken(): Token {
    return this.tokens[this.tokenPosition - 1];
  }

  get nextToken(): Token {
    return this.tokens[this.tokenPosition + 1];
  }

  private tokens: Token[];
  private tokenPosition = 0;

  get isLastToken(): boolean {
    return this.tokenPosition === this.tokens.length - 1;
  }

  constructor(private tokenizer: Tokenizer) {}

  public forEach(callback: (token: Token, i: number) => void): void {
    this.tokens = this.tokenizer.tokenize();

    this.tokens.forEach((token, i) => {
      this.#begin = this.#end;
      this.#end = this.#begin + token.value.length;

      this.#token = new Token(token, this.#begin);

      this.tokenPosition = i;
      callback(this.token, i);
    });
  }
}
