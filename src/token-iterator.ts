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

  get currentValue(): string {
    return this.token?.value;
  }

  get prevToken(): Token {
    return this.#token.prev;
  }

  get nextToken(): Token {
    return this.#token.next;
  }

  private tokenPosition = 0;

  get isLastToken(): boolean {
    return !this.#token.next;
  }

  constructor(private tokenizer: Tokenizer) {}

  public forEach(callback: (token: Token, i: number) => void): void {
    this.#token = this.tokenizer.tokenize();

    while (this.#token) {
      this.tokenPosition++;
      callback(this.token, this.tokenPosition);
      this.#token = this.#token.next;
    }
  }
}
