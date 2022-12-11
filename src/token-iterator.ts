import { Tokenizer } from 'tokenizer';
import { Token, TokenType } from 'types';

export class TokenIterator {
  #token: Token;
  #begin: number = 0;
  #end: number = 0;

  get token(): Token {
    return this.#token;
  }

  get type(): TokenType {
    return this.token?.type;
  }

  get currentTokenValue(): string {
    return this.token?.value;
  }

  get prevToken(): Token {
    return this.tokens[this.tokenPosition - 1];
  }

  get nextToken(): Token {
    return this.tokens[this.tokenPosition + 1];
  }

  // TODO: master make private
  public tokens: Token[];
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
      this.#begin = this.#end;
      this.#end = this.#begin + token.value.length;

      this.#token = {
        ...token,
        begin: this.#begin,
        end: this.#end,
      };

      this.tokenPosition = i;
      callback(this.token, i);
    });
  }
}
