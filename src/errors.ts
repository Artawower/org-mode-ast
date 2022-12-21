import { Token } from 'types';

export class HandlerNotFoundError extends Error {
  public readonly type = 'HandlerNotFoundError';

  constructor(public operator: string) {
    super();
    this.message = `Handler not found for operator: ${this.operator}`;
  }
}

export class UnsupportedOperator extends Error {
  public readonly type = 'HandlerDidNotReturnValue';

  constructor(public operator: string) {
    super();
    this.message = `Handler for operator: ${this.operator} did not return a value`;
  }
}

/**
 * This exception should not occur in a normal use case.
 * Only in development mode
 *
 * @param {string} operator
 */
export class HandlerDidNotReturnValue extends Error {
  public readonly type = 'UnsupportedOperator';

  constructor(public token: Token) {
    super();
    this.message = `Handler for token ${token.type} with value ${token.value} returned undefined`;
  }
}
