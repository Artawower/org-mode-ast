export class HandlerNotFoundError extends Error {
  public readonly type = 'HandlerNotFoundError';

  constructor(public operator: string) {
    super();
    this.message = `Handler not found for operator: ${this.operator}`;
  }
}

/**
 * This exception should not occur in a normal use case.
 * Only in development mode
 *
 * @param {string} operator
 */
export class UnsupportedOperator extends Error {
  public readonly type = 'UnsupportedOperator';

  constructor(public operator: string) {
    super();
    this.message = `Unsupported operator: ${this.operator}`;
  }
}
