export class HandlerNotFoundError extends Error {
  public readonly type = 'HandlerNotFoundError';

  constructor(public operator: string) {
    super();
    this.message = `Handler not found for operator: ${this.operator}`;
  }
}
