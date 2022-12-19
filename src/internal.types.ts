import { WithValue, WithRange, List } from './types';

export interface OrgHandler {
  handle();

  // TODO: master implement this method
  // newLineAppear();
}

// TODO: master add support for all types we know
export interface Unspecified extends WithValue, Partial<WithRange & List> {}
