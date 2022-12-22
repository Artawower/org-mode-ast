import { PartialUniversalOrgNode } from './types';

export interface OrgHandler {
  handle();
}

export interface Unspecified extends PartialUniversalOrgNode {}
