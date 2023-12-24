import { OrgNode } from './org-node.js';

export interface OrgHandler {
  handle(): OrgNode | null;
}
