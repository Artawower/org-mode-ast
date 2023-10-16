import { OrgNode } from 'models';
import { walkTree } from './tree-walker';

// TODO: master test it
export function findNextNode(
  node: OrgNode,
  cb: (n: OrgNode) => boolean
): OrgNode {
  let foundOrgNode: OrgNode;
  walkTree(node, (n: OrgNode) => {
    const isFound = cb(n);
    if (isFound) {
      foundOrgNode = n;
      return true;
    }
  });
  return foundOrgNode;
}
