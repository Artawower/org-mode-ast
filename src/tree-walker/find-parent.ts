import { OrgNode } from 'models';

export function walkThroughParents(
  node: OrgNode,
  callback: (node: OrgNode) => boolean
) {
  if (!node?.parent) {
    return;
  }
  const finished = callback(node.parent);
  if (finished) {
    return;
  }
  walkThroughParents(node.parent, callback);
}

export function findParent(
  node: OrgNode,
  callback: (node: OrgNode) => boolean | [boolean, boolean]
): OrgNode {
  let foundOrgNode: OrgNode;
  walkThroughParents(node, (n) => {
    const res = callback(n);
    const isFound = Array.isArray(res) ? res[0] : res;
    const isFinished = Array.isArray(res) ? res[1] : false;

    if (isFound) {
      foundOrgNode = n;
      return true;
    }

    if (isFinished) {
      return true;
    }
  });
  return foundOrgNode;
}
