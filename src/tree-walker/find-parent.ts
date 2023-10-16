import { OrgNode } from 'models';

export function findParent(
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
  findParent(node.parent, callback);
}
