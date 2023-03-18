import { OrgNode } from 'models';

export function walkTree(node: OrgNode, callback: (node: OrgNode) => true) {
  const finished = callback(node);
  if (finished) {
    return;
  }
  node.children?.forEach((child) => walkTree(child, callback));
}
