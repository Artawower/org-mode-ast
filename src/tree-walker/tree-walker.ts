import { OrgNode } from 'models';

export function walkTree(node: OrgNode, callback: (node: OrgNode) => boolean) {
  const finished = callback(node);
  if (finished) {
    return;
  }
  if (node.title) {
    walkTree(node.title, callback);
  }
  if (node.section) {
    walkTree(node.section, callback);
  }
  node.children?.forEach((child) => walkTree(child, callback));
}
