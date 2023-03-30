import { OrgNode } from 'models';

export function walkTree(node: OrgNode, callback: (node: OrgNode) => boolean) {
  const finished = callback(node);
  if (finished) {
    return;
  }
  node.section?.title?.children?.forEach((child) => walkTree(child, callback));
  node.section?.children?.forEach((child) => walkTree(child, callback));
  node.children?.forEach((child) => walkTree(child, callback));
}
