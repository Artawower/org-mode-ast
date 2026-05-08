import { OrgNode } from 'models';

export function findNextSibling(
  node: OrgNode,
  predicate: (node: OrgNode) => boolean,
  stopPredicate?: (node: OrgNode) => boolean
): OrgNode | null {
  let current = node.next;
  while (current) {
    if (predicate(current)) {
      return current;
    }
    if (stopPredicate?.(current)) {
      return null;
    }
    current = current.next;
  }
  return null;
}
