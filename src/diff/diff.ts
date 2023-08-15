import { NodeType, OrgNode } from 'models';
import { parse } from '../parser';

export function getDiff(newText: string, oldText: string): OrgNode[] {
  const newNode = parse(newText);
  const oldNode = parse(oldText);
  return findOrgNodesDiff(newNode, oldNode);
}

const fieldsToCompare = ['section', 'title'];
function nodeChanged(newNode: OrgNode, oldNode?: OrgNode): boolean {
  if (!oldNode) {
    return true;
  }
  const valuesNotEqual = oldNode.value !== newNode.value;
  const titleOrSectionChanged = fieldsToCompare.some(
    (f) => !!oldNode[f] !== !!newNode[f]
  );
  const childrenLengthChanged =
    oldNode.children?.length !== newNode.children?.length;

  const rangeChanged =
    oldNode.length < newNode.length &&
    newNode.isNot(
      NodeType.Root,
      NodeType.Title,
      NodeType.Section,
      NodeType.List,
      NodeType.ListItem
    );

  return (
    valuesNotEqual ||
    titleOrSectionChanged ||
    childrenLengthChanged ||
    rangeChanged
  );
}
export function findOrgNodesDiff(
  newNode?: OrgNode,
  oldNode?: OrgNode
): OrgNode[] {
  const changedNodes: OrgNode[] = [];

  if (!newNode) {
    return changedNodes;
  }

  if (nodeChanged(newNode, oldNode)) {
    changedNodes.push(newNode);
    return changedNodes;
  }

  newNode.children?.forEach((newChild, index) => {
    const oldChild = oldNode.children?.get(index);
    if (!oldChild) {
      changedNodes.push(newChild);
      return;
    }
    const childDiff = findOrgNodesDiff(newChild, oldChild);
    changedNodes.push(...childDiff);
  });

  changedNodes.push(...findOrgNodesDiff(newNode.section, oldNode.section));
  changedNodes.push(...findOrgNodesDiff(newNode.title, oldNode.title));

  return changedNodes;
}
