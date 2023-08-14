import { NodeType, OrgNode } from 'models';
import { parse } from '../parser';

export function getDiff(oldText: string, newText: string): OrgNode[] {
  const oldNode = parse(oldText);
  const newNode = parse(newText);
  return findOrgNodesDiff(oldNode, newNode);
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

  return valuesNotEqual || titleOrSectionChanged || childrenLengthChanged;
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
    console.log('✎: [line 51][diff.ts] oldChild type: ', oldChild?.type);
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
