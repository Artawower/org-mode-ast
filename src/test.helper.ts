import { OrgData } from 'types';

export function removeInformationAboutParents(node: OrgData): void {
  delete node.parent;
  (node as any).children?.forEach((child) => {
    delete child.parent;
    removeInformationAboutParents(child);
  });

  delete (node as any).section?.parent;
  (node as any).section?.children.forEach((child) => {
    delete child.parent;
    removeInformationAboutParents(child);
  });
}
