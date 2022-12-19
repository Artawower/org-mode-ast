import { OrgData } from 'types';
import * as fs from 'fs';

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

export const saveTree = (data: any): void => {
  fs.writeFileSync('last-test.json', JSON.stringify(data, null, 2));
};
