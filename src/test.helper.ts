import { OrgData, WithNeighbors } from 'types';
import * as fs from 'fs';

export function removeInformationAboutRelatives(node: OrgData): void {
  delete node.parent;
  delete (node as WithNeighbors).prev;
  delete (node as WithNeighbors).next;
  (node as any).children?.forEach((child) => {
    delete child.parent;
    removeInformationAboutRelatives(child);
  });

  delete (node as any).section?.parent;
  (node as any).section?.children.forEach((child) => {
    delete child.parent;
    removeInformationAboutRelatives(child);
  });
}

export const saveTree = (data: any): void => {
  fs.writeFileSync('last-test.json', JSON.stringify(data, null, 2));
};
