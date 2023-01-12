import * as fs from 'fs';

export const saveTree = (data: any): void => {
  fs.writeFileSync('last-test.json', JSON.stringify(data, null, 2));
};
