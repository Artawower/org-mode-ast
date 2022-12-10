import { OrgData, WithChildren, WithSection, WithValue } from 'types';

/**
 * Method for pretty debug org tree. Useful for long AST.
 *
 * @param data - OrgData
 * @param level - number of indention
 */
export function prettyTreePrint(data: OrgData, level = 0): string {
  const indent = ' '.repeat(level * 2);
  let result = indent;

  const val = (data as WithValue).value ? ` (${JSON.stringify((data as WithValue).value)})` : '';
  result += `${data.type} [${data.start}-${data.end}]${val}\n`;
  if ((data as any).ordered != null) {
    result += `${indent}    :${(data as any).ordered ? 'ordered' : 'unordered'}:\n`;
  }

  if ((data as any).level != null) {
    result += `${indent}    :level ${(data as any).level}:\n`;
  }

  level++;

  (data as WithChildren).children?.forEach((child) => {
    result += prettyTreePrint(child, level);
  });

  if ((data as WithSection).section) {
    // result += indent + `┏${'-'.repeat(50)}┓\n`;
    result += prettyTreePrint((data as WithSection).section, level);
  }

  return result;
}
