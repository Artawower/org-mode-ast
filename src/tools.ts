import { OrgNode } from 'org-node';
import { NodeType, OrgStruct, WithValue } from 'types';

/**
 * Method for pretty debug org tree. Useful for long AST.
 *
 * @param data - OrgData
 * @param level - number of indention
 */
export function prettyTreePrint(data: OrgNode<OrgStruct>, level = 0): string {
  data.safeCheckMode = false;
  const indent = ' '.repeat(level * 2);
  let result = indent;

  const val = data.value && data.type !== NodeType.NewLine ? ` (${JSON.stringify(data.value)})` : '';

  result += `${data.type} [${data.start}-${data.end}]${val}\n`;

  if (data.ordered != null) {
    result += `${indent}    :${(data as any).ordered ? 'ordered' : 'unordered'}:\n`;
  }

  if (data.level != null) {
    result += `${indent}    :level ${data.level}:\n`;
  }

  if (data.properties) {
    Object.keys((data as any).properties).forEach((k: string) => {
      result += `${indent}    :${k} ${(data as any).properties[k]}:\n`;
    });
  }

  level++;

  data.children?.forEach((child) => {
    result += prettyTreePrint(child, level);
  });

  if (data.section) {
    // result += indent + `┏${'-'.repeat(50)}┓\n`;
    result += prettyTreePrint(data.section, level);
  }

  return result;
}
