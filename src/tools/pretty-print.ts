import { NodeType, OrgNode } from '../models/index.js';

function prettifyArrayValue(indent: string, key: string, value: any): string {
  if (key === 'headings') {
    return value.reduce((acc, v) => {
      acc += `${indent}${' '.repeat(v.level * 2)}${v.title}\n`;
      return acc;
    }, '\n');
  }
  return value.join(',');
}

/**
 * Method for pretty debug org tree. Useful for long AST.
 *
 * @param data - OrgData
 * @param level - number of indention
 */
export function prettyTreePrint(data: OrgNode, level = 0): string {
  if (!data) {
    return '[EMPTY]';
  }
  const indent = ' '.repeat(level * 2);
  let result = indent;

  const val =
    data.value && data.isNot(NodeType.NewLine)
      ? ` (${JSON.stringify(data.value)})`
      : '';

  result += `${data.type} [${data.start}-${data.end}]${val}\n`;

  if (data.ordered != null) {
    result += `${indent}    :${data.ordered ? 'ordered' : 'unordered'}:\n`;
  }

  if (data.checked != null) {
    result += `${indent}    :${data.checked ? 'checked' : 'unchecked'}:\n`;
  }

  if (data.level != null) {
    result += `${indent}    :level ${data.level}:\n`;
  }
  if (data.meta) {
    result += Object.keys(data.meta).reduce<string>((acc, k) => {
      const value = Array.isArray(data.meta[k])
        ? prettifyArrayValue(indent + '    ', k, data.meta[k])
        : data.meta[k];

      if (k === 'headings') {
        return `${acc}${indent}    :${k}: ${value}\n`;
      }
      return `${acc}${indent}    :${k} ${value}:\n`;
    }, '');
  }

  if (data.properties) {
    Object.keys(data.properties).forEach((k: string) => {
      result += `${indent}    :${k} ${data.properties[k]}:\n`;
    });
  }

  level++;

  data.children?.forEach((child) => {
    result += prettyTreePrint(child, level);
  });

  if (data.title) {
    result += prettyTreePrint(data.title, level);
  }

  if (data.section) {
    // result += indent + `┏${'-'.repeat(50)}┓\n`;
    result += prettyTreePrint(data.section, level);
  }

  return result;
}

export function printNodes(nodes: OrgNode[]) {
  nodes.forEach((node) => {
    console.log({
      ...node,
      value: node.value,
    });
  });
}
