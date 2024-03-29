import { OrgNode } from 'models';

/**
 * Determines if a node has incorrect ranges.
 *
 * @param {OrgNode} node - The node to check.
 * @param {string} text - The text that the node ranges should correspond to.
 *
 * @return {boolean|string} Returns `false` if the node ranges are correct, otherwise returns a string indicating which node has an incorrect range.
 */
export function hasNodeIncorrectRanges(
  node: OrgNode,
  text: string
): boolean | string {
  if (node.value) {
    const slicedText = text.slice(node.start, node.end);
    return node.value === slicedText
      ? false
      : `Incorrect range for node ${node.type} with range ${node.start} - ${node.end} it should be:\n"${slicedText}" but got:\n"${node.value}"`;
  }

  let currentNode = node;
  while (currentNode) {
    if (currentNode.children) {
      for (const n of currentNode.children) {
        const hasChildNodeIncorrectRanges = hasNodeIncorrectRanges(n, text);
        if (hasChildNodeIncorrectRanges) {
          return hasChildNodeIncorrectRanges;
        }
      }
    }

    currentNode = currentNode.next;
  }
  return false;
}
