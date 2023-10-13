import { parse } from 'parser/parser';
import { walkTree } from './tree-walker.js';
import { NodeType } from 'models/types.js';
import { OrgNode } from 'models/org-node.js';

describe('Tree walker', () => {
  it('Should walk through title and section!', () => {
    const orgDoc = `* Title!
Section`;

    const parsed = parse(orgDoc);

    const parsedNodeTypes: NodeType[] = [];

    walkTree(parsed, (node: OrgNode) => {
      parsedNodeTypes.push(node.type);
      return false;
    });

    expect(parsedNodeTypes).toMatchInlineSnapshot(`
      [
        "root",
        "headline",
        "title",
        "operator",
        "text",
        "newLine",
        "section",
        "text",
      ]
    `);
  });
});
