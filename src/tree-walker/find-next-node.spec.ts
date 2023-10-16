import { parse } from 'parser/parser';
import { findNextNode } from './find-next-node';
import { NodeType } from 'models';

describe('find next node', () => {
  it('Should italic node form root', () => {
    const orgDoc = `* Heading
** Second headline
some text and /*italic with bold*/
`;

    const parsed = parse(orgDoc);

    const italicNode = findNextNode(parsed, (n) => n.is(NodeType.Italic));
    expect(italicNode.toString()).toMatchInlineSnapshot(`
      "italic [43-63]
        operator [43-44] ("/")
        bold [44-62]
          operator [44-45] ("*")
          text [45-61] ("italic with bold")
          operator [61-62] ("*")
        operator [62-63] ("/")
      "
    `);
  });

  it('Should not find unexisting node', () => {
    const orgDoc = `#+TITLE: hello
*bold*
/italic/

| Header 1 | Header 2 | Header 3 |
| val 1    |  val 2   |   val3   |`;

    const parsed = parse(orgDoc);

    const unexistingNode = findNextNode(parsed, (n) => n.is(NodeType.ListItem));
    expect(unexistingNode).toBeUndefined();
  });
});
