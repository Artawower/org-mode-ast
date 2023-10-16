import { NodeType, OrgNode } from 'models';
import { findParent } from './find-parent';
import { parse } from 'parser/parser';
import { findNextNode } from './find-next-node';

describe('find parent', () => {
  it('Should find parent', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const headline = new OrgNode({
      type: NodeType.Headline,
      start: 0,
      end: 0,
    });

    const title = new OrgNode({
      type: NodeType.Title,
      start: 0,
      end: 0,
    });
    const titleText = new OrgNode({
      type: NodeType.Text,
      value: 'This is title',
    });

    const section = new OrgNode({
      type: NodeType.Section,
      start: 0,
      end: 0,
    });

    const sectionText = new OrgNode({
      type: NodeType.Text,
      value: 'This is section',
    });

    section.addChild(sectionText);
    title.addChild(titleText);
    headline.setTitle(title);
    headline.setSection(section);
    rootNode.addChild(headline);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        headline [0-28]
          title [0-13]
            text [0-13] ("This is title")
          section [13-28]
            text [13-28] ("This is section")
      "
    `);
    const collectedParents = [];
    findParent(sectionText, (orgNode) => {
      collectedParents.push(orgNode.type);
      return false;
    });

    expect(collectedParents).toMatchInlineSnapshot(`
      [
        "section",
        "headline",
        "root",
      ]
    `);
  });

  it('Should find parent list and stop iter', () => {
    const orgDoc = `* Headline
- List 1
- /*List 2*/`;

    const parsed = parse(orgDoc);
    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-32]
        headline [0-32]
            :level 1:
          title [0-11]
            operator [0-2] ("* ")
            text [2-10] ("Headline")
            newLine [10-11]
          section [11-32]
            list [11-32]
                :unordered:
                :level 0:
              listItem [11-20]
                title [11-20]
                  operator [11-13] ("- ")
                  text [13-19] ("List 1")
                  newLine [19-20]
              listItem [20-32]
                title [20-32]
                  operator [20-22] ("- ")
                  italic [22-32]
                    operator [22-23] ("/")
                    bold [23-31]
                      operator [23-24] ("*")
                      text [24-30] ("List 2")
                      operator [30-31] ("*")
                    operator [31-32] ("/")
      "
    `);

    const foundItalicNode = findNextNode(parsed, (n) => {
      return n.is(NodeType.Italic);
    });

    const collectedParents = [];
    findParent(foundItalicNode, (orgNode) => {
      collectedParents.push(orgNode.type);
      return false;
    });

    expect(collectedParents).toMatchInlineSnapshot(`
      [
        "title",
        "listItem",
        "list",
        "section",
        "headline",
        "root",
      ]
    `);
  });
});
