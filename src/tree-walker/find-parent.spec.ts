import { NodeType, OrgNode } from 'models';
import { findParent, walkThroughParents } from './find-parent';
import { parse } from 'parser/parser';
import { findNextNode } from './find-next-node';

describe('Walk through parents', () => {
  it('Should walk through parents', () => {
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
    walkThroughParents(sectionText, (orgNode) => {
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

  it('Should walk through list and stop iter', () => {
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
    walkThroughParents(foundItalicNode, (orgNode) => {
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

describe('find parent', () => {
  it('Should find parent node', () => {
    const orgDoc = `* Headline
- List item
- List item *2*`;

    const parsed = parse(orgDoc);
    const boldNode = findNextNode(parsed, (n) => {
      return n.is(NodeType.Bold);
    });

    const foundParent = findParent(boldNode, (n) => {
      return n.is(NodeType.Title);
    });

    expect(foundParent.toString()).toMatchInlineSnapshot(`
      "title [23-38]
        operator [23-25] ("- ")
        text [25-35] ("List item ")
        bold [35-38]
          operator [35-36] ("*")
          text [36-37] ("2")
          operator [37-38] ("*")
      "
    `);
  });

  it('Should not find parent node', () => {
    const orgDoc = `* Headline
- List item
- List item *2*`;

    const parsed = parse(orgDoc);
    const boldNode = findNextNode(parsed, (n) => {
      return n.is(NodeType.Bold);
    });

    const foundParent = findParent(boldNode, (n) => {
      return n.is(NodeType.SrcBlock);
    });

    expect(foundParent).toBeUndefined();
  });

  it('Should not find parent node cause of stop statement', () => {
    const orgDoc = `* Headline
- List item
- List item *2*`;

    const parsed = parse(orgDoc);
    const boldNode = findNextNode(parsed, (n) => {
      return n.is(NodeType.Bold);
    });

    const foundParent = findParent(boldNode, (n) => {
      if (n.is(NodeType.ListItem)) {
        return [false, true];
      }
      return n.is(NodeType.Headline);
    });

    expect(foundParent).toBeUndefined();
  });
});
