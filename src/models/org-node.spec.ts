import { OrgNode } from './org-node';
import { NodeType } from './types';
import { AstBuilder, parse } from '../parser/index';

describe('Org node', () => {
  const astBuilder = new AstBuilder({} as any, {} as any);

  it('Should correct add child, recalculate length', () => {
    const parentOrgNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const childNode = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    parentOrgNode.addChild(childNode);

    expect(parentOrgNode.toString()).toMatchInlineSnapshot(`
      "root [0-11]
        text [0-11] ("Hello world")
      "
    `);
  });

  it('Should correct add nested nodes and recalculate all parents positions', () => {
    const parentOrgNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const boldNode = new OrgNode({
      type: NodeType.Bold,
      start: 0,
      end: 0,
    });

    const thirdChildNode = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    parentOrgNode.addChild(boldNode);
    boldNode.addChild(thirdChildNode);
    expect(parentOrgNode.toString()).toMatchInlineSnapshot(`
      "root [0-11]
        bold [0-11]
          text [0-11] ("Hello world")
      "
    `);
  });

  it('Should correct link neighbor nodes', () => {
    const parentOrgNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const firstChildNode = new OrgNode({
      type: NodeType.Text,
      value: 'Hello',
      start: 0,
      end: 5,
    });

    const secondChildNode = new OrgNode({
      type: NodeType.Text,
      value: 'world',
      start: 6,
      end: 11,
    });

    parentOrgNode.addChild(firstChildNode);
    parentOrgNode.addChild(secondChildNode);

    expect(firstChildNode.next).toBe(secondChildNode);
    expect(secondChildNode.prev).toBe(firstChildNode);
    expect(firstChildNode.prev).toBeFalsy();
    expect(secondChildNode.next).toBeFalsy();
  });

  it('Should correct remove children from org node and recalculate range', () => {
    const parentOrgNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    const childNode2 = new OrgNode({
      type: NodeType.Text,
      value: 'Another text',
      start: 11,
      end: 23,
    });

    parentOrgNode.addChild(childNode1);
    parentOrgNode.addChild(childNode2);

    expect(parentOrgNode.toString()).toMatchInlineSnapshot(`
      "root [0-23]
        text [0-11] ("Hello world")
        text [11-23] ("Another text")
      "
    `);

    parentOrgNode.removeChildren([childNode1]);
    expect(parentOrgNode.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("Another text")
      "
    `);
  });

  it('Should correct recalculate range positions after add new child', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    rootNode.addChild(childNode1);

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(11);

    const childNode2 = new OrgNode({
      type: NodeType.Text,
      value: 'Another text',
      start: 11,
      end: 23,
    });

    rootNode.addChild(childNode2);

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(23);
  });

  it('Should correct recalculate range positions after add nested child', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    rootNode.addChild(childNode1);

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(11);

    const childNode2 = new OrgNode({
      type: NodeType.Bold,
      start: 11,
      end: 11,
    });

    rootNode.addChild(childNode2);

    const childNode3 = new OrgNode({
      type: NodeType.Text,
      value: 'Another text',
      start: 11,
      end: 23,
    });

    childNode2.addChild(childNode3);

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(23);
  });

  it('Should correct recalculate range positions after add nested child with reverse order', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const orgCrossed = new OrgNode({
      type: NodeType.Crossed,
    });

    const crossedText = new OrgNode({
      type: NodeType.Text,
      value: 'Crossed text',
    });

    orgCrossed.addChild(crossedText);
    rootNode.addChild(orgCrossed);

    const orgBold = new OrgNode({
      type: NodeType.Bold,
    });

    const orgText = new OrgNode({
      type: NodeType.Text,
      value: 'Another text',
    });

    orgBold.addChild(orgText);
    rootNode.addChild(orgBold);
    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(24);
  });

  it('Should correct recalculate length when append value', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 0,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
      start: 0,
      end: 11,
    });

    rootNode.addChild(childNode1);

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(11);

    childNode1.appendValue(' Another text');

    expect(rootNode.start).toBe(0);
    expect(rootNode.end).toBe(24);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-24]
        text [0-24] ("Hello world Another text")
      "
    `);
  });

  it('Should correct recalculate length when prepend value', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world',
    });

    rootNode.addChild(childNode1);

    childNode1.prependValue('Another text ');
    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-24]
        text [0-24] ("Another text Hello world")
      "
    `);
  });

  it('Should recalculate neighbors position after append a text', () => {
    const rootNode = new OrgNode({
      type: NodeType.Root,
    });

    const childNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world! ',
    });

    rootNode.addChild(childNode1);

    const childNode2 = new OrgNode({
      type: NodeType.Text,
      value: 'And i like it!',
    });

    rootNode.addChild(childNode2);

    childNode1.appendValue('Its so awesome!');

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-42]
        text [0-28] ("Hello world! Its so awesome!")
        text [28-42] ("And i like it!")
      "
    `);
  });

  it('Should recalculate neighbors when add new child', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const childNode1 = new OrgNode({ type: NodeType.Bold });

    const childNode1Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma bold text',
    });

    const childNode2 = new OrgNode({ type: NodeType.Crossed });

    const childNode2Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma crossed text',
    });

    rootNode.addChild(childNode1);
    rootNode.addChild(childNode2);
    childNode2.addChild(childNode2Text);

    childNode1.addChild(childNode1Text);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-31]
        bold [0-14]
          text [0-14] ("Amma bold text")
        crossed [14-31]
          text [14-31] ("Amma crossed text")
      "
    `);
  });

  it('Should recalculate neighbors and nested nodes after some above value changed', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const childNode1 = new OrgNode({ type: NodeType.Bold });

    const childNode1Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma bold text',
    });

    const childNode2 = new OrgNode({ type: NodeType.Crossed });

    const childNode2Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma crossed text',
    });

    rootNode.addChild(childNode1);
    rootNode.addChild(childNode2);
    childNode2.addChild(childNode2Text);

    childNode1.addChild(childNode1Text);

    childNode1Text.appendValue(' and more');

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-40]
        bold [0-23]
          text [0-23] ("Amma bold text and more")
        crossed [23-40]
          text [23-40] ("Amma crossed text")
      "
    `);
  });

  it('Should recalculate neighbors and nested nodes when above node prepend text', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const childNode1 = new OrgNode({ type: NodeType.Bold });

    const childNode1Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma bold text',
    });

    const childNode2 = new OrgNode({ type: NodeType.Crossed });

    const childNode2Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma crossed text',
    });

    rootNode.addChild(childNode1);
    rootNode.addChild(childNode2);
    childNode2.addChild(childNode2Text);

    childNode1.addChild(childNode1Text);

    childNode1Text.prependValue('More text ');

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-41]
        bold [0-24]
          text [0-24] ("More text Amma bold text")
        crossed [24-41]
          text [24-41] ("Amma crossed text")
      "
    `);
  });

  it('Should recalculate neighbors and nested nodes when above node set text', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const childNode1 = new OrgNode({ type: NodeType.Bold });

    const childNode1Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma bold text',
    });

    const childNode2 = new OrgNode({ type: NodeType.Crossed });

    const childNode2Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma crossed text',
    });

    rootNode.addChild(childNode1);
    rootNode.addChild(childNode2);
    childNode2.addChild(childNode2Text);

    childNode1.addChild(childNode1Text);

    childNode1Text.setValue('New text');

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-25]
        bold [0-8]
          text [0-8] ("New text")
        crossed [8-25]
          text [8-25] ("Amma crossed text")
      "
    `);
  });

  it('Should correct recalculate ranges when remove nested child', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const childNode1 = new OrgNode({ type: NodeType.Bold });

    const childNode1Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma bold text',
    });

    const childNode2 = new OrgNode({ type: NodeType.Crossed });

    const childNode2Text = new OrgNode({
      type: NodeType.Text,
      value: 'Amma crossed text',
    });

    rootNode.addChild(childNode1);
    rootNode.addChild(childNode2);

    childNode2.addChild(childNode2Text);
    childNode1.addChild(childNode1Text);

    childNode1.removeNode(childNode1Text);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        bold [0-0]
        crossed [0-17]
          text [0-17] ("Amma crossed text")
      "
    `);
  });

  it('Should correct add complex node', () => {
    const orgRoot = new OrgNode({ type: NodeType.Root });
    const orgBold = new OrgNode({ type: NodeType.Bold });
    const orgOperator = new OrgNode({ type: NodeType.Operator, value: '*' });
    const orgText = new OrgNode({ type: NodeType.Text, value: 'Bold text' });
    const orgOperator2 = new OrgNode({ type: NodeType.Operator, value: '*' });

    orgBold.addChild(orgOperator);
    orgBold.addChild(orgText);
    orgBold.addChild(orgOperator2);
    orgRoot.addChild(orgBold);

    expect(orgRoot.toString()).toMatchInlineSnapshot(`
      "root [0-11]
        bold [0-11]
          operator [0-1] ("*")
          text [1-10] ("Bold text")
          operator [10-11] ("*")
      "
    `);
  });

  it('Should recalculate positions when bulk add nested children', () => {
    const orgRoot = new OrgNode({ type: NodeType.Root });

    const orgBold = new OrgNode({ type: NodeType.Bold });
    const orgOperator = new OrgNode({ type: NodeType.Operator, value: '*' });
    const orgText = new OrgNode({ type: NodeType.Text, value: 'Bold text' });
    const orgOperator2 = new OrgNode({ type: NodeType.Operator, value: '*' });

    orgBold.addChildren([orgOperator, orgText, orgOperator2]);
    orgRoot.addChild(orgBold);

    expect(orgRoot.toString()).toMatchInlineSnapshot(`
      "root [0-11]
        bold [0-11]
          operator [0-1] ("*")
          text [1-10] ("Bold text")
          operator [10-11] ("*")
      "
    `);
  });

  it('Should correct work with remove multiple nested nodes above new one', () => {
    const orgRoot = new OrgNode({ type: NodeType.Root });

    const simpleText = new OrgNode({
      type: NodeType.Text,
      value: 'Simple text',
    });
    const orgBold = new OrgNode({ type: NodeType.Bold });
    const orgOperator = new OrgNode({ type: NodeType.Operator, value: '*' });
    const orgText = new OrgNode({ type: NodeType.Text, value: 'Bold text' });
    const orgOperator2 = new OrgNode({ type: NodeType.Operator, value: '*' });

    orgRoot.addChild(simpleText);
    orgRoot.addChild(orgOperator);
    orgRoot.addChild(orgText);
    orgRoot.addChild(orgOperator2);

    orgRoot.removeChildren([orgOperator, orgText, orgOperator2]);
    orgBold.addChild(orgOperator);
    orgBold.addChild(orgText);
    orgBold.addChild(orgOperator2);

    orgRoot.addChild(orgBold);

    expect(orgRoot.toString()).toMatchInlineSnapshot(`
      "root [0-22]
        text [0-11] ("Simple text")
        bold [11-22]
          operator [11-12] ("*")
          text [12-21] ("Bold text")
          operator [21-22] ("*")
      "
    `);
  });

  it('Shpuld correct recalculate subtree length when nested nodes are added/removed', () => {
    const orgRootNode = new OrgNode({ type: NodeType.Root });

    const italicOperator1 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });
    const italicText = new OrgNode({
      type: NodeType.Text,
      value: 'Italic and',
    });
    const italicOperator2 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });

    const boldOperator1 = new OrgNode({ type: NodeType.Operator, value: '*' });
    const boldText = new OrgNode({ type: NodeType.Text, value: 'Bold text' });
    const boldOperator2 = new OrgNode({ type: NodeType.Operator, value: '*' });

    orgRootNode.addChildren([
      italicOperator1,
      italicText,
      italicOperator2,
      boldOperator1,
      boldText,
      boldOperator2,
    ]);

    const orgBoldNode = new OrgNode({ type: NodeType.Bold });
    const orgItalicNode = new OrgNode({ type: NodeType.Italic });

    orgRootNode.removeChildren([italicOperator1, italicText, italicOperator2]);

    orgItalicNode.addChildren([italicOperator1, italicText, italicOperator2]);

    orgRootNode.addChild(orgItalicNode);

    orgRootNode.removeChildren([boldOperator1, boldText, boldOperator2]);

    orgBoldNode.addChildren([boldOperator1, boldText, boldOperator2]);
    orgRootNode.addChild(orgBoldNode);

    expect(orgRootNode.toString()).toMatchInlineSnapshot(`
      "root [0-23]
        italic [0-12]
          operator [0-1] ("/")
          text [1-11] ("Italic and")
          operator [11-12] ("/")
        bold [12-23]
          operator [12-13] ("*")
          text [13-22] ("Bold text")
          operator [22-23] ("*")
      "
    `);
  });

  it('Removing above record should recalculate nested neighbors positions', () => {
    const orgRootNode = new OrgNode({ type: NodeType.Root });

    const simplteText = new OrgNode({
      type: NodeType.Text,
      value: 'Simple text',
    });
    orgRootNode.addChild(simplteText);

    const italicOperator1 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });
    const italicText = new OrgNode({
      type: NodeType.Text,
      value: 'Italic and',
    });
    const italicOperator2 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });

    const orgItalicNode = new OrgNode({ type: NodeType.Italic });
    orgItalicNode.addChildren([italicOperator1, italicText, italicOperator2]);

    orgRootNode.addChildren([orgItalicNode]);
    orgRootNode.removeNode(simplteText);

    expect(orgRootNode.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        italic [0-12]
          operator [0-1] ("/")
          text [1-11] ("Italic and")
          operator [11-12] ("/")
      "
    `);
  });

  it('Should recalculate nested nodes positions with multiple add/remove operations', () => {
    const orgRootNode = new OrgNode({ type: NodeType.Root });

    const simplteText = new OrgNode({
      type: NodeType.Text,
      value: 'Simple text',
    });

    const italicOperator1 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });
    const crossedOperator1 = new OrgNode({
      type: NodeType.Operator,
      value: '+',
    });
    const nestedText = new OrgNode({
      type: NodeType.Text,
      value: 'Nested text',
    });
    const crossedOperator2 = new OrgNode({
      type: NodeType.Operator,
      value: '+',
    });
    const italicOperator2 = new OrgNode({
      type: NodeType.Operator,
      value: '/',
    });

    const simpleText2 = new OrgNode({
      type: NodeType.Text,
      value: 'Simple text 2',
    });

    orgRootNode.addChildren([
      simplteText,
      italicOperator1,
      crossedOperator1,
      nestedText,
      crossedOperator2,
      italicOperator2,
    ]);

    const crossedParentNode = new OrgNode({ type: NodeType.Crossed });
    orgRootNode.removeChildren([
      crossedOperator1,
      nestedText,
      crossedOperator2,
    ]);
    crossedParentNode.addChildren([
      crossedOperator1,
      nestedText,
      crossedOperator2,
    ]);
    orgRootNode.addChild(crossedParentNode);

    const italicParentNode = new OrgNode({ type: NodeType.Italic });
    orgRootNode.removeChildren([
      italicOperator1,
      crossedParentNode,
      italicOperator2,
    ]);
    italicParentNode.addChildren([
      italicOperator1,
      crossedParentNode,
      italicOperator2,
    ]);
    orgRootNode.addChild(italicParentNode);

    orgRootNode.addChildren([simpleText2]);
    expect(orgRootNode.toString()).toMatchInlineSnapshot(`
      "root [0-39]
        text [0-11] ("Simple text")
        italic [11-26]
          operator [11-12] ("/")
          crossed [12-25]
            operator [12-13] ("+")
            text [13-24] ("Nested text")
            operator [24-25] ("+")
          operator [25-26] ("/")
        text [26-39] ("Simple text 2")
      "
    `);
  });

  it('Should reset indexes for removed nested children', () => {
    const orgRootNode = new OrgNode({ type: NodeType.Root });

    const simplteTextNode = new OrgNode({
      type: NodeType.Text,
      value: 'simple text',
    });

    const orgLinkNode = new OrgNode({ type: NodeType.Link });

    const orgLinkUrlNode = new OrgNode({
      type: NodeType.LinkUrl,
      value: 'https://google.com',
    });

    const orgLinkTextNode = new OrgNode({
      type: NodeType.LinkName,
      value: 'Google',
    });

    const anotherSimpleTextNode = new OrgNode({
      type: NodeType.Text,
      value: 'something else',
    });

    const crossedOrgNode = new OrgNode({ type: NodeType.Crossed });

    orgLinkNode.addChildren([orgLinkUrlNode, orgLinkTextNode]);

    crossedOrgNode.addChildren([orgLinkNode, anotherSimpleTextNode]);

    orgRootNode.addChildren([simplteTextNode, crossedOrgNode]);

    expect(orgRootNode.toString()).toMatchInlineSnapshot(`
      "root [0-49]
        text [0-11] ("simple text")
        crossed [11-49]
          link [11-35]
            linkUrl [11-29] ("https://google.com")
            linkName [29-35] ("Google")
          text [35-49] ("something else")
      "
    `);

    orgRootNode.removeNode(crossedOrgNode);

    expect(crossedOrgNode.toString()).toMatchInlineSnapshot(`
      "crossed [0-38]
        link [0-24]
          linkUrl [0-18] ("https://google.com")
          linkName [18-24] ("Google")
        text [24-38] ("something else")
      "
    `);
  });

  it('Should correct recalculate length of tree after node with section were added', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const headerNode = new OrgNode({ type: NodeType.Headline });
    const sectionNode = new OrgNode({ type: NodeType.Section });
    const textNode = new OrgNode({
      type: NodeType.Text,
      value: 'header title',
    });
    const sectionContentText = new OrgNode({
      type: NodeType.Text,
      value: 'section content',
    });

    rootNode.addChildren([headerNode]);
    headerNode.addChild(textNode);
    headerNode.setSection(sectionNode);
    sectionNode.addChild(sectionContentText);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-27]
        headline [0-27]
          text [0-12] ("header title")
          section [12-27]
            text [12-27] ("section content")
      "
    `);
  });

  it('Should correct remove node with section', () => {
    const rootNode = new OrgNode({ type: NodeType.Root });
    const headerNode = new OrgNode({ type: NodeType.Headline });
    const sectionNode = new OrgNode({ type: NodeType.Section });
    const textNode = new OrgNode({
      type: NodeType.Text,
      value: 'header title',
    });
    const sectionContentText = new OrgNode({
      type: NodeType.Text,
      value: 'section content',
    });

    rootNode.addChildren([headerNode]);
    headerNode.addChild(textNode);
    headerNode.setSection(sectionNode);
    sectionNode.addChild(sectionContentText);

    rootNode.removeNode(headerNode);

    expect(rootNode.toString()).toMatchInlineSnapshot(`
      "root [0-0]
      "
    `);
  });

  it('Should return correct raw value from node', () => {
    const orgNode = new OrgNode({ type: NodeType.Text, value: 'Hello world!' });
    expect(orgNode.rawValue).toEqual('Hello world!');
  });

  it('Should return correct raw value from multiple nested nodes', () => {
    const orgRoot = new OrgNode({ type: NodeType.Root });
    const orgBold = new OrgNode({ type: NodeType.Bold });
    const orgBoldOperator = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });
    const orgText = new OrgNode({ type: NodeType.Text, value: 'Hello ' });
    const orgBold2Operator = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });
    const orgCrossed = new OrgNode({ type: NodeType.Crossed });
    const orgCrossedOperator = new OrgNode({
      type: NodeType.Operator,
      value: '+',
    });
    const orgCrossedText = new OrgNode({
      type: NodeType.Text,
      value: 'world!',
    });
    const orgCrossedOperator2 = new OrgNode({
      type: NodeType.Operator,
      value: '+',
    });

    orgRoot.addChild(orgBold);
    orgCrossed.addChildren([
      orgCrossedOperator,
      orgCrossedText,
      orgCrossedOperator2,
    ]);
    orgBold.addChildren([
      orgBoldOperator,
      orgText,
      orgCrossed,
      orgBold2Operator,
    ]);

    expect(orgRoot.rawValue).toEqual('*Hello +world!+*');
  });

  it('Should correct convert org node to JSON', () => {
    const orgRoot = new OrgNode({ type: NodeType.Root });
    const orgHeadline = new OrgNode({
      type: NodeType.Headline,
      title: new OrgNode({ type: NodeType.Title }),
    });

    orgRoot.addChild(orgHeadline);
    const orgHeadlineText = new OrgNode({
      type: NodeType.Text,
      value: 'Hello world!',
    });

    orgHeadline.title.addChild(orgHeadlineText);
    const section = new OrgNode({ type: NodeType.Section });
    orgHeadline.setSection(section);

    const secionText = new OrgNode({
      type: NodeType.Text,
      value: 'Section content',
    });

    section.addChild(secionText);

    expect(orgRoot.toJson()).toMatchInlineSnapshot(`
      {
        "children": [
          {
            "end": 15,
            "section": {
              "children": [
                {
                  "end": 27,
                  "start": 12,
                  "type": "text",
                  "value": "Section content",
                },
              ],
              "end": 27,
              "start": 12,
              "type": "section",
            },
            "start": 0,
            "title": {
              "children": [
                {
                  "end": 12,
                  "start": 0,
                  "type": "text",
                  "value": "Hello world!",
                },
              ],
              "end": 12,
              "start": 0,
              "type": "title",
            },
            "type": "headline",
          },
        ],
        "end": 15,
        "start": 0,
        "type": "root",
      }
    `);
  });

  it('Should convert to raw value from complex note', () => {
    const orgDoc = `:PROPERTIES:
:ID: kitty-auto-dark-mode
:END:

#+TITLE: Mac OS. Kitty auto dark mode.
#+DESCRIPTION: Easy way to configure automatic dark mode for kitty + macos.
#+FILETAGS: :kitty:terminal:macos:osx:trick:
#+ID: kitty-auto-dark-mode

* Connected Links
- [[https://www.reddit.com/r/KittyTerminal/comments/vthmcc/set_kitty_theme_based_on_macos_theme_dark_or_light/][Inspired by this post (reddit)]] 
* Problem.
There is no native way to automatically synchronize Kitty terminal theme with the macOS system theme.`;

    const parsed = parse(orgDoc);
    expect(parsed.rawValue).toMatchInlineSnapshot(`
      ":PROPERTIES:
      :ID: kitty-auto-dark-mode
      :END:

      #+TITLE: Mac OS. Kitty auto dark mode.
      #+DESCRIPTION: Easy way to configure automatic dark mode for kitty + macos.
      #+FILETAGS: :kitty:terminal:macos:osx:trick:
      #+ID: kitty-auto-dark-mode

      * Connected Links
      - [[https://www.reddit.com/r/KittyTerminal/comments/vthmcc/set_kitty_theme_based_on_macos_theme_dark_or_light/][Inspired by this post (reddit)]] 
      * Problem.
      There is no native way to automatically synchronize Kitty terminal theme with the macOS system theme."
    `);
  });

  it('Should correctly stringify text from checkbox node', () => {
    const doc = `- [ ] checkbox
- [x] checked checkbox`;

    const parsed = parse(doc);

    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        list [0-37]
            :unordered:
            :level 0:
          listItem [0-15]
            title [0-15]
              operator [0-2] ("- ")
              checkbox [2-5] ("[ ]")
                  :unchecked:
                operator [2-3] ("[")
                text [3-4] (" ")
                operator [4-5] ("]")
              text [5-14] (" checkbox")
              newLine [14-15]
          listItem [15-37]
            title [15-37]
              operator [15-17] ("- ")
              checkbox [17-20] ("[x]")
                  :checked:
                operator [17-18] ("[")
                text [18-19] ("x")
                operator [19-20] ("]")
              text [20-37] (" checked checkbox")
      "
    `);
  });

  it('Should correctly get raw value from checkbox node', () => {
    const doc = `- [ ] checkbox
- [x] checked checkbox`;

    const parsed = parse(doc);
    expect(parsed.rawValue).toMatchInlineSnapshot(`
      "- [ ] checkbox
      - [x] checked checkbox"
    `);
  });

  it('Should return raw value from latex export block', () => {
    const doc = `#+BEGIN_EXPORT latex
\LaTeX
#+END_EXPORT`;

    const parsed = parse(doc);
    console.log('✎: [line 1048][org-node.spec.ts] parsed: ', parsed);
    expect(
      parsed.children.get(0).children.get(2).rawValue
    ).toMatchInlineSnapshot(`"LaTeX"`);
  });
});
