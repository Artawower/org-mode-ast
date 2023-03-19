import { OrgChildrenList } from './org-children-list.js';
import { OrgNode } from './org-node.js';
import { NodeType } from './types.js';

describe('Org children', () => {
  let children: OrgChildrenList;

  beforeEach(() => {
    children = new OrgChildrenList();
  });

  it('Should correct store children', () => {
    const firstOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
      start: 0,
      end: 10,
    });

    const secondOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'Second node',
      start: 10,
      end: 21,
    });

    children.push(firstOrgNode);
    children.push(secondOrgNode);
    expect(children.length).toBe(2);
    expect(children.get(0)?.toString()).toBe(firstOrgNode.toString());
    expect(children.get(1)?.toString()).toBe(secondOrgNode.toString());
  });

  it('Should correct implement spread operator', () => {
    const firstOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
      start: 0,
      end: 10,
    });

    const secondOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'Second node',
      start: 10,
      end: 21,
    });

    children.push(firstOrgNode);
    children.push(secondOrgNode);

    const array = [...children];
    expect(array[0].toString()).toBe(firstOrgNode.toString());
    expect(array[1].toString()).toBe(secondOrgNode.toString());
  });

  it('Should correct remove nodes', () => {
    const firstOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
    });

    const secondOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'Second node',
    });

    children.push(firstOrgNode);
    children.push(secondOrgNode);

    children.removeNodes([firstOrgNode]);
    expect(children.asArray()).toMatchInlineSnapshot(`
      [
        OrgNode {
          "_checked": undefined,
          "_level": undefined,
          "_ordered": undefined,
          "_properties": undefined,
          "_title": undefined,
          "end": 0,
          "safeCheckMode": false,
          "start": 0,
          "type": "text",
          "value": "Second node",
        },
      ]
    `);
  });

  it('Should correct extract nodes between two nodes', () => {
    const orgChildrenList = new OrgChildrenList();

    const firstOrgNode = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });
    const secondOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
    });

    const thirdOrgNode = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });

    orgChildrenList.push(firstOrgNode);
    orgChildrenList.push(secondOrgNode);
    orgChildrenList.push(thirdOrgNode);

    const extractedNodes = orgChildrenList.getNodesBetweenPairs(
      orgChildrenList.first,
      orgChildrenList.last
    );

    expect(extractedNodes.length).toBe(1);
    expect(extractedNodes.first.toString()).toMatchInlineSnapshot(`
      "text [0-0] ("First node")
      "
    `);
  });

  it('Should correct slice items', () => {
    const orgChildrenList = new OrgChildrenList();

    const firstOrgNode = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });
    const secondOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
    });

    const thirdOrgNode = new OrgNode({
      type: NodeType.Text,
      value: 'Third node',
    });

    const fourthNode = new OrgNode({
      type: NodeType.Operator,
      value: '*',
    });

    orgChildrenList.push(firstOrgNode);
    orgChildrenList.push(secondOrgNode);
    orgChildrenList.push(thirdOrgNode);
    orgChildrenList.push(fourthNode);

    const slicedNodes = orgChildrenList.slice(1, 3);

    expect(slicedNodes.length).toBe(2);
    expect(slicedNodes).toMatchInlineSnapshot(`
      OrgChildrenList {
        "header": OrgListChild {
          "next": OrgListChild {
            "prev": [Circular],
            "value": OrgNode {
              "_checked": undefined,
              "_level": undefined,
              "_ordered": undefined,
              "_properties": undefined,
              "_title": undefined,
              "end": 0,
              "safeCheckMode": false,
              "start": 0,
              "type": "text",
              "value": "Third node",
            },
          },
          "prev": null,
          "value": OrgNode {
            "_checked": undefined,
            "_level": undefined,
            "_ordered": undefined,
            "_properties": undefined,
            "_title": undefined,
            "end": 0,
            "safeCheckMode": false,
            "start": 0,
            "type": "text",
            "value": "First node",
          },
        },
        "length": 2,
        "tail": OrgListChild {
          "prev": OrgListChild {
            "next": [Circular],
            "prev": null,
            "value": OrgNode {
              "_checked": undefined,
              "_level": undefined,
              "_ordered": undefined,
              "_properties": undefined,
              "_title": undefined,
              "end": 0,
              "safeCheckMode": false,
              "start": 0,
              "type": "text",
              "value": "First node",
            },
          },
          "value": OrgNode {
            "_checked": undefined,
            "_level": undefined,
            "_ordered": undefined,
            "_properties": undefined,
            "_title": undefined,
            "end": 0,
            "safeCheckMode": false,
            "start": 0,
            "type": "text",
            "value": "Third node",
          },
        },
      }
    `);
  });

  it('Should remove node from children list after modification', () => {
    const d = new OrgNode({
      type: NodeType.List,
    });

    const e = new OrgNode({
      type: NodeType.Text,
      value: 'e',
    });

    const list = new OrgChildrenList(d, e);

    d.addChild(new OrgNode({ type: NodeType.Text, value: 'f' }));
    d.value = 'qewqwe';

    list.removeNodes([d]);

    expect(list.length).toBe(1);
    expect(Array.from(list)).toMatchInlineSnapshot(`
      [
        OrgNode {
          "_checked": undefined,
          "_level": undefined,
          "_ordered": undefined,
          "_properties": undefined,
          "_title": undefined,
          "end": 0,
          "safeCheckMode": false,
          "start": 0,
          "type": "text",
          "value": "e",
        },
      ]
    `);
  });

  it('Should return empty list with no sliced data', () => {
    const orgNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
    });
    const orgNode2 = new OrgNode({
      type: NodeType.Text,
      value: 'Second node',
    });

    const childrenList = new OrgChildrenList(orgNode1, orgNode2);
    expect(childrenList.slice(1, -1).length).toBe(0);
  });

  it('Should replace nodes inside org children list', () => {
    const orgNode1 = new OrgNode({
      type: NodeType.Text,
      value: 'First node',
    });

    const orgNode2 = new OrgNode({
      type: NodeType.Text,
      value: 'Second node',
    });

    const orgNode3 = new OrgNode({
      type: NodeType.Text,
      value: 'Third node',
    });

    const childrenList = new OrgChildrenList(orgNode1, orgNode2, orgNode3);

    childrenList.replaceNodes(
      [orgNode2],
      new OrgNode({ type: NodeType.Text, value: 'New node' })
    );

    expect(childrenList.length).toBe(3);
    expect(childrenList.toString()).toMatchInlineSnapshot(`
      "

      text [0-0] ("First node")

      text [0-0] ("New node")

      text [0-0] ("Third node")

      -----------------"
    `);
  });
});
