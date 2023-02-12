import { OrgChildrenList } from './org-children-list';
import { OrgNode } from './org-node';
import { NodeType } from './types';

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
    expect(children.get(0).toString()).toBe(firstOrgNode.toString());
    expect(children.get(1).toString()).toBe(secondOrgNode.toString());
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
    console.log('✎: [line 53][org-children-list.spec.ts] array: ', array);
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
});
