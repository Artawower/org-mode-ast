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
});
