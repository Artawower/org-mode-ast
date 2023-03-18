import { AstBuilder } from './ast-builder';
import { AstContext } from './ast-context';
import { NodeType, OrgChildrenList, OrgNode } from '../models';

describe('AST builder tests', () => {
  let builder: AstBuilder;

  beforeEach(() => {
    const ctx = new AstContext();
    builder = new AstBuilder(ctx, {} as any);
  });

  it('Should correct collect nested value from ast tree', () => {
    const nestedOrgData = new OrgNode({
      type: NodeType.Root,
      start: 0,
      end: 20,
      children: [
        {
          type: NodeType.InlineCode,
          start: 0,
          end: 20,
          children: [
            {
              type: NodeType.Operator,
              value: '=',
              start: 0,
              end: 1,
            },
            {
              type: NodeType.Bold,
              start: 1,
              end: 19,
              children: [
                {
                  type: NodeType.Operator,
                  value: '*',
                  start: 1,
                  end: 2,
                },
                {
                  type: NodeType.Text,
                  value: 'console.log(123)',
                  start: 2,
                  end: 18,
                },
                {
                  type: NodeType.Operator,
                  value: '*',
                  start: 18,
                  end: 19,
                },
              ],
            },
            {
              type: NodeType.Operator,
              value: '=',
              start: 19,
              end: 20,
            },
          ],
        },
      ],
    });

    const rawValue = builder.getRawValueFromNodes(nestedOrgData);
    expect(rawValue).toBe('=*console.log(123)*=');
  });

  it('Should correct determine list operator', () => {
    expect(builder.isListOperator('- ')).toBe(true);
    expect(builder.isListOperator('+ ')).toBe(true);
    expect(builder.isListOperator('1) ')).toBe(true);
    expect(builder.isListOperator('100500) ')).toBe(true);
  });

  it('Should not correct determine list operator', () => {
    expect(builder.isListOperator(' -')).toBe(false);
    expect(builder.isListOperator('-')).toBe(false);
    expect(builder.isListOperator(' +')).toBe(false);
    expect(builder.isListOperator('+')).toBe(false);
    expect(builder.isListOperator('0) ')).toBe(false);
    expect(builder.isListOperator('100500 ')).toBe(false);
    expect(builder.isListOperator(') ')).toBe(false);
    expect(builder.isListOperator(')')).toBe(false);
  });

  it('Should correct determine empty checkbox operator', () => {
    const checkboxList = new OrgChildrenList();
    checkboxList.append(
      new OrgNode({
        type: NodeType.Text,
        value: '[',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: ' ',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: ']',
      })
    );
    expect(builder.isNodesCheckbox(checkboxList)).toBe(true);

    checkboxList.clear();

    checkboxList.append(
      new OrgNode({
        type: NodeType.Text,
        value: '[',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: 'x',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: ']',
      })
    );

    expect(builder.isNodesCheckbox(checkboxList)).toBe(true);

    checkboxList.clear();
    checkboxList.append(
      new OrgNode({
        type: NodeType.Text,
        value: '[',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: 'X',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: ']',
      })
    );
    expect(builder.isNodesCheckbox(checkboxList)).toBe(true);

    checkboxList.clear();
    checkboxList.append(
      new OrgNode({
        type: NodeType.Text,
        value: '[',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: '-',
      }),
      new OrgNode({
        type: NodeType.Text,
        value: ']',
      })
    );

    expect(builder.isNodesCheckbox(checkboxList)).toBe(true);
  });

  it('Should not determine empty checkbox operator', () => {
    expect(
      builder.isNodesCheckbox([
        {
          type: NodeType.Text,
          value: '[',
        },
        {
          type: NodeType.Text,
          value: 'x',
        },
        {
          type: NodeType.Text,
          value: ' ',
        },
        {
          type: NodeType.Text,
          value: ']',
        },
      ] as any)
    ).toBe(false);

    expect(
      builder.isNodesCheckbox([
        {
          type: NodeType.Text,
          value: '[',
        },
        {
          type: NodeType.Text,
          value: '  ',
        },
        {
          type: NodeType.Text,
          value: ']',
        },
      ] as any)
    ).toBe(false);

    expect(
      builder.isNodesCheckbox([
        {
          type: NodeType.Text,
          value: '[',
        },
        {
          type: NodeType.Text,
          value: '  ',
        },
        {
          type: NodeType.Text,
          value: '] ',
        },
      ] as any)
    ).toBe(false);

    expect(
      builder.isNodesCheckbox([
        {
          type: NodeType.Text,
          value: ' ',
        },
        {
          type: NodeType.Text,
          value: '[',
        },
        {
          type: NodeType.Text,
          value: 'X',
        },
        {
          type: NodeType.Text,
          value: ']',
        },
      ] as any)
    ).toBe(false);
  });

  it('Should merge neighbors nodes with same types', () => {
    const firstNode = new OrgNode({
      type: NodeType.Text,
      value: 'Hello',
    });
    const secondNode = new OrgNode({
      type: NodeType.Unresolved,
      value: ' World',
    });

    const thirdNode = new OrgNode({
      type: NodeType.Text,
      value: '!',
    });

    const orgNodeRoot = new OrgNode({
      type: NodeType.Root,
    });

    orgNodeRoot.addChildren([firstNode, secondNode, thirdNode]);

    builder.mergeNeighborsNodesWithSameType(firstNode);

    expect(orgNodeRoot.children?.first?.toString()).toMatchInlineSnapshot(`
      "text [0-12] ("Hello World!")
      "
    `);

    expect(firstNode.parent).toBeTruthy();
  });
});
