import { parse } from './parser';
import { NodeType, OrgData } from './types';

import { removeInformationAboutParents } from './test.helper';

describe('Bold test', () => {
  it('Should parse nested text formatters', () => {
    const orgData = '* Hello +*world*+';
    const result = parse(orgData);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 17,
      children: [
        {
          type: NodeType.Headline,
          level: 1,
          start: 0,
          end: 17,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: 'Hello ', start: 2, end: 8 },
            {
              type: NodeType.Crossed,
              start: 8,
              end: 17,
              children: [
                { type: NodeType.Operator, value: '+', start: 8, end: 9 },
                {
                  type: NodeType.Bold,
                  start: 9,
                  end: 16,
                  children: [
                    { type: NodeType.Operator, value: '*', start: 9, end: 10 },
                    { type: NodeType.Text, value: 'world', start: 10, end: 15 },
                    { type: NodeType.Operator, value: '*', start: 15, end: 16 },
                  ],
                },
                { type: NodeType.Operator, value: '+', start: 16, end: 17 },
              ],
            },
          ],
        },
      ],
    });
  });
});
