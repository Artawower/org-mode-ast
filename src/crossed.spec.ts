import { parse } from './parser';
import { NodeType } from './types';

import { removeInformationAboutParents } from './test.helper';

describe('Crossed tests', () => {
  it('Should parse crossed text with crossed tokens', () => {
    const orgData = `+Crossed text+`;
    const result = parse(orgData);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 14,
      children: [
        {
          type: NodeType.Crossed,
          start: 0,
          end: 14,
          children: [
            { type: NodeType.Operator, value: '+', start: 0, end: 1 },
            { type: NodeType.Text, value: 'Crossed text', start: 1, end: 13 },
            { type: NodeType.Operator, value: '+', start: 13, end: 14 },
          ],
        },
      ],
    });
  });

  it('Should parse crossed text inside headline', () => {
    const orgData = `* Hello +world+`;
    const result = parse(orgData);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 15,
      children: [
        {
          type: NodeType.Headline,
          level: 1,
          start: 0,
          end: 15,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: 'Hello ', start: 2, end: 8 },
            {
              type: NodeType.Crossed,
              start: 8,
              end: 15,
              children: [
                { type: NodeType.Operator, value: '+', start: 8, end: 9 },
                { type: NodeType.Text, value: 'world', start: 9, end: 14 },
                { type: NodeType.Operator, value: '+', start: 14, end: 15 },
              ],
            },
          ],
        },
      ],
    });
  });
});
