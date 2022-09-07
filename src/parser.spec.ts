import { parse } from './parser';
import { NodeType } from './types';

describe('Headline tests', () => {
  it('should parse first level headline', () => {
    const headline = '* Hello world';
    const result = parse(headline);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 13,
      children: [
        { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
        {
          type: 'headline',
          level: 1,
          start: 2,
          end: 13,
          children: [{ type: NodeType.Text, value: 'Hello world', start: 2, end: 13 }],
        },
      ],
    });
  });
});
