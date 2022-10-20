import { parse } from './parser';
import { NodeType } from './types';

import { removeInformationAboutParents } from './test.helper';

describe('InlineCode', () => {
  it('Should parse inline code', () => {
    const orgText = '=console.log(123)=';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 18,
      children: [
        {
          type: NodeType.InlineCode,
          start: 0,
          end: 18,
          children: [
            {
              type: NodeType.Operator,
              start: 0,
              end: 1,
              value: '=',
            },
            {
              type: NodeType.Text,
              start: 1,
              end: 17,
              value: 'console.log(123)',
            },
            {
              type: NodeType.Operator,
              start: 17,
              end: 18,
              value: '=',
            },
          ],
        },
      ],
    });
  });

  xit('Should not parse nested formatters inside inline code', () => {
    const orgText = '=console.log(123)=';
  });
});
