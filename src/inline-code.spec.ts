import { parse } from './parser';
import { NodeType } from './types';

import { removeInformationAboutRelatives } from './test.helper';

describe('InlineCode', () => {
  it('Should parse inline code', () => {
    const orgText = '=console.log(123)=';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
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

  it('Should not parse incline code that started from equal operator', () => {
    const orgText = '=console.log(123)';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 17,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 17,
          value: '=console.log(123)',
        },
      ],
    });
  });

  it('Should not parse incline code that contain equal operator at the middle', () => {
    const orgText = 'a = 12';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 6,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 6,
          value: 'a = 12',
        },
      ],
    });
  });

  it('Should not parse incline code that contain equal operator at the end', () => {
    const orgText = 'a =';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 3,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 3,
          value: 'a =',
        },
      ],
    });
  });

  it('Should not parse nested formatters inside inline code', () => {
    const orgText = '=*console.log(123)*=';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
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
              start: 0,
              end: 1,
              value: '=',
            },
            {
              type: NodeType.Text,
              start: 1,
              end: 19,
              value: '*console.log(123)*',
            },
            {
              type: NodeType.Operator,
              start: 19,
              end: 20,
              value: '=',
            },
          ],
        },
      ],
    });
  });

  it('Should not interpret equal operator as inline code inside nesting formatters', () => {
    const orgText = '*=not inline code=*';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(JSON.stringify(result, null, 2));

    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 19,
      children: [
        {
          type: NodeType.Bold,
          start: 0,
          end: 19,
          children: [
            {
              type: NodeType.Operator,
              start: 0,
              end: 1,
              value: '*',
            },
            {
              type: NodeType.Text,
              start: 1,
              end: 18,
              value: '=not inline code=',
            },
            {
              type: NodeType.Operator,
              start: 18,
              end: 19,
              value: '*',
            },
          ],
        },
      ],
    });
  });

  it('Should parse inline code inside headline', () => {
    const orgText = '* =console.log(123)=';
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 20,
      children: [
        {
          type: NodeType.Headline,
          start: 0,
          end: 20,
          level: 1,
          children: [
            {
              type: NodeType.Operator,
              start: 0,
              end: 2,
              value: '* ',
            },
            {
              type: NodeType.InlineCode,
              start: 2,
              end: 20,
              children: [
                {
                  type: NodeType.Operator,
                  start: 2,
                  end: 3,
                  value: '=',
                },
                {
                  type: NodeType.Text,
                  start: 3,
                  end: 19,
                  value: 'console.log(123)',
                },
                {
                  type: NodeType.Operator,
                  start: 19,
                  end: 20,
                  value: '=',
                },
              ],
            },
          ],
        },
      ],
    });
  });
});
