import { parse } from './parser';
import { NodeType, OrgData } from './types';

import { removeInformationAboutParents } from './test.helper';

describe('Bold test', () => {
  it('Should not parse text as bold with single asterisk', () => {
    const headline = 'Hello *world';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 12,
      children: [{ type: NodeType.Text, value: 'Hello *world', start: 0, end: 12 }],
    });
  });

  it('Should not parse bold text started from single asterisk', () => {
    const orgText = '*Not a bold text';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));

    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 16,
      children: [
        {
          type: NodeType.Text,
          value: '*Not a bold text',
          start: 0,
          end: 16,
        },
      ],
    });
  });

  it('Should not parse text as bold with asterisk at the end', () => {
    const headline = 'Hello world*';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 12,
      children: [{ type: NodeType.Text, value: 'Hello world*', start: 0, end: 12 }],
    });
  });

  it('Should not parse text as bold with another bracket symbols', () => {
    const headline = 'Hello *+[world';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 14,
      children: [{ type: NodeType.Text, value: 'Hello *+[world', start: 0, end: 14 }],
    });
  });

  it('should parse bold text', () => {
    const orgData = '*Hello world*';
    const result = parse(orgData);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 13,
      children: [
        {
          type: 'bold',
          start: 0,
          end: 13,
          children: [
            { type: NodeType.Operator, value: '*', start: 0, end: 1 },
            { type: NodeType.Text, value: 'Hello world', start: 1, end: 12 },
            { type: NodeType.Operator, value: '*', start: 12, end: 13 },
          ],
        },
      ],
    });
  });

  it('Should parse bold text with intersection of other pair tokens', () => {
    const orgData = '*Hello +world*';
    const result = parse(orgData);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 14,
      children: [
        {
          type: 'bold',
          start: 0,
          end: 14,
          children: [
            { type: NodeType.Operator, value: '*', start: 0, end: 1 },
            { type: NodeType.Text, value: 'Hello +world', start: 1, end: 13 },
            { type: NodeType.Operator, value: '*', start: 13, end: 14 },
          ],
        },
      ],
    });
  });

  it('Should parse bold text from headline', () => {
    const orgData = '* Hello *world*';
    const result = parse(orgData);
    removeInformationAboutParents(result);

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
              type: NodeType.Bold,
              start: 8,
              end: 15,
              children: [
                { type: NodeType.Operator, value: '*', start: 8, end: 9 },
                { type: NodeType.Text, value: 'world', start: 9, end: 14 },
                { type: NodeType.Operator, value: '*', start: 14, end: 15 },
              ],
            },
          ],
        },
      ],
    });
  });

  it('Should parse bold text inside nested headline', () => {
    const orgData = `* Hello world
** Hello *world*`;
    const result = parse(orgData);
    removeInformationAboutParents(result);

    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 30,
      children: [
        {
          type: NodeType.Headline,
          level: 1,
          start: 0,
          end: 14,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: 'Hello world\n', start: 2, end: 14 },
          ],
          section: {
            type: NodeType.Section,
            start: 14,
            end: 30,
            children: [
              {
                type: NodeType.Headline,
                level: 2,
                start: 14,
                end: 30,
                children: [
                  { type: NodeType.Operator, value: '** ', start: 14, end: 17 },
                  { type: NodeType.Text, value: 'Hello ', start: 17, end: 23 },
                  {
                    type: NodeType.Bold,
                    start: 23,
                    end: 30,
                    children: [
                      { type: NodeType.Operator, value: '*', start: 23, end: 24 },
                      { type: NodeType.Text, value: 'world', start: 24, end: 29 },
                      { type: NodeType.Operator, value: '*', start: 29, end: 30 },
                    ],
                  },
                ],
              },
            ],
          },
        },
      ],
    });
  });

  it('Should parse bold with that started from brackets symbols', () => {
    const orgData = `* Hello +[*world*`;
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
            { type: NodeType.Text, value: 'Hello +[', start: 2, end: 10 },
            {
              type: NodeType.Bold,
              start: 10,
              end: 17,
              children: [
                { type: NodeType.Operator, value: '*', start: 10, end: 11 },
                { type: NodeType.Text, value: 'world', start: 11, end: 16 },
                { type: NodeType.Operator, value: '*', start: 16, end: 17 },
              ],
            },
          ],
        },
      ],
    });
  });
});
