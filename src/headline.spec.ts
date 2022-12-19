import { parse } from './parser';
import { NodeType } from './types';
import { removeInformationAboutParents } from './test.helper';

describe('Headline tests', () => {
  // Headline tests start
  it('should parse first level headline', () => {
    const headline = '* Hello world';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 13,
      children: [
        {
          type: 'headline',
          level: 1,
          start: 0,
          end: 13,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: 'Hello world', start: 2, end: 13 },
          ],
        },
      ],
    });
  });

  it('Should parse headline with long start space', () => {
    const headline = '*        Hello world';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 20,
      children: [
        {
          type: 'headline',
          level: 1,
          start: 0,
          end: 20,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: '       Hello world', start: 2, end: 20 },
          ],
        },
      ],
    });
  });

  it('Should not parse text with start space and asterisk as headline', () => {
    const headline = ' * Hello world';
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 14,
      children: [
        { type: NodeType.Indent, value: ' ', start: 0, end: 1 },
        { type: NodeType.Text, value: '* Hello world', start: 1, end: 14 },
      ],
    });
  });

  it('Should parse nested section for headline', () => {
    const orgData = `* Title
some text`;
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
          end: 8,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            { type: NodeType.Text, value: 'Title\n', start: 2, end: 8 },
          ],
          section: {
            type: NodeType.Section,
            start: 8,
            end: 17,
            children: [{ type: NodeType.Text, value: 'some text', start: 8, end: 17 }],
          },
        },
      ],
    });
  });

  it('Should parse nested headlines', () => {
    const headline = `* Hello world
** Hello world 2
*** Headline level 3`;
    const result = parse(headline);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 51,
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
            end: 51,
            children: [
              {
                type: NodeType.Headline,
                level: 2,
                start: 14,
                end: 31,
                children: [
                  { type: NodeType.Operator, value: '** ', start: 14, end: 17 },
                  { type: NodeType.Text, value: 'Hello world 2\n', start: 17, end: 31 },
                ],
                section: {
                  type: NodeType.Section,
                  start: 31,
                  end: 51,
                  children: [
                    {
                      type: NodeType.Headline,
                      level: 3,
                      start: 31,
                      end: 51,
                      children: [
                        { type: NodeType.Operator, value: '*** ', start: 31, end: 35 },
                        { type: NodeType.Text, value: 'Headline level 3', start: 35, end: 51 },
                      ],
                    },
                  ],
                },
              },
            ],
          },
        },
      ],
    });
  });
});
