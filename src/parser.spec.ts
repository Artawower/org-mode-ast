import { parse } from './parser';
import { NodeType, OrgData } from './types';

export function removeInformationAboutParents(node: OrgData): void {
  delete node.parent;
  (node as any).children?.forEach((child) => {
    delete child.parent;
    removeInformationAboutParents(child);
  });
}

describe('Headline tests', () => {
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
      children: [{ type: NodeType.Text, value: ' * Hello world', start: 0, end: 14 }],
    });
  });

  // Bold here
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

  it('Should not parse text as bold with asterisk at the end', () => {
    const headline = 'Hello world*';
    const result = parse(headline);
    removeInformationAboutParents(result);
    // console.log(JSON.stringify(result, null, 2));
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

  fit('Should parse bold text from headline', () => {
    const orgData = '* Hello *world*';
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
});
