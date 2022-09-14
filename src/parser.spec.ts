import { parse } from './parser';
import { NodeType, OrgData } from './types';

function removeInformationAboutParents(node: OrgData): void {
  delete node.parent;
  (node as any).children?.forEach((child) => {
    delete child.parent;
    removeInformationAboutParents(child);
  });
}

fdescribe('Headline tests', () => {
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

  // it('should parse bold text', () => {
  //   const headline = '*hello world*';
  //   const result = parse(headline);
  //   expect(result).toEqual({
  //     type: 'root',
  //     start: 0,
  //     end: 13,
  //     children: [
  //       {
  //         type: 'bold',
  //         start: 0,
  //         end: 13,
  //         children: [
  //           { type: NodeType.Operator, value: '*', start: 0, end: 1 },
  //           { type: NodeType.Text, value: 'Hello world', start: 1, end: 12 },
  //           { type: NodeType.Operator, value: '*', start: 12, end: 13 },
  //         ],
  //       },
  //     ],
  //   });
  // });
});
