import { parse } from './parser';
import { removeInformationAboutParents } from './test.helper';
import { NodeType } from './types';

describe('Italic', () => {
  it('should be italic', () => {
    const orgText = '/This is italic/';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 16,
      children: [
        {
          type: 'italic',
          start: 0,
          end: 16,
          children: [
            {
              type: 'operator',
              start: 0,
              end: 1,
              value: '/',
            },
            {
              type: 'text',
              start: 1,
              end: 15,
              value: 'This is italic',
            },
            {
              type: 'operator',
              start: 15,
              end: 16,
              value: '/',
            },
          ],
        },
      ],
    });
  });

  it('Should not parse italic text with single slash', () => {
    const orgText = 'This is /italic';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));

    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 15,
      children: [
        {
          type: 'text',
          start: 0,
          end: 15,
          value: 'This is /italic',
        },
      ],
    });
  });

  it('Should not parse italic when sentence start from the single slash', () => {
    const orgText = '/Not a italic text!';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 19,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 19,
          value: '/Not a italic text!',
        },
      ],
    });
  });

  it('Should not parse italic text with multiple opened brackets', () => {
    const orgText = 'This is +[/*simple not italic text';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 34,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 34,
          value: 'This is +[/*simple not italic text',
        },
      ],
    });
  });

  it('Should not parse italic text when sentence ends with slash', () => {
    const orgText = 'This is italic/';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 15,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 15,
          value: 'This is italic/',
        },
      ],
    });
  });

  xit('Should parse italic text with nested formatting', () => {
    const orgText = 'This is */italic with bold/* text';
    const result = parse(orgText);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: NodeType.Root,
      start: 0,
      end: 33,
      children: [
        {
          type: NodeType.Text,
          start: 0,
          end: 8,
          value: 'This is ',
        },
        {
          type: NodeType.Bold,
          start: 8,
          end: 28,
          children: [
            {
              type: NodeType.Operator,
              start: 8,
              end: 9,
              value: '*',
            },
            {
              type: NodeType.Italic,
              start: 9,
              end: 27,
              children: [
                {
                  type: NodeType.Operator,
                  start: 9,
                  end: 10,
                  value: '/',
                },
                {
                  type: NodeType.Text,
                  start: 10,
                  end: 26,
                  value: 'italic with bold',
                },
                {
                  type: NodeType.Operator,
                  start: 26,
                  end: 27,
                  value: '/',
                },
              ],
            },
            {
              type: NodeType.Operator,
              start: 27,
              end: 28,
              value: '*',
            },
          ],
        },
        {
          type: NodeType.Text,
          start: 28,
          end: 33,
          value: ' text',
        },
      ],
    });
  });
});
