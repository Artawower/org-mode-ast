import { parse } from './parser';
import { NodeType, OrgData } from './types';

import { removeInformationAboutRelatives } from './test.helper';

describe('Checkbox tests', () => {
  it('Should parse checkboxed headline', () => {
    const orgData = `* [ ] Hello world`;
    const result = parse(orgData);
    removeInformationAboutRelatives(result);
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
          checked: false,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            {
              type: NodeType.Checkbox,
              value: '[ ]',
              start: 2,
              end: 17,
              checked: false,
              children: [{ type: NodeType.Text, value: ' Hello world', start: 5, end: 17 }],
            },
          ],
        },
      ],
    });
  });

  it('Should parse checked checkboxed headline', () => {
    const orgData = `* [X] Hello world`;
    const result = parse(orgData);
    removeInformationAboutRelatives(result);
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
          checked: true,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            {
              type: NodeType.Checkbox,
              value: '[X]',
              start: 2,
              end: 17,
              checked: true,
              children: [{ type: NodeType.Text, value: ' Hello world', start: 5, end: 17 }],
            },
          ],
        },
      ],
    });
  });

  it('Should parse checked checkboxed headline with nested bold text', () => {
    const orgData = `* [X] Hello *world*`;
    const result = parse(orgData);
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 19,
      children: [
        {
          type: NodeType.Headline,
          level: 1,
          start: 0,
          end: 19,
          checked: true,
          children: [
            { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
            {
              type: NodeType.Checkbox,
              value: '[X]',
              start: 2,
              end: 19,
              checked: true,
              children: [
                { type: NodeType.Text, value: ' Hello ', start: 5, end: 12 },
                {
                  type: NodeType.Bold,
                  start: 12,
                  end: 19,
                  children: [
                    { type: NodeType.Operator, value: '*', start: 12, end: 13 },
                    { type: NodeType.Text, value: 'world', start: 13, end: 18 },
                    { type: NodeType.Operator, value: '*', start: 18, end: 19 },
                  ],
                },
              ],
            },
          ],
        },
      ],
    });
  });
});
