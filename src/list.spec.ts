import { parse } from './parser';

import { removeInformationAboutParents } from './test.helper';

describe('List tests', () => {
  it('Should parse simple list', () => {
    const orgData = `- Item 1
- Item 2
- Item 3`;

    const result = parse(orgData);
    removeInformationAboutParents(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 26,
      children: [
        {
          type: 'list',
          start: 0,
          end: 26,
          ordered: false,
          children: [
            {
              type: 'listItem',
              start: 0,
              end: 9,
              children: [
                {
                  type: 'operator',
                  start: 0,
                  end: 2,
                  value: '- ',
                },
                {
                  type: 'text',
                  start: 2,
                  end: 9,
                  value: 'Item 1\n',
                },
              ],
            },
            {
              type: 'listItem',
              start: 9,
              end: 18,
              children: [
                {
                  type: 'operator',
                  start: 9,
                  end: 11,
                  value: '- ',
                },
                {
                  type: 'text',
                  start: 11,
                  end: 18,
                  value: 'Item 2\n',
                },
              ],
            },
            {
              type: 'listItem',
              start: 18,
              end: 26,
              children: [
                {
                  type: 'operator',
                  start: 18,
                  end: 20,
                  value: '- ',
                },
                {
                  type: 'text',
                  start: 20,
                  end: 26,
                  value: 'Item 3',
                },
              ],
            },
          ],
        },
      ],
    });
  });

  it('Should parse list with nested nodes', () => {
    const orgText = `- *Item 1*
- +Item 2+
- /Item 3/`;

    const result = parse(orgText);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 32,
      children: [
        {
          type: 'list',
          start: 0,
          end: 32,
          ordered: false,
          children: [
            {
              type: 'listItem',
              start: 0,
              end: 11,
              children: [
                {
                  type: 'operator',
                  start: 0,
                  end: 2,
                  value: '- ',
                },
                {
                  type: 'bold',
                  start: 2,
                  end: 10,
                  children: [
                    {
                      type: 'operator',
                      start: 2,
                      end: 3,
                      value: '*',
                    },
                    {
                      type: 'text',
                      start: 3,
                      end: 9,
                      value: 'Item 1',
                    },
                    {
                      type: 'operator',
                      start: 9,
                      end: 10,
                      value: '*',
                    },
                  ],
                },
                {
                  type: 'text',
                  start: 10,
                  end: 11,
                  value: '\n',
                },
              ],
            },
            {
              type: 'listItem',
              start: 11,
              end: 22,
              children: [
                {
                  type: 'operator',
                  start: 11,
                  end: 13,
                  value: '- ',
                },
                {
                  type: 'crossed',
                  start: 13,
                  end: 21,
                  children: [
                    {
                      type: 'operator',
                      start: 13,
                      end: 14,
                      value: '+',
                    },
                    {
                      type: 'text',
                      start: 14,
                      end: 20,
                      value: 'Item 2',
                    },
                    {
                      type: 'operator',
                      start: 20,
                      end: 21,
                      value: '+',
                    },
                  ],
                },
                {
                  type: 'text',
                  start: 21,
                  end: 22,
                  value: '\n',
                },
              ],
            },
            {
              type: 'listItem',
              start: 22,
              end: 32,
              children: [
                {
                  type: 'operator',
                  start: 22,
                  end: 24,
                  value: '- ',
                },
                {
                  type: 'italic',
                  start: 24,
                  end: 32,
                  children: [
                    {
                      type: 'operator',
                      start: 24,
                      end: 25,
                      value: '/',
                    },
                    {
                      type: 'text',
                      start: 25,
                      end: 31,
                      value: 'Item 3',
                    },
                    {
                      type: 'operator',
                      start: 31,
                      end: 32,
                      value: '/',
                    },
                  ],
                },
              ],
            },
          ],
        },
      ],
    });
  });

  xit('Should parse list with section', () => {
    const orgText = `- Item 1
 I'am subgroup with *bold* text
- Item 2`;
    const result = parse(orgText);
    removeInformationAboutParents(result);
    console.log(JSON.stringify(result, null, 2));
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 49,
      children: [
        {
          type: 'list',
          start: 0,
          end: 49,
          ordered: false,
          children: [
            {
              type: 'listItem',
              start: 0,
              end: 9,
              children: [
                {
                  type: 'operator',
                  start: 0,
                  end: 2,
                  value: '- ',
                },
                {
                  type: 'text',
                  start: 2,
                  end: 9,
                  value: 'Item 1\n',
                },
              ],
            },
            {
              type: 'section',
              start: 9,
              end: 41,
              children: [
                {
                  type: 'text',
                  start: 9,
                  end: 29,
                  value: " I'am subgroup with ",
                },
                {
                  type: 'bold',
                  start: 29,
                  end: 35,
                  children: [
                    {
                      type: 'operator',
                      start: 29,
                      end: 30,
                      value: '*',
                    },
                    {
                      type: 'text',
                      start: 30,
                      end: 34,
                      value: 'bold',
                    },
                    {
                      type: 'operator',
                      start: 34,
                      end: 35,
                      value: '*',
                    },
                  ],
                },
                {
                  type: 'text',
                  start: 35,
                  end: 40,
                  value: ' text',
                },
              ],
            },
            {
              type: 'listItem',
              start: 41,
              end: 49,
              children: [
                {
                  type: 'operator',
                  start: 41,
                  end: 43,
                  value: '- ',
                },
                {
                  type: 'text',
                  start: 43,
                  end: 49,
                  value: 'Item 2',
                },
              ],
            },
          ],
        },
      ],
    });
  });
});
