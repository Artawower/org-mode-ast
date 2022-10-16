import { parse } from './parser';

import { removeInformationAboutParents } from './test.helper';

describe('List tests', () => {
  fit('Should parse simple list', () => {
    const orgData = `- Item 1
- Item 2
- Item 3`;

    const result = parse(orgData);
    removeInformationAboutParents(result);
    console.log('🦄: [line 13][list.spec.ts] [35mresult: ', JSON.stringify(result, null, 2));
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
});
