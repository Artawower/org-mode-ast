import { parse } from './parser';

import { removeInformationAboutRelatives } from './test.helper';
import { prettyTreePrint } from './tools';

fdescribe('List tests', () => {
  it('Should parse simple list', () => {
    const orgData = `- Item 1
- Item 2
- Item 3`;

    const result = parse(orgData);
    // console.log(prettyTreePrint(result));

    removeInformationAboutRelatives(result);

    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 26,
      children: [
        {
          type: 'list',
          start: 0,
          end: 26,
          level: 0,
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
                  end: 8,
                  value: 'Item 1',
                },
                {
                  type: 'newLine',
                  start: 8,
                  end: 9,
                  value: '\n',
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
                  end: 17,
                  value: 'Item 2',
                },
                {
                  type: 'newLine',
                  start: 17,
                  end: 18,
                  value: '\n',
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
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 32,
      children: [
        {
          type: 'list',
          start: 0,
          end: 32,
          level: 0,
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
                  type: 'newLine',
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
                  type: 'newLine',
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

  it('Should parse list with section', () => {
    const orgText = `- Item 1
 I'am subgroup with *bold* text
- Item 2`;
    const result = parse(orgText);
    // console.log(prettyTreePrint(result));
    removeInformationAboutRelatives(result);
    expect(prettyTreePrint(result)).toEqual(`root [0-49]
  list [0-49]
      :unordered:
      :level 0:
    listItem [0-9]
      operator [0-2] ("- ")
      text [2-8] ("Item 1")
      newLine [8-9]
      section [9-41]
        indent [9-10] (" ")
        text [10-29] ("I'am subgroup with ")
        bold [29-35]
          operator [29-30] ("*")
          text [30-34] ("bold")
          operator [34-35] ("*")
        text [35-40] (" text")
        newLine [40-41]
    listItem [41-49]
      operator [41-43] ("- ")
      text [43-49] ("Item 2")
`);
  });

  it('Should parse list with single section item', () => {
    const orgText = `- Item 1
 Some nested text`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(prettyTreePrint(result));
    expect(prettyTreePrint(result)).toEqual(`root [0-26]
  list [0-26]
      :unordered:
      :level 0:
    listItem [0-9]
      operator [0-2] ("- ")
      text [2-8] ("Item 1")
      newLine [8-9]
      section [9-26]
        indent [9-10] (" ")
        text [10-26] ("Some nested text")
`);
  });

  it('Should parse list with section and nested multiple nodes', () => {
    const orgText = `- Item 1
 Some nested text
 End another one text
- Item 2
This text will end list
- New item 1 of second list`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(prettyTreePrint(result));
    expect(prettyTreePrint(result)).toEqual(`root [0-109]
  list [0-58]
      :unordered:
      :level 0:
    listItem [0-9]
      operator [0-2] ("- ")
      text [2-8] ("Item 1")
      newLine [8-9]
      section [9-49]
        indent [9-10] (" ")
        text [10-26] ("Some nested text")
        newLine [26-27]
        indent [27-28] (" ")
        text [28-48] ("End another one text")
        newLine [48-49]
    listItem [49-58]
      operator [49-51] ("- ")
      text [51-57] ("Item 2")
      newLine [57-58]
  text [58-81] ("This text will end list")
  newLine [81-82]
  list [82-109]
      :unordered:
      :level 0:
    listItem [82-109]
      operator [82-84] ("- ")
      text [84-109] ("New item 1 of second list")
`);
  });

  it('Should parse list with plus items', () => {
    const orgText = `+ Item 1
+ Item 2`;
    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    expect(prettyTreePrint(result)).toEqual(`root [0-17]
  list [0-17]
      :unordered:
      :level 0:
    listItem [0-9]
      operator [0-2] ("+ ")
      text [2-8] ("Item 1")
      newLine [8-9]
    listItem [9-17]
      operator [9-11] ("+ ")
      text [11-17] ("Item 2")
`);
  });

  it('Should parse list with nested nodes', () => {
    const orgText = `+ *Item 1*
+ +Item 2+
+ /Item 3/`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    expect(result).toEqual({
      type: 'root',
      start: 0,
      end: 32,
      children: [
        {
          type: 'list',
          start: 0,
          level: 0,
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
                  value: '+ ',
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
                  type: 'newLine',
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
                  value: '+ ',
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
                  type: 'newLine',
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
                  value: '+ ',
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

  it('Should parse nested lists', () => {
    const orgText = `- item 1 level 1
  - item 1 level 2
  - item 2 level 2
- item 2 level 1`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    // console.log(JSON.stringify(result, null, 2));
    // console.log(prettyTreePrint(result));

    // Experemental test flow for nice pretty print ;)
    expect(prettyTreePrint(result)).toEqual(`root [0-71]
  list [0-71]
      :unordered:
      :level 0:
    listItem [0-17]
      operator [0-2] ("- ")
      text [2-16] ("item 1 level 1")
      newLine [16-17]
      section [17-55]
        list [17-55]
            :unordered:
            :level 1:
          listItem [17-36]
            indent [17-19] ("  ")
            operator [19-21] ("- ")
            text [21-35] ("item 1 level 2")
            newLine [35-36]
          listItem [36-55]
            indent [36-38] ("  ")
            operator [38-40] ("- ")
            text [40-54] ("item 2 level 2")
            newLine [54-55]
    listItem [55-71]
      operator [55-57] ("- ")
      text [57-71] ("item 2 level 1")
`);
  });

  it('Should parse ordered list', () => {
    const orgText = `1. Item 1
2. Item 2
3. Item 3`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    console.log(prettyTreePrint(result));
    expect(prettyTreePrint(result)).toEqual(`root [0-29]
  list [0-29]
      :ordered:
      :level 0:
    listItem [0-10]
      operator [0-3] ("1. ")
      text [3-9] ("Item 1")
      newLine [9-10]
    listItem [10-20]
      operator [10-13] ("2. ")
      text [13-19] ("Item 2")
      newLine [19-20]
    listItem [20-29]
      operator [20-23] ("3. ")
      text [23-29] ("Item 3")
`);
  });

  it('Should parse ordered list with parenthesis list item', () => {
    const orgText = `1) Item 1
2) Item 2
3) Item 3`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    console.log(prettyTreePrint(result));
    expect(prettyTreePrint(result)).toEqual(`root [0-29]
  list [0-29]
      :ordered:
      :level 0:
    listItem [0-10]
      operator [0-3] ("1) ")
      text [3-9] ("Item 1")
      newLine [9-10]
    listItem [10-20]
      operator [10-13] ("2) ")
      text [13-19] ("Item 2")
      newLine [19-20]
    listItem [20-29]
      operator [20-23] ("3) ")
      text [23-29] ("Item 3")
`);
  });

  it('Should parse nested ordered list with parenthesis list items', () => {
    const orgText = `1) Item 1
  1) Nested item 1
  2) Nested item 2
2) Item 2`;

    const result = parse(orgText);
    removeInformationAboutRelatives(result);
    console.log(prettyTreePrint(result));
    expect(prettyTreePrint(result)).toEqual(`root [0-57]
  list [0-57]
      :ordered:
      :level 0:
    listItem [0-10]
      operator [0-3] ("1) ")
      text [3-9] ("Item 1")
      newLine [9-10]
      section [10-48]
        list [10-48]
            :ordered:
            :level 1:
          listItem [10-29]
            indent [10-12] ("  ")
            operator [12-15] ("1) ")
            text [15-28] ("Nested item 1")
            newLine [28-29]
          listItem [29-48]
            indent [29-31] ("  ")
            operator [31-34] ("2) ")
            text [34-47] ("Nested item 2")
            newLine [47-48]
    listItem [48-57]
      operator [48-51] ("2) ")
      text [51-57] ("Item 2")
`);
  });
});
