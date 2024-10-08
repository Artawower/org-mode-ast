import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('List tests', () => {
  it('Should parse simple list', () => {
    const orgDoc = `- Item 1
- Item 2
- Item 3`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-26]
        list [0-26]
            :unordered:
            :level 0:
          listItem [0-9]
            title [0-9]
              operator [0-2] ("- ")
              text [2-8] ("Item 1")
              newLine [8-9]
          listItem [9-18]
            title [9-18]
              operator [9-11] ("- ")
              text [11-17] ("Item 2")
              newLine [17-18]
          listItem [18-26]
            title [18-26]
              operator [18-20] ("- ")
              text [20-26] ("Item 3")
      "
    `);
  });

  it('Should parse list with nested nodes', () => {
    const orgDoc = `- *Item 1*
- +Item 2+
- /Item 3/`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-32]
        list [0-32]
            :unordered:
            :level 0:
          listItem [0-11]
            title [0-11]
              operator [0-2] ("- ")
              bold [2-10]
                operator [2-3] ("*")
                text [3-9] ("Item 1")
                operator [9-10] ("*")
              newLine [10-11]
          listItem [11-22]
            title [11-22]
              operator [11-13] ("- ")
              crossed [13-21]
                operator [13-14] ("+")
                text [14-20] ("Item 2")
                operator [20-21] ("+")
              newLine [21-22]
          listItem [22-32]
            title [22-32]
              operator [22-24] ("- ")
              italic [24-32]
                operator [24-25] ("/")
                text [25-31] ("Item 3")
                operator [31-32] ("/")
      "
    `);
  });

  it('Should parse list with section', () => {
    const orgDoc = `- Item 1
 I'am subgroup with *bold* text
- Item 2`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-49]
        list [0-49]
            :unordered:
            :level 0:
          listItem [0-41]
            title [0-9]
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
            title [41-49]
              operator [41-43] ("- ")
              text [43-49] ("Item 2")
      "
    `);
  });

  it('Should parse list with single section item', () => {
    const orgDoc = `- Item 1
 Some nested text`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-26]
        list [0-26]
            :unordered:
            :level 0:
          listItem [0-26]
            title [0-9]
              operator [0-2] ("- ")
              text [2-8] ("Item 1")
              newLine [8-9]
            section [9-26]
              indent [9-10] (" ")
              text [10-26] ("Some nested text")
      "
    `);
  });

  it('Should parse list with section and nested multiple nodes', () => {
    const orgDoc = `- Item 1
 Some nested text
 End another one text
- Item 2
This text will end list
- New item 1 of second list`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-109]
        list [0-58]
            :unordered:
            :level 0:
          listItem [0-49]
            title [0-9]
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
            title [49-58]
              operator [49-51] ("- ")
              text [51-57] ("Item 2")
              newLine [57-58]
        text [58-81] ("This text will end list")
        newLine [81-82]
        list [82-109]
            :unordered:
            :level 0:
          listItem [82-109]
            title [82-109]
              operator [82-84] ("- ")
              text [84-109] ("New item 1 of second list")
      "
    `);
  });

  it('Should parse list with plus items', () => {
    const orgDoc = `+ Item 1
+ Item 2`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        list [0-17]
            :unordered:
            :level 0:
          listItem [0-9]
            title [0-9]
              operator [0-2] ("+ ")
              text [2-8] ("Item 1")
              newLine [8-9]
          listItem [9-17]
            title [9-17]
              operator [9-11] ("+ ")
              text [11-17] ("Item 2")
      "
    `);
  });

  it('Should parse list with nested nodes', () => {
    const orgDoc = `+ *Item 1*
+ +Item 2+
+ /Item 3/`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-32]
        list [0-32]
            :unordered:
            :level 0:
          listItem [0-11]
            title [0-11]
              operator [0-2] ("+ ")
              bold [2-10]
                operator [2-3] ("*")
                text [3-9] ("Item 1")
                operator [9-10] ("*")
              newLine [10-11]
          listItem [11-22]
            title [11-22]
              operator [11-13] ("+ ")
              crossed [13-21]
                operator [13-14] ("+")
                text [14-20] ("Item 2")
                operator [20-21] ("+")
              newLine [21-22]
          listItem [22-32]
            title [22-32]
              operator [22-24] ("+ ")
              italic [24-32]
                operator [24-25] ("/")
                text [25-31] ("Item 3")
                operator [31-32] ("/")
      "
    `);
  });

  it('Should parse nested lists', () => {
    const orgDoc = `- item 1 level 1
  - item 1 level 2
  - item 2 level 2
- item 2 level 1`;

    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-71]
        list [0-71]
            :unordered:
            :level 0:
          listItem [0-55]
            title [0-17]
              operator [0-2] ("- ")
              text [2-16] ("item 1 level 1")
              newLine [16-17]
            section [17-55]
              list [17-55]
                  :unordered:
                  :level 1:
                listItem [17-36]
                  title [17-36]
                    indent [17-19] ("  ")
                    operator [19-21] ("- ")
                    text [21-35] ("item 1 level 2")
                    newLine [35-36]
                listItem [36-55]
                  title [36-55]
                    indent [36-38] ("  ")
                    operator [38-40] ("- ")
                    text [40-54] ("item 2 level 2")
                    newLine [54-55]
          listItem [55-71]
            title [55-71]
              operator [55-57] ("- ")
              text [57-71] ("item 2 level 1")
      "
    `);
  });

  it('Should parse ordered list', () => {
    const orgDoc = `1. Item 1
2. Item 2
3. Item 3`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        list [0-29]
            :ordered:
            :level 0:
          listItem [0-10]
            title [0-10]
              operator [0-3] ("1. ")
              text [3-9] ("Item 1")
              newLine [9-10]
          listItem [10-20]
            title [10-20]
              operator [10-13] ("2. ")
              text [13-19] ("Item 2")
              newLine [19-20]
          listItem [20-29]
            title [20-29]
              operator [20-23] ("3. ")
              text [23-29] ("Item 3")
      "
    `);
  });

  it('Should parse ordered list with parenthesis list item', () => {
    const orgDoc = `1) Item 1
2) Item 2
3) Item 3`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        list [0-29]
            :ordered:
            :level 0:
          listItem [0-10]
            title [0-10]
              operator [0-3] ("1) ")
              text [3-9] ("Item 1")
              newLine [9-10]
          listItem [10-20]
            title [10-20]
              operator [10-13] ("2) ")
              text [13-19] ("Item 2")
              newLine [19-20]
          listItem [20-29]
            title [20-29]
              operator [20-23] ("3) ")
              text [23-29] ("Item 3")
      "
    `);
  });

  it('Should parse nested ordered list with parenthesis list items', () => {
    const orgDoc = `1) Item 1
  1) Nested item 1
  2) Nested item 2
2) Item 2`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-57]
        list [0-57]
            :ordered:
            :level 0:
          listItem [0-48]
            title [0-10]
              operator [0-3] ("1) ")
              text [3-9] ("Item 1")
              newLine [9-10]
            section [10-48]
              list [10-48]
                  :ordered:
                  :level 1:
                listItem [10-29]
                  title [10-29]
                    indent [10-12] ("  ")
                    operator [12-15] ("1) ")
                    text [15-28] ("Nested item 1")
                    newLine [28-29]
                listItem [29-48]
                  title [29-48]
                    indent [29-31] ("  ")
                    operator [31-34] ("2) ")
                    text [34-47] ("Nested item 2")
                    newLine [47-48]
          listItem [48-57]
            title [48-57]
              operator [48-51] ("2) ")
              text [51-57] ("Item 2")
      "
    `);
  });

  it('Should parse list with tag', () => {
    const orgData = `- Tag :: Item 1
- Item 2
- Tag2 :: item 3`;

    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-41]
        list [0-41]
            :unordered:
            :level 0:
          listItem [0-16]
            title [0-16]
              operator [0-2] ("- ")
              listTag [2-6] ("Tag ")
              operator [6-8] ("::")
              text [8-15] (" Item 1")
              newLine [15-16]
          listItem [16-25]
            title [16-25]
              operator [16-18] ("- ")
              text [18-24] ("Item 2")
              newLine [24-25]
          listItem [25-41]
            title [25-41]
              operator [25-27] ("- ")
              listTag [27-32] ("Tag2 ")
              operator [32-34] ("::")
              text [34-41] (" item 3")
      "
    `);
  });

  it("Should not parse list tag which doesn't start from space", () => {
    const orgData = `- Tag:: Item 1`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        list [0-14]
            :unordered:
            :level 0:
          listItem [0-14]
            title [0-14]
              operator [0-2] ("- ")
              text [2-14] ("Tag:: Item 1")
      "
    `);
  });

  it("Should not parse list tag which doesn't end with space", () => {
    const orgData = `- Tag ::Item 1`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        list [0-14]
            :unordered:
            :level 0:
          listItem [0-14]
            title [0-14]
              operator [0-2] ("- ")
              text [2-14] ("Tag ::Item 1")
      "
    `);
  });

  it('Should parse list inside headlines', () => {
    const orgData = `* headline
- list 1 item 1
* Headline 2
- list 2 item 1`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-55]
        headline [0-27]
            :level 1:
          title [0-11]
            operator [0-2] ("* ")
            text [2-10] ("headline")
            newLine [10-11]
          section [11-27]
            list [11-27]
                :unordered:
                :level 0:
              listItem [11-27]
                title [11-27]
                  operator [11-13] ("- ")
                  text [13-26] ("list 1 item 1")
                  newLine [26-27]
        headline [27-55]
            :level 1:
          title [27-40]
            operator [27-29] ("* ")
            text [29-39] ("Headline 2")
            newLine [39-40]
          section [40-55]
            list [40-55]
                :unordered:
                :level 0:
              listItem [40-55]
                title [40-55]
                  operator [40-42] ("- ")
                  text [42-55] ("list 2 item 1")
      "
    `);
  });

  it('Should exit list when new line appeared without indent', () => {
    const orgData = `- list item 1
  - nested
Some text
---
  - list item 1 of the new list`;

    const parsed = parse(orgData);
    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-70]
        list [0-25]
            :unordered:
            :level 0:
          listItem [0-25]
            title [0-14]
              operator [0-2] ("- ")
              text [2-13] ("list item 1")
              newLine [13-14]
            section [14-25]
              list [14-25]
                  :unordered:
                  :level 1:
                listItem [14-25]
                  title [14-25]
                    indent [14-16] ("  ")
                    operator [16-18] ("- ")
                    text [18-24] ("nested")
                    newLine [24-25]
        text [25-34] ("Some text")
        newLine [34-35]
        text [35-38] ("---")
        newLine [38-39]
        list [39-70]
            :unordered:
            :level 1:
          listItem [39-70]
            title [39-70]
              indent [39-41] ("  ")
              operator [41-43] ("- ")
              text [43-70] ("list item 1 of the new list")
      "
    `);
    expect(hasNodeIncorrectRanges(parsed, orgData)).toBeFalsy();
  });

  it('Should parse two different lists', () => {
    const orgData = `- 1
123
- 2`;

    const parsed = parse(orgData);
    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-11]
        list [0-4]
            :unordered:
            :level 0:
          listItem [0-4]
            title [0-4]
              operator [0-2] ("- ")
              text [2-3] ("1")
              newLine [3-4]
        text [4-7] ("123")
        newLine [7-8]
        list [8-11]
            :unordered:
            :level 0:
          listItem [8-11]
            title [8-11]
              operator [8-10] ("- ")
              text [10-11] ("2")
      "
    `);
    expect(hasNodeIncorrectRanges(parsed, orgData)).toBeFalsy();
  });

  it('Should break lists with indent between them', () => {
    const orgDoc = `- i1

- i2`;

    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-10]
        list [0-5]
            :unordered:
            :level 0:
          listItem [0-5]
            title [0-5]
              operator [0-2] ("- ")
              text [2-4] ("i1")
              newLine [4-5]
        newLine [5-6]
        list [6-10]
            :unordered:
            :level 0:
          listItem [6-10]
            title [6-10]
              operator [6-8] ("- ")
              text [8-10] ("i2")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should not parser list operator in the middle of string', () => {
    const orgDoc = `[[./Apple.png]] - is a fruit`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        link [0-15]
            :linkType image:
          operator [0-1] ("[")
          linkUrl [1-14]
            operator [1-2] ("[")
            text [2-13] ("./Apple.png")
            operator [13-14] ("]")
          operator [14-15] ("]")
        text [15-28] (" - is a fruit")
      "
    `);
  });

  it('Should parse nested markup in list', () => {
    const orgDoc = `- *bold*
- ~verbose~`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        list [0-20]
            :unordered:
            :level 0:
          listItem [0-9]
            title [0-9]
              operator [0-2] ("- ")
              bold [2-8]
                operator [2-3] ("*")
                text [3-7] ("bold")
                operator [7-8] ("*")
              newLine [8-9]
          listItem [9-20]
            title [9-20]
              operator [9-11] ("- ")
              inlineCode [11-20]
                operator [11-12] ("~")
                text [12-19] ("verbose")
                operator [19-20] ("~")
      "
    `);
  });
});
