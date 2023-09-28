import { parse } from 'parser/parser';
import { hasNodeIncorrectRanges } from 'test-helper';

describe('Table', () => {
  it('Should parse simple table', () => {
    const orgDoc = `| Header 1 | Header 2 |`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-23]
        table [0-23]
          tableRow [0-23]
            operator [0-1] ("|")
            tableCell [1-11]
              text [1-11] (" Header 1 ")
            operator [11-12] ("|")
            tableCell [12-22]
              text [12-22] (" Header 2 ")
            operator [22-23] ("|")
      "
    `);
  });

  it('Should not parse table when it start not from new line', () => {
    const orgDoc = `text | not cell | not cell 2 |`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-30]
        text [0-30] ("text | not cell | not cell 2 |")
      "
    `);
  });

  it('Should parse simple row with indent', () => {
    const orgDoc = `     | Header 1 | Header 2 |`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        table [0-28]
          indent [0-5] ("     ")
          tableRow [5-28]
            operator [5-6] ("|")
            tableCell [6-16]
              text [6-16] (" Header 1 ")
            operator [16-17] ("|")
            tableCell [17-27]
              text [17-27] (" Header 2 ")
            operator [27-28] ("|")
      "
    `);
  });

  it('Should create multiline table', () => {
    const orgDoc = `| Header 1 | Header 2 |
    | Cell 1 | Cell 2 |`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-47]
        table [0-47]
          tableRow [0-23]
            operator [0-1] ("|")
            tableCell [1-11]
              text [1-11] (" Header 1 ")
            operator [11-12] ("|")
            tableCell [12-22]
              text [12-22] (" Header 2 ")
            operator [22-23] ("|")
          newLine [23-24]
          indent [24-28] ("    ")
          tableRow [28-47]
            operator [28-29] ("|")
            tableCell [29-37]
              text [29-37] (" Cell 1 ")
            operator [37-38] ("|")
            tableCell [38-46]
              text [38-46] (" Cell 2 ")
            operator [46-47] ("|")
      "
    `);
  });

  it('Should parse table which start from indent', () => {
    const orgDoc = `   | qweqwe | 2 |
| bbrbr | bu |
    | qwe | hello |`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-52]
        table [0-52]
          indent [0-3] ("   ")
          tableRow [3-17]
            operator [3-4] ("|")
            tableCell [4-12]
              text [4-12] (" qweqwe ")
            operator [12-13] ("|")
            tableCell [13-16]
              text [13-16] (" 2 ")
            operator [16-17] ("|")
          newLine [17-18]
          tableRow [18-32]
            operator [18-19] ("|")
            tableCell [19-26]
              text [19-26] (" bbrbr ")
            operator [26-27] ("|")
            tableCell [27-31]
              text [27-31] (" bu ")
            operator [31-32] ("|")
          newLine [32-33]
          indent [33-37] ("    ")
          tableRow [37-52]
            operator [37-38] ("|")
            tableCell [38-43]
              text [38-43] (" qwe ")
            operator [43-44] ("|")
            tableCell [44-51]
              text [44-51] (" hello ")
            operator [51-52] ("|")
      "
    `);
  });

  it('Should parse multirow table without end oeprator', () => {
    const orgDoc = `| qweqwe | 2
    | bbrbr | bu`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        table [0-29]
          tableRow [0-12]
            operator [0-1] ("|")
            tableCell [1-9]
              text [1-9] (" qweqwe ")
            operator [9-10] ("|")
            tableCell [10-12]
              text [10-12] (" 2")
          newLine [12-13]
          indent [13-17] ("    ")
          tableRow [17-29]
            operator [17-18] ("|")
            tableCell [18-25]
              text [18-25] (" bbrbr ")
            operator [25-26] ("|")
            tableCell [26-29]
              text [26-29] (" bu")
      "
    `);
  });

  it('Should parse formatting text inside table cells', () => {
    const orgDoc = `| *bold* | /italic/ | _underline_ | ~code~ | +strike+ |`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-55]
        table [0-55]
          tableRow [0-55]
            operator [0-1] ("|")
            tableCell [1-9]
              text [1-2] (" ")
              bold [2-8]
                operator [2-3] ("*")
                text [3-7] ("bold")
                operator [7-8] ("*")
              text [8-9] (" ")
            operator [9-10] ("|")
            tableCell [10-20]
              text [10-11] (" ")
              italic [11-19]
                operator [11-12] ("/")
                text [12-18] ("italic")
                operator [18-19] ("/")
              text [19-20] (" ")
            operator [20-21] ("|")
            tableCell [21-34]
              text [21-34] (" _underline_ ")
            operator [34-35] ("|")
            tableCell [35-43]
              text [35-36] (" ")
              inlineCode [36-42]
                operator [36-37] ("~")
                text [37-41] ("code")
                operator [41-42] ("~")
              text [42-43] (" ")
            operator [43-44] ("|")
            tableCell [44-54]
              text [44-45] (" ")
              crossed [45-53]
                operator [45-46] ("+")
                text [46-52] ("strike")
                operator [52-53] ("+")
              text [53-54] (" ")
            operator [54-55] ("|")
      "
    `);
  });

  it('Should parse table with empty cells', () => {
    const orgDoc = `
- Item 1
| A |
| b |`;

    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        newLine [0-1]
        list [1-10]
            :unordered:
            :level 0:
          listItem [1-10]
            title [1-10]
              operator [1-3] ("- ")
              text [3-9] ("Item 1")
              newLine [9-10]
        table [10-21]
          tableRow [10-15]
            operator [10-11] ("|")
            tableCell [11-14]
              text [11-14] (" A ")
            operator [14-15] ("|")
          newLine [15-16]
          tableRow [16-21]
            operator [16-17] ("|")
            tableCell [17-20]
              text [17-20] (" b ")
            operator [20-21] ("|")
      "
    `);
  });

  it('Should parse small table with special symbol inside', () => {
    const orgDoc = `| 1 | + |
| 2 | + |`;
    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        table [0-19]
          tableRow [0-9]
            operator [0-1] ("|")
            tableCell [1-4]
              text [1-4] (" 1 ")
            operator [4-5] ("|")
            tableCell [5-8]
              text [5-8] (" + ")
            operator [8-9] ("|")
          newLine [9-10]
          tableRow [10-19]
            operator [10-11] ("|")
            tableCell [11-14]
              text [11-14] (" 2 ")
            operator [14-15] ("|")
            tableCell [15-18]
              text [15-18] (" + ")
            operator [18-19] ("|")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should parse table with special symbols', () => {
    const orgDoc = `| Название          | highlight |
|-------------------+-----------|
| eprfr             | +         |
| neprdu            | +         |`;

    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-135]
        table [0-68]
          tableRow [0-33]
            operator [0-1] ("|")
            tableCell [1-20]
              text [1-20] (" Название          ")
            operator [20-21] ("|")
            tableCell [21-32]
              text [21-32] (" highlight ")
            operator [32-33] ("|")
          newLine [33-34]
          tableDelimiter [34-67] ("|-------------------+-----------|")
          newLine [67-68]
        table [68-135]
          tableRow [68-101]
            operator [68-69] ("|")
            tableCell [69-88]
              text [69-88] (" eprfr             ")
            operator [88-89] ("|")
            tableCell [89-100]
              text [89-100] (" +         ")
            operator [100-101] ("|")
          newLine [101-102]
          tableRow [102-135]
            operator [102-103] ("|")
            tableCell [103-122]
              text [103-122] (" neprdu            ")
            operator [122-123] ("|")
            tableCell [123-134]
              text [123-134] (" +         ")
            operator [134-135] ("|")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should not include end of line  into end of the table', () => {
    const orgDoc = `| Header1          | header2 |
| col 1 | col 2 |

`;
    const result = parse(orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-50]
        table [0-48]
          tableRow [0-30]
            operator [0-1] ("|")
            tableCell [1-19]
              text [1-19] (" Header1          ")
            operator [19-20] ("|")
            tableCell [20-29]
              text [20-29] (" header2 ")
            operator [29-30] ("|")
          newLine [30-31]
          tableRow [31-48]
            operator [31-32] ("|")
            tableCell [32-39]
              text [32-39] (" col 1 ")
            operator [39-40] ("|")
            tableCell [40-47]
              text [40-47] (" col 2 ")
            operator [47-48] ("|")
        newLine [48-49]
        newLine [49-50]
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });
});
