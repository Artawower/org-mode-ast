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
});
