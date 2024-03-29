import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Date', () => {
  it('Should parse inactive date', () => {
    const orgDoc = `Need to buy new mechanical keyboard [2023-01-15 Sun]`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-52]
        text [0-36] ("Need to buy new mechanical keyboard ")
        date [36-52]
          operator [36-37] ("[")
          text [37-51] ("2023-01-15 Sun")
          operator [51-52] ("]")
      "
    `);
  });

  it('Should parse inactive date and time', () => {
    const orgDoc = `Meeting at 2pm on 2022-12-25 [2022-12-25 Sun 14:00]`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-51]
        text [0-29] ("Meeting at 2pm on 2022-12-25 ")
        date [29-51]
          operator [29-30] ("[")
          text [30-50] ("2022-12-25 Sun 14:00")
          operator [50-51] ("]")
      "
    `);
  });

  it('Should not parse date without brackets', () => {
    const orgDoc = `Meeting at 2pm on 2022-12-25 2022-12-25 Sun 14:00`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-49]
        text [0-49] ("Meeting at 2pm on 2022-12-25 2022-12-25 Sun 14:00")
      "
    `);
  });

  it('Should parse active date at the middle of the bold text', () => {
    const orgDoc = `*Meeting at 2pm on 2022-12-25 [2022-12-25 Sun 14:00]*`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-53]
        bold [0-53]
          operator [0-1] ("*")
          text [1-30] ("Meeting at 2pm on 2022-12-25 ")
          date [30-52]
            operator [30-31] ("[")
            text [31-51] ("2022-12-25 Sun 14:00")
            operator [51-52] ("]")
          operator [52-53] ("*")
      "
    `);
  });

  it('Should parse active date at the start of the bold text', () => {
    const orgDoc = `*[2022-12-25 Sun 14:00] Meeting at 2pm on 2022-12-25*`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-53]
        bold [0-53]
          operator [0-1] ("*")
          date [1-23]
            operator [1-2] ("[")
            text [2-22] ("2022-12-25 Sun 14:00")
            operator [22-23] ("]")
          text [23-52] (" Meeting at 2pm on 2022-12-25")
          operator [52-53] ("*")
      "
    `);
  });

  it('Should parse active date', () => {
    const orgDoc = `<2023-01-09 Mon>`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-16]
        date [0-16]
          operator [0-1] ("<")
          text [1-15] ("2023-01-09 Mon")
          operator [15-16] (">")
      "
    `);
  });

  it('Should parse active date with time', () => {
    const orgDoc = `<2023-01-09 Mon 14:00>`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-22]
        date [0-22]
          operator [0-1] ("<")
          text [1-21] ("2023-01-09 Mon 14:00")
          operator [21-22] (">")
      "
    `);
  });

  it('Should parse date and time in center of text', () => {
    const orgDoc = `This is a reminder for meeting on <2023-01-09 Mon 14:00>. Don't forget to attend.`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-81]
        text [0-34] ("This is a reminder for meeting on ")
        date [34-56]
          operator [34-35] ("<")
          text [35-55] ("2023-01-09 Mon 14:00")
          operator [55-56] (">")
        text [56-81] (". Don't forget to attend.")
      "
    `);
  });

  it('Should parse inactive date inside bold and crossed text', () => {
    const orgDoc = `*This is a reminder for meeting on +[2023-01-09 Mon 14:00]+. Don't forget to attend.*`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-85]
        bold [0-85]
          operator [0-1] ("*")
          text [1-35] ("This is a reminder for meeting on ")
          crossed [35-59]
            operator [35-36] ("+")
            date [36-58]
              operator [36-37] ("[")
              text [37-57] ("2023-01-09 Mon 14:00")
              operator [57-58] ("]")
            operator [58-59] ("+")
          text [59-84] (". Don't forget to attend.")
          operator [84-85] ("*")
      "
    `);
  });

  it('Should not parse date time without brackets', () => {
    const orgDoc = `This is a reminder for meeting on <2023-01-09 Mon 14:00. Don't forget to attend.`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-80]
        text [0-80] ("This is a reminder for meeting on <2023-01-09 Mon 14:00. Don't forget to attend.")
      "
    `);
  });

  it('Should not parse tags as active date', () => {
    const orgDoc = `<div>
  <p>Some text</p>
</div>`;
    const result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-31]
        text [0-5] ("<div>")
        newLine [5-6]
        indent [6-8] ("  ")
        text [8-24] ("<p>Some text</p>")
        newLine [24-25]
        text [25-31] ("</div>")
      "
    `);
  });

  it('Should parse brackets between triangle brackets', () => {
    const orgDoc = `<I'am not a date, but i have nested formatting =lululu= =)>`,
      result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-59]
        text [0-47] ("<I'am not a date, but i have nested formatting ")
        verbatim [47-55]
          operator [47-48] ("=")
          text [48-54] ("lululu")
          operator [54-55] ("=")
        text [55-59] (" =)>")
      "
    `);
  });

  it('Should parse smile as text', () => {
    const orgDoc = `<=)>`,
      result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-4]
        text [0-4] ("<=)>")
      "
    `);
  });

  it('Should parse date with hour offset', () => {
    const orgDoc = `<2023-01-09 Mon 14:00 +1h>`,
      result = parse(orgDoc.toString());

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-26]
        date [0-26]
          operator [0-1] ("<")
          text [1-25] ("2023-01-09 Mon 14:00 +1h")
          operator [25-26] (">")
      "
    `);
  });

  it('Should parse date with negative offset', () => {
    const orgDoc = `<2023-01-09 Mon 14:00 ++3y -1h>`,
      result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-31]
        date [0-31]
          operator [0-1] ("<")
          text [1-30] ("2023-01-09 Mon 14:00 ++3y -1h")
          operator [30-31] (">")
      "
    `);
  });

  it('Should parse date range with offset', () => {
    const orgDoc = `<2023-01-09 Mon>--<2023-01-10 Tue ++1w +1d>`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-43]
        dateRange [0-43]
          date [0-16]
            operator [0-1] ("<")
            text [1-15] ("2023-01-09 Mon")
            operator [15-16] (">")
          text [16-18] ("--")
          date [18-43]
            operator [18-19] ("<")
            text [19-42] ("2023-01-10 Tue ++1w +1d")
            operator [42-43] (">")
      "
    `);
  });

  it('Should parse date range', () => {
    const orgDoc = `<2023-01-09 Mon>--<2023-01-10 Tue>`,
      result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-34]
        dateRange [0-34]
          date [0-16]
            operator [0-1] ("<")
            text [1-15] ("2023-01-09 Mon")
            operator [15-16] (">")
          text [16-18] ("--")
          date [18-34]
            operator [18-19] ("<")
            text [19-33] ("2023-01-10 Tue")
            operator [33-34] (">")
      "
    `);
  });

  it('Should parser date range with offset between text nodes', () => {
    const orgDoc = `This is *bold text with <2023-01-09 Mon>--<2023-01-10 Tue> date range*`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-70]
        text [0-8] ("This is ")
        bold [8-70]
          operator [8-9] ("*")
          text [9-24] ("bold text with ")
          dateRange [24-58]
            date [24-40]
              operator [24-25] ("<")
              text [25-39] ("2023-01-09 Mon")
              operator [39-40] (">")
            text [40-42] ("--")
            date [42-58]
              operator [42-43] ("<")
              text [43-57] ("2023-01-10 Tue")
              operator [57-58] (">")
          text [58-69] (" date range")
          operator [69-70] ("*")
      "
    `);
  });
});
