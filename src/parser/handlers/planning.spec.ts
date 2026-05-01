import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Planning', () => {
  it('should parse DEADLINE planning keyword', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-38]
        headline [0-38]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-38]
            planning [12-38]
              planningKeyword [12-21] ("DEADLINE:")
              text [21-22] (" ")
              date [22-38]
                operator [22-23] ("<")
                text [23-37] ("2026-05-10 Mon")
                operator [37-38] (">")
      "
    `);
  });

  it('should parse SCHEDULED planning keyword', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-39]
        headline [0-39]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-39]
            planning [12-39]
              planningKeyword [12-22] ("SCHEDULED:")
              text [22-23] (" ")
              date [23-39]
                operator [23-24] ("<")
                text [24-38] ("2026-05-01 Fri")
                operator [38-39] (">")
      "
    `);
  });

  it('should parse CLOSED with inactive timestamp', () => {
    const orgDoc = `* DONE Task
CLOSED: [2026-04-30 Wed]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-36]
        headline [0-36]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("DONE")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-36]
            planning [12-36]
              planningKeyword [12-19] ("CLOSED:")
              text [19-20] (" ")
              date [20-36]
                operator [20-21] ("[")
                text [21-35] ("2026-04-30 Wed")
                operator [35-36] ("]")
      "
    `);
  });

  it('should parse two planning keywords on one line', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon> SCHEDULED: <2026-05-01 Fri>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-66]
        headline [0-66]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-66]
            planning [12-66]
              planningKeyword [12-21] ("DEADLINE:")
              text [21-22] (" ")
              date [22-38]
                operator [22-23] ("<")
                text [23-37] ("2026-05-10 Mon")
                operator [37-38] (">")
              text [38-39] (" ")
              planningKeyword [39-49] ("SCHEDULED:")
              text [49-50] (" ")
              date [50-66]
                operator [50-51] ("<")
                text [51-65] ("2026-05-01 Fri")
                operator [65-66] (">")
      "
    `);
  });

  it('should parse all three planning keywords on one line', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon> SCHEDULED: <2026-05-01 Fri> CLOSED: [2026-04-30 Wed]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-91]
        headline [0-91]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-91]
            planning [12-91]
              planningKeyword [12-21] ("DEADLINE:")
              text [21-22] (" ")
              date [22-38]
                operator [22-23] ("<")
                text [23-37] ("2026-05-10 Mon")
                operator [37-38] (">")
              text [38-39] (" ")
              planningKeyword [39-49] ("SCHEDULED:")
              text [49-50] (" ")
              date [50-66]
                operator [50-51] ("<")
                text [51-65] ("2026-05-01 Fri")
                operator [65-66] (">")
              text [66-67] (" ")
              planningKeyword [67-74] ("CLOSED:")
              text [74-75] (" ")
              date [75-91]
                operator [75-76] ("[")
                text [76-90] ("2026-04-30 Wed")
                operator [90-91] ("]")
      "
    `);
  });

  it('should parse planning followed by body text', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>
Body text`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-48]
        headline [0-48]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-48]
            planning [12-38]
              planningKeyword [12-21] ("DEADLINE:")
              text [21-22] (" ")
              date [22-38]
                operator [22-23] ("<")
                text [23-37] ("2026-05-10 Mon")
                operator [37-38] (">")
            newLine [38-39]
            text [39-48] ("Body text")
      "
    `);
  });

  it('should parse planning without TODO keyword in headline', () => {
    const orgDoc = `* My Task
SCHEDULED: <2026-05-01 Fri>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        headline [0-37]
            :level 1:
          title [0-10]
            operator [0-2] ("* ")
            text [2-9] ("My Task")
            newLine [9-10]
          section [10-37]
            planning [10-37]
              planningKeyword [10-20] ("SCHEDULED:")
              text [20-21] (" ")
              date [21-37]
                operator [21-22] ("<")
                text [22-36] ("2026-05-01 Fri")
                operator [36-37] (">")
      "
    `);
  });

  it('should parse SCHEDULED with date range', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri>--<2026-05-10 Mon>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-57]
        headline [0-57]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-57]
            planning [12-57]
              planningKeyword [12-22] ("SCHEDULED:")
              text [22-23] (" ")
              dateRange [23-57]
                date [23-39]
                  operator [23-24] ("<")
                  text [24-38] ("2026-05-01 Fri")
                  operator [38-39] (">")
                text [39-41] ("--")
                date [41-57]
                  operator [41-42] ("<")
                  text [42-56] ("2026-05-10 Mon")
                  operator [56-57] (">")
      "
    `);
  });

  it('should NOT parse planning if keyword is not on first section line', () => {
    const orgDoc = `* Task
Some text
DEADLINE: <2026-05-10 Mon>`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-43]
        headline [0-43]
            :level 1:
          title [0-7]
            operator [0-2] ("* ")
            text [2-6] ("Task")
            newLine [6-7]
          section [7-43]
            text [7-16] ("Some text")
            newLine [16-17]
            planningKeyword [17-26] ("DEADLINE:")
            text [26-27] (" ")
            date [27-43]
              operator [27-28] ("<")
              text [28-42] ("2026-05-10 Mon")
              operator [42-43] (">")
      "
    `);
  });

  it('should NOT parse planning when first line is regular text', () => {
    const orgDoc = `* Task
Some regular text here`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        headline [0-29]
            :level 1:
          title [0-7]
            operator [0-2] ("* ")
            text [2-6] ("Task")
            newLine [6-7]
          section [7-29]
            text [7-29] ("Some regular text here")
      "
    `);
  });

  it('should wrap planning line even when keyword has no timestamp', () => {
    const orgDoc = `* TODO Task
DEADLINE: some text without date`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-44]
        headline [0-44]
            :level 1:
          title [0-12]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-11] (" Task")
            newLine [11-12]
          section [12-44]
            planning [12-44]
              planningKeyword [12-21] ("DEADLINE:")
              text [21-44] (" some text without date")
      "
    `);
  });

  it('should not affect headlines without section', () => {
    const orgDoc = `* Task`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-6]
        headline [0-6]
            :level 1:
          title [0-6]
            operator [0-2] ("* ")
            text [2-6] ("Task")
      "
    `);
  });

  it('should parse planning under nested headline', () => {
    const orgDoc = `* Parent
** Child task
DEADLINE: <2026-05-10 Mon>
Content`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-57]
        headline [0-57]
            :level 1:
          title [0-9]
            operator [0-2] ("* ")
            text [2-8] ("Parent")
            newLine [8-9]
          section [9-57]
            headline [9-57]
                :level 2:
              title [9-23]
                operator [9-12] ("** ")
                text [12-22] ("Child task")
                newLine [22-23]
              section [23-57]
                planning [23-49]
                  planningKeyword [23-32] ("DEADLINE:")
                  text [32-33] (" ")
                  date [33-49]
                    operator [33-34] ("<")
                    text [34-48] ("2026-05-10 Mon")
                    operator [48-49] (">")
                newLine [49-50]
                text [50-57] ("Content")
      "
    `);
  });
});

// Regression: property drawer after planning must still be recognized
it('should parse property drawer after planning', () => {
  const orgDoc = `* Task
DEADLINE: <2026-05-10 Mon>
:PROPERTIES:
:ID: abc123
:END:`;
  const result = parse(orgDoc);

  expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  expect(result.toString()).toMatchInlineSnapshot(`
    "root [0-64]
      headline [0-64]
          :level 1:
          :id  abc123:
        title [0-7]
          operator [0-2] ("* ")
          text [2-6] ("Task")
          newLine [6-7]
        section [7-64]
          planning [7-33]
            planningKeyword [7-16] ("DEADLINE:")
            text [16-17] (" ")
            date [17-33]
              operator [17-18] ("<")
              text [18-32] ("2026-05-10 Mon")
              operator [32-33] (">")
          newLine [33-34]
          propertyDrawer [34-64]
            property [34-46]
              text [34-46] (":PROPERTIES:")
            newLine [46-47]
            property [47-58]
              text [47-52] (":ID: ")
              text [52-58] ("abc123")
            newLine [58-59]
            property [59-64]
              text [59-64] (":END:")
    "
  `);
});

it('should parse planning followed by property drawer and body text', () => {
  const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>
:PROPERTIES:
:ID: abc123
:END:
Body text`;
  const result = parse(orgDoc);

  expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  expect(result.toString()).toMatchInlineSnapshot(`
    "root [0-79]
      headline [0-79]
          :level 1:
          :id  abc123:
        title [0-12]
          operator [0-2] ("* ")
          todoKeyword [2-6] ("TODO")
          text [6-11] (" Task")
          newLine [11-12]
        section [12-79]
          planning [12-38]
            planningKeyword [12-21] ("DEADLINE:")
            text [21-22] (" ")
            date [22-38]
              operator [22-23] ("<")
              text [23-37] ("2026-05-10 Mon")
              operator [37-38] (">")
          newLine [38-39]
          propertyDrawer [39-69]
            property [39-51]
              text [39-51] (":PROPERTIES:")
            newLine [51-52]
            property [52-63]
              text [52-57] (":ID: ")
              text [57-63] ("abc123")
            newLine [63-64]
            property [64-69]
              text [64-69] (":END:")
          newLine [69-70]
          text [70-79] ("Body text")
    "
  `);
});

it('should preserve trailing text after planning timestamp', () => {
  const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon> extra note`;
  const result = parse(orgDoc);

  expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  expect(result.toString()).toMatchInlineSnapshot(`
    "root [0-49]
      headline [0-49]
          :level 1:
        title [0-12]
          operator [0-2] ("* ")
          todoKeyword [2-6] ("TODO")
          text [6-11] (" Task")
          newLine [11-12]
        section [12-49]
          planning [12-49]
            planningKeyword [12-21] ("DEADLINE:")
            text [21-22] (" ")
            date [22-38]
              operator [22-23] ("<")
              text [23-37] ("2026-05-10 Mon")
              operator [37-38] (">")
            text [38-49] (" extra note")
    "
  `);
});

it('should preserve non-standard text inside planning line', () => {
  const orgDoc = `* Task
DEADLINE: invalid text <2026-05-10 Mon>`;
  const result = parse(orgDoc);

  expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  expect(result.toString()).toMatchInlineSnapshot(`
    "root [0-46]
      headline [0-46]
          :level 1:
        title [0-7]
          operator [0-2] ("* ")
          text [2-6] ("Task")
          newLine [6-7]
        section [7-46]
          planning [7-46]
            planningKeyword [7-16] ("DEADLINE:")
            text [16-30] (" invalid text ")
            date [30-46]
              operator [30-31] ("<")
              text [31-45] ("2026-05-10 Mon")
              operator [45-46] (">")
    "
  `);
});
