import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Clock', () => {
  it('should parse open (running) clock', () => {
    const orgDoc = `CLOCK: [2024-01-01 Mon 10:00]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        clock [0-29]
          clockKeyword [0-6] ("CLOCK:")
          text [6-7] (" ")
          date [7-29]
            operator [7-8] ("[")
            text [8-28] ("2024-01-01 Mon 10:00")
            operator [28-29] ("]")
      "
    `);
  });

  it('should parse closed clock with duration', () => {
    const orgDoc = `CLOCK: [2024-01-01 Mon 10:00]--[2024-01-01 Mon 12:00] =>  2:00`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-62]
        clock [0-62]
          clockKeyword [0-6] ("CLOCK:")
          text [6-7] (" ")
          dateRange [7-53]
            date [7-29]
              operator [7-8] ("[")
              text [8-28] ("2024-01-01 Mon 10:00")
              operator [28-29] ("]")
            text [29-31] ("--")
            date [31-53]
              operator [31-32] ("[")
              text [32-52] ("2024-01-01 Mon 12:00")
              operator [52-53] ("]")
          clockDuration [53-62]
            operator [53-58] (" =>  ")
            text [58-62] ("2:00")
      "
    `);
  });

  it('should parse multi-day clock', () => {
    const orgDoc = `CLOCK: [2024-01-01 Mon 23:00]--[2024-01-02 Tue 01:30] =>  2:30`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-62]
        clock [0-62]
          clockKeyword [0-6] ("CLOCK:")
          text [6-7] (" ")
          dateRange [7-53]
            date [7-29]
              operator [7-8] ("[")
              text [8-28] ("2024-01-01 Mon 23:00")
              operator [28-29] ("]")
            text [29-31] ("--")
            date [31-53]
              operator [31-32] ("[")
              text [32-52] ("2024-01-02 Tue 01:30")
              operator [52-53] ("]")
          clockDuration [53-62]
            operator [53-58] (" =>  ")
            text [58-62] ("2:30")
      "
    `);
  });

  it('should parse clock inside logbook drawer', () => {
    const orgDoc = `* Task
:LOGBOOK:
CLOCK: [2024-01-01 Mon 10:00]--[2024-01-01 Mon 12:00] =>  2:00
:END:`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-85]
        headline [0-85]
            :level 1:
          title [0-7]
            operator [0-2] ("* ")
            text [2-6] ("Task")
            newLine [6-7]
          section [7-85]
            property [7-16]
              text [7-16] (":LOGBOOK:")
            newLine [16-17]
            clock [17-79]
              clockKeyword [17-23] ("CLOCK:")
              text [23-24] (" ")
              dateRange [24-70]
                date [24-46]
                  operator [24-25] ("[")
                  text [25-45] ("2024-01-01 Mon 10:00")
                  operator [45-46] ("]")
                text [46-48] ("--")
                date [48-70]
                  operator [48-49] ("[")
                  text [49-69] ("2024-01-01 Mon 12:00")
                  operator [69-70] ("]")
              clockDuration [70-79]
                operator [70-75] (" =>  ")
                text [75-79] ("2:00")
            newLine [79-80]
            property [80-85]
              text [80-85] (":END:")
      "
    `);
  });

  it('should parse clock with two-digit hours in duration', () => {
    const orgDoc = `CLOCK: [2024-01-01 Mon 00:00]--[2024-01-01 Mon 23:45] => 23:45`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-62]
        clock [0-62]
          clockKeyword [0-6] ("CLOCK:")
          text [6-7] (" ")
          dateRange [7-53]
            date [7-29]
              operator [7-8] ("[")
              text [8-28] ("2024-01-01 Mon 00:00")
              operator [28-29] ("]")
            text [29-31] ("--")
            date [31-53]
              operator [31-32] ("[")
              text [32-52] ("2024-01-01 Mon 23:45")
              operator [52-53] ("]")
          clockDuration [53-62]
            operator [53-57] (" => ")
            text [57-62] ("23:45")
      "
    `);
  });

  it('should NOT parse CLOCK keyword in middle of line as clock', () => {
    const orgDoc = `Some text CLOCK: [2024-01-01 Mon 10:00]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-39]
        text [0-17] ("Some text CLOCK: ")
        date [17-39]
          operator [17-18] ("[")
          text [18-38] ("2024-01-01 Mon 10:00")
          operator [38-39] ("]")
      "
    `);
  });
});
