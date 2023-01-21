import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Date', () => {
  it('Should parse inactive date', () => {
    const orgDoc = `Need to buy new mechanical keyboard [2023-01-15 Sun]`;
    const result = prettyTreePrint(parse(orgDoc));

    expect(result).toMatchInlineSnapshot(`
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
    const result = prettyTreePrint(parse(orgDoc));

    expect(result).toMatchInlineSnapshot(`
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
    const result = prettyTreePrint(parse(orgDoc));
    expect(result).toMatchInlineSnapshot(`
      "root [0-49]
        text [0-49] ("Meeting at 2pm on 2022-12-25 2022-12-25 Sun 14:00")
      "
    `);
  });

  it('Should parse active date at the middle of the bold text', () => {
    const orgDoc = `*Meeting at 2pm on 2022-12-25 [2022-12-25 Sun 14:00]*`;
    const result = prettyTreePrint(parse(orgDoc));

    expect(result).toMatchInlineSnapshot(`
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
    const result = prettyTreePrint(parse(orgDoc));

    expect(result).toMatchInlineSnapshot(`
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
});
