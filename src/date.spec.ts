import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Date', () => {
  it('Should parse inactive date', () => {
    const orgDoc = `Need to buy new mechanical keyboard [2023-01-15 Sun]`;
    const result = prettyTreePrint(parse(orgDoc));

    expect(result).toMatchInlineSnapshot(`
      "root [0-52]
        text [0-36] ("Need to buy new mechanical keyboard ")
        undefined [36-52]
          operator [36-37] ("[")
          text [37-51] ("2023-01-15 Sun")
          operator [51-52] ("]")
      "
    `);
  });
});
