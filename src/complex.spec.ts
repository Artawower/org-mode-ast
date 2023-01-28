import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Bold test', () => {
  it('Should parse nested text formatters', () => {
    const orgData = '* Hello +*world*+';
    const result = parse(orgData);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          operator [0-2] ("* ")
          text [2-8] ("Hello ")
          crossed [8-17]
            operator [8-9] ("+")
            bold [9-16]
              operator [9-10] ("*")
              text [10-15] ("world")
              operator [15-16] ("*")
            operator [16-17] ("+")
      "
    `);
  });

  it('Should parse multiple opened brackets with text as simple text', () => {
    const orgData = 'Hello [<+*-_world';
    const result = parse(orgData);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("Hello [<+*-_world")
      "
    `);
  });
});
