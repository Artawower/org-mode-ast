import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Crossed tests', () => {
  it('Should parse crossed text with crossed tokens', () => {
    const orgData = `+Crossed text+`;
    const result = parse(orgData);
    // console.log(prettyTreePrint(result));

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-14]
        crossed [0-14]
          operator [0-1] ("+")
          text [1-13] ("Crossed text")
          operator [13-14] ("+")
      "
    `);
  });

  it('Should not parse text as crossed when it starts from single plus', () => {
    const orgText = '+Not a crossed text';
    const result = parse(orgText);
    // console.log(prettyTreePrint(result));

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-19]
        text [0-19] ("+Not a crossed text")
      "
    `);
  });

  it('Should parse crossed text inside headline', () => {
    const orgData = `* Hello +world+`;
    const result = parse(orgData);
    // console.log(prettyTreePrint(result));

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-15]
        headline [0-15]
            :level 1:
          operator [0-2] ("* ")
          text [2-8] ("Hello ")
          crossed [8-15]
            operator [8-9] ("+")
            text [9-14] ("world")
            operator [14-15] ("+")
      "
    `);
  });
});
