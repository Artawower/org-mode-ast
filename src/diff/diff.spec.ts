import { parse } from 'parser';
import { getDiff } from './diff';

describe('diff', () => {
  it('Should collect diffs when add symbol', () => {
    const oldDoc = `*Some text
+qwe+`;
    const newDoc = `*Some text*
+qwe+`;

    const diffs = getDiff(newDoc, oldDoc);

    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`
      [
        "bold [0-11]
        operator [0-1] ("*")
        text [1-10] ("Some text")
        operator [10-11] ("*")
      ",
      ]
    `);
  });

  it('Should not collect diffs inside headline', () => {
    const oldDoc = `* Headline
+crossed+`;

    const newDoc = `* Headline
+crossed+`;

    const diffs = getDiff(newDoc, oldDoc);
    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`[]`);
  });

  it('Should find diff after delete some record', () => {
    const oldDoc = `*bold +crossed+*`;
    const newDoc = `+crossed+`;

    const diffs = getDiff(newDoc, oldDoc);
    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`
      [
        "crossed [0-9]
        operator [0-1] ("+")
        text [1-8] ("crossed")
        operator [8-9] ("+")
      ",
      ]
    `);
  });

  it('Should find diff when delete one operator', () => {
    const oldDoc = `* Headline 1
*bold*
** Headline 2
+crossed+`;

    const newDoc = `* Headline 1
*bold
** Headline 2
+crossed+`;

    const diffs = getDiff(newDoc, oldDoc);
    expect(parse(oldDoc).toString()).toMatchInlineSnapshot(`
      "root [0-43]
        headline [0-43]
            :level 1:
          title [0-13]
            operator [0-2] ("* ")
            text [2-12] ("Headline 1")
            newLine [12-13]
          section [13-43]
            bold [13-19]
              operator [13-14] ("*")
              text [14-18] ("bold")
              operator [18-19] ("*")
            newLine [19-20]
            headline [20-43]
                :level 2:
              title [20-34]
                operator [20-23] ("** ")
                text [23-33] ("Headline 2")
                newLine [33-34]
              section [34-43]
                crossed [34-43]
                  operator [34-35] ("+")
                  text [35-42] ("crossed")
                  operator [42-43] ("+")
      "
    `);

    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`
      [
        "text [13-18] ("*bold")
      ",
      ]
    `);
  });

  it('Should find diff when add symbols into existing node', () => {
    const oldDoc = `*bold one 1*`;
    const newDoc = `*bold one 12345678910*`;

    const diffs = getDiff(newDoc, oldDoc);
    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`
      [
        "bold [0-22]
        operator [0-1] ("*")
        text [1-21] ("bold one 12345678910")
        operator [21-22] ("*")
      ",
      ]
    `);
  });
});
