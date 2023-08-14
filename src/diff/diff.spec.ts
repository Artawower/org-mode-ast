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
    expect(diffs.map((d) => d.toString())).toMatchInlineSnapshot(`
      [
        "text [13-18] ("*bold")
      ",
      ]
    `);
  });
});
