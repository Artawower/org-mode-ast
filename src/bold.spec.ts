import { parse } from './parser';

import { prettyTreePrint } from './tools';

describe('Bold test', () => {
  it('Should not parse text as bold with single asterisk', () => {
    const headline = 'Hello *world';
    const result = parse(headline);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("Hello *world")
      "
    `);
  });

  it('Should not parse bold text started from single asterisk', () => {
    const orgText = '*Not a bold text';
    const result = parse(orgText);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-16]
        text [0-16] ("*Not a bold text")
      "
    `);
  });

  it('Should not parse text as bold with asterisk at the end', () => {
    const headline = 'Hello world*';
    const result = parse(headline);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("Hello world*")
      "
    `);
  });

  it('Should not parse text as bold with another bracket symbols', () => {
    const headline = 'Hello *+[world';
    const result = parse(headline);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-14]
        text [0-14] ("Hello *+[world")
      "
    `);
  });

  it('should parse bold text', () => {
    const orgData = '*Hello world*';
    const result = parse(orgData);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-13]
        bold [0-13]
          operator [0-1] ("*")
          text [1-12] ("Hello world")
          operator [12-13] ("*")
      "
    `);
  });

  it('Should parse bold text with intersection of other pair tokens', () => {
    const orgData = '*Hello +world*';
    const result = parse(orgData);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-14]
        bold [0-14]
          operator [0-1] ("*")
          text [1-13] ("Hello ")
          operator [13-14] ("*")
      "
    `);
  });

  it('Should parse bold text from headline', () => {
    const orgData = '* Hello *world*';
    const result = parse(orgData);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-15]
        headline [0-15]
            :level 1:
          operator [0-2] ("* ")
          text [2-8] ("Hello ")
          bold [8-15]
            operator [8-9] ("*")
            text [9-14] ("world")
            operator [14-15] ("*")
      "
    `);
  });

  it('Should parse bold text inside nested headline', () => {
    const orgData = `* Hello world
** Hello *world*`;
    const result = parse(orgData);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-30]
        headline [0-14]
            :level 1:
          operator [0-2] ("* ")
          text [2-13] ("Hello world")
          newLine [13-14]
          section [14-30]
            headline [14-30]
                :level 2:
              operator [14-17] ("** ")
              text [17-23] ("Hello ")
              bold [23-30]
                operator [23-24] ("*")
                text [24-29] ("world")
                operator [29-30] ("*")
      "
    `);
  });

  it('Should parse bold with that started from brackets symbols', () => {
    const orgData = `* Hello +[*world*`;
    const result = prettyTreePrint(parse(orgData));

    expect(result).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          operator [0-2] ("* ")
          text [2-10] ("Hello +[")
          bold [10-17]
            operator [10-11] ("*")
            text [11-16] ("world")
            operator [16-17] ("*")
      "
    `);
  });
});
