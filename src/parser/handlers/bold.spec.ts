import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Bold test', () => {
  it('Should not parse text as bold with single asterisk', () => {
    const orgDoc = 'Hello *world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("Hello *world")
      "
    `);
  });

  it('Should not parse bold text started from single asterisk', () => {
    const orgDoc = '*Not a bold text';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-16]
        text [0-16] ("*Not a bold text")
      "
    `);
  });

  it('Should not parse text as bold with asterisk at the end', () => {
    const orgDoc = 'Hello world*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("Hello world*")
      "
    `);
  });

  it('Should not parse text as bold with another bracket symbols', () => {
    const orgDoc = 'Hello *+[world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        text [0-14] ("Hello *+[world")
      "
    `);
  });

  it('should parse bold text', () => {
    const orgDoc = '*Hello world*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-13]
        bold [0-13]
          operator [0-1] ("*")
          text [1-12] ("Hello world")
          operator [12-13] ("*")
      "
    `);
  });

  it('Should parse bold text with intersection of other pair tokens', () => {
    const orgDoc = '*Hello +world*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        bold [0-14]
          operator [0-1] ("*")
          text [1-13] ("Hello +world")
          operator [13-14] ("*")
      "
    `);
  });

  it('Should parse bold text from headline', () => {
    const orgDoc = '* Hello *world*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        headline [0-15]
            :level 1:
          title [0-15]
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
    const orgDoc = `* Hello world
** Hello *world*`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-30]
        headline [0-30]
            :level 1:
          title [0-14]
            operator [0-2] ("* ")
            text [2-13] ("Hello world")
            newLine [13-14]
          section [14-30]
            headline [14-30]
                :level 2:
              title [14-30]
                operator [14-17] ("** ")
                text [17-23] ("Hello ")
                bold [23-30]
                  operator [23-24] ("*")
                  text [24-29] ("world")
                  operator [29-30] ("*")
      "
    `);
  });

  it('Should not parse bold with that started from brackets symbols', () => {
    const orgDoc = `* Hello +[*world*`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-17]
            operator [0-2] ("* ")
            text [2-17] ("Hello +[*world*")
      "
    `);
  });

  it('Should parse bold text which started from the beginning of the document', () => {
    const orgDoc = `*Hello*`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-7]
        bold [0-7]
          operator [0-1] ("*")
          text [1-6] ("Hello")
          operator [6-7] ("*")
      "
    `);
  });

  it('Should parse bold text after new line', () => {
    const orgDoc = `
*Hello*`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-8]
        newLine [0-1]
        bold [1-8]
          operator [1-2] ("*")
          text [2-7] ("Hello")
          operator [7-8] ("*")
      "
    `);
  });

  it('Should not parse bold for wrong slash at the end', () => {
    const orgText = `*Not italic *`;
    const result = parse(orgText);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-13]
        text [0-13] ("*Not italic *")
      "
    `);
  });
});
