import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Verbatim', () => {
  it('Should parse verbatim', () => {
    const orgDoc = '=Some sentence=';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        verbatim [0-15]
          operator [0-1] ("=")
          text [1-14] ("Some sentence")
          operator [14-15] ("=")
      "
    `);
  });

  it('Should not parse verbatim that started from equal operator', () => {
    const orgDoc = '=console.log(123)';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("=console.log(123)")
      "
    `);
  });

  it('Should not parse verbatim that contain equal operator at the middle', () => {
    const orgDoc = 'a = 12';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-6]
        text [0-6] ("a = 12")
      "
    `);
  });

  it('Should not parse verbatim that contain equal operator at the end', () => {
    const orgDoc = 'a =';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-3]
        text [0-3] ("a =")
      "
    `);
  });

  it('Should not parse nested formatters inside verbatim', () => {
    const orgDoc = '=*console.log(123)*=';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        verbatim [0-20]
          operator [0-1] ("=")
          text [1-19] ("*console.log(123)*")
          operator [19-20] ("=")
      "
    `);
  });

  it('Should interpret equal operator as verbatim inside nesting formatters', () => {
    const orgDoc = '*=not verbatim=*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-16]
        bold [0-16]
          operator [0-1] ("*")
          verbatim [1-15]
            operator [1-2] ("=")
            text [2-14] ("not verbatim")
            operator [14-15] ("=")
          operator [15-16] ("*")
      "
    `);
  });

  it('Should parse verbatim inside headline', () => {
    const orgDoc = '* =console.log(123)=';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        headline [0-20]
            :level 1:
          title [0-20]
            operator [0-2] ("* ")
            verbatim [2-20]
              operator [2-3] ("=")
              text [3-19] ("console.log(123)")
              operator [19-20] ("=")
      "
    `);
  });

  it('Should not parse verbatim when no space or end/start of line around =', () => {
    const orgDoc = '* =console.log(123)=1';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        headline [0-21]
            :level 1:
          title [0-21]
            operator [0-2] ("* ")
            text [2-21] ("=console.log(123)=1")
      "
    `);
  });
});
