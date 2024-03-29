import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('InlineCode', () => {
  it('Should parse inline code', () => {
    const orgDoc = '~console.log(123)~';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-18]
        inlineCode [0-18]
          operator [0-1] ("~")
          text [1-17] ("console.log(123)")
          operator [17-18] ("~")
      "
    `);
  });

  it('Should not parse incline code that started from tilde operator', () => {
    const orgDoc = '~console.log(123)';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("~console.log(123)")
      "
    `);
  });

  it('Should not parse incline code that contain tilde operator at the middle', () => {
    const orgDoc = 'a ~ 12';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-6]
        text [0-6] ("a ~ 12")
      "
    `);
  });

  it('Should not parse incline code that contain tilde operator at the end', () => {
    const orgDoc = 'a ~';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-3]
        text [0-3] ("a ~")
      "
    `);
  });

  it('Should not parse nested formatters inside inline code', () => {
    const orgDoc = '~*console.log(123)*~';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        inlineCode [0-20]
          operator [0-1] ("~")
          text [1-19] ("*console.log(123)*")
          operator [19-20] ("~")
      "
    `);
  });

  it('Should interpret tilde operator as inline code inside nesting formatters', () => {
    const orgDoc = '*~not inline code~*';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        bold [0-19]
          operator [0-1] ("*")
          inlineCode [1-18]
            operator [1-2] ("~")
            text [2-17] ("not inline code")
            operator [17-18] ("~")
          operator [18-19] ("*")
      "
    `);
  });

  it('Should parse inline code inside headline', () => {
    const orgDoc = '* ~console.log(123)~';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        headline [0-20]
            :level 1:
          title [0-20]
            operator [0-2] ("* ")
            inlineCode [2-20]
              operator [2-3] ("~")
              text [3-19] ("console.log(123)")
              operator [19-20] ("~")
      "
    `);
  });

  it('Should parse complex document from real world 2', () => {
    const orgDoc = `1: ~{ hey }~`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-3] ("1: ")
        inlineCode [3-12]
          operator [3-4] ("~")
          text [4-11] ("{ hey }")
          operator [11-12] ("~")
      "
    `);
  });

  fit('Should parse inline code after new line', () => {
    const orgDoc = `The text beloc is inline code
=-hey=`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-36]
        text [0-29] ("The text beloc is inline code")
        newLine [29-30]
        verbatim [30-36]
          operator [30-31] ("=")
          text [31-35] ("-hey")
          operator [35-36] ("=")
      "
    `);
  });
});
