import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('InlineCode', () => {
  it('Should parse inline code', () => {
    const orgText = '=console.log(123)=';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-18]
        inlineCode [0-18]
          operator [0-1] ("=")
          text [1-17] ("console.log(123)")
          operator [17-18] ("=")
      "
    `);
  });

  it('Should not parse incline code that started from equal operator', () => {
    const orgText = '=console.log(123)';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-17]
        text [0-17] ("=console.log(123)")
      "
    `);
  });

  it('Should not parse incline code that contain equal operator at the middle', () => {
    const orgText = 'a = 12';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-6]
        text [0-6] ("a = 12")
      "
    `);
  });

  it('Should not parse incline code that contain equal operator at the end', () => {
    const orgText = 'a =';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-3]
        text [0-3] ("a =")
      "
    `);
  });

  it('Should not parse nested formatters inside inline code', () => {
    const orgText = '=*console.log(123)*=';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-20]
        inlineCode [0-20]
          operator [0-1] ("=")
          text [1-19] ("*console.log(123)*")
          operator [19-20] ("=")
      "
    `);
  });

  it('Should not interpret equal operator as inline code inside nesting formatters', () => {
    const orgText = '*=not inline code=*';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-19]
        bold [0-19]
          operator [0-1] ("*")
          text [1-18] ("=not inline code=")
          operator [18-19] ("*")
      "
    `);
  });

  it('Should parse inline code inside headline', () => {
    const orgText = '* =console.log(123)=';
    const result = parse(orgText);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-20]
        headline [0-20]
            :level 1:
          operator [0-2] ("* ")
          inlineCode [2-20]
            operator [2-3] ("=")
            text [3-19] ("console.log(123)")
            operator [19-20] ("=")
      "
    `);
  });
});
