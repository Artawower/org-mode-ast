import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Keyword', () => {
  it('Should parse simple keyword', () => {
    // TODO: make split keyword into
    // #+ - operator
    // text
    // text

    const orgDoc = `#+KEYWORD: value`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-16]
        keyword [0-16]
          text [0-10] ("#+KEYWORD:")
          text [10-16] (" value")
      "
    `);
  });

  it('Should parse keyword without value', () => {
    const orgDoc = `#+KEYWORD:`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-10]
        keyword [0-10]
          text [0-10] ("#+KEYWORD:")
      "
    `);
  });

  it('Should parse keyword with value that contains spaces', () => {
    const orgDoc = `#+KEYWORD: value with spaces`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        keyword [0-28]
          text [0-10] ("#+KEYWORD:")
          text [10-28] (" value with spaces")
      "
    `);
  });
});
