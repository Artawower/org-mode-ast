import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Keyword', () => {
  it('Should parse simple keyword', () => {
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
    // TODO: need to interpret spaces outside of the value
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        keyword [0-28]
          text [0-10] ("#+KEYWORD:")
          text [10-28] (" value with spaces")
      "
    `);
  });

  it('Should parse keyword between other nodes', () => {
    const orgDoc = `* Hello! Amma headline
    #+KEYWORD: some value
And amma text after keyword`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-76]
        headline [0-76]
            :level 1:
          title [0-23]
            operator [0-2] ("* ")
            text [2-22] ("Hello! Amma headline")
            newLine [22-23]
          section [23-76]
            indent [23-27] ("    ")
            keyword [27-48]
              text [27-37] ("#+KEYWORD:")
              text [37-48] (" some value")
            newLine [48-49]
            text [49-76] ("And amma text after keyword")
      "
    `);
  });
});
