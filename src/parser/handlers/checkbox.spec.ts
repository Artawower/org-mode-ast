import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Checkbox tests', () => {
  it('Should parse checkboxed headline', () => {
    const orgDoc = `* [ ] Hello world`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-17]
            operator [0-2] ("* ")
            checkbox [2-5] ("[ ]")
                :unchecked:
              operator [2-3] ("[")
              text [3-4] (" ")
              operator [4-5] ("]")
            text [5-17] (" Hello world")
      "
    `);
  });

  it('Should parse checked checkboxed headline', () => {
    const orgDoc = `* [X] Hello world`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-17]
            operator [0-2] ("* ")
            checkbox [2-5] ("[X]")
                :checked:
              operator [2-3] ("[")
              text [3-4] ("X")
              operator [4-5] ("]")
            text [5-17] (" Hello world")
      "
    `);
  });

  it('Should parse checked checkboxed headline with nested bold text', () => {
    const orgDoc = `* [X] Hello *world*`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        headline [0-19]
            :level 1:
          title [0-19]
            operator [0-2] ("* ")
            checkbox [2-5] ("[X]")
                :checked:
              operator [2-3] ("[")
              text [3-4] ("X")
              operator [4-5] ("]")
            text [5-12] (" Hello ")
            bold [12-19]
              operator [12-13] ("*")
              text [13-18] ("world")
              operator [18-19] ("*")
      "
    `);
  });
});
