import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Crossed tests', () => {
  it('Should parse crossed text with crossed tokens', () => {
    const orgDoc = `+Crossed text+`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        crossed [0-14]
          operator [0-1] ("+")
          text [1-13] ("Crossed text")
          operator [13-14] ("+")
      "
    `);
  });

  it('Should not parse text as crossed when it starts from single plus', () => {
    const orgDoc = '+Not a crossed text';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        text [0-19] ("+Not a crossed text")
      "
    `);
  });

  it('Should parse crossed text inside headline', () => {
    const orgDoc = `* Hello +world+`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        headline [0-15]
            :level 1:
          title [0-15]
            operator [0-2] ("* ")
            text [2-8] ("Hello ")
            crossed [8-15]
              operator [8-9] ("+")
              text [9-14] ("world")
              operator [14-15] ("+")
      "
    `);
  });

  it('Should not split text inside property drawer', () => {
    const orgDoc = `#+DESCRIPTION: Easy way to configure automatic dark mode for kitty +macos`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-73]
        keyword [0-73]
          text [0-14] ("#+DESCRIPTION:")
          text [14-73] (" Easy way to configure automatic dark mode for kitty +macos")
      "
    `);
  });
});
