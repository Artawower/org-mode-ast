import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Link test', () => {
  it('Should parse simple link', () => {
    const orgDoc = `[[https://google.com][Google]]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-30]
        link [0-30]
          operator [0-1] ("[")
          linkUrl [1-21]
            operator [1-2] ("[")
            text [2-20] ("https://google.com")
            operator [20-21] ("]")
          linkName [21-29]
            operator [21-22] ("[")
            text [22-28] ("Google")
            operator [28-29] ("]")
          operator [29-30] ("]")
      "
    `);
  });

  it('Should parse link without name', () => {
    const orgDoc = `[[https://google.com]]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-22]
        link [0-22]
          operator [0-1] ("[")
          linkUrl [1-21]
            operator [1-2] ("[")
            text [2-20] ("https://google.com")
            operator [20-21] ("]")
          operator [21-22] ("]")
      "
    `);
  });

  it('Should parse few links', () => {
    const orgDoc = `[[https://google.com][Google]] [[https://google.com][Google]]`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-61]
        link [0-30]
          operator [0-1] ("[")
          linkUrl [1-21]
            operator [1-2] ("[")
            text [2-20] ("https://google.com")
            operator [20-21] ("]")
          linkName [21-29]
            operator [21-22] ("[")
            text [22-28] ("Google")
            operator [28-29] ("]")
          operator [29-30] ("]")
        text [30-31] (" ")
        link [31-61]
          operator [31-32] ("[")
          linkUrl [32-52]
            operator [32-33] ("[")
            text [33-51] ("https://google.com")
            operator [51-52] ("]")
          linkName [52-60]
            operator [52-53] ("[")
            text [53-59] ("Google")
            operator [59-60] ("]")
          operator [60-61] ("]")
      "
    `);
  });

  it('Should parse link inside nested structure in center of text', () => {
    const orgDoc = `Some +*text* [[https://google.com][Google]]+ some text`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-54]
        text [0-5] ("Some ")
        crossed [5-44]
          operator [5-6] ("+")
          bold [6-12]
            operator [6-7] ("*")
            text [7-11] ("text")
            operator [11-12] ("*")
          text [12-13] (" ")
          link [13-43]
            operator [13-14] ("[")
            linkUrl [14-34]
              operator [14-15] ("[")
              text [15-33] ("https://google.com")
              operator [33-34] ("]")
            linkName [34-42]
              operator [34-35] ("[")
              text [35-41] ("Google")
              operator [41-42] ("]")
            operator [42-43] ("]")
          operator [43-44] ("+")
        text [44-54] (" some text")
      "
    `);
  });

  it('Should parse link between other structures', () => {
    const orgDoc = `Some *bold text* and +crossed text+ [[https://google.com][Google]] ~some code~`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-78]
        text [0-5] ("Some ")
        bold [5-16]
          operator [5-6] ("*")
          text [6-15] ("bold text")
          operator [15-16] ("*")
        text [16-21] (" and ")
        crossed [21-35]
          operator [21-22] ("+")
          text [22-34] ("crossed text")
          operator [34-35] ("+")
        text [35-36] (" ")
        link [36-66]
          operator [36-37] ("[")
          linkUrl [37-57]
            operator [37-38] ("[")
            text [38-56] ("https://google.com")
            operator [56-57] ("]")
          linkName [57-65]
            operator [57-58] ("[")
            text [58-64] ("Google")
            operator [64-65] ("]")
          operator [65-66] ("]")
        text [66-67] (" ")
        inlineCode [67-78]
          operator [67-68] ("~")
          text [68-77] ("some code")
          operator [77-78] ("~")
      "
    `);
  });

  it('Should not parse as link', () => {
    const orgDoc = `[https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/]`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-51]
        text [0-51] ("[https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/]")
      "
    `);
  });

  it('Should parse link with slashes', () => {
    const orgDoc = `[[https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/][Emacs lisp за 15 минут]]`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-77]
        link [0-77]
          operator [0-1] ("[")
          linkUrl [1-52]
            operator [1-2] ("[")
            text [2-51] ("https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/")
            operator [51-52] ("]")
          linkName [52-76]
            operator [52-53] ("[")
            text [53-75] ("Emacs lisp за 15 минут")
            operator [75-76] ("]")
          operator [76-77] ("]")
      "
    `);
  });

  it('Should not parse bracketed text as link', () => {
    const orgDoc = `[\\w\\[:digit]]+`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        text [0-12] ("[\\\\w\\\\[:digit]")
      "
    `);
  });

  it('Should not parse link and should merge text with previous result', () => {
    const orgDoc = `$[\\w\\[:digit]]`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        text [0-14] ("$[\\\\w\\\\[:digit]]")
      "
    `);
  });

  it('Should not parse multiple bracket nodes as link', () => {
    const orgDoc = `$[\\w\\[:digit]]+ = [\\d[:digit:]]$`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-32]
        latexFragment [0-32]
          operator [0-1] ("$")
          text [1-31] ("[\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]]")
          operator [31-32] ("$")
      "
    `);
  });
});
