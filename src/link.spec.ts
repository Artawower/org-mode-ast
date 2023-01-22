import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Link test', () => {
  it('Should parse simple link', () => {
    const orgData = `[[https://google.com][Google]]`;
    const result = prettyTreePrint(parse(orgData));
    expect(result).toMatchInlineSnapshot(`
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
    const orgData = `[[https://google.com]]`;
    const result = prettyTreePrint(parse(orgData));
    expect(result).toMatchInlineSnapshot(`
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
    const orgData = `[[https://google.com][Google]] [[https://google.com][Google]]`;
    const result = prettyTreePrint(parse(orgData));
    expect(result).toMatchInlineSnapshot(`
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
    const orgData = `Some +*text* [[https://google.com][Google]]+ some text`;
    const result = prettyTreePrint(parse(orgData));
    expect(result).toMatchInlineSnapshot(`
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
});
