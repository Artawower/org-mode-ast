import { parse } from 'parser/parser';
import { hasNodeIncorrectRanges } from 'test-helper';

describe('Inline html', () => {
  it('Should parse inline html blocks', () => {
    const orgDoc = `#+html: <div class="test">Test</div>
#+html: <div class="test">Test</div>`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-73]
        inlineHtml [0-73]
          keyword [0-36]
            text [0-7] ("#+html:")
            text [7-36] (" <div class=\\"test\\">Test</div>")
          newLine [36-37]
          keyword [37-73]
            text [37-44] ("#+html:")
            text [44-73] (" <div class=\\"test\\">Test</div>")
      "
    `);
  });

  it('Should parse inline html inside nested nodes block', () => {
    const orgDoc = `* Hello amma headline
    *With bold content* And some text.
#+HTML: <div>Hello</div>
#+HTML: <a href="https://google.com" target="_blank">link</a>"`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-148]
        headline [0-148]
            :level 1:
          title [0-22]
            operator [0-2] ("* ")
            text [2-21] ("Hello amma headline")
            newLine [21-22]
          section [22-148]
            indent [22-26] ("    ")
            bold [26-45]
              operator [26-27] ("*")
              text [27-44] ("With bold content")
              operator [44-45] ("*")
            text [45-60] (" And some text.")
            newLine [60-61]
            inlineHtml [61-148]
              keyword [61-85]
                text [61-68] ("#+HTML:")
                text [68-85] (" <div>Hello</div>")
              newLine [85-86]
              keyword [86-148]
                text [86-93] ("#+HTML:")
                text [93-148] (" <a href=\\"https://google.com\\" target=\\"_blank\\">link</a>\\"")
      "
    `);
  });

  it('Should parse multiple inline html blocks', () => {
    const orgDoc = `#+HTML: <div>Hello</div>
    * Hello world
    - List item
    #+html: <div>Hello</div>`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-87]
        inlineHtml [0-25]
          keyword [0-24]
            text [0-7] ("#+HTML:")
            text [7-24] (" <div>Hello</div>")
          newLine [24-25]
        indent [25-29] ("    ")
        text [29-42] ("* Hello world")
        newLine [42-43]
        list [43-87]
            :unordered:
            :level 2:
          listItem [43-87]
            title [43-59]
              indent [43-47] ("    ")
              operator [47-49] ("- ")
              text [49-58] ("List item")
              newLine [58-59]
            section [59-87]
              indent [59-63] ("    ")
              inlineHtml [63-87]
                keyword [63-87]
                  text [63-70] ("#+html:")
                  text [70-87] (" <div>Hello</div>")
      "
    `);
  });
});
