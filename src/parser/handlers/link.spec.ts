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
            :linkType network:
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
            :linkType network:
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
            :linkType network:
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
            :linkType network:
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
              :linkType network:
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
            :linkType network:
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
            :linkType network:
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

  it('Should parse file link', () => {
    const orgDoc = `[[file:~/Documents/notes.org][Notes]]`;
    const result = parse(orgDoc);
    hasNodeIncorrectRanges(result, orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        link [0-37]
            :linkType file:
          operator [0-1] ("[")
          linkUrl [1-29]
            operator [1-2] ("[")
            text [2-28] ("file:~/Documents/notes.org")
            operator [28-29] ("]")
          linkName [29-36]
            operator [29-30] ("[")
            text [30-35] ("Notes")
            operator [35-36] ("]")
          operator [36-37] ("]")
      "
    `);
  });

  it('Should parse id link', () => {
    const orgDoc = `[[id:1234567890][Notes]]`;
    const result = parse(orgDoc);
    hasNodeIncorrectRanges(result, orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-24]
        link [0-24]
            :linkType id:
          operator [0-1] ("[")
          linkUrl [1-16]
            operator [1-2] ("[")
            text [2-15] ("id:1234567890")
            operator [15-16] ("]")
          linkName [16-23]
            operator [16-17] ("[")
            text [17-22] ("Notes")
            operator [22-23] ("]")
          operator [23-24] ("]")
      "
    `);
  });

  it('Should parse image link', () => {
    const orgDoc = `[[./image.png]]`;
    const result = parse(orgDoc);
    hasNodeIncorrectRanges(result, orgDoc);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        link [0-15]
            :linkType image:
          operator [0-1] ("[")
          linkUrl [1-14]
            operator [1-2] ("[")
            text [2-13] ("./image.png")
            operator [13-14] ("]")
          operator [14-15] ("]")
      "
    `);
  });

  it('Should correct parse image link from complex org document', () => {
    const orgDoc = `:PROPERTIES:
:ID: tmp_bucket
:END:
#+TITLE: Черный ящик ( ͡° ͜ʖ ͡°)
#+DESCRIPTION: Временное хранилище для информации к изучению!
#+STARTUP: show2levels
#+STARTUP: inlineimages
#+FILETAGS: :bucket:временное:blackbox:
#+ACTIVE:
# 13[[img https://www.befunky.com/images/wp/wp-2014-08-milky-way-1023340_1280.jpg?auto=webp&format=jpg&width=1750&crop=16:9]]

[[./space-ca302762-d65b-4e3c-b691-20c29b822bdf.jpeg]]
`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-408]
          :id  tmp_bucket:
        propertyDrawer [0-34]
          property [0-12]
            text [0-12] (":PROPERTIES:")
          newLine [12-13]
          property [13-28]
            text [13-17] (":ID:")
            text [17-28] (" tmp_bucket")
          newLine [28-29]
          property [29-34]
            text [29-34] (":END:")
        newLine [34-35]
        keyword [35-67]
          text [35-43] ("#+TITLE:")
          text [43-67] (" Черный ящик ( ͡° ͜ʖ ͡°)")
        newLine [67-68]
        keyword [68-129]
          text [68-82] ("#+DESCRIPTION:")
          text [82-129] (" Временное хранилище для информации к изучению!")
        newLine [129-130]
        keyword [130-152]
          text [130-140] ("#+STARTUP:")
          text [140-152] (" show2levels")
        newLine [152-153]
        keyword [153-176]
          text [153-163] ("#+STARTUP:")
          text [163-176] (" inlineimages")
        newLine [176-177]
        keyword [177-216]
          text [177-188] ("#+FILETAGS:")
          text [188-189] (" ")
          tagList [189-216]
            operator [189-190] (":")
            text [190-196] ("bucket")
            operator [196-197] (":")
            text [197-206] ("временное")
            operator [206-207] (":")
            text [207-215] ("blackbox")
            operator [215-216] (":")
        newLine [216-217]
        keyword [217-226]
          text [217-226] ("#+ACTIVE:")
        newLine [226-227]
        comment [227-352]
          operator [227-228] ("#")
          text [228-352] (" 13[[img https://www.befunky.com/images/wp/wp-2014-08-milky-way-1023340_1280.jpg?auto=webp&format=jpg&width=1750&crop=16:9]]")
        newLine [352-353]
        newLine [353-354]
        link [354-407]
            :linkType image:
          operator [354-355] ("[")
          linkUrl [355-406]
            operator [355-356] ("[")
            text [356-405] ("./space-ca302762-d65b-4e3c-b691-20c29b822bdf.jpeg")
            operator [405-406] ("]")
          operator [406-407] ("]")
        newLine [407-408]
      "
    `);
  });
});
