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
          text [0-11] ("#+KEYWORD: ")
          text [11-16] ("value")
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
          text [0-11] ("#+KEYWORD: ")
          text [11-28] ("value with spaces")
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
              text [27-38] ("#+KEYWORD: ")
              text [38-48] ("some value")
            newLine [48-49]
            text [49-76] ("And amma text after keyword")
      "
    `);
  });

  // TODO: think about complex keyword structure
  it('Should parse keyword with tag list', () => {
    const orgData = `#+FILETAGS: :tag1:tag2:tag3:`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        keyword [0-28]
          text [0-12] ("#+FILETAGS: ")
          tagList [12-28]
            operator [12-13] (":")
            text [13-17] ("tag1")
            operator [17-18] (":")
            text [18-22] ("tag2")
            operator [22-23] (":")
            text [23-27] ("tag3")
            operator [27-28] (":")
      "
    `);
  });

  // TODO: master uncomment after tokenizer will be changed
  // to correct handle tags with delimiters
  xit('Should parse filetags with properties tags', () => {
    const orgDoc = `#+FILETAGS: :test:first big note:hello world:`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-45]
        keyword [0-12]
          text [0-11] ("#+FILETAGS:")
          text [11-12] (" ")
        text [12-17] (":test")
        blockProperty [17-45]
          text [17-23] (":first")
          text [23-45] (" big note:hello world:")
      "
    `);
  });

  it('Tags list parging should not have conflict with src block', () => {
    const orgDoc = `#+BEGIN_SRC js :tangle no :exports none
    console.log('Hello world!');
    #+END_SRC`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-86]
        srcBlock [0-86]
            :language js:
          blockHeader [0-39]
            keyword [0-15]
                :language js:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-15] ("js ")
            blockProperty [15-26]
              text [15-22] (":tangle")
              text [22-26] (" no ")
            blockProperty [26-39]
              text [26-34] (":exports")
              text [34-39] (" none")
          newLine [39-40]
          blockBody [40-73]
            text [40-73] ("    console.log('Hello world!');\\n")
          blockFooter [73-86]
            indent [73-77] ("    ")
            keyword [77-86]
              text [77-86] ("#+END_SRC")
      "
    `);
  });

  it('Should not have conflict with elisp code which contains colon operator', () => {
    const orgDoc = `#+BEGIN_SRC emacs-lisp
    (message "Name %s, middle name %s" (plist-get args :name) (plist-get args :middle-name))
    #+END_SRC`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-129]
        srcBlock [0-129]
            :language emacs-lisp:
          blockHeader [0-22]
            keyword [0-22]
                :language emacs-lisp:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-22] ("emacs-lisp")
          newLine [22-23]
          blockBody [23-116]
            text [23-116] ("    (message \\"Name %s, middle name %s\\" (plist-get args :name) (plist-get args :middle-name))\\n")
          blockFooter [116-129]
            indent [116-120] ("    ")
            keyword [120-129]
              text [120-129] ("#+END_SRC")
      "
    `);
  });

  it('Should not break src block with colons', () => {
    const orgDoc = `#+BEGIN_SRC emacs-lisp
    (setq test (map-delete test :hi))
    #+END_SRC`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-74]
        srcBlock [0-74]
            :language emacs-lisp:
          blockHeader [0-22]
            keyword [0-22]
                :language emacs-lisp:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-22] ("emacs-lisp")
          newLine [22-23]
          blockBody [23-61]
            text [23-61] ("    (setq test (map-delete test :hi))\\n")
          blockFooter [61-74]
            indent [61-65] ("    ")
            keyword [65-74]
              text [65-74] ("#+END_SRC")
      "
    `);
  });
});
