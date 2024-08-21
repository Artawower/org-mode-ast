import { hasNodeIncorrectRanges } from '../../test-helper';
import { parse } from '../parser';

describe('Properties', () => {
  it('Should parse properties', () => {
    const orgDoc = `:PROPERTIES:
:ID: headline-test2
:PUBLISHED: true
:END:`;

    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-55]
          :id  headline-test2:
          :published  true:
        propertyDrawer [0-55]
          property [0-12]
            text [0-12] (":PROPERTIES:")
          newLine [12-13]
          property [13-32]
            text [13-18] (":ID: ")
            text [18-32] ("headline-test2")
          newLine [32-33]
          property [33-49]
            text [33-45] (":PUBLISHED: ")
            text [45-49] ("true")
          newLine [49-50]
          property [50-55]
            text [50-55] (":END:")
      "
    `);
  });

  it('Should not parse properties if not closed', () => {
    const orgDoc = `:PROPERTIES:`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-12]
        property [0-12]
          text [0-12] (":PROPERTIES:")
      "
    `);
  });

  it('Should not parse property drawer if parent not root or headline', () => {
    const orgDoc = `Some text that ruine the property drawer
:PROPERTIES:
:ID: headline-test2
:PUBLISHED: true
:END:`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-96]
        text [0-40] ("Some text that ruine the property drawer")
        newLine [40-41]
        property [41-53]
          text [41-53] (":PROPERTIES:")
        newLine [53-54]
        property [54-73]
          text [54-59] (":ID: ")
          text [59-73] ("headline-test2")
        newLine [73-74]
        property [74-90]
          text [74-86] (":PUBLISHED: ")
          text [86-90] ("true")
        newLine [90-91]
        property [91-96]
          text [91-96] (":END:")
      "
    `);
  });

  it('Should parse property drawer with headline', () => {
    const orgDoc = `* Headline
:PROPERTIES:
:Title:     Goldberg Variations
:Composer:  J.S. Bach
:Publisher: Deutsche Grammophon
:NDisks:    1
:END:`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-129]
        headline [0-129]
            :level 1:
            :title      Goldberg Variations:
            :composer   J.S. Bach:
            :publisher  Deutsche Grammophon:
            :ndisks     1:
          title [0-11]
            operator [0-2] ("* ")
            text [2-10] ("Headline")
            newLine [10-11]
          section [11-129]
            propertyDrawer [11-129]
              property [11-23]
                text [11-23] (":PROPERTIES:")
              newLine [23-24]
              property [24-55]
                text [24-32] (":Title: ")
                text [32-55] ("    Goldberg Variations")
              newLine [55-56]
              property [56-77]
                text [56-67] (":Composer: ")
                text [67-77] (" J.S. Bach")
              newLine [77-78]
              property [78-109]
                text [78-90] (":Publisher: ")
                text [90-109] ("Deutsche Grammophon")
              newLine [109-110]
              property [110-123]
                text [110-119] (":NDisks: ")
                text [119-123] ("   1")
              newLine [123-124]
              property [124-129]
                text [124-129] (":END:")
      "
    `);
  });

  it('Should parse single line property', () => {
    const orgDoc = `#+PROPERTY: header-args :tangle no`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-34]
          :header-args :tangle no:
        keyword [0-34]
          text [0-12] ("#+PROPERTY: ")
          text [12-24] ("header-args ")
          blockProperty [24-34]
            text [24-31] (":tangle")
            text [31-34] (" no")
      "
    `);
  });

  it('Should parse multiple properties with conflict', () => {
    const orgDoc = `#+PROPERTY: header-args :tangle no
  #+PROPERTY: header-args :tangle yes`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-72]
          :header-args :tangle yes:
        keyword [0-72]
          text [0-12] ("#+PROPERTY: ")
          text [12-24] ("header-args ")
          blockProperty [24-34]
            text [24-31] (":tangle")
            text [31-34] (" no")
          newLine [34-35]
          indent [35-37] ("  ")
          keyword [37-72]
            text [37-49] ("#+PROPERTY: ")
            text [49-61] ("header-args ")
            blockProperty [61-72]
              text [61-68] (":tangle")
              text [68-72] (" yes")
      "
    `);
  });

  it('Should parse properties started with long space', () => {
    const orgDoc = `:PROPERTIES:
:ID:       c9eda354-3eb2-472c-818c-7167158be782
:END: 
#+title: Garten Klone

<2024-06-06 Thu>
-
-

<2024-08-13 Tue>
-
`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-132]
          :id        c9eda354-3eb2-472c-818c-7167158be782:
        propertyDrawer [0-67]
          property [0-12]
            text [0-12] (":PROPERTIES:")
          newLine [12-13]
          property [13-60]
            text [13-18] (":ID: ")
            text [18-60] ("      c9eda354-3eb2-472c-818c-7167158be782")
          newLine [60-61]
          property [61-67]
            text [61-67] (":END: ")
        newLine [67-68]
        keyword [68-89]
          text [68-77] ("#+title: ")
          text [77-89] ("Garten Klone")
        newLine [89-90]
        newLine [90-91]
        date [91-107]
          operator [91-92] ("<")
          text [92-106] ("2024-06-06 Thu")
          operator [106-107] (">")
        newLine [107-108]
        text [108-109] ("-")
        newLine [109-110]
        text [110-111] ("-")
        newLine [111-112]
        newLine [112-113]
        date [113-129]
          operator [113-114] ("<")
          text [114-128] ("2024-08-13 Tue")
          operator [128-129] (">")
        newLine [129-130]
        text [130-131] ("-")
        newLine [131-132]
      "
    `);
  });
});
