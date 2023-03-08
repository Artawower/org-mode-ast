import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Headline tests', () => {
  // Headline tests start
  it('should parse first level headline', () => {
    const orgDoc = '* Hello world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-13]
        headline [0-13]
            :level 1:
          title [0-13]
            operator [0-2] ("* ")
            text [2-13] ("Hello world")
      "
    `);
  });

  it('Should parse headline with long start space', () => {
    const orgDoc = '*        Hello world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        headline [0-20]
            :level 1:
          title [0-20]
            operator [0-2] ("* ")
            text [2-20] ("       Hello world")
      "
    `);
  });

  it('Should not parse text with start space and asterisk as headline', () => {
    const orgDoc = ' * Hello world';
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        indent [0-1] (" ")
        text [1-14] ("* Hello world")
      "
    `);
  });

  it('Should parse nested section for headline', () => {
    const orgDoc = `* Title
some text`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-8]
            operator [0-2] ("* ")
            text [2-7] ("Title")
            newLine [7-8]
          section [8-17]
            text [8-17] ("some text")
      "
    `);
  });

  it('Should parse nested headlines', () => {
    const orgDoc = `* Hello world
** Hello world 2
*** Headline level 3`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-51]
        headline [0-51]
            :level 1:
          title [0-14]
            operator [0-2] ("* ")
            text [2-13] ("Hello world")
            newLine [13-14]
          section [14-51]
            headline [14-51]
                :level 2:
              title [14-31]
                operator [14-17] ("** ")
                text [17-30] ("Hello world 2")
                newLine [30-31]
              section [31-51]
                headline [31-51]
                    :level 3:
                  title [31-51]
                    operator [31-35] ("*** ")
                    text [35-51] ("Headline level 3")
      "
    `);
  });

  it('Should parse headline with nested list progress', () => {
    const orgDoc = `* TODO Hello world [0/0]
    - [ ] item 1
    - [x] item 2
    - [ ] item 3`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-75]
        headline [0-75]
            :level 1:
          title [0-25]
            operator [0-2] ("* ")
            todoKeyword [2-6] ("TODO")
            text [6-19] (" Hello world ")
            progress [19-24]
              operator [19-20] ("[")
              text [20-23] ("0/0")
              operator [23-24] ("]")
            newLine [24-25]
          section [25-75]
            list [25-75]
                :unordered:
                :level 2:
              listItem [25-42]
                title [25-42]
                  indent [25-29] ("    ")
                  operator [29-31] ("- ")
                  checkbox [31-34] ("[ ]")
                      :unchecked:
                    operator [31-32] ("[")
                    text [32-33] (" ")
                    operator [33-34] ("]")
                  text [34-41] (" item 1")
                  newLine [41-42]
              listItem [42-59]
                title [42-59]
                  indent [42-46] ("    ")
                  operator [46-48] ("- ")
                  checkbox [48-51] ("[x]")
                      :checked:
                    operator [48-49] ("[")
                    text [49-50] ("x")
                    operator [50-51] ("]")
                  text [51-58] (" item 2")
                  newLine [58-59]
              listItem [59-75]
                title [59-75]
                  indent [59-63] ("    ")
                  operator [63-65] ("- ")
                  checkbox [65-68] ("[ ]")
                      :unchecked:
                    operator [65-66] ("[")
                    text [66-67] (" ")
                    operator [67-68] ("]")
                  text [68-75] (" item 3")
      "
    `);
  });

  it('Should parse headline with priority', () => {
    const orgDoc = `* [#A] Hello world`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-18]
        headline [0-18]
            :level 1:
          title [0-18]
            operator [0-2] ("* ")
            priority [2-6]
              operator [2-3] ("[")
              text [3-5] ("#A")
              operator [5-6] ("]")
            text [6-18] (" Hello world")
      "
    `);
  });

  it('Should parse headline with numberic priority', () => {
    const orgDoc = `* [#2] Hello world`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-18]
        headline [0-18]
            :level 1:
          title [0-18]
            operator [0-2] ("* ")
            priority [2-6]
              operator [2-3] ("[")
              text [3-5] ("#2")
              operator [5-6] ("]")
            text [6-18] (" Hello world")
      "
    `);
  });

  it('Should not parse priority when it not inside headline', () => {
    const orgDoc = `[#2] Hello world
Another text`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        text [0-16] ("[#2] Hello world")
        newLine [16-17]
        text [17-29] ("Another text")
      "
    `);
  });

  it('Should parse headline with single tag', () => {
    const orgDoc = `* headline with :tag:`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        headline [0-21]
            :level 1:
          title [0-21]
            operator [0-2] ("* ")
            text [2-16] ("headline with ")
            tagList [16-21]
              operator [16-17] (":")
              text [17-20] ("tag")
              operator [20-21] (":")
      "
    `);
  });

  it('Should parse multiple headline tags', () => {
    const orgDoc = `* Headline
** headline 2 :tag1:tag2:amma_tag?:`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-46]
        headline [0-46]
            :level 1:
          title [0-11]
            operator [0-2] ("* ")
            text [2-10] ("Headline")
            newLine [10-11]
          section [11-46]
            headline [11-46]
                :level 2:
              title [11-46]
                operator [11-14] ("** ")
                text [14-25] ("headline 2 ")
                tagList [25-46]
                  operator [25-26] (":")
                  text [26-30] ("tag1")
                  operator [30-31] (":")
                  text [31-35] ("tag2")
                  operator [35-36] (":")
                  text [36-45] ("amma_tag?")
                  operator [45-46] (":")
      "
    `);
  });

  it('Should not parse tags without ending colon', () => {
    const orgDoc = `* Headline :not_tag:not_tag2`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        headline [0-28]
            :level 1:
          title [0-28]
            operator [0-2] ("* ")
            text [2-28] ("Headline :not_tag:not_tag2")
      "
    `);
  });

  it('Should not parse tag list when it does not start from colon', () => {
    const orgDoc = `* Headline not_tag:not_tag2:`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-28]
        headline [0-28]
            :level 1:
          title [0-28]
            operator [0-2] ("* ")
            text [2-28] ("Headline not_tag:not_tag2:")
      "
    `);
  });
});
