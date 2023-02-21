import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Org src block test', () => {
  it('Should parse simple src block', () => {
    const orgDoc = `#+BEGIN_SRC
print('Hello world')
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-42]
        srcBlock [0-42]
          blockHeader [0-11]
            keyword [0-11]
              text [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockBody [12-32]
            text [12-32] ("print('Hello world')")
          newLine [32-33]
          blockFooter [33-42]
            keyword [33-42]
              text [33-42] ("#+END_SRC")
      "
    `);
  });

  it('Should parse simple src block with empty body', () => {
    const orgDoc = `#+BEGIN_SRC
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        srcBlock [0-21]
          blockHeader [0-11]
            keyword [0-11]
              text [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockFooter [12-21]
            keyword [12-21]
              text [12-21] ("#+END_SRC")
      "
    `);
  });

  it('Should parse simple src block with language and additional properties', () => {
    const orgDoc = `#+BEGIN_SRC python :tangle test.py
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-44]
        srcBlock [0-44]
          blockHeader [0-34]
            keyword [0-19]
              text [0-11] ("#+BEGIN_SRC")
              text [11-19] (" python ")
            blockProperty [19-34]
              text [19-26] (":tangle")
              text [26-34] (" test.py")
          newLine [34-35]
          blockFooter [35-44]
            keyword [35-44]
              text [35-44] ("#+END_SRC")
      "
    `);
  });

  it('Should not parse html block', () => {
    const orgDoc = `#+BEGIN_SRC
Some text without end src`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        keyword [0-11]
          text [0-11] ("#+BEGIN_SRC")
        newLine [11-12]
        text [12-37] ("Some text without end src")
      "
    `);
  });

  it('Should parse block node with multiple header args', () => {
    const orgDoc = `#+BEGIN_SRC python :tangle test.py :results output
print('Hello world')
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-81]
        srcBlock [0-81]
          blockHeader [0-50]
            keyword [0-19]
              text [0-11] ("#+BEGIN_SRC")
              text [11-19] (" python ")
            blockProperty [19-35]
              text [19-26] (":tangle")
              text [26-35] (" test.py ")
            blockProperty [35-50]
              text [35-43] (":results")
              text [43-50] (" output")
          newLine [50-51]
          blockBody [51-71]
            text [51-71] ("print('Hello world')")
          newLine [71-72]
          blockFooter [72-81]
            keyword [72-81]
              text [72-81] ("#+END_SRC")
      "
    `);
  });
});
