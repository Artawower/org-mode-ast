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
            keyword [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockBody [12-32]
            text [12-32] ("print('Hello world')")
          newLine [32-33]
          blockFooter [33-42]
            keyword [33-42] ("#+END_SRC")
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
            keyword [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockFooter [12-21]
            keyword [12-21] ("#+END_SRC")
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
            :language python:
            :tangle test.py:
          blockHeader [0-34]
            keyword [0-11] ("#+BEGIN_SRC")
            blockLanguage [11-19] (" python ")
                :language python:
            blockProperty [19-34]
                :tangle test.py:
              keyword [19-26] (":tangle")
              text [26-34] (" test.py")
          newLine [34-35]
          blockFooter [35-44]
            keyword [35-44] ("#+END_SRC")
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
        keyword [0-11] ("#+BEGIN_SRC")
        newLine [11-12]
        text [12-37] ("Some text without end src")
      "
    `);
  });
});