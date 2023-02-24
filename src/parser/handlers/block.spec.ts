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

  it('Should parse latex export block', () => {
    const orgDoc = `#+BEGIN_EXPORT latex
In physics, the mass-energy equivalence is stated 
by the equation $E=mc^2$, discovered in 1905 by Albert Einstein.    
#+END_EXPORT`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-153]
        exportBlock [0-153]
          blockHeader [0-20]
            keyword [0-20]
              text [0-14] ("#+BEGIN_EXPORT")
              text [14-20] (" latex")
          newLine [20-21]
          blockBody [21-140]
            text [21-140] ("In physics, the mass-energy equivalence is stated \\nby the equation $E=mc^2$, discovered in 1905 by Albert Einstein.    ")
          newLine [140-141]
          blockFooter [141-153]
            keyword [141-153]
              text [141-153] ("#+END_EXPORT")
      "
    `);
  });

  it('Should parse simple comment block', () => {
    const orgDoc = `#+BEGIN_COMMENT
    This is my comment with *Bold* text!
#+END_COMMENT`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-70]
        commentBlock [0-70]
          blockHeader [0-15]
            keyword [0-15]
              text [0-15] ("#+BEGIN_COMMENT")
          newLine [15-16]
          blockBody [16-56]
            indent [16-20] ("    ")
            text [20-44] ("This is my comment with ")
            bold [44-50]
              operator [44-45] ("*")
              text [45-49] ("Bold")
              operator [49-50] ("*")
            text [50-56] (" text!")
          newLine [56-57]
          blockFooter [57-70]
            keyword [57-70]
              text [57-70] ("#+END_COMMENT")
      "
    `);
  });
});
