import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Latex environment', () => {
  it('Should parse a simple latex environment', () => {
    const orgDoc = `\\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end{align*}`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-58]
        latexEnvironment [0-58] ("\\\\begin{align*}\\n2x - 5y &= 8 \\\\\\\\\\n3x + 9y &= -12\\n\\\\end{align*}")
      "
    `);
  });

  it('Should parse latex environment between other nodes', () => {
    const orgDoc = `* Heading
\\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end{align*}

    This is *description*
`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-96]
        headline [0-96]
            :level 1:
          title [0-10]
            operator [0-2] ("* ")
            text [2-9] ("Heading")
            newLine [9-10]
          section [10-96]
            latexEnvironment [10-68] ("\\\\begin{align*}\\n2x - 5y &= 8 \\\\\\\\\\n3x + 9y &= -12\\n\\\\end{align*}")
            newLine [68-69]
            newLine [69-70]
            indent [70-74] ("    ")
            text [74-82] ("This is ")
            bold [82-95]
              operator [82-83] ("*")
              text [83-94] ("description")
              operator [94-95] ("*")
            newLine [95-96]
      "
    `);
  });

  // TODO: master add negative scenarios

  it('Should not parse latex environment without end of block', () => {
    const orgDoc = `\\begin{align*}
    3x + 9y &= -12`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-33]
        text [0-14] ("\\\\begin{align*}")
        newLine [14-15]
        indent [15-19] ("    ")
        text [19-33] ("3x + 9y &= -12")
      "
    `);
  });

  it('Should not parse latex environment without end of name', () => {
    const orgDoc = `\\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-50]
        text [0-14] ("\\\\begin{align*}")
        newLine [14-15]
        text [15-30] ("2x - 5y &= 8 \\\\\\\\")
        newLine [30-31]
        text [31-45] ("3x + 9y &= -12")
        newLine [45-46]
        text [46-50] ("\\\\end")
      "
    `);
  });

  it('Should parse latex environment that doesnt contain body', () => {
    const orgDoc = `\\begin{align*}
\\end{align*}`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-27]
        latexEnvironment [0-27] ("\\\\begin{align*}\\n\\\\end{align*}")
      "
    `);
  });

  it('Should not parse latex environment when no previous end of line', () => {
    const orgDoc = `Some text \\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end{align*}`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-68]
        text [0-16] ("Some text \\\\begin")
        text [16-24] ("{align*}")
        newLine [24-25]
        text [25-40] ("2x - 5y &= 8 \\\\\\\\")
        newLine [40-41]
        text [41-55] ("3x + 9y &= -12")
        newLine [55-56]
        text [56-60] ("\\\\end")
        text [60-68] ("{align*}")
      "
    `);
  });

  it('Should extract stored latex envrionment keywords when src block end', () => {
    const orgDoc = `#+BEGIN_SRC typescript
  private initAuthConfig(): void {
    this.authConfig = {
    };
  }
  #+END_SRC`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-104]
        srcBlock [0-104]
          blockHeader [0-22]
            keyword [0-22]
              text [0-11] ("#+BEGIN_SRC")
              text [11-22] (" typescript")
          newLine [22-23]
          blockBody [23-95]
            text [23-95] ("  private initAuthConfig(): void {\\n    this.authConfig = {\\n    };\\n  }\\n  ")
          blockFooter [95-104]
            keyword [95-104]
              text [95-104] ("#+END_SRC")
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });
});
