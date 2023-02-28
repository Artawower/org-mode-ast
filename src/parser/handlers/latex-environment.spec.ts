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
      "root [0-50]
        latexEnvironment [0-50] ("\\\\begin{align*}\\n2x - 5y &= 8 \\\\\\\\\\n3x + 9y &= -12\\n\\\\end")
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
      "root [0-88]
        headline [0-88]
            :level 1:
          title [0-10]
            operator [0-2] ("* ")
            text [2-9] ("Heading")
            newLine [9-10]
          section [10-88]
            latexEnvironment [10-60] ("\\\\begin{align*}\\n2x - 5y &= 8 \\\\\\\\\\n3x + 9y &= -12\\n\\\\end")
            newLine [60-61]
            newLine [61-62]
            indent [62-66] ("    ")
            text [66-74] ("This is ")
            bold [74-87]
              operator [74-75] ("*")
              text [75-86] ("description")
              operator [86-87] ("*")
            newLine [87-88]
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
        text [15-28] ("2x - 5y &= 8 ")
        keyword [28-29]
          text [28-29] ("\\\\")
        keyword [29-30]
          text [29-30] ("\\\\")
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
      "root [0-19]
        latexEnvironment [0-19] ("\\\\begin{align*}\\n\\\\end")
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
        text [0-24] ("Some text \\\\begin{align*}")
        newLine [24-25]
        text [25-38] ("2x - 5y &= 8 ")
        keyword [38-39]
          text [38-39] ("\\\\")
        keyword [39-40]
          text [39-40] ("\\\\")
        newLine [40-41]
        text [41-55] ("3x + 9y &= -12")
        newLine [55-56]
        text [56-68] ("\\\\end{align*}")
      "
    `);
  });
});
