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
      "root [0-92]
        headline [0-92]
            :level 1:
          title [0-10]
            operator [0-2] ("* ")
            text [2-9] ("Heading")
            newLine [9-10]
          section [10-92]
            indent [10-14] ("    ")
            latexEnvironment [14-64] ("\\\\begin{align*}\\n2x - 5y &= 8 \\\\\\\\\\n3x + 9y &= -12\\n\\\\end")
            newLine [64-65]
            newLine [65-66]
            indent [66-70] ("    ")
            text [70-78] ("This is ")
            bold [78-91]
              operator [78-79] ("*")
              text [79-90] ("description")
              operator [90-91] ("*")
            newLine [91-92]
      "
    `);
  });
});
