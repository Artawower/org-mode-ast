import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Latex environment', () => {
  it('Should parse a simple latex environment', () => {
    const orgDoc = `\begin{align*}
2x - 5y &= 8 \\
3x + 9y &= -12
\end{align*}`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-55]
        text [0-13] ("\\begin{align*}")
        newLine [13-14]
        text [14-27] ("2x - 5y &= 8 ")
        keyword [27-28]
          text [27-28] ("\\\\")
        newLine [28-29]
        text [29-43] ("3x + 9y &= -12")
        newLine [43-44]
        text [44-55] ("end{align*}")
      "
    `);
  });
});
