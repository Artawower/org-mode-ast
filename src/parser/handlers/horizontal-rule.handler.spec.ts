import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Horizontal rule', () => {
  it('should render a horizontal rule', () => {
    const orgDoc = `Some text
-----
Text after horizontal rule`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-42]
        text [0-9] ("Some text")
        newLine [9-10]
        horizontalRule [10-15] ("-----")
        newLine [15-16]
        text [16-42] ("Text after horizontal rule")
      "
    `);
  });

  it('Should parse horizontal rule with very long line', () => {
    const orgDoc = `Some text
${'-'.repeat(50)}
Text after horizontal rule`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-87]
        text [0-9] ("Some text")
        newLine [9-10]
        horizontalRule [10-60] ("--------------------------------------------------")
        newLine [60-61]
        text [61-87] ("Text after horizontal rule")
      "
    `);
  });

  it('Should not parse horizontal rule when length is too short', () => {
    const orgDoc = `Some text
    ---
    Another text`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-34]
        text [0-9] ("Some text")
        newLine [9-10]
        indent [10-14] ("    ")
        text [14-17] ("---")
        newLine [17-18]
        indent [18-22] ("    ")
        text [22-34] ("Another text")
      "
    `);
  });

  it('Should not parse horizontal rule when another text follows by rule', () => {
    const orgDoc = `Some text
------- Its not a horizontal rule
      `;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-50]
        text [0-9] ("Some text")
        newLine [9-10]
        text [10-43] ("------- Its not a horizontal rule")
        newLine [43-44]
        indent [44-50] ("      ")
      "
    `);
  });
});
