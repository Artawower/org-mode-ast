import { hasNodeIncorrectRanges } from '../../test-helper';
import { parse } from '../parser';

describe('Latex', () => {
  it('Should parse latex fragment', () => {
    const orgDoc = `This is a $\\alpha$ inline latex block`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBe(false);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        text [0-10] ("This is a ")
        latexFragment [10-18]
          operator [10-11] ("$")
          text [11-17] ("\\\\alpha")
          operator [17-18] ("$")
        text [18-37] (" inline latex block")
      "
    `);
  });

  it('Shoul parse latex fragment with dual $$', () => {
    const orgDoc = `This is also a latex text: $$1+1=2$$`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBe(false);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-36]
        text [0-27] ("This is also a latex text: ")
        latexFragment [27-36]
          operator [27-29] ("$$")
          text [29-34] ("1+1=2")
          operator [34-36] ("$$")
      "
    `);
  });
});
