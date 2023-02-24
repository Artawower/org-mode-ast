import { hasNodeIncorrectRanges } from '../../test-helper';
import { parse } from '../parser';

describe('Latex', () => {
  it('Should parse inline latex block', () => {
    const orgDoc = `This is a $\\alpha$ inline latex block`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges);
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
});
