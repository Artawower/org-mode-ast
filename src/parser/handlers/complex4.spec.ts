import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Complex verbose markup content', () => {
  it('Should parse complex markup', () => {
    const orgDoc = `~find . -name '*.mp3' | while read path; do mid3iconv -eCP1251 --remove-vl "$path"; done~`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-89]
        inlineCode [0-89]
          operator [0-1] ("~")
          text [1-88] ("find . -name '*.mp3' | while read path; do mid3iconv -eCP1251 --remove-vl \\"$path\\"; done")
          operator [88-89] ("~")
      "
    `);
  });
});
