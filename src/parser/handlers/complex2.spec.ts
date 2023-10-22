import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Set of complex examples', () => {
  it('Should parse src block after raw link!', () => {
    const orgDoc = `https://google.com/qwe/qwe/

#+BEGIN_SRC js
console.log(1)
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-68]
        rawLink [0-27] ("https://google.com/qwe/qwe/")
        newLine [27-28]
        newLine [28-29]
        srcBlock [29-68]
            :language js:
          blockHeader [29-43]
            keyword [29-43]
                :language js:
              text [29-41] ("#+BEGIN_SRC ")
              srcLanguage [41-43] ("js")
          newLine [43-44]
          blockBody [44-58]
            text [44-58] ("console.log(1)")
          newLine [58-59]
          blockFooter [59-68]
            keyword [59-68]
              text [59-68] ("#+END_SRC")
      "
    `);
  });
});
