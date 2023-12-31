import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Set 3 of complex examples', () => {
  it('Should parse sample 4', () => {
    const orgDoc = `The consequence is that CLS dislocation grid can get denser only up to a certain point (strain\\approx{}b/2h). Larger strains will make N=2 pile-up more favourable than N=1 CLS`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-175]
        text [0-94] ("The consequence is that CLS dislocation grid can get denser only up to a certain point (strain")
        entity [94-103] ("\\\\approx{}")
        text [103-175] ("b/2h). Larger strains will make N=2 pile-up more favourable than N=1 CLS")
      "
    `);
  });
});
