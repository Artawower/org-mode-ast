import { hasIntersection } from './has-intersection';

describe('Intersection check', () => {
  it('Should be intersected when second range inside first range', () => {
    expect(hasIntersection(0, 10, 5, 8)).toBeTruthy();
  });

  it('Ranges should be intersected when first range inside second range', () => {
    expect(hasIntersection(5, 8, 0, 10)).toBeTruthy();
  });

  it('Ranges should be intersected when only start1 value in second range', () => {
    expect(hasIntersection(9, 10, 0, 9)).toBeTruthy();
  });

  it('Ranges should be intersected when only end1 value in second range', () => {
    expect(hasIntersection(0, 10, 9, 11)).toBeTruthy();
  });

  it('Ranges should be intersected when first range inside second range', () => {
    expect(hasIntersection(5, 8, 0, 10)).toBeTruthy();
  });

  it('Ranges should be intersected when start of the first range is end of the second range', () => {
    expect(hasIntersection(5, 8, 8, 10)).toBeTruthy();
  });

  it('Ranges should be intersected when end of the first range is start of the second range', () => {
    expect(hasIntersection(5, 8, 0, 5)).toBeTruthy();
  });

  it('Ranges should not be intersected when first range before second range', () => {
    expect(hasIntersection(0, 5, 6, 10)).toBeFalsy();
  });

  it('Ranges should not be intersected when first range after second range', () => {
    expect(hasIntersection(6, 10, 0, 5)).toBeFalsy();
  });
});
