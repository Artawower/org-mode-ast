import { isOrgTimestamp, parseOrgTimestamp } from './org-timestamp.js';

describe('isOrgTimestamp', () => {
  it('should accept active date', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon>')).toBe(true);
  });

  it('should accept inactive date', () => {
    expect(isOrgTimestamp('[2024-01-01 Mon]')).toBe(true);
  });

  it('should accept active date with time', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon 10:00>')).toBe(true);
  });

  it('should accept date with cumulate repeater', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon +1w>')).toBe(true);
  });

  it('should accept date with catch-up repeater', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon ++1d>')).toBe(true);
  });

  it('should accept date with restart repeater', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon .+1m>')).toBe(true);
  });

  it('should accept date with warning delay', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon -2d>')).toBe(true);
  });

  it('should accept date with first-only warning', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon --1d>')).toBe(true);
  });

  it('should accept date with repeater and warning', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon +1w -2d>')).toBe(true);
  });

  it('should reject plain text', () => {
    expect(isOrgTimestamp('not a date')).toBe(false);
  });

  it('should reject empty string', () => {
    expect(isOrgTimestamp('')).toBe(false);
  });

  it('should reject partial timestamp', () => {
    expect(isOrgTimestamp('<2024-01-01>')).toBe(false);
  });

  it('should reject timestamp with wrong day name', () => {
    expect(isOrgTimestamp('<2024-01-01 XYZ>')).toBe(false);
  });

  it('should reject mismatched brackets', () => {
    expect(isOrgTimestamp('<2024-01-01 Mon]')).toBe(false);
  });
});

describe('parseOrgTimestamp', () => {
  it('should parse active date', () => {
    const result = parseOrgTimestamp('<2024-06-15 Sat>');
    expect(result).toEqual({
      active: true,
      date: '2024-06-15',
      hasTime: false,
    });
  });

  it('should parse inactive date', () => {
    const result = parseOrgTimestamp('[2024-06-15 Sat]');
    expect(result).toEqual({
      active: false,
      date: '2024-06-15',
      hasTime: false,
    });
  });

  it('should parse date with time', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon 10:30>');
    expect(result).toMatchObject({ date: '2024-01-01T10:30', hasTime: true });
  });

  it('should parse cumulate repeater +', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon +1w>');
    expect(result?.repeater).toEqual({ type: '+', value: 1, unit: 'w' });
  });

  it('should parse catch-up repeater ++', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon ++2d>');
    expect(result?.repeater).toEqual({ type: '++', value: 2, unit: 'd' });
  });

  it('should parse restart repeater .+', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon .+1m>');
    expect(result?.repeater).toEqual({ type: '.+', value: 1, unit: 'm' });
  });

  it('should parse warning delay -', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon -2d>');
    expect(result?.warning).toEqual({ type: '-', value: 2, unit: 'd' });
    expect(result?.repeater).toBeUndefined();
  });

  it('should parse first-only warning --', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon --1d>');
    expect(result?.warning).toEqual({ type: '--', value: 1, unit: 'd' });
  });

  it('should parse repeater and warning together', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon +1w -2d>');
    expect(result?.repeater).toEqual({ type: '+', value: 1, unit: 'w' });
    expect(result?.warning).toEqual({ type: '-', value: 2, unit: 'd' });
  });

  it('should parse inactive timestamp with repeater and warning', () => {
    const result = parseOrgTimestamp('[2024-03-15 Fri ++1w --1d]');
    expect(result?.active).toBe(false);
    expect(result?.repeater).toEqual({ type: '++', value: 1, unit: 'w' });
    expect(result?.warning).toEqual({ type: '--', value: 1, unit: 'd' });
  });

  it('should set active=true for angle brackets', () => {
    expect(parseOrgTimestamp('<2024-01-01 Mon>')?.active).toBe(true);
  });

  it('should set active=false for square brackets', () => {
    expect(parseOrgTimestamp('[2024-01-01 Mon]')?.active).toBe(false);
  });

  it('should return null for invalid input', () => {
    expect(parseOrgTimestamp('not a date')).toBeNull();
  });

  it('should return null for empty string', () => {
    expect(parseOrgTimestamp('')).toBeNull();
  });

  it('should not include repeater field when absent', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon>');
    expect(result).not.toHaveProperty('repeater');
  });

  it('should not include warning field when absent', () => {
    const result = parseOrgTimestamp('<2024-01-01 Mon>');
    expect(result).not.toHaveProperty('warning');
  });
});
