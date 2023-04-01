import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Comment', () => {
  it('Should parse simple comment', () => {
    const orgDoc = `# I'am comment!`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        comment [0-15]
          operator [0-1] ("#")
          text [1-15] (" I'am comment!")
      "
    `);
  });

  it('Should parse comment started with spaces', () => {
    const orgDoc = `    # I'am comment!`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        indent [0-4] ("    ")
        comment [4-19]
          operator [4-5] ("#")
          text [5-19] (" I'am comment!")
      "
    `);
  });

  it('Should not parse comment without any spaces', () => {
    const orgDoc = `#I am not a comment!`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        text [0-20] ("#I am not a comment!")
      "
    `);
  });

  it('Should not parse comment with single # symbol', () => {
    const orgDoc = `#`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-1]
        text [0-1] ("#")
      "
    `);
  });

  it('Should not parse comment at the center of the line', () => {
    const orgDoc = `I am not a # comment!`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        text [0-21] ("I am not a # comment!")
      "
    `);
  });

  it('Should not parse comment when # placed at the end', () => {
    const orgDoc = `I am not a comment!#`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        text [0-20] ("I am not a comment!#")
      "
    `);
  });

  it('Should parse entire string with nested potential nodes as comment', () => {
    const orgDoc = `# [[https://www.google.com][Google]]`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-36]
        comment [0-36]
          operator [0-1] ("#")
          text [1-36] (" [[https://www.google.com][Google]]")
      "
    `);
  });

  it('Should parse nodes outside of comments', () => {
    const orgDoc = `# I am a comment!
*Bold text*`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-29]
        comment [0-17]
          operator [0-1] ("#")
          text [1-17] (" I am a comment!")
        newLine [17-18]
        bold [18-29]
          operator [18-19] ("*")
          text [19-28] ("Bold text")
          operator [28-29] ("*")
      "
    `);
  });
});
