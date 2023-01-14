import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Comment', () => {
  it('Should parse simple comment', () => {
    const orgComment = `# I'am comment!`;
    const result = parse(orgComment);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-15]
        comment [0-15]
          operator [0-1] ("#")
          text [1-15] (" I'am comment!")
      "
    `);
  });

  it('Should parse comment started with spaces', () => {
    const orgComment = `    # I'am comment!`;
    const result = parse(orgComment);

    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-19]
        indent [0-4] ("    ")
        comment [4-19]
          operator [4-5] ("#")
          text [5-19] (" I'am comment!")
      "
    `);
  });

  it('Should not parse comment without any spaces', () => {
    const orgComment = `#I am not a comment!`;
    const result = parse(orgComment);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-20]
        text [0-20] ("#I am not a comment!")
      "
    `);
  });

  it('Should not parse comment with single # symbol', () => {
    const orgComment = `#`;
    const result = parse(orgComment);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-1]
        text [0-1] ("#")
      "
    `);
  });

  it('Should not parse comment at the center of the line', () => {
    const orgComment = `I am not a # comment!`;
    const result = parse(orgComment);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-21]
        text [0-21] ("I am not a # comment!")
      "
    `);
  });

  it('Should not parse comment when # placed at the end', () => {
    const orgComment = `I am not a comment!#`;
    const result = parse(orgComment);
    expect(prettyTreePrint(result)).toMatchInlineSnapshot(`
      "root [0-20]
        text [0-20] ("I am not a comment!#")
      "
    `);
  });
});
