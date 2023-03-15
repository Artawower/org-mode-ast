import { parse } from '../parser';

describe('Italic', () => {
  it('should be italic', () => {
    const orgText = '/This is italic/';
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-16]
        italic [0-16]
          operator [0-1] ("/")
          text [1-15] ("This is italic")
          operator [15-16] ("/")
      "
    `);
  });

  it('Should not parse italic text with single slash', () => {
    const orgText = 'This is /italic';
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        text [0-15] ("This is /italic")
      "
    `);
  });

  it('Should not parse italic when sentence start from the single slash', () => {
    const orgText = '/Not a italic text!';
    const result = parse(orgText);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-19]
        text [0-19] ("/Not a italic text!")
      "
    `);
  });

  it('Should not parse italic text with multiple opened brackets', () => {
    const orgText = 'This is +[/*simple not italic text';
    const result = parse(orgText);
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-34]
        text [0-34] ("This is +[/*simple not italic text")
      "
    `);
  });

  it('Should not parse italic text when sentence ends with slash', () => {
    const orgText = 'This is italic/';
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-15]
        text [0-15] ("This is italic/")
      "
    `);
  });

  it('Should parse italic text with nested formatting', () => {
    const orgText = 'This is */italic with bold/* text';
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-33]
        text [0-8] ("This is ")
        bold [8-28]
          operator [8-9] ("*")
          italic [9-27]
            operator [9-10] ("/")
            text [10-26] ("italic with bold")
            operator [26-27] ("/")
          operator [27-28] ("*")
        text [28-33] (" text")
      "
    `);
  });

  it('Should parse italic text that wrapped crossed text', () => {
    const orgText = `This is /+italic text that wrapped crossed text+/ and this is not italic text`;
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-77]
        text [0-8] ("This is ")
        italic [8-49]
          operator [8-9] ("/")
          crossed [9-48]
            operator [9-10] ("+")
            text [10-47] ("italic text that wrapped crossed text")
            operator [47-48] ("+")
          operator [48-49] ("/")
        text [49-77] (" and this is not italic text")
      "
    `);
  });

  it('Should parse italic text with nested formatting and different length', () => {
    const orgText = `/italic and *bold* text/`;
    const result = parse(orgText);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-24]
        italic [0-24]
          operator [0-1] ("/")
          text [1-12] ("italic and ")
          bold [12-18]
            operator [12-13] ("*")
            text [13-17] ("bold")
            operator [17-18] ("*")
          text [18-23] (" text")
          operator [23-24] ("/")
      "
    `);
  });
});
