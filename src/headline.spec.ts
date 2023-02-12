import { parse } from './parser';

describe('Headline tests', () => {
  // Headline tests start
  it('should parse first level headline', () => {
    const headline = '* Hello world';
    const result = parse(headline);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-13]
        headline [0-13]
            :level 1:
          title [0-13]
            operator [0-2] ("* ")
            text [2-13] ("Hello world")
      "
    `);
  });

  it('Should parse headline with long start space', () => {
    const headline = '*        Hello world';
    const result = parse(headline);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-20]
        headline [0-20]
            :level 1:
          title [0-20]
            operator [0-2] ("* ")
            text [2-20] ("       Hello world")
      "
    `);
  });

  it('Should not parse text with start space and asterisk as headline', () => {
    const headline = ' * Hello world';
    const result = parse(headline);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-14]
        indent [0-1] (" ")
        text [1-14] ("* Hello world")
      "
    `);
  });

  it('Should parse nested section for headline', () => {
    const orgData = `* Title
some text`;
    const result = parse(orgData);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-17]
        headline [0-17]
            :level 1:
          title [0-8]
            operator [0-2] ("* ")
            text [2-7] ("Title")
            newLine [7-8]
          section [8-17]
            text [8-17] ("some text")
      "
    `);
  });

  it('Should parse nested headlines', () => {
    const headline = `* Hello world
** Hello world 2
*** Headline level 3`;
    const result = parse(headline);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-51]
        headline [0-51]
            :level 1:
          title [0-14]
            operator [0-2] ("* ")
            text [2-13] ("Hello world")
            newLine [13-14]
          section [14-51]
            headline [14-51]
                :level 2:
              title [14-31]
                operator [14-17] ("** ")
                text [17-30] ("Hello world 2")
                newLine [30-31]
              section [31-51]
                headline [31-51]
                    :level 3:
                  title [31-51]
                    operator [31-35] ("*** ")
                    text [35-51] ("Headline level 3")
      "
    `);
  });
});
