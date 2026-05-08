import { parse } from '../parser';

describe('Quote block', () => {
  it('Should parse simple quote block', () => {
    const orgNode = `#+BEGIN_QUOTE
This is a quote
#+END_QUOTE`;

    const parsed = parse(orgNode);

    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-41]
        quoteBlock [0-41]
          blockHeader [0-13]
            keyword [0-13]
              text [0-13] ("#+BEGIN_QUOTE")
          newLine [13-14]
          blockBody [14-29]
            text [14-29] ("This is a quote")
          newLine [29-30]
          blockFooter [30-41]
            keyword [30-41]
              text [30-41] ("#+END_QUOTE")
      "
    `);
  });

  it('Should parse quote block with nested text formatting', () => {
    const orgNode = `#+BEGIN_QUOTE
*This is a bold quote* with some /italic/ text.
also this block has a *second* line.
And small list!
- item 1
- item 2
#+END_QUOTE
`;
    const parsed = parse(orgNode);
    expect(parsed.toString()).toMatchInlineSnapshot(`
      "root [0-145]
        quoteBlock [0-144]
          blockHeader [0-13]
            keyword [0-13]
              text [0-13] ("#+BEGIN_QUOTE")
          newLine [13-14]
          blockBody [14-133]
            bold [14-36]
              operator [14-15] ("*")
              text [15-35] ("This is a bold quote")
              operator [35-36] ("*")
            text [36-47] (" with some ")
            italic [47-55]
              operator [47-48] ("/")
              text [48-54] ("italic")
              operator [54-55] ("/")
            text [55-61] (" text.")
            newLine [61-62]
            text [62-84] ("also this block has a ")
            bold [84-92]
              operator [84-85] ("*")
              text [85-91] ("second")
              operator [91-92] ("*")
            text [92-98] (" line.")
            newLine [98-99]
            text [99-114] ("And small list!")
            newLine [114-115]
            list [115-133]
                :unordered:
                :level 0:
              listItem [115-124]
                title [115-124]
                  operator [115-117] ("- ")
                  text [117-123] ("item 1")
                  newLine [123-124]
              listItem [124-133]
                title [124-133]
                  operator [124-126] ("- ")
                  text [126-132] ("item 2")
                  newLine [132-133]
          blockFooter [133-144]
            keyword [133-144]
              text [133-144] ("#+END_QUOTE")
        newLine [144-145]
      "
    `);
  });
});
