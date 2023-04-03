import { hasNodeIncorrectRanges } from 'test-helper';
import { parse } from '../parser';

describe('Fixed width', () => {
  it('Should parse simple fixed width', () => {
    const orgDoc = `: Content with fixed width`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc));

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-26]
        fixedWidth [0-26]
          operator [0-2] (": ")
          text [2-26] ("Content with fixed width")
      "
    `);
  });

  it('Should parse multiline fixed width with nested formatters', () => {
    const orgDoc = `: Content with fixed width and some /*brackets* []]
: Second line of content with |>aqweq!++eqweq__qeqwe`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc));

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-104]
        fixedWidth [0-51]
          operator [0-2] (": ")
          text [2-51] ("Content with fixed width and some /*brackets* []]")
        newLine [51-52]
        fixedWidth [52-104]
          operator [52-54] (": ")
          text [54-104] ("Second line of content with |>aqweq!++eqweq__qeqwe")
      "
    `);
  });

  it('Should break fixed width by new line', () => {
    const orgDoc = `: Content with fixed width
*Bold text*`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-38]
        fixedWidth [0-26]
          operator [0-2] (": ")
          text [2-26] ("Content with fixed width")
        newLine [26-27]
        bold [27-38]
          operator [27-28] ("*")
          text [28-37] ("Bold text")
          operator [37-38] ("*")
      "
    `);
  });

  it('Should parse fixed width inside nested node', () => {
    const orgDoc = `* Headline
    : Fixed width 1
    *Bold text*
    : Fixed width 2`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc));

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-66]
        headline [0-66]
            :level 1:
          title [0-11]
            operator [0-2] ("* ")
            text [2-10] ("Headline")
            newLine [10-11]
          section [11-66]
            indent [11-15] ("    ")
            fixedWidth [15-30]
              operator [15-17] (": ")
              text [17-30] ("Fixed width 1")
            newLine [30-31]
            indent [31-35] ("    ")
            bold [35-46]
              operator [35-36] ("*")
              text [36-45] ("Bold text")
              operator [45-46] ("*")
            newLine [46-47]
            indent [47-51] ("    ")
            fixedWidth [51-66]
              operator [51-53] (": ")
              text [53-66] ("Fixed width 2")
      "
    `);
  });

  // FIXME: master
  xit('Should parse colon as fixed width inside text', () => {
    const orgDoc = `Дабы не забыть обновить комменты :O`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-35]
        text [0-33] ("Дабы не забыть обновить комменты ")
        operator [33-34] (":")
        text [34-35] ("O")
      "
    `);
  });
});
