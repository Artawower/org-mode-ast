import { parse } from './parser';
import { prettyTreePrint } from './tools';

describe('Html block', () => {
  xit('Should parse simple html block', () => {
    const orgDoc = `#+BEGIN_HTML
<div>
  <p>Some text</p>
</div>
#+END_HTML`;

    const result = prettyTreePrint(parse(orgDoc));
    expect(result).toMatchInlineSnapshot(`
      "root [0-55]
        htmlBlock [0-55]
          blockHeader [0-12]
            keyword [0-12] ("#+BEGIN_HTML")
          newLine [12-13]
          blockBody [13-44]
            text [13-44] ("<div>\\n  <p>Some text</p>\\n</div>")
          newLine [44-45]
          blockFooter [45-55]
            keyword [45-55] ("#+END_HTML")
      "
    `);
  });

  // FIXME: bug with bracket collapsing
  xit('Should parse complex html block node', () => {
    const orgDoc = `#+BEGIN_HTML
<!doctype html>
<html lang="en">
    <head>
        <meta charset="UTF-8"/>
        <title>Document</title>
        <style>
        body {
          background-color: #fff;
        }
        </style>
    </head>
    <body>
        <h1>Some text</h1>
        <input type="text" value="Some text" />
        <br />
        <p>Some text</p>
    </body>
</html>
#+END_HTML`;

    const result = prettyTreePrint(parse(orgDoc));
    expect(result).toMatchInlineSnapshot(`
      "root [0-381]
        htmlBlock [0-381]
          blockHeader [0-12]
            keyword [0-12] ("#+BEGIN_HTML")
          newLine [12-13]
          blockBody [13-395]
            text [13-395] ("<!doctype html>\\n<html lang=\\"en\\">\\n    <head>\\n        <meta charset=\\"UTF-8\\"/>\\n        <title>Document</titletitle>\\n        <style>\\n        body {\\n          background-color: #fff;\\n        }\\n        </stylestyle>\\n    </headhead>\\n    <body>\\n        <h1>Some text</h1h1>\\n        <input type=\\"text\\" value=\\"Some text\\" />\\n        <br />\\n        <p>Some text</pp>\\n    </bodybody>\\n</htmlhtml>")
          newLine [370-371]
          blockFooter [371-381]
            keyword [371-381] ("#+END_HTML")
      "
    `);
  });
});
