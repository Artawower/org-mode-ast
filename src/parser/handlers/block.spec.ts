import { parse } from '../parser';
import { hasNodeIncorrectRanges } from '../../test-helper';

describe('Org src block test', () => {
  it('Should parse simple src block', () => {
    const orgDoc = `#+BEGIN_SRC
print('Hello world')
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-42]
        srcBlock [0-42]
          blockHeader [0-11]
            keyword [0-11]
              text [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockBody [12-32]
            text [12-32] ("print('Hello world')")
          newLine [32-33]
          blockFooter [33-42]
            keyword [33-42]
              text [33-42] ("#+END_SRC")
      "
    `);
  });

  it('Should parse simple src block with empty body', () => {
    const orgDoc = `#+BEGIN_SRC
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-21]
        srcBlock [0-21]
          blockHeader [0-11]
            keyword [0-11]
              text [0-11] ("#+BEGIN_SRC")
          newLine [11-12]
          blockFooter [12-21]
            keyword [12-21]
              text [12-21] ("#+END_SRC")
      "
    `);
  });

  it('Should parse simple src block with language and additional properties', () => {
    const orgDoc = `#+BEGIN_SRC python :tangle test.py
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-44]
        srcBlock [0-44]
            :language python:
          blockHeader [0-34]
            keyword [0-19]
                :language python:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-19] ("python ")
            blockProperty [19-34]
              text [19-26] (":tangle")
              text [26-34] (" test.py")
          newLine [34-35]
          blockFooter [35-44]
            keyword [35-44]
              text [35-44] ("#+END_SRC")
      "
    `);
  });

  it('Should not parse html block', () => {
    const orgDoc = `#+BEGIN_SRC
Some text without end src`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-37]
        keyword [0-11]
          text [0-11] ("#+BEGIN_SRC")
        newLine [11-12]
        text [12-37] ("Some text without end src")
      "
    `);
  });

  it('Should parse block node with multiple header args', () => {
    const orgDoc = `#+BEGIN_SRC python :tangle test.py :results output
print('Hello world')
#+END_SRC`;
    const result = parse(orgDoc);

    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-81]
        srcBlock [0-81]
            :language python:
          blockHeader [0-50]
            keyword [0-19]
                :language python:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-19] ("python ")
            blockProperty [19-35]
              text [19-26] (":tangle")
              text [26-35] (" test.py ")
            blockProperty [35-50]
              text [35-43] (":results")
              text [43-50] (" output")
          newLine [50-51]
          blockBody [51-71]
            text [51-71] ("print('Hello world')")
          newLine [71-72]
          blockFooter [72-81]
            keyword [72-81]
              text [72-81] ("#+END_SRC")
      "
    `);
  });

  it('Should parse latex export block', () => {
    const orgDoc = `#+BEGIN_EXPORT latex
In physics, the mass-energy equivalence is stated 
by the equation $E=mc^2$, discovered in 1905 by Albert Einstein.    
#+END_EXPORT`;
    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-153]
        exportBlock [0-153]
            :language latex:
          blockHeader [0-20]
            keyword [0-20]
                :language latex:
              text [0-15] ("#+BEGIN_EXPORT ")
              srcLanguage [15-20] ("latex")
          newLine [20-21]
          blockBody [21-140]
            text [21-140] ("In physics, the mass-energy equivalence is stated \\nby the equation $E=mc^2$, discovered in 1905 by Albert Einstein.    ")
          newLine [140-141]
          blockFooter [141-153]
            keyword [141-153]
              text [141-153] ("#+END_EXPORT")
      "
    `);
  });

  it('Should parse simple comment block', () => {
    const orgDoc = `#+BEGIN_COMMENT
    This is my comment with *Bold* text!
#+END_COMMENT`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-70]
        commentBlock [0-70]
          blockHeader [0-15]
            keyword [0-15]
              text [0-15] ("#+BEGIN_COMMENT")
          newLine [15-16]
          blockBody [16-56]
            indent [16-20] ("    ")
            text [20-44] ("This is my comment with ")
            bold [44-50]
              operator [44-45] ("*")
              text [45-49] ("Bold")
              operator [49-50] ("*")
            text [50-56] (" text!")
          newLine [56-57]
          blockFooter [57-70]
            keyword [57-70]
              text [57-70] ("#+END_COMMENT")
      "
    `);
  });

  it('Should pare src block with difficult elisp code', () => {
    const orgDoc = `#+begin_src emacs-lisp
(message "%s" (string-match "\\({\\|;$\\)\\|\\(const [\\w\\[:digit]]+ = [\\d[:digit:]]+$\\)" "  const foo = 1"))
#+end_src
`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-137]
        srcBlock [0-136]
            :language emacs-lisp:
          blockHeader [0-22]
            keyword [0-22]
                :language emacs-lisp:
              text [0-12] ("#+begin_src ")
              srcLanguage [12-22] ("emacs-lisp")
          newLine [22-23]
          blockBody [23-126]
            text [23-126] ("(message \\"%s\\" (string-match \\"\\\\({\\\\|;$\\\\)\\\\|\\\\(const [\\\\w\\\\[:digit]]+ = [\\\\d[:digit:]]+$\\\\)\\" \\"  const foo = 1\\"))")
          newLine [126-127]
          blockFooter [127-136]
            keyword [127-136]
              text [127-136] ("#+end_src")
        newLine [136-137]
      "
    `);
  });

  it('Should exit nested list after src block closed', () => {
    const orgDoc = `* docker-compose.yaml
#+BEGIN_SRC yaml
  volumes:
    - /var/lib/drone:/data
  environment:
    DRONE_GITLAB_SERVER: https://gitlab.com
  ports:
    - 3000:3000
#+END_SRC

* Запуск
`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-181]
        headline [0-172]
            :level 1:
          title [0-22]
            operator [0-2] ("* ")
            text [2-21] ("docker-compose.yaml")
            newLine [21-22]
          section [22-172]
            srcBlock [22-170]
                :language yaml:
              blockHeader [22-38]
                keyword [22-38]
                    :language yaml:
                  text [22-34] ("#+BEGIN_SRC ")
                  srcLanguage [34-38] ("yaml")
              newLine [38-39]
              blockBody [39-161]
                text [39-161] ("  volumes:\\n    - /var/lib/drone:/data\\n  environment:\\n    DRONE_GITLAB_SERVER: https://gitlab.com\\n  ports:\\n    - 3000:3000\\n")
              blockFooter [161-170]
                keyword [161-170]
                  text [161-170] ("#+END_SRC")
            newLine [170-171]
            newLine [171-172]
        headline [172-181]
            :level 1:
          title [172-181]
            operator [172-174] ("* ")
            text [174-180] ("Запуск")
            newLine [180-181]
          section [181-181]
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should parse src block with indents', () => {
    const orgDoc = `
  #+BEGIN_SRC js
console.log('qweqwe')
  #+END_SRC
`;

    const result = parse(orgDoc);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-52]
        newLine [0-1]
        srcBlock [1-51]
            :language js:
          blockHeader [1-17]
            indent [1-3] ("  ")
            keyword [3-17]
                :language js:
              text [3-15] ("#+BEGIN_SRC ")
              srcLanguage [15-17] ("js")
          newLine [17-18]
          blockBody [18-40]
            text [18-40] ("console.log('qweqwe')\\n")
          blockFooter [40-51]
            indent [40-42] ("  ")
            keyword [42-51]
              text [42-51] ("#+END_SRC")
        newLine [51-52]
      "
    `);
  });

  it('Should parse src block with potential nested list', () => {
    const orgDoc = `
#+BEGIN_SRC yaml
  containers:
    - name: nginx
      ports:
        - containerPort: 80
---
spec:
        - name: nginx
#+END_SRC

#+BEGIN_SRC yaml
spec:
  containers:
  - name: nginx
#+END_SRC

*** Просмотр разницы между конфиг файлами
`;
    const result = parse(orgDoc);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-240]
        newLine [0-1]
        srcBlock [1-132]
            :language yaml:
          blockHeader [1-17]
            keyword [1-17]
                :language yaml:
              text [1-13] ("#+BEGIN_SRC ")
              srcLanguage [13-17] ("yaml")
          newLine [17-18]
          blockBody [18-123]
            text [18-123] ("  containers:\\n    - name: nginx\\n      ports:\\n        - containerPort: 80\\n---\\nspec:\\n        - name: nginx\\n")
          blockFooter [123-132]
            keyword [123-132]
              text [123-132] ("#+END_SRC")
        newLine [132-133]
        newLine [133-134]
        srcBlock [134-196]
            :language yaml:
          blockHeader [134-150]
            keyword [134-150]
                :language yaml:
              text [134-146] ("#+BEGIN_SRC ")
              srcLanguage [146-150] ("yaml")
          newLine [150-151]
          blockBody [151-187]
            text [151-187] ("spec:\\n  containers:\\n  - name: nginx\\n")
          blockFooter [187-196]
            keyword [187-196]
              text [187-196] ("#+END_SRC")
        newLine [196-197]
        newLine [197-198]
        headline [198-240]
            :level 3:
          title [198-240]
            operator [198-202] ("*** ")
            text [202-239] ("Просмотр разницы между конфиг файлами")
            newLine [239-240]
          section [240-240]
      "
    `);
    expect(hasNodeIncorrectRanges(result, orgDoc)).toBeFalsy();
  });

  it('Should parse indents as part of block header', () => {
    const orgData = `  #+BEGIN_SRC python
print(123)
#+END_SRC`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-41]
        srcBlock [0-41]
            :language python:
          blockHeader [0-20]
            indent [0-2] ("  ")
            keyword [2-20]
                :language python:
              text [2-14] ("#+BEGIN_SRC ")
              srcLanguage [14-20] ("python")
          newLine [20-21]
          blockBody [21-31]
            text [21-31] ("print(123)")
          newLine [31-32]
          blockFooter [32-41]
            keyword [32-41]
              text [32-41] ("#+END_SRC")
      "
    `);
  });

  it('Should parse indents as part of block footer', () => {
    const orgData = `#+BEGIN_SRC python
print(123)
  #+END_SRC`;
    const result = parse(orgData);
    expect(hasNodeIncorrectRanges(result, orgData)).toBeFalsy();
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-41]
        srcBlock [0-41]
            :language python:
          blockHeader [0-18]
            keyword [0-18]
                :language python:
              text [0-12] ("#+BEGIN_SRC ")
              srcLanguage [12-18] ("python")
          newLine [18-19]
          blockBody [19-30]
            text [19-30] ("print(123)\\n")
          blockFooter [30-41]
            indent [30-32] ("  ")
            keyword [32-41]
              text [32-41] ("#+END_SRC")
      "
    `);
  });
});
