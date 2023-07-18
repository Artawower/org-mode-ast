import { parse } from '../parser/index.js';
import { withMetaInfo } from './meta-info.handler.js';

describe('Meta information', () => {
  it('Should include filetags, title, description and category', () => {
    const orgDoc = `:PROPERTIES:
:ID: qweqwebebe
:PUBLISHED: true
:END:
#+TITLE: some title
#+DESCRIPTION: some description
#+CATEGORY: some category
#+FILETAGS: :tag1:tag2:`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.published).toBe(true);
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "category": "some category",
        "description": "some description",
        "fileTags": [
          "tag1",
          "tag2",
        ],
        "id": "qweqwebebe",
        "published": true,
        "title": "some title",
      }
    `);
  });

  it('Should collect all images from org doc to meta info', () => {
    const orgDoc = `#+TITLE: some title
* Heading 1
[[./image1.png]]
** Heading 2
[[./image2.png]]`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-78]
          :title some title:
          :headings: 
            Heading 1
              Heading 2

          :images ./image1.png,./image2.png:
        keyword [0-19]
          text [0-8] ("#+TITLE:")
          text [8-19] (" some title")
        newLine [19-20]
        headline [20-78]
            :level 1:
          title [20-32]
            operator [20-22] ("* ")
            text [22-31] ("Heading 1")
            newLine [31-32]
          section [32-78]
            link [32-48]
                :linkType image:
              operator [32-33] ("[")
              linkUrl [33-47]
                operator [33-34] ("[")
                text [34-46] ("./image1.png")
                operator [46-47] ("]")
              operator [47-48] ("]")
            newLine [48-49]
            headline [49-78]
                :level 2:
              title [49-62]
                operator [49-52] ("** ")
                text [52-61] ("Heading 2")
                newLine [61-62]
              section [62-78]
                link [62-78]
                    :linkType image:
                  operator [62-63] ("[")
                  linkUrl [63-77]
                    operator [63-64] ("[")
                    text [64-76] ("./image2.png")
                    operator [76-77] ("]")
                  operator [77-78] ("]")
      "
    `);
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "headings": [
          {
            "level": 1,
            "title": "Heading 1",
          },
          {
            "level": 2,
            "title": "Heading 2",
          },
        ],
        "images": [
          "./image1.png",
          "./image2.png",
        ],
        "title": "some title",
      }
    `);
  });

  it('Should collect filetags from complex example', () => {
    const orgDoc = `
:PROPERTIES:
:ID: tmp_bucket
:END:
#+TITLE: Черный ящик ( ͡° ͜ʖ ͡°)
#+DESCRIPTION: Временное хранилище для информации к изучению!
#+STARTUP: show2levels
#+STARTUP: inlineimages
#+FILETAGS: :bucket:временное:blackbox:
#+ACTIVE:
# 13[[img https://www.befunky.com/images/wp/wp-2014-08-milky-way-1023340_1280.jpg?auto=webp&format=jpg&width=1750&crop=16:9]]

[[./space-ca302762-d65b-4e3c-b691-20c29b822bdf.jpeg]]

Это просто временное, постоянно обновляющееся хранилище для тех вещей который я бы очень хотел изучить и посмотреть, но на которые я не нашел время.
Кроме того, это нечто вроде мотиватора, когда этот список слишком большой - в мире грустит 1 маленький котенок.`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "active": "#+ACTIVE:",
        "description": "Временное хранилище для информации к изучению!",
        "fileTags": [
          "bucket",
          "временное",
          "blackbox",
        ],
        "images": [
          "./space-ca302762-d65b-4e3c-b691-20c29b822bdf.jpeg",
        ],
        "startup": "inlineimages",
        "title": "Черный ящик ( ͡° ͜ʖ ͡°)",
      }
    `);
    expect(result.meta.fileTags).toMatchInlineSnapshot(`
      [
        "bucket",
        "временное",
        "blackbox",
      ]
    `);
  });

  it('Should parse filetags from entire document', () => {
    const orgDoc = `:PROPERTIES:
:ID: tmp_bucket
:END:
#+FILETAGS: :bucket:временное:blackbox:
* Some heading
Text
** Nested heading
#+FILETAGS: :complex:tag:
`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "fileTags": [
          "bucket",
          "временное",
          "blackbox",
          "complex",
          "tag",
        ],
        "headings": [
          {
            "level": 1,
            "title": "Some heading",
          },
          {
            "level": 2,
            "title": "Nested heading",
          },
        ],
        "id": "tmp_bucket",
      }
    `);
  });

  fit('Should not raise an error when document has no properties', () => {
    const orgDoc = `:PROPERTIES:

:ID: 45d833f9-9429-491c-993a-59bf462e8f41

:END:


#+TITLE:
`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "id": "45d833f9-9429-491c-993a-59bf462e8f41",
        "title": "#+TITLE:",
      }
    `);
  });
});
