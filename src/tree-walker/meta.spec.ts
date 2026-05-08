/* eslint-disable no-irregular-whitespace */
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
          text [0-9] ("#+TITLE: ")
          text [9-19] ("some title")
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
            "end": 78,
            "level": 1,
            "start": 20,
            "title": "Heading 1",
          },
          {
            "end": 78,
            "level": 2,
            "start": 49,
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
        "active": "",
        "description": "Временное хранилище для информации к изучению!",
        "fileTags": [
          "bucket",
          "временное",
          "blackbox",
        ],
        "id": "tmp_bucket",
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
            "end": 139,
            "level": 1,
            "start": 75,
            "title": "Some heading",
          },
          {
            "end": 139,
            "level": 2,
            "start": 95,
            "title": "Nested heading",
          },
        ],
        "id": "tmp_bucket",
      }
    `);
  });

  it('Should not raise an error when document has no properties', () => {
    const orgDoc = `:PROPERTIES:

:ID: 45d833f9-9429-491c-993a-59bf462e8f41

:END:


#+TITLE:
`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "id": "45d833f9-9429-491c-993a-59bf462e8f41",
        "title": "",
      }
    `);
  });

  it('Should not create description meta info property with empty description keyword', () => {
    const orgDoc = `#+DESCRIPTION:`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "description": "",
      }
    `);
  });

  // eslint-disable-next-line no-irregular-whitespace
  it('Should parse hello world example!', () => {
    const orgDoc = `#+TITLE: 12312



`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "title": "12312",
      }
    `);

    expect(result.toString()).toMatchInlineSnapshot(`
      "root [0-18]
          :title 12312:
        keyword [0-14]
          text [0-9] ("#+TITLE: ")
          text [9-14] ("12312")
        newLine [14-15]
        newLine [15-16]
        newLine [16-17]
        newLine [17-18]
      "
    `);
  });

  it('Should extract connected links of other notes', () => {
    const orgDoc = `* Some heading

[[id:123123123][Note A]]

** Subheading
[[id:lalalalal][Note B]]`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "connectedNotes": {
          "123123123": "Note A",
          "lalalalal": "Note B",
        },
        "headings": [
          {
            "end": 80,
            "level": 1,
            "start": 0,
            "title": "Some heading",
          },
          {
            "end": 80,
            "level": 2,
            "start": 42,
            "title": "Subheading",
          },
        ],
      }
    `);
  });

  it('Should extract connected note without name', () => {
    const orgDoc = `[[id:people]]`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "connectedNotes": {
          "people": "",
        },
      }
    `);
  });

  it('should collect start and end positions for headings', () => {
    const orgDoc = `* Task
some content`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].start).toBe(0);
    expect(result.meta.headings[0].end).toBe(orgDoc.length);
  });

  it('should collect DEADLINE from planning into heading', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].deadline).toMatchInlineSnapshot(`
      {
        "active": true,
        "date": "2026-05-10",
        "end": 38,
        "hasTime": false,
        "start": 22,
      }
    `);
  });

  it('should collect SCHEDULED from planning into heading', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri>`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].scheduled).toMatchInlineSnapshot(`
      {
        "active": true,
        "date": "2026-05-01",
        "end": 39,
        "hasTime": false,
        "start": 23,
      }
    `);
  });

  it('should collect CLOSED with inactive timestamp into heading', () => {
    const orgDoc = `* DONE Task
CLOSED: [2026-04-30 Wed]`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].closed).toMatchInlineSnapshot(`
      {
        "active": false,
        "date": "2026-04-30",
        "end": 36,
        "hasTime": false,
        "start": 20,
      }
    `);
  });

  it('should collect all three planning keywords into heading', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon> SCHEDULED: <2026-05-01 Fri> CLOSED: [2026-04-30 Wed]`;
    const result = withMetaInfo(parse(orgDoc));
    const heading = result.meta.headings[0];
    expect(heading.deadline.date).toBe('2026-05-10');
    expect(heading.scheduled.date).toBe('2026-05-01');
    expect(heading.closed.date).toBe('2026-04-30');
    expect(heading.closed.active).toBe(false);
  });

  it('should collect planning with time component', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri 14:00>`;
    const result = withMetaInfo(parse(orgDoc));
    const scheduled = result.meta.headings[0].scheduled;
    expect(scheduled.date).toBe('2026-05-01T14:00');
    expect(scheduled.hasTime).toBe(true);
  });

  it('should collect catch-up repeater (.+) from planning', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri .+1w>`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].scheduled.repeater).toMatchInlineSnapshot(`
      {
        "type": ".+",
        "unit": "w",
        "value": 1,
      }
    `);
  });

  it('should collect planning with repeater', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri +1w>`;
    const result = withMetaInfo(parse(orgDoc));
    const scheduled = result.meta.headings[0].scheduled;
    expect(scheduled.repeater).toMatchInlineSnapshot(`
      {
        "type": "+",
        "unit": "w",
        "value": 1,
      }
    `);
  });

  it('should collect first-occurrence delay (--) from scheduled', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2004-12-25 Sat --2d>`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].scheduled.warning).toMatchInlineSnapshot(`
      {
        "type": "--",
        "unit": "d",
        "value": 2,
      }
    `);
  });

  it('should collect deadline with warning interval', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon -3d>`;
    const result = withMetaInfo(parse(orgDoc));
    const deadline = result.meta.headings[0].deadline;
    expect(deadline.warning).toMatchInlineSnapshot(`
      {
        "type": "-",
        "unit": "d",
        "value": 3,
      }
    `);
  });

  it('should collect deadline with date range', () => {
    const orgDoc = `* TODO Task
SCHEDULED: <2026-05-01 Fri>--<2026-05-10 Mon>`;
    const result = withMetaInfo(parse(orgDoc));
    const scheduled = result.meta.headings[0].scheduled;
    expect(scheduled.date).toBe('2026-05-01');
    expect(scheduled.to).toBe('2026-05-10');
  });

  it('should collect clocks from logbook into heading', () => {
    const orgDoc = `* Task
:LOGBOOK:
CLOCK: [2024-01-01 Mon 10:00]--[2024-01-01 Mon 12:00] =>  2:00
:END:`;
    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta.headings[0].clocks).toMatchInlineSnapshot(`
      [
        {
          "date": "2024-01-01T10:00",
          "end": 79,
          "start": 17,
          "to": "2024-01-01T12:00",
        },
      ]
    `);
  });

  it('should collect open clock (no end) into heading', () => {
    const orgDoc = `* Task
:LOGBOOK:
CLOCK: [2024-01-01 Mon 10:00]
:END:`;
    const result = withMetaInfo(parse(orgDoc));
    const clock = result.meta.headings[0].clocks[0];
    expect(clock.date).toBe('2024-01-01T10:00');
    expect(clock.to).toBeUndefined();
  });

  it('should collect clocks only from own section, not nested headlines', () => {
    const orgDoc = `* Parent
:LOGBOOK:
CLOCK: [2024-01-01 Mon 10:00]--[2024-01-01 Mon 11:00] =>  1:00
:END:
** Child
:LOGBOOK:
CLOCK: [2024-01-02 Tue 09:00]--[2024-01-02 Tue 10:00] =>  1:00
:END:`;
    const result = withMetaInfo(parse(orgDoc));
    const [parent, child] = result.meta.headings;
    expect(parent.clocks).toHaveLength(1);
    expect(parent.clocks[0].date).toBe('2024-01-01T10:00');
    expect(child.clocks).toHaveLength(1);
    expect(child.clocks[0].date).toBe('2024-01-02T09:00');
  });

  it('should not set planning fields when headline has no planning', () => {
    const orgDoc = `* Plain headline
some text`;
    const result = withMetaInfo(parse(orgDoc));
    const heading = result.meta.headings[0];
    expect(heading.deadline).toBeUndefined();
    expect(heading.scheduled).toBeUndefined();
    expect(heading.closed).toBeUndefined();
    expect(heading.clocks).toBeUndefined();
  });

  it('should collect planning per each nested headline independently', () => {
    const orgDoc = `* Parent
** Child task
DEADLINE: <2026-05-10 Mon>
content`;
    const result = withMetaInfo(parse(orgDoc));
    const [parent, child] = result.meta.headings;
    expect(parent.deadline).toBeUndefined();
    expect(child.deadline.date).toBe('2026-05-10');
  });

  it('Should parse meta info id with many spaces', () => {
    const orgDoc = `:PROPERTIES:
:ID:       c9eda354-3eb2-472c-818c-7167158be782
:END: 
#+title: Garten Klone

<2024-06-06 Thu>
-
-

<2024-08-13 Tue>
-
`;

    const result = withMetaInfo(parse(orgDoc));

    expect(result.meta).toMatchInlineSnapshot(`
      {
        "id": "c9eda354-3eb2-472c-818c-7167158be782",
        "title": "Garten Klone",
      }
    `);
  });
});
