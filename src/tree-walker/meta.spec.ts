import { parse } from '../parser/index.js';
import { withMetaInfo } from './meta-info.handler.js';

describe('Meta information', () => {
  it('Should include filetags, title, description and category', () => {
    const orgDoc = `:PROPERTIES:
:ID: qweqwebebe
:END:
#+TITLE: some title
#+DESCRIPTION: some description
#+CATEGORY: some category
#+FILETAGS: :tag1:tag2:`;

    const result = withMetaInfo(parse(orgDoc));
    expect(result.meta).toMatchInlineSnapshot(`
      {
        "category": "some category",
        "description": "some description",
        "filetags": [
          "tag1",
          "tag2",
        ],
        "id": "qweqwebebe",
        "title": "some title",
      }
    `);
  });
});
