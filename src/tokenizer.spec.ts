import { TokenType } from './types';
import { tokenize } from './tokenizer';

fdescribe('Tokenizer', () => {
  // Headings
  it('Should create tokens for simple headline', () => {
    const headline = '* Hello world';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
    ]);
  });

  it('Should create tokens for simple headline with space at the end', () => {
    const headline = '* Hello world ';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world ', start: 2, end: 14 },
    ]);
  });

  it('Should create tokens for headline with large start space', () => {
    const headline = '*    Hello world';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: '   Hello world', start: 2, end: 16 },
    ]);
  });

  it('Should create tokens for multiple headlines', () => {
    const headline = `* Hello world
** Nested headline
*** Another one headline!
**** And 4 headline level
***** And 5 headline level
* And again 1 level headline`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world\n', start: 2, end: 14 },
      { type: TokenType.Headline, value: '** ', start: 14, end: 17 },
      { type: TokenType.Text, value: 'Nested headline\n', start: 17, end: 33 },
      { type: TokenType.Headline, value: '*** ', start: 33, end: 37 },
      { type: TokenType.Text, value: 'Another one headline!\n', start: 37, end: 59 },
      { type: TokenType.Headline, value: '**** ', start: 59, end: 64 },
      { type: TokenType.Text, value: 'And 4 headline level\n', start: 64, end: 85 },
      { type: TokenType.Headline, value: '***** ', start: 85, end: 91 },
      { type: TokenType.Text, value: 'And 5 headline level\n', start: 91, end: 112 },
      { type: TokenType.Headline, value: '* ', start: 112, end: 114 },
      { type: TokenType.Text, value: 'And again 1 level headline', start: 114, end: 140 },
    ]);
  });

  it('Should create tokens for headlines at same level', () => {
    const headline = `* Hello world
* Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world\n', start: 2, end: 14 },
      { type: TokenType.Headline, value: '* ', start: 14, end: 16 },
      { type: TokenType.Text, value: 'Headline 2\n', start: 16, end: 27 },
      { type: TokenType.Headline, value: '* ', start: 27, end: 29 },
      { type: TokenType.Text, value: 'Headline 3', start: 29, end: 39 },
    ]);
  });

  it('Should correct parse tokens for string with first space and asterisk', () => {
    const headline = ` * Hello world`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Indent, value: ' ', start: 0, end: 1 },
      { type: TokenType.Text, value: '* Hello world', start: 1, end: 14 },
    ]);
  });

  it('Should correct parse tokens for string with first space and aasterisk before word', () => {
    const headline = ` *Hello world`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Indent, value: ' ', start: 0, end: 1 },
      { type: TokenType.Bracket, value: '*', start: 1, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
    ]);
  });

  it('Should create tokens for todo keywords inside headlines', () => {
    const headline = `* TODO Hello world
*** DONE Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' Hello world\n', start: 6, end: 19 },
      { type: TokenType.Headline, value: '*** ', start: 19, end: 23 },
      { type: TokenType.Keyword, value: 'DONE', start: 23, end: 27 },
      { type: TokenType.Text, value: ' Headline 2\n', start: 27, end: 39 },
      { type: TokenType.Headline, value: '* ', start: 39, end: 41 },
      { type: TokenType.Text, value: 'Headline 3', start: 41, end: 51 },
    ]);
  });

  it('Should not create tokens for todo keywords in middle of headline', () => {
    const headline = `* This is not a TODO
* And this is not a DONE
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'This is not a TODO\n', start: 2, end: 21 },
      { type: TokenType.Headline, value: '* ', start: 21, end: 23 },
      { type: TokenType.Text, value: 'And this is not a DONE\n', start: 23, end: 46 },
      { type: TokenType.Headline, value: '* ', start: 46, end: 48 },
      { type: TokenType.Text, value: 'Headline 3', start: 48, end: 58 },
    ]);
  });

  it('Should create tokens for headline with priority', () => {
    const headline = `* [#A] Most important headline
*** [#B] Third most important headline
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Comment, value: '#A', start: 3, end: 5 },
      { type: TokenType.Bracket, value: ']', start: 5, end: 6 },
      { type: TokenType.Text, value: ' Most important headline\n', start: 6, end: 31 },
      { type: TokenType.Headline, value: '*** ', start: 31, end: 35 },
      { type: TokenType.Bracket, value: '[', start: 35, end: 36 },
      { type: TokenType.Comment, value: '#B', start: 36, end: 38 },
      { type: TokenType.Bracket, value: ']', start: 38, end: 39 },
      { type: TokenType.Text, value: ' Third most important headline\n', start: 39, end: 70 },
      { type: TokenType.Headline, value: '* ', start: 70, end: 72 },
      { type: TokenType.Text, value: 'Headline 3', start: 72, end: 82 },
    ]);
  });

  it('Should create tokens for headlines with todo keyword and priority', () => {
    const headline = `* TODO [#A] Most important headline
*** DONE [#B] Third most important headline
* HOLD [#c] Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' ', start: 6, end: 7 },
      { type: TokenType.Bracket, value: '[', start: 7, end: 8 },
      { type: TokenType.Comment, value: '#A', start: 8, end: 10 },
      { type: TokenType.Bracket, value: ']', start: 10, end: 11 },
      { type: TokenType.Text, value: ' Most important headline\n', start: 11, end: 36 },
      { type: TokenType.Headline, value: '*** ', start: 36, end: 40 },
      { type: TokenType.Keyword, value: 'DONE', start: 40, end: 44 },
      { type: TokenType.Text, value: ' ', start: 44, end: 45 },
      { type: TokenType.Bracket, value: '[', start: 45, end: 46 },
      { type: TokenType.Comment, value: '#B', start: 46, end: 48 },
      { type: TokenType.Bracket, value: ']', start: 48, end: 49 },
      { type: TokenType.Text, value: ' Third most important headline\n', start: 49, end: 80 },
      { type: TokenType.Headline, value: '* ', start: 80, end: 82 },
      { type: TokenType.Keyword, value: 'HOLD', start: 82, end: 86 },
      { type: TokenType.Text, value: ' ', start: 86, end: 87 },
      { type: TokenType.Bracket, value: '[', start: 87, end: 88 },
      { type: TokenType.Comment, value: '#c', start: 88, end: 90 },
      { type: TokenType.Bracket, value: ']', start: 90, end: 91 },
      { type: TokenType.Text, value: ' Headline 3', start: 91, end: 102 },
    ]);
  });

  it('Should create tokens for statistics cookies', () => {
    const headline = `* TODO [50%] [#A] Most important headline`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' ', start: 6, end: 7 },
      { type: TokenType.Bracket, value: '[', start: 7, end: 8 },
      { type: TokenType.Text, value: '50%', start: 8, end: 11 },
      { type: TokenType.Bracket, value: ']', start: 11, end: 12 },
      { type: TokenType.Text, value: ' ', start: 12, end: 13 },
      { type: TokenType.Bracket, value: '[', start: 13, end: 14 },
      { type: TokenType.Comment, value: '#A', start: 14, end: 16 },
      { type: TokenType.Bracket, value: ']', start: 16, end: 17 },
      { type: TokenType.Text, value: ' Most important headline', start: 17, end: 41 },
    ]);
  });

  it('Should create statistic with delimiter', () => {
    const headline = `* [1/2] Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: '1', start: 3, end: 4 },
      { type: TokenType.Bracket, value: '/', start: 4, end: 5 },
      { type: TokenType.Text, value: '2', start: 5, end: 6 },
      { type: TokenType.Bracket, value: ']', start: 6, end: 7 },
      { type: TokenType.Text, value: ' Headline 123', start: 7, end: 20 },
    ]);
  });

  it('Should tokenize statistic without trailing space', () => {
    const headline = `* [1/2]Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: '1', start: 3, end: 4 },
      { type: TokenType.Bracket, value: '/', start: 4, end: 5 },
      { type: TokenType.Text, value: '2', start: 5, end: 6 },
      { type: TokenType.Bracket, value: ']', start: 6, end: 7 },
      { type: TokenType.Text, value: 'Headline 123', start: 7, end: 19 },
    ]);
  });

  it('Should tokenize statistic with large trailing space', () => {
    const headline = `* [1/2]      Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: '1', start: 3, end: 4 },
      { type: TokenType.Bracket, value: '/', start: 4, end: 5 },
      { type: TokenType.Text, value: '2', start: 5, end: 6 },
      { type: TokenType.Bracket, value: ']', start: 6, end: 7 },
      { type: TokenType.Text, value: '      Headline 123', start: 7, end: 25 },
    ]);
  });

  it('Should tokenize headline after simple text', () => {
    const headline = `Some text
* Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Text, value: 'Some text\n', start: 0, end: 10 },
      { type: TokenType.Headline, value: '* ', start: 10, end: 12 },
      { type: TokenType.Text, value: 'Headline 123', start: 12, end: 24 },
    ]);
  });

  it('Should tokenize simple text after headline', () => {
    const headline = `* Headline 123
Some text`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Headline 123\n', start: 2, end: 15 },
      { type: TokenType.Text, value: 'Some text', start: 15, end: 24 },
    ]);
  });

  // Simple text
  it('Should tokenize simple text', () => {
    const headline = `Some text`;
    const result = tokenize(headline);
    expect(result).toEqual([{ type: TokenType.Text, value: 'Some text', start: 0, end: 9 }]);
  });

  it('Should not create token from empty imput string', () => {
    const headline = ``;
    const result = tokenize(headline);
    expect(result).toEqual([]);
  });

  // Lists
  it('Should tokenize list values with dash', () => {
    const orgData = `- [ ] List item
- [ ] List item 2
- [ ] List item 3`;
    const result = tokenize(orgData);
    expect(result).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: ' ', start: 3, end: 4 },
      { type: TokenType.Bracket, value: ']', start: 4, end: 5 },
      { type: TokenType.Text, value: ' List item\n', start: 5, end: 16 },
      { type: TokenType.Operator, value: '- ', start: 16, end: 18 },
      { type: TokenType.Bracket, value: '[', start: 18, end: 19 },
      { type: TokenType.Text, value: ' ', start: 19, end: 20 },
      { type: TokenType.Bracket, value: ']', start: 20, end: 21 },
      { type: TokenType.Text, value: ' List item 2\n', start: 21, end: 34 },
      { type: TokenType.Operator, value: '- ', start: 34, end: 36 },
      { type: TokenType.Bracket, value: '[', start: 36, end: 37 },
      { type: TokenType.Text, value: ' ', start: 37, end: 38 },
      { type: TokenType.Bracket, value: ']', start: 38, end: 39 },
      { type: TokenType.Text, value: ' List item 3', start: 39, end: 51 },
    ]);
  });

  it('Should tokenize the dash as text when the dash is not at the start position', () => {
    const orgData = `Some text - with dash`;
    const result = tokenize(orgData);
    expect(result).toEqual([{ type: TokenType.Text, value: 'Some text - with dash', start: 0, end: 21 }]);
  });

  it('Should tokenize list values with plus', () => {
    const orgDoc = `+ [ ] List item
+ [ ] List item 2
+ [ ] List item 3`;
    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: '+ ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: ' ', start: 3, end: 4 },
      { type: TokenType.Bracket, value: ']', start: 4, end: 5 },
      { type: TokenType.Text, value: ' List item\n', start: 5, end: 16 },
      { type: TokenType.Operator, value: '+ ', start: 16, end: 18 },
      { type: TokenType.Bracket, value: '[', start: 18, end: 19 },
      { type: TokenType.Text, value: ' ', start: 19, end: 20 },
      { type: TokenType.Bracket, value: ']', start: 20, end: 21 },
      { type: TokenType.Text, value: ' List item 2\n', start: 21, end: 34 },
      { type: TokenType.Operator, value: '+ ', start: 34, end: 36 },
      { type: TokenType.Bracket, value: '[', start: 36, end: 37 },
      { type: TokenType.Text, value: ' ', start: 37, end: 38 },
      { type: TokenType.Bracket, value: ']', start: 38, end: 39 },
      { type: TokenType.Text, value: ' List item 3', start: 39, end: 51 },
    ]);
  });

  it('Should tokenize properties keywords', () => {
    const orgDoc = `:PROPERTIES:
:ID:      123
:END:`;
    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: ':', start: 0, end: 1 },
      { type: TokenType.Text, value: 'PROPERTIES', start: 1, end: 11 },
      { type: TokenType.Operator, value: ':', start: 11, end: 12 },
      { type: TokenType.Text, value: '\n', start: 12, end: 13 },
      { type: TokenType.Operator, value: ':', start: 13, end: 14 },
      { type: TokenType.Text, value: 'ID', start: 14, end: 16 },
      { type: TokenType.Operator, value: ':', start: 16, end: 17 },
      { type: TokenType.Text, value: '      123\n', start: 17, end: 27 },
      { type: TokenType.Operator, value: ':', start: 27, end: 28 },
      { type: TokenType.Text, value: 'END', start: 28, end: 31 },
      { type: TokenType.Operator, value: ':', start: 31, end: 32 },
    ]);
  });

  it('Should parse bold text from start of line correctly', () => {
    const orgDoc = `*bold text*`;
    const result = tokenize(orgDoc);

    expect(result).toEqual([
      { type: TokenType.Bracket, value: '*', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '*', start: 10, end: 11 },
    ]);
  });

  it('Should parse plus as bracket when it doesnt start as list', () => {
    const orgDoc = `+bold text+`;
    const result = tokenize(orgDoc);

    expect(result).toEqual([
      { type: TokenType.Bracket, value: '+', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '+', start: 10, end: 11 },
    ]);
  });

  it('Should tokenize nested italic text', () => {
    const orgDoc = `This is */italic with bold/* text`;
    const result = tokenize(orgDoc);

    expect(result).toEqual([
      { type: TokenType.Text, value: 'This is ', start: 0, end: 8 },
      { type: TokenType.Bracket, value: '*', start: 8, end: 9 },
      { type: TokenType.Bracket, value: '/', start: 9, end: 10 },
      { type: TokenType.Text, value: 'italic with bold', start: 10, end: 26 },
      { type: TokenType.Bracket, value: '/', start: 26, end: 27 },
      { type: TokenType.Bracket, value: '*', start: 27, end: 28 },
      { type: TokenType.Text, value: ' text', start: 28, end: 33 },
    ]);
  });

  it('Should tokenize list content as well', () => {
    const orgDoc = `- List element
- Second list element
  Oh! i'am an nested section content`;

    const result = tokenize(orgDoc);

    expect(result).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List element\n', start: 2, end: 15 },
      { type: TokenType.Operator, value: '- ', start: 15, end: 17 },
      { type: TokenType.Text, value: 'Second list element\n', start: 17, end: 37 },
      { type: TokenType.Indent, value: '  ', start: 37, end: 39 },
      { type: TokenType.Text, value: `Oh! i'am an nested section content`, start: 39, end: 73 },
    ]);
  });

  it('Should tokenize nested lists', () => {
    const orgDoc = `- List item 1 1
  - List item 2 1
  - List item 2 2
- List item 1 2`;

    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List item 1 1\n', start: 2, end: 16 },
      { type: TokenType.Indent, value: '  ', start: 16, end: 18 },
      { type: TokenType.Operator, value: '- ', start: 18, end: 20 },
      { type: TokenType.Text, value: 'List item 2 1\n', start: 20, end: 34 },
      { type: TokenType.Indent, value: '  ', start: 34, end: 36 },
      { type: TokenType.Operator, value: '- ', start: 36, end: 38 },
      { type: TokenType.Text, value: 'List item 2 2\n', start: 38, end: 52 },
      { type: TokenType.Operator, value: '- ', start: 52, end: 54 },
      { type: TokenType.Text, value: 'List item 1 2', start: 54, end: 67 },
    ]);
  });

  it('Should parse tokens with indents', () => {
    const orgDoc = `- List item 1
  - Nested list item 1
  - Nested list item 2
- List item 2

  Text with indent`;

    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List item 1\n', start: 2, end: 14 },
      { type: TokenType.Indent, value: '  ', start: 14, end: 16 },
      { type: TokenType.Operator, value: '- ', start: 16, end: 18 },
      { type: TokenType.Text, value: 'Nested list item 1\n', start: 18, end: 37 },
      { type: TokenType.Indent, value: '  ', start: 37, end: 39 },
      { type: TokenType.Operator, value: '- ', start: 39, end: 41 },
      { type: TokenType.Text, value: 'Nested list item 2\n', start: 41, end: 60 },
      { type: TokenType.Operator, value: '- ', start: 60, end: 62 },
      { type: TokenType.Text, value: 'List item 2\n', start: 62, end: 74 },
      { type: TokenType.Text, value: '\n', start: 74, end: 75 },
      { type: TokenType.Indent, value: '  ', start: 75, end: 77 },
      { type: TokenType.Text, value: 'Text with indent', start: 77, end: 93 },
    ]);
  });
});
