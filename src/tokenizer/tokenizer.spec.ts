import { Token, TokenType } from '../models';
import { tokenize } from './tokenizer';

function tokenListToArray(token: Token): Token[] {
  const tokens: Token[] = [];
  let currToken: Token | undefined = token;
  while (currToken) {
    tokens.push(currToken);
    currToken = currToken.next;
    delete tokens[tokens.length - 1].next;
    delete tokens[tokens.length - 1].prev;
  }
  return tokens;
}

fdescribe('Tokenizer', () => {
  // Headings
  it('Should create tokens for simple headline', () => {
    const headline = '* Hello world';
    const result = tokenize(headline);

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
    ]);
  });

  it('Should create tokens for simple headline with space at the end', () => {
    const headline = '* Hello world ';
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world ', start: 2, end: 14 },
    ]);
  });

  it('Should create tokens for headline with large start space', () => {
    const headline = '*    Hello world';
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
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
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
      { type: TokenType.NewLine, value: '\n', start: 13, end: 14 },
      { type: TokenType.Headline, value: '** ', start: 14, end: 17 },
      { type: TokenType.Text, value: 'Nested headline', start: 17, end: 32 },
      { type: TokenType.NewLine, value: '\n', start: 32, end: 33 },
      { type: TokenType.Headline, value: '*** ', start: 33, end: 37 },
      {
        type: TokenType.Text,
        value: 'Another one headline!',
        start: 37,
        end: 58,
      },
      { type: TokenType.NewLine, value: '\n', start: 58, end: 59 },
      { type: TokenType.Headline, value: '**** ', start: 59, end: 64 },
      {
        type: TokenType.Text,
        value: 'And 4 headline level',
        start: 64,
        end: 84,
      },
      { type: TokenType.NewLine, value: '\n', start: 84, end: 85 },
      { type: TokenType.Headline, value: '***** ', start: 85, end: 91 },
      {
        type: TokenType.Text,
        value: 'And 5 headline level',
        start: 91,
        end: 111,
      },
      { type: TokenType.NewLine, value: '\n', start: 111, end: 112 },
      { type: TokenType.Headline, value: '* ', start: 112, end: 114 },
      {
        type: TokenType.Text,
        value: 'And again 1 level headline',
        start: 114,
        end: 140,
      },
    ]);
  });

  it('Should create tokens for headlines at same level', () => {
    const headline = `* Hello world
* Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
      { type: TokenType.NewLine, value: '\n', start: 13, end: 14 },
      { type: TokenType.Headline, value: '* ', start: 14, end: 16 },
      { type: TokenType.Text, value: 'Headline 2', start: 16, end: 26 },
      { type: TokenType.NewLine, value: '\n', start: 26, end: 27 },
      { type: TokenType.Headline, value: '* ', start: 27, end: 29 },
      { type: TokenType.Text, value: 'Headline 3', start: 29, end: 39 },
    ]);
  });

  it('Should correct parse tokens for string with first space and asterisk', () => {
    const headline = ` * Hello world`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Indent, value: ' ', start: 0, end: 1 },
      { type: TokenType.Text, value: '* Hello world', start: 1, end: 14 },
    ]);
  });

  it('Should correct parse tokens for string with first space and aasterisk before word', () => {
    const headline = ` *Hello world`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
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
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' Hello world', start: 6, end: 18 },
      { type: TokenType.NewLine, value: '\n', start: 18, end: 19 },
      { type: TokenType.Headline, value: '*** ', start: 19, end: 23 },
      { type: TokenType.Keyword, value: 'DONE', start: 23, end: 27 },
      { type: TokenType.Text, value: ' Headline 2', start: 27, end: 38 },
      { type: TokenType.NewLine, value: '\n', start: 38, end: 39 },
      { type: TokenType.Headline, value: '* ', start: 39, end: 41 },
      { type: TokenType.Text, value: 'Headline 3', start: 41, end: 51 },
    ]);
  });

  it('Should not create tokens for todo keywords in middle of headline', () => {
    const headline = `* This is not a TODO
* And this is not a DONE
* Headline 3`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'This is not a TODO', start: 2, end: 20 },
      { type: TokenType.NewLine, value: '\n', start: 20, end: 21 },
      { type: TokenType.Headline, value: '* ', start: 21, end: 23 },
      {
        type: TokenType.Text,
        value: 'And this is not a DONE',
        start: 23,
        end: 45,
      },
      { type: TokenType.NewLine, value: '\n', start: 45, end: 46 },
      { type: TokenType.Headline, value: '* ', start: 46, end: 48 },
      { type: TokenType.Text, value: 'Headline 3', start: 48, end: 58 },
    ]);
  });

  it('Should create tokens for headline with priority', () => {
    const headline = `* [#A] Most important headline
*** [#B] Third most important headline
* Headline 3`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: '#A', start: 3, end: 5 },
      { type: TokenType.Bracket, value: ']', start: 5, end: 6 },
      {
        type: TokenType.Text,
        value: ' Most important headline',
        start: 6,
        end: 30,
      },
      { type: TokenType.NewLine, value: '\n', start: 30, end: 31 },
      { type: TokenType.Headline, value: '*** ', start: 31, end: 35 },
      { type: TokenType.Bracket, value: '[', start: 35, end: 36 },
      { type: TokenType.Text, value: '#B', start: 36, end: 38 },
      { type: TokenType.Bracket, value: ']', start: 38, end: 39 },
      {
        type: TokenType.Text,
        value: ' Third most important headline',
        start: 39,
        end: 69,
      },
      { type: TokenType.NewLine, value: '\n', start: 69, end: 70 },
      { type: TokenType.Headline, value: '* ', start: 70, end: 72 },
      { type: TokenType.Text, value: 'Headline 3', start: 72, end: 82 },
    ]);
  });

  it('Should create tokens for headlines with todo keyword and priority', () => {
    const headline = `* TODO [#A] Most important headline
*** DONE [#B] Third most important headline
* HOLD [#c] Headline 3`;
    const result = tokenize(headline);
    console.log('✎: [line 162][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' ', start: 6, end: 7 },
      { type: TokenType.Bracket, value: '[', start: 7, end: 8 },
      { type: TokenType.Text, value: '#A', start: 8, end: 10 },
      { type: TokenType.Bracket, value: ']', start: 10, end: 11 },
      {
        type: TokenType.Text,
        value: ' Most important headline',
        start: 11,
        end: 35,
      },
      { type: TokenType.NewLine, value: '\n', start: 35, end: 36 },
      { type: TokenType.Headline, value: '*** ', start: 36, end: 40 },
      { type: TokenType.Keyword, value: 'DONE', start: 40, end: 44 },
      { type: TokenType.Text, value: ' ', start: 44, end: 45 },
      { type: TokenType.Bracket, value: '[', start: 45, end: 46 },
      { type: TokenType.Text, value: '#B', start: 46, end: 48 },
      { type: TokenType.Bracket, value: ']', start: 48, end: 49 },
      {
        type: TokenType.Text,
        value: ' Third most important headline',
        start: 49,
        end: 79,
      },
      { type: TokenType.NewLine, value: '\n', start: 79, end: 80 },
      { type: TokenType.Headline, value: '* ', start: 80, end: 82 },
      { type: TokenType.Keyword, value: 'HOLD', start: 82, end: 86 },
      { type: TokenType.Text, value: ' ', start: 86, end: 87 },
      { type: TokenType.Bracket, value: '[', start: 87, end: 88 },
      { type: TokenType.Text, value: '#c', start: 88, end: 90 },
      { type: TokenType.Bracket, value: ']', start: 90, end: 91 },
      { type: TokenType.Text, value: ' Headline 3', start: 91, end: 102 },
    ]);
  });

  it('Should create tokens for statistics cookies', () => {
    const headline = `* TODO [50%] [#A] Most important headline`;
    const result = tokenize(headline);
    console.log('✎: [line 194][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Keyword, value: 'TODO', start: 2, end: 6 },
      { type: TokenType.Text, value: ' ', start: 6, end: 7 },
      { type: TokenType.Bracket, value: '[', start: 7, end: 8 },
      { type: TokenType.Text, value: '50%', start: 8, end: 11 },
      { type: TokenType.Bracket, value: ']', start: 11, end: 12 },
      { type: TokenType.Text, value: ' ', start: 12, end: 13 },
      { type: TokenType.Bracket, value: '[', start: 13, end: 14 },
      { type: TokenType.Text, value: '#A', start: 14, end: 16 },
      { type: TokenType.Bracket, value: ']', start: 16, end: 17 },
      {
        type: TokenType.Text,
        value: ' Most important headline',
        start: 17,
        end: 41,
      },
    ]);
  });

  it('Should create statistic with delimiter', () => {
    const headline = `* [1/2] Headline 123`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
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
    expect(tokenListToArray(result)).toEqual([
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
    expect(tokenListToArray(result)).toEqual([
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
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Text, value: 'Some text', start: 0, end: 9 },
      { type: TokenType.NewLine, value: '\n', start: 9, end: 10 },
      { type: TokenType.Headline, value: '* ', start: 10, end: 12 },
      { type: TokenType.Text, value: 'Headline 123', start: 12, end: 24 },
    ]);
  });

  it('Should tokenize simple text after headline', () => {
    const headline = `* Headline 123
Some text`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Headline 123', start: 2, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Text, value: 'Some text', start: 15, end: 24 },
    ]);
  });

  // Simple text
  it('Should tokenize simple text', () => {
    const headline = `Some text`;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Text, value: 'Some text', start: 0, end: 9 },
    ]);
  });

  it('Should not create token from empty imput string', () => {
    const headline = ``;
    const result = tokenize(headline);
    expect(tokenListToArray(result)).toEqual([]);
  });

  // Lists
  it('Should tokenize list values with dash', () => {
    const orgData = `- [ ] List item
- [ ] List item 2
- [ ] List item 3`;
    const result = tokenize(orgData);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: ' ', start: 3, end: 4 },
      { type: TokenType.Bracket, value: ']', start: 4, end: 5 },
      { type: TokenType.Text, value: ' List item', start: 5, end: 15 },
      { type: TokenType.NewLine, value: '\n', start: 15, end: 16 },
      { type: TokenType.Operator, value: '- ', start: 16, end: 18 },
      { type: TokenType.Bracket, value: '[', start: 18, end: 19 },
      { type: TokenType.Text, value: ' ', start: 19, end: 20 },
      { type: TokenType.Bracket, value: ']', start: 20, end: 21 },
      { type: TokenType.Text, value: ' List item 2', start: 21, end: 33 },
      { type: TokenType.NewLine, value: '\n', start: 33, end: 34 },
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
    expect(tokenListToArray(result)).toEqual([
      {
        type: TokenType.Text,
        value: 'Some text - with dash',
        start: 0,
        end: 21,
      },
    ]);
  });

  it('Should tokenize list values with plus', () => {
    const orgDoc = `+ [ ] List item
+ [ ] List item 2
+ [ ] List item 3`;
    const result = tokenize(orgDoc);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '+ ', start: 0, end: 2 },
      { type: TokenType.Bracket, value: '[', start: 2, end: 3 },
      { type: TokenType.Text, value: ' ', start: 3, end: 4 },
      { type: TokenType.Bracket, value: ']', start: 4, end: 5 },
      { type: TokenType.Text, value: ' List item', start: 5, end: 15 },
      { type: TokenType.NewLine, value: '\n', start: 15, end: 16 },
      { type: TokenType.Operator, value: '+ ', start: 16, end: 18 },
      { type: TokenType.Bracket, value: '[', start: 18, end: 19 },
      { type: TokenType.Text, value: ' ', start: 19, end: 20 },
      { type: TokenType.Bracket, value: ']', start: 20, end: 21 },
      { type: TokenType.Text, value: ' List item 2', start: 21, end: 33 },
      { type: TokenType.NewLine, value: '\n', start: 33, end: 34 },
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
    console.log('✎: [line 351][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Keyword, value: ':PROPERTIES:', start: 0, end: 12 },
      { type: TokenType.NewLine, value: '\n', start: 12, end: 13 },
      { type: TokenType.Keyword, value: ':ID:', start: 13, end: 17 },
      { type: TokenType.Text, value: '      123', start: 17, end: 26 },
      { type: TokenType.NewLine, value: '\n', start: 26, end: 27 },
      { type: TokenType.Keyword, value: ':END:', start: 27, end: 32 },
    ]);
  });

  it('Should parse bold text from start of line correctly', () => {
    const orgDoc = `*bold text*`;
    const result = tokenize(orgDoc);

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Bracket, value: '*', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '*', start: 10, end: 11 },
    ]);
  });

  it('Should parse plus as bracket when it doesnt start as list', () => {
    const orgDoc = `+bold text+`;
    const result = tokenize(orgDoc);

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Bracket, value: '+', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '+', start: 10, end: 11 },
    ]);
  });

  it('Should tokenize nested italic text', () => {
    const orgDoc = `This is */italic with bold/* text`;
    const result = tokenize(orgDoc);

    expect(tokenListToArray(result)).toEqual([
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

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List element', start: 2, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Operator, value: '- ', start: 15, end: 17 },
      {
        type: TokenType.Text,
        value: 'Second list element',
        start: 17,
        end: 36,
      },
      { type: TokenType.NewLine, value: '\n', start: 36, end: 37 },
      { type: TokenType.Indent, value: '  ', start: 37, end: 39 },
      {
        type: TokenType.Text,
        value: `Oh! i'am an nested section content`,
        start: 39,
        end: 73,
      },
    ]);
  });

  it('Should tokenize nested lists', () => {
    const orgDoc = `- List item 1 1
  - List item 2 1
  - List item 2 2
- List item 1 2`;

    const result = tokenize(orgDoc);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List item 1 1', start: 2, end: 15 },
      { type: TokenType.NewLine, value: '\n', start: 15, end: 16 },
      { type: TokenType.Indent, value: '  ', start: 16, end: 18 },
      { type: TokenType.Operator, value: '- ', start: 18, end: 20 },
      { type: TokenType.Text, value: 'List item 2 1', start: 20, end: 33 },
      { type: TokenType.NewLine, value: '\n', start: 33, end: 34 },
      { type: TokenType.Indent, value: '  ', start: 34, end: 36 },
      { type: TokenType.Operator, value: '- ', start: 36, end: 38 },
      { type: TokenType.Text, value: 'List item 2 2', start: 38, end: 51 },
      { type: TokenType.NewLine, value: '\n', start: 51, end: 52 },
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
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '- ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'List item 1', start: 2, end: 13 },
      { type: TokenType.NewLine, value: '\n', start: 13, end: 14 },
      { type: TokenType.Indent, value: '  ', start: 14, end: 16 },
      { type: TokenType.Operator, value: '- ', start: 16, end: 18 },
      { type: TokenType.Text, value: 'Nested list item 1', start: 18, end: 36 },
      { type: TokenType.NewLine, value: '\n', start: 36, end: 37 },
      { type: TokenType.Indent, value: '  ', start: 37, end: 39 },
      { type: TokenType.Operator, value: '- ', start: 39, end: 41 },
      { type: TokenType.Text, value: 'Nested list item 2', start: 41, end: 59 },
      { type: TokenType.NewLine, value: '\n', start: 59, end: 60 },
      { type: TokenType.Operator, value: '- ', start: 60, end: 62 },
      { type: TokenType.Text, value: 'List item 2', start: 62, end: 73 },
      { type: TokenType.NewLine, value: '\n', start: 73, end: 74 },
      { type: TokenType.NewLine, value: '\n', start: 74, end: 75 },
      { type: TokenType.Indent, value: '  ', start: 75, end: 77 },
      { type: TokenType.Text, value: 'Text with indent', start: 77, end: 93 },
    ]);
  });

  it('Should parse tokens from numeric list', () => {
    const orgDoc = `1. List item 1
2. List item 2`;

    const result = tokenize(orgDoc);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '1. ', start: 0, end: 3 },
      { type: TokenType.Text, value: 'List item 1', start: 3, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Operator, value: '2. ', start: 15, end: 18 },
      { type: TokenType.Text, value: 'List item 2', start: 18, end: 29 },
    ]);
  });

  it('Should parse tokens from numeric list with curvy bracket', () => {
    const orgDoc = `1) List item 1
2) List item 2
3) List item 3`;

    const result = tokenize(orgDoc);
    console.log(result);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '1) ', start: 0, end: 3 },
      { type: TokenType.Text, value: 'List item 1', start: 3, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Operator, value: '2) ', start: 15, end: 18 },
      { type: TokenType.Text, value: 'List item 2', start: 18, end: 29 },
      { type: TokenType.NewLine, value: '\n', start: 29, end: 30 },
      { type: TokenType.Operator, value: '3) ', start: 30, end: 33 },
      { type: TokenType.Text, value: 'List item 3', start: 33, end: 44 },
    ]);
  });

  it('Should parse tokens from nested numeric list with curvy bracket', () => {
    const orgDoc = `1) List item 1
  1) Nested List item 2
2) List item 3`;

    const result = tokenize(orgDoc);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Operator, value: '1) ', start: 0, end: 3 },
      { type: TokenType.Text, value: 'List item 1', start: 3, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Indent, value: '  ', start: 15, end: 17 },
      { type: TokenType.Operator, value: '1) ', start: 17, end: 20 },
      { type: TokenType.Text, value: 'Nested List item 2', start: 20, end: 38 },
      { type: TokenType.NewLine, value: '\n', start: 38, end: 39 },
      { type: TokenType.Operator, value: '2) ', start: 39, end: 42 },
      { type: TokenType.Text, value: 'List item 3', start: 42, end: 53 },
    ]);
  });

  it('Should parse tokens for src block', () => {
    const orgDoc = `#+BEGIN_SRC js
const a = 1;
#+END_SRC`;

    const result = tokenize(orgDoc);
    console.log('✎: [line 536][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 11, type: 'keyword', value: '#+BEGIN_SRC' },
      { start: 11, end: 14, type: 'text', value: ' js' },
      { start: 14, end: 15, type: 'newLine', value: '\n' },
      { start: 15, end: 23, type: 'text', value: 'const a ' },
      { start: 23, end: 24, type: 'bracket', value: '=' },
      { start: 24, end: 27, type: 'text', value: ' 1;' },
      { start: 27, end: 28, type: 'newLine', value: '\n' },
      { start: 28, end: 37, type: 'keyword', value: '#+END_SRC' },
    ]);
  });

  it('Should tokenize src block with properties', () => {
    const orgDoc = `#+BEGIN_SRC js :tangle test.js :exports no :noweb yes
const a = 1;
console.log(a);
#+END_SRC`;

    const result = tokenize(orgDoc);
    console.log('✎: [line 556][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 11, type: 'keyword', value: '#+BEGIN_SRC' },
      { start: 11, end: 15, type: 'text', value: ' js ' },
      { start: 15, end: 22, type: 'keyword', value: ':tangle' },
      { start: 22, end: 31, type: 'text', value: ' test.js ' },
      { start: 31, end: 39, type: 'keyword', value: ':exports' },
      { start: 39, end: 43, type: 'text', value: ' no ' },
      { start: 43, end: 49, type: 'keyword', value: ':noweb' },
      { start: 49, end: 53, type: 'text', value: ' yes' },
      { start: 53, end: 54, type: 'newLine', value: '\n' },
      { start: 54, end: 62, type: 'text', value: 'const a ' },
      { start: 62, end: 63, type: 'bracket', value: '=' },
      { start: 63, end: 66, type: 'text', value: ' 1;' },
      { start: 66, end: 67, type: 'newLine', value: '\n' },
      { start: 67, end: 82, type: 'text', value: 'console.log(a);' },
      { start: 82, end: 83, type: 'newLine', value: '\n' },
      { start: 83, end: 92, type: 'keyword', value: '#+END_SRC' },
    ]);
  });

  it('Should tokenize comment block', () => {
    const orgDoc = `# comment`;
    const result = tokenize(orgDoc);

    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 1, type: 'comment', value: '#' },
      { start: 1, end: 9, type: 'text', value: ' comment' },
    ]);
  });

  it('Should not parse comment without space', () => {
    const orgDoc = `#Comment without first space`;
    const result = tokenize(orgDoc);
    expect(tokenListToArray(result)).toEqual([
      {
        start: 0,
        end: 28,
        type: 'text',
        value: '#Comment without first space',
      },
    ]);
  });

  it('Should not tokenize comment token from middle of text', () => {
    const orgDoc = `This is not a # comment`;
    const result = tokenize(orgDoc);
    console.log('✎: [line 593][tokenizer.spec.ts] result: ', result);
    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 23, type: 'text', value: 'This is not a # comment' },
    ]);
  });

  it('Should tokenize active date', () => {
    const orgDatas = [
      '<2023-01-09 Mon>',
      '<2023-01-10 Tue>',
      '<2023-01-11 Wed>',
      '<2023-01-12 Thu>',
      '<2023-01-13 Fri>',
      '<2023-01-14 Sat>',
      '<2023-01-15 Sun>',
    ];
    orgDatas.forEach((orgData) => {
      const result = tokenListToArray(tokenize(orgData));
      console.log('✎: [line 606][tokenizer.spec.ts] result: ', result);
      expect(result).toEqual([
        { start: 0, end: 1, type: 'bracket', value: '<' },
        {
          start: 1,
          end: 15,
          type: 'text',
          value: orgData.slice(1, orgData.length - 1),
        },
        { start: 15, end: 16, type: 'bracket', value: '>' },
      ]);
    });
  });

  it('Should tokenize cative timestamp at the middle of text', () => {
    const orgDoc = `This is <2023-01-09 Mon> active date!`;
    const result = tokenListToArray(tokenize(orgDoc));
    console.log('✎: [line 613][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 8, type: 'text', value: 'This is ' },
      { start: 8, end: 9, type: 'bracket', value: '<' },
      { start: 9, end: 23, type: 'text', value: '2023-01-09 Mon' },
      { start: 23, end: 24, type: 'bracket', value: '>' },
      { start: 24, end: 37, type: 'text', value: ' active date!' },
    ]);
  });

  it('Should tokenize inactive timestamp', () => {
    const orgDatas = [
      '[2023-01-09 Mon]',
      '[2023-01-10 Tue]',
      '[2023-01-11 Wed]',
      '[2023-01-12 Thu]',
      '[2023-01-13 Fri]',
      '[2023-01-14 Sat]',
      '[2023-01-15 Sun]',
    ];
    orgDatas.forEach((orgData) => {
      const result = tokenListToArray(tokenize(orgData));
      console.log('✎: [line 606][tokenizer.spec.ts] result: ', result);
      expect(result).toEqual([
        { start: 0, end: 1, type: 'bracket', value: '[' },
        {
          start: 1,
          end: 15,
          type: 'text',
          value: orgData.slice(1, orgData.length - 1),
        },
        { start: 15, end: 16, type: 'bracket', value: ']' },
      ]);
    });
  });

  it('Should tokenize incative timestamp at the middle of text', () => {
    const orgDoc = `* Need to buy new mechanical keyboard [2023-01-09 Mon]!`;

    const result = tokenListToArray(tokenize(orgDoc));
    console.log('✎: [line 614][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 2, type: 'headline', value: '* ' },
      {
        start: 2,
        end: 38,
        type: 'text',
        value: 'Need to buy new mechanical keyboard ',
      },
      { start: 38, end: 39, type: 'bracket', value: '[' },
      { start: 39, end: 53, type: 'text', value: '2023-01-09 Mon' },
      { start: 53, end: 54, type: 'bracket', value: ']' },
      { start: 54, end: 55, type: 'text', value: '!' },
    ]);
  });

  it('Should tokenize active date range', () => {
    const orgDoc = `<2023-01-09 Mon>--<2023-01-10 Tue>`;
    const result = tokenListToArray(tokenize(orgDoc));
    // console.log('✎: [line 624][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '<' },
      { start: 1, end: 15, type: 'text', value: '2023-01-09 Mon' },
      { start: 15, end: 16, type: 'bracket', value: '>' },
      { start: 16, end: 18, type: 'text', value: '--' },
      { start: 18, end: 19, type: 'bracket', value: '<' },
      { start: 19, end: 33, type: 'text', value: '2023-01-10 Tue' },
      { start: 33, end: 34, type: 'bracket', value: '>' },
    ]);
  });

  it('Should tokenize inactive date with time', () => {
    const orgDoc = `[2023-01-09 Mon 10:00]`;
    const result = tokenListToArray(tokenize(orgDoc));
    // console.log('✎: [line 634][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '[' },
      { start: 1, end: 21, type: 'text', value: '2023-01-09 Mon 10:00' },
      { start: 21, end: 22, type: 'bracket', value: ']' },
    ]);
  });

  it('Should tokenize text with opened bracket', () => {
    const orgDoc = `This is a reminder for meeting on <2023-01-09 Mon 14:00. Don't forget to attend.`;
    const result = tokenListToArray(tokenize(orgDoc));
    // console.log('✎: [line 644][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      {
        start: 0,
        end: 34,
        type: 'text',
        value: 'This is a reminder for meeting on ',
      },
      { start: 34, end: 35, type: 'bracket', value: '<' },
      {
        start: 35,
        end: 80,
        type: 'text',
        value: `2023-01-09 Mon 14:00. Don't forget to attend.`,
      },
    ]);
  });

  it('Should tokenize simple org link', () => {
    const orgDoc = `[[http://google.com][LINK NAME!]]`;
    const result = tokenListToArray(tokenize(orgDoc));
    console.log('✎: [line 654][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '[' },
      { start: 1, end: 2, type: 'bracket', value: '[' },
      { start: 2, end: 19, type: 'text', value: 'http://google.com' },
      { start: 19, end: 20, type: 'bracket', value: ']' },
      { start: 20, end: 21, type: 'bracket', value: '[' },
      { start: 21, end: 31, type: 'text', value: 'LINK NAME!' },
      { start: 31, end: 32, type: 'bracket', value: ']' },
      { start: 32, end: 33, type: 'bracket', value: ']' },
    ]);
  });

  it('Should tokenize html block', () => {
    const orgDoc = `#+BEGIN_HTML
<div>
  <p>Some text</p>
</div>
#+END_HTML`;
    const result = tokenListToArray(tokenize(orgDoc));
    console.log('✎: [line 670][tokenizer.spec.ts] result: ', result);
    expect(result).toEqual([
      { start: 0, end: 12, type: 'keyword', value: '#+BEGIN_HTML' },
      { start: 12, end: 13, type: 'newLine', value: '\n' },
      { start: 13, end: 14, type: 'bracket', value: '<' },
      { start: 14, end: 17, type: 'text', value: 'div' },
      { start: 17, end: 18, type: 'bracket', value: '>' },
      { start: 18, end: 19, type: 'newLine', value: '\n' },
      { start: 19, end: 21, type: 'indent', value: '  ' },
      { start: 21, end: 22, type: 'bracket', value: '<' },
      { start: 22, end: 23, type: 'text', value: 'p' },
      { start: 23, end: 24, type: 'bracket', value: '>' },
      { start: 24, end: 33, type: 'text', value: 'Some text' },
      { start: 33, end: 34, type: 'bracket', value: '<' },
      { start: 34, end: 35, type: 'bracket', value: '/' },
      { start: 35, end: 36, type: 'text', value: 'p' },
      { start: 36, end: 37, type: 'bracket', value: '>' },
      { start: 37, end: 38, type: 'newLine', value: '\n' },
      { start: 38, end: 39, type: 'bracket', value: '<' },
      { start: 39, end: 40, type: 'bracket', value: '/' },
      { start: 40, end: 43, type: 'text', value: 'div' },
      { start: 43, end: 44, type: 'bracket', value: '>' },
      { start: 44, end: 45, type: 'newLine', value: '\n' },
      { start: 45, end: 55, type: 'keyword', value: '#+END_HTML' },
    ]);
  });
});
