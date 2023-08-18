import { Token, TokenType } from '../models';
import { tokenize } from './tokenizer';
import { parserConfiguration } from '../parser/parser.configuration';

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
    const result = tokenListToArray(tokenize(headline, parserConfiguration));

    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world', start: 2, end: 13 },
    ]);
  });

  it('Should create tokens for simple headline with space at the end', () => {
    const headline = '* Hello world ';
    const result = tokenListToArray(tokenize(headline, parserConfiguration));

    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Hello world ', start: 2, end: 14 },
    ]);
  });

  it('Should create tokens for headline with large start space', () => {
    const headline = '*    Hello world';
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Indent, value: ' ', start: 0, end: 1 },
      { type: TokenType.Text, value: '* Hello world', start: 1, end: 14 },
    ]);
  });

  it('Should correct parse tokens for string with first space and aasterisk before word', () => {
    const headline = ` *Hello world`;
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenListToArray(tokenize(headline, parserConfiguration));
    expect(result).toEqual([
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
    const result = tokenListToArray(tokenize(headline, parserConfiguration));
    expect(result).toEqual([
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
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
    const result = tokenize(headline, parserConfiguration);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Headline 123', start: 2, end: 14 },
      { type: TokenType.NewLine, value: '\n', start: 14, end: 15 },
      { type: TokenType.Text, value: 'Some text', start: 15, end: 24 },
    ]);
  });

  it('Should tokenize headline with tags', () => {
    const headline = `* Headline 123 :tag1:tag2:`;
    const result = tokenListToArray(tokenize(headline, parserConfiguration));
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ', start: 0, end: 2 },
      { type: TokenType.Text, value: 'Headline 123 ', start: 2, end: 15 },
      { type: TokenType.Operator, value: ':', start: 15, end: 16 },
      { type: TokenType.Text, value: 'tag1', start: 16, end: 20 },
      { type: TokenType.Operator, value: ':', start: 20, end: 21 },
      { type: TokenType.Text, value: 'tag2', start: 21, end: 25 },
      { type: TokenType.Operator, value: ':', start: 25, end: 26 },
    ]);
  });

  // Simple text
  it('Should tokenize simple text', () => {
    const headline = `Some text`;
    const result = tokenize(headline, parserConfiguration);
    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Text, value: 'Some text', start: 0, end: 9 },
    ]);
  });

  it('Should not create token from empty imput string', () => {
    const headline = ``;
    const result = tokenize(headline, parserConfiguration);
    expect(tokenListToArray(result)).toEqual([]);
  });

  // Lists
  it('Should tokenize list values with dash', () => {
    const orgDoc = `- [ ] List item
- [ ] List item 2
- [ ] List item 3`;
    const result = tokenize(orgDoc, parserConfiguration);
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
    const orgDoc = `Some text - with dash`;
    const result = tokenize(orgDoc, parserConfiguration);
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
    const result = tokenize(orgDoc, parserConfiguration);
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
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
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
    const result = tokenize(orgDoc, parserConfiguration);

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Bracket, value: '*', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '*', start: 10, end: 11 },
    ]);
  });

  it('Should parse plus as bracket when it doesnt start as list', () => {
    const orgDoc = `+bold text+`;
    const result = tokenize(orgDoc, parserConfiguration);

    expect(tokenListToArray(result)).toEqual([
      { type: TokenType.Bracket, value: '+', start: 0, end: 1 },
      { type: TokenType.Text, value: 'bold text', start: 1, end: 10 },
      { type: TokenType.Bracket, value: '+', start: 10, end: 11 },
    ]);
  });

  it('Should tokenize nested italic text', () => {
    const orgDoc = `This is */italic with bold/* text`;
    const result = tokenize(orgDoc, parserConfiguration);

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

    const result = tokenize(orgDoc, parserConfiguration);

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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenize(orgDoc, parserConfiguration);
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

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
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
    const result = tokenize(orgDoc, parserConfiguration);

    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 1, type: 'comment', value: '#' },
      { start: 1, end: 9, type: 'text', value: ' comment' },
    ]);
  });

  it('Should not parse comment without space', () => {
    const orgDoc = `#Comment without first space`;
    const result = tokenize(orgDoc, parserConfiguration);
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
    const result = tokenize(orgDoc, parserConfiguration);
    expect(tokenListToArray(result)).toEqual([
      { start: 0, end: 23, type: 'text', value: 'This is not a # comment' },
    ]);
  });

  it('Should tokenize active date', () => {
    const orgDocs = [
      '<2023-01-09 Mon>',
      '<2023-01-10 Tue>',
      '<2023-01-11 Wed>',
      '<2023-01-12 Thu>',
      '<2023-01-13 Fri>',
      '<2023-01-14 Sat>',
      '<2023-01-15 Sun>',
    ];
    orgDocs.forEach((orgDoc) => {
      const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
      expect(result).toEqual([
        { start: 0, end: 1, type: 'bracket', value: '<' },
        {
          start: 1,
          end: 15,
          type: 'text',
          value: orgDoc.slice(1, orgDoc.length - 1),
        },
        { start: 15, end: 16, type: 'bracket', value: '>' },
      ]);
    });
  });

  it('Should tokenize cative timestamp at the middle of text', () => {
    const orgDoc = `This is <2023-01-09 Mon> active date!`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 8, type: 'text', value: 'This is ' },
      { start: 8, end: 9, type: 'bracket', value: '<' },
      { start: 9, end: 23, type: 'text', value: '2023-01-09 Mon' },
      { start: 23, end: 24, type: 'bracket', value: '>' },
      { start: 24, end: 37, type: 'text', value: ' active date!' },
    ]);
  });

  it('Should tokenize inactive timestamp', () => {
    const orgDocs = [
      '[2023-01-09 Mon]',
      '[2023-01-10 Tue]',
      '[2023-01-11 Wed]',
      '[2023-01-12 Thu]',
      '[2023-01-13 Fri]',
      '[2023-01-14 Sat]',
      '[2023-01-15 Sun]',
    ];
    orgDocs.forEach((orgDoc) => {
      const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
      expect(result).toEqual([
        { start: 0, end: 1, type: 'bracket', value: '[' },
        {
          start: 1,
          end: 15,
          type: 'text',
          value: orgDoc.slice(1, orgDoc.length - 1),
        },
        { start: 15, end: 16, type: 'bracket', value: ']' },
      ]);
    });
  });

  it('Should tokenize incative timestamp at the middle of text', () => {
    const orgDoc = `* Need to buy new mechanical keyboard [2023-01-09 Mon]!`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
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
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
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

  it('Should tokenize date with offset', () => {
    const orgDoc = `<2023-01-09 Mon +1h>`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '<' },
      { start: 1, end: 16, type: 'text', value: '2023-01-09 Mon ' },
      { start: 16, end: 17, type: 'bracket', value: '+' },
      { start: 17, end: 19, type: 'text', value: '1h' },
      { start: 19, end: 20, type: 'bracket', value: '>' },
    ]);
  });

  it('Should tokenize inactive date with time', () => {
    const orgDoc = `[2023-01-09 Mon 10:00]`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '[' },
      { start: 1, end: 21, type: 'text', value: '2023-01-09 Mon 10:00' },
      { start: 21, end: 22, type: 'bracket', value: ']' },
    ]);
  });

  it('Should tokenize text with opened bracket', () => {
    const orgDoc = `This is a reminder for meeting on <2023-01-09 Mon 14:00. Don't forget to attend.`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
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
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '[' },
      { start: 1, end: 2, type: 'bracket', value: '[' },
      { start: 2, end: 19, type: 'link', value: 'http://google.com' },
      { start: 19, end: 20, type: 'bracket', value: ']' },
      { start: 20, end: 21, type: 'bracket', value: '[' },
      { start: 21, end: 31, type: 'text', value: 'LINK NAME!' },
      { start: 31, end: 32, type: 'bracket', value: ']' },
      { start: 32, end: 33, type: 'bracket', value: ']' },
    ]);
  });

  it('Should tokenize link with nested path', () => {
    const orgDoc = `[[https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/][Emacs lisp за 15 минут]]`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 1, type: 'bracket', value: '[' },
      { start: 1, end: 2, type: 'bracket', value: '[' },
      {
        start: 2,
        end: 51,
        type: 'link',
        value: 'https://bzg.fr/en/learn-emacs-lisp-in-15-minutes/',
      },
      { start: 51, end: 52, type: 'bracket', value: ']' },
      { start: 52, end: 53, type: 'bracket', value: '[' },
      { start: 53, end: 75, type: 'text', value: 'Emacs lisp за 15 минут' },
      { start: 75, end: 76, type: 'bracket', value: ']' },
      { start: 76, end: 77, type: 'bracket', value: ']' },
    ]);
  });

  it('Should tokenize html block', () => {
    const orgDoc = `#+BEGIN_HTML
<div>
  <p>Some text</p>
</div>
#+END_HTML`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
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

  it('Should tokenize inline code', () => {
    const orgDoc = `This is a code block: ~code~`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 22, type: 'text', value: 'This is a code block: ' },
      { start: 22, end: 23, type: 'bracket', value: '~' },
      { start: 23, end: 27, type: 'text', value: 'code' },
      { start: 27, end: 28, type: 'bracket', value: '~' },
    ]);
  });

  it('Should tokenize property keyword', () => {
    const orgDoc = `#+PROPERTY: NAME VALUE`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 11, type: 'keyword', value: '#+PROPERTY:' },
      { start: 11, end: 22, type: 'text', value: ' NAME VALUE' },
    ]);
  });

  it('Should tokenize keyword', () => {
    const orgDoc = `#+KEYWORD: VALUE`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 10, type: 'keyword', value: '#+KEYWORD:' },
      { start: 10, end: 16, type: 'text', value: ' VALUE' },
    ]);
  });

  it('Should tokenize keyword started with indentation', () => {
    const orgDoc = `* Heading
     #+KEYWORD: VALUE`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'headline', value: '* ' },
      { start: 2, end: 9, type: 'text', value: 'Heading' },
      { start: 9, end: 10, type: 'newLine', value: '\n' },
      { start: 10, end: 15, type: 'indent', value: '     ' },
      { start: 15, end: 25, type: 'keyword', value: '#+KEYWORD:' },
      { start: 25, end: 31, type: 'text', value: ' VALUE' },
    ]);
  });

  it('Should tokenize inline latex block', () => {
    const orgDoc = `This is a latex block: $\\alpha$`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      {
        start: 0,
        end: 23,
        type: 'text',
        value: 'This is a latex block: ',
      },
      { start: 23, end: 24, type: 'bracket', value: '$' },
      { start: 24, end: 30, type: 'keyword', value: '\\alpha' },
      { start: 30, end: 31, type: 'bracket', value: '$' },
    ]);
  });

  it('Should tokenize latex with 2 $', () => {
    const orgDoc = `This is also a latex text: $$1+1=2$$`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));

    expect(result).toEqual([
      {
        start: 0,
        end: 27,
        type: 'text',
        value: 'This is also a latex text: ',
      },
      { start: 27, end: 29, type: 'bracket', value: '$$' },
      { start: 29, end: 30, type: 'text', value: '1' },
      { start: 30, end: 31, type: 'bracket', value: '+' },
      { start: 31, end: 32, type: 'text', value: '1' },
      { start: 32, end: 33, type: 'bracket', value: '=' },
      { start: 33, end: 34, type: 'text', value: '2' },
      { start: 34, end: 36, type: 'bracket', value: '$$' },
    ]);
  });

  it('Should parse horizontal rule', () => {
    const orgDoc = '------';
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 6, type: 'horizontalRule', value: '------' },
    ]);
  });

  it('Should not parse text as horizontal rule', () => {
    const orgDoc = 'Some text ------';
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 16, type: 'text', value: 'Some text ------' },
    ]);
  });

  it('Should not tokenize text as horizontal rule when text is exist after rule', () => {
    const orgDoc = '------ Some text';
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 16, type: 'text', value: '------ Some text' },
    ]);
  });

  it('Should tokenize latex environment', () => {
    const orgDoc = `\\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
\\end{align*}`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      {
        start: 0,
        end: 6,
        type: 'latexEnvironmentKeyword',
        value: '\\begin',
      },
      { start: 6, end: 7, type: 'latexBracket', value: '{' },
      { start: 7, end: 12, type: 'text', value: 'align' },
      { start: 12, end: 13, type: 'bracket', value: '*' },
      { start: 13, end: 14, type: 'latexBracket', value: '}' },
      { start: 14, end: 15, type: 'newLine', value: '\n' },
      { start: 15, end: 24, type: 'text', value: '2x - 5y &' },
      { start: 24, end: 25, type: 'bracket', value: '=' },
      { start: 25, end: 28, type: 'text', value: ' 8 ' },
      { start: 28, end: 29, type: 'keyword', value: '\\' },
      { start: 29, end: 30, type: 'keyword', value: '\\' },
      { start: 30, end: 31, type: 'newLine', value: '\n' },
      { start: 31, end: 34, type: 'text', value: '3x ' },
      { start: 34, end: 35, type: 'bracket', value: '+' },
      { start: 35, end: 40, type: 'text', value: ' 9y &' },
      { start: 40, end: 41, type: 'bracket', value: '=' },
      { start: 41, end: 42, type: 'text', value: ' ' },
      { start: 42, end: 43, type: 'operator', value: '-' },
      { start: 43, end: 45, type: 'text', value: '12' },
      { start: 45, end: 46, type: 'newLine', value: '\n' },
      {
        start: 46,
        end: 50,
        type: 'latexEnvironmentKeyword',
        value: '\\end',
      },
      { start: 50, end: 51, type: 'latexBracket', value: '{' },
      { start: 51, end: 56, type: 'text', value: 'align' },
      { start: 56, end: 57, type: 'bracket', value: '*' },
      { start: 57, end: 58, type: 'latexBracket', value: '}' },
    ]);
  });

  it('Should not tokenize latex environment keyword when previous line is not EOL', () => {
    const orgDoc = `Some text that broken latex environment \\begin{align*}
2x - 5y &= 8 \\\\
3x + 9y &= -12
BROKE\\end{align*}`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      {
        start: 0,
        end: 46,
        type: 'text',
        value: 'Some text that broken latex environment \\begin',
      },
      { start: 46, end: 47, type: 'latexBracket', value: '{' },
      { start: 47, end: 52, type: 'text', value: 'align' },
      { start: 52, end: 53, type: 'bracket', value: '*' },
      { start: 53, end: 54, type: 'latexBracket', value: '}' },
      { start: 54, end: 55, type: 'newLine', value: '\n' },
      { start: 55, end: 64, type: 'text', value: '2x - 5y &' },
      { start: 64, end: 65, type: 'bracket', value: '=' },
      { start: 65, end: 68, type: 'text', value: ' 8 ' },
      { start: 68, end: 69, type: 'keyword', value: '\\' },
      { start: 69, end: 70, type: 'keyword', value: '\\' },
      { start: 70, end: 71, type: 'newLine', value: '\n' },
      { start: 71, end: 74, type: 'text', value: '3x ' },
      { start: 74, end: 75, type: 'bracket', value: '+' },
      { start: 75, end: 80, type: 'text', value: ' 9y &' },
      { start: 80, end: 81, type: 'bracket', value: '=' },
      { start: 81, end: 82, type: 'text', value: ' ' },
      { start: 82, end: 83, type: 'operator', value: '-' },
      { start: 83, end: 85, type: 'text', value: '12' },
      { start: 85, end: 86, type: 'newLine', value: '\n' },
      { start: 86, end: 95, type: 'text', value: 'BROKE\\end' },
      { start: 95, end: 96, type: 'latexBracket', value: '{' },
      { start: 96, end: 101, type: 'text', value: 'align' },
      { start: 101, end: 102, type: 'bracket', value: '*' },
      { start: 102, end: 103, type: 'latexBracket', value: '}' },
    ]);
  });

  it('Should parse colon with space as operator', () => {
    const orgDoc = `: `;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: ': ' },
    ]);
  });

  it('Should tokenize fixed with values', () => {
    const orgDoc = `: Fixed value`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: ': ' },
      { start: 2, end: 13, type: 'text', value: 'Fixed value' },
    ]);
  });

  it('Should tokenize multiple fixed width values', () => {
    const orgDoc = `: Fixed value
: Fixed value`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: ': ' },
      { start: 2, end: 13, type: 'text', value: 'Fixed value' },
      { start: 13, end: 14, type: 'newLine', value: '\n' },
      { start: 14, end: 16, type: 'operator', value: ': ' },
      { start: 16, end: 27, type: 'text', value: 'Fixed value' },
    ]);
  });

  it('Should tokenize fixed width started from spaces', () => {
    const orgDoc = `: Fixed width line 1
        *Bold text*
        : Fixed width line 2`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));

    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: ': ' },
      {
        start: 2,
        end: 20,
        type: 'text',
        value: 'Fixed width line 1',
      },
      { start: 20, end: 21, type: 'newLine', value: '\n' },
      { start: 21, end: 29, type: 'indent', value: '        ' },
      { start: 29, end: 30, type: 'bracket', value: '*' },
      { start: 30, end: 39, type: 'text', value: 'Bold text' },
      { start: 39, end: 40, type: 'bracket', value: '*' },
      { start: 40, end: 41, type: 'newLine', value: '\n' },
      { start: 41, end: 49, type: 'indent', value: '        ' },
      { start: 49, end: 51, type: 'operator', value: ': ' },
      {
        start: 51,
        end: 69,
        type: 'text',
        value: 'Fixed width line 2',
      },
    ]);
  });

  it('Should note tokenize colon operator inside center of text', () => {
    const orgDoc = `Some text: Hello world`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 22, type: 'text', value: 'Some text: Hello world' },
    ]);
  });

  it('Should not tokenize fixed width operator when previous value is not space or eol', () => {
    const orgDoc = `Some text : Hello world`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 23, type: 'text', value: 'Some text : Hello world' },
    ]);
  });

  xit('Should tokenize list tag', () => {
    const orgDoc = `- I'am a tag :: I'am a value`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: '- ' },
      { start: 2, end: 13, type: 'text', value: "I'am a tag " },
      { start: 13, end: 20, type: 'keyword', value: ":: I'am" },
      { start: 20, end: 28, type: 'text', value: ' a value' },
    ]);
  });

  it('Should tokenize file tags', () => {
    const orgDoc = `#+FILETAGS: :tag1:tag2:tag3:`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 11, type: 'keyword', value: '#+FILETAGS:' },
      { start: 11, end: 12, type: 'text', value: ' ' },
      { start: 12, end: 13, type: 'operator', value: ':' },
      { start: 13, end: 17, type: 'text', value: 'tag1' },
      { start: 17, end: 18, type: 'operator', value: ':' },
      { start: 18, end: 22, type: 'text', value: 'tag2' },
      { start: 22, end: 23, type: 'operator', value: ':' },
      { start: 23, end: 27, type: 'text', value: 'tag3' },
      { start: 27, end: 28, type: 'operator', value: ':' },
    ]);
  });

  // TODO: master Conflict with src block peroperties. Need to change logic of src block attribute collecting inside parser.
  // TODO: tokenizer.ts:150 should be removed.
  xit('Should tokenize tags with spaces', () => {
    const orgDoc = `#+FILETAGS: :test:first big note:hello world:`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 11, type: 'keyword', value: '#+FILETAGS:' },
      { start: 11, end: 12, type: 'text', value: ' ' },
      { start: 12, end: 13, type: 'operator', value: ':' },
      { start: 13, end: 17, type: 'text', value: 'test' },
      { start: 17, end: 23, type: 'keyword', value: ':first' },
      {
        start: 23,
        end: 45,
        type: 'text',
        value: ' big note:hello world:',
      },
    ]);
  });

  it('Should tokenize list tag as operator', () => {
    const orgDoc = `- I'am a tag :: I'am a value`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: '- ' },
      { start: 2, end: 13, type: 'text', value: "I'am a tag " },
      { start: 13, end: 15, type: 'operator', value: '::' },
      { start: 15, end: 28, type: 'text', value: " I'am a value" },
    ]);
  });

  it('Should not tokenize list tag when no space after operator', () => {
    const orgDoc = `- I'am a tag ::I'am a value`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 2, type: 'operator', value: '- ' },
      {
        start: 2,
        end: 27,
        type: 'text',
        value: "I'am a tag ::I'am a value",
      },
    ]);
  });

  it('Should tokenize table', () => {
    const orgDoc = `| A | B | C |
|---+---+---|
| 1 | 2 | 3 |`;

    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));

    expect(result).toEqual([
      { start: 0, end: 1, type: 'tableOperator', value: '|' },
      { start: 1, end: 4, type: 'text', value: ' A ' },
      { start: 4, end: 5, type: 'tableOperator', value: '|' },
      { start: 5, end: 8, type: 'text', value: ' B ' },
      { start: 8, end: 9, type: 'tableOperator', value: '|' },
      { start: 9, end: 12, type: 'text', value: ' C ' },
      { start: 12, end: 13, type: 'tableOperator', value: '|' },
      { start: 13, end: 14, type: 'newLine', value: '\n' },
      { start: 14, end: 15, type: 'tableOperator', value: '|' },
      { start: 15, end: 18, type: 'text', value: '---' },
      { start: 18, end: 19, type: 'bracket', value: '+' },
      { start: 19, end: 22, type: 'text', value: '---' },
      { start: 22, end: 23, type: 'bracket', value: '+' },
      { start: 23, end: 26, type: 'text', value: '---' },
      { start: 26, end: 27, type: 'tableOperator', value: '|' },
      { start: 27, end: 28, type: 'newLine', value: '\n' },
      { start: 28, end: 29, type: 'tableOperator', value: '|' },
      { start: 29, end: 32, type: 'text', value: ' 1 ' },
      { start: 32, end: 33, type: 'tableOperator', value: '|' },
      { start: 33, end: 36, type: 'text', value: ' 2 ' },
      { start: 36, end: 37, type: 'tableOperator', value: '|' },
      { start: 37, end: 40, type: 'text', value: ' 3 ' },
      { start: 40, end: 41, type: 'tableOperator', value: '|' },
    ]);
  });

  it('Should tokenize raw link', () => {
    const orgDoc = `it's a link https://www.google.com`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 12, type: 'text', value: "it's a link " },
      { start: 12, end: 34, type: 'link', value: 'https://www.google.com' },
    ]);
  });

  it('Should tokenize raw link', () => {
    const orgDoc = `it's a link https://www.google.com/some-link?hello=123`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));

    expect(result).toEqual([
      { start: 0, end: 12, type: 'text', value: "it's a link " },
      {
        start: 12,
        end: 54,
        type: 'link',
        value: 'https://www.google.com/some-link?hello=123',
      },
    ]);
  });

  it('Should tokenize complex raw link', () => {
    const orgDoc = `https://vc.ru/u/1213284-relocatus/451414-kak-poluchit-vnzh-gruzii-rossiyaninu-v-2022-godu#:~:text=%D0%96%D0%B5%D0%BB%D0%B0%D1%8E%D1%89%D0%B8%D0%BC%20%D0%BF%D0%BE%D0%BB%D1%83%D1%87%D0%B8%D1%82%D1%8C%20%D1%8D%D1%82%D0%BE%D1%82%20%D0%B2%D0%B8%D0%B4%20%D0%92%D0%9D%D0%96,%D0%B4%D0%BE%D0%BB%D0%B6%D0%BD%D0%B0%20%D0%BF%D0%BE%D0%B4%D1%82%D0%B2%D0%B5%D1%80%D0%B6%D0%B4%D0%B0%D1%82%D1%8C%D1%81%D1%8F%20%D0%BE%D1%86%D0%B5%D0%BD%D0%BA%D0%BE%D0%B9%20%D0%BD%D0%B5%D0%B7%D0%B0%D0%B2%D0%B8%D1%81%D0%B8%D0%BC%D0%BE%D0%B3%D0%BE%20%D1%8D%D0%BA%D1%81%D0%BF%D0%B5%D1%80%D1%82%D0%B0.`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { start: 0, end: 562, type: 'link', value: orgDoc },
    ]);
  });

  it('Should tokenize table cell with nested dash', () => {
    const orgDoc = `| -      | 2  |`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { end: 1, start: 0, type: 'tableOperator', value: '|' },
      { end: 9, start: 1, type: 'text', value: ' -      ' },
      { end: 10, start: 9, type: 'tableOperator', value: '|' },
      { end: 14, start: 10, type: 'text', value: ' 2  ' },
      { end: 15, start: 14, type: 'tableOperator', value: '|' },
    ]);
  });

  it('Should tokenize table cell with dash after word', () => {
    const orgDoc = `| qwa-      | 2  |`;
    const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
    expect(result).toEqual([
      { end: 1, start: 0, type: 'tableOperator', value: '|' },
      { end: 12, start: 1, type: 'text', value: ' qwa-      ' },
      { end: 13, start: 12, type: 'tableOperator', value: '|' },
      { end: 17, start: 13, type: 'text', value: ' 2  ' },
      { end: 18, start: 17, type: 'tableOperator', value: '|' },
    ]);
  });

  // it('Should parse latex fragment with backslash', () => {
  //   const orgDoc = "\\(e^{i \\pi}\\)"
  //   const result = tokenListToArray(tokenize(orgDoc, parserConfiguration));
  //   // console.log('✎: [line 709][tokenizer.spec.ts] result: ', result);
  //   expect(result).toEqual([
  //     { start: 0, end: 1, type: 'bracket', value: '\\' },
  //     { start: 1, end }
  //   ])

  // })
});
