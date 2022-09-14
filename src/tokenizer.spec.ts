import { TokenType } from './types';
import { tokenize } from './tokenizer';

describe('Tokenizer', () => {
  // Headings
  it('Should create tokens for simple headline', () => {
    const headline = '* Hello world';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Hello world' },
    ]);
  });

  it('Should create tokens for simple headline with space at the end', () => {
    const headline = '* Hello world ';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Hello world ' },
    ]);
  });

  it('Should create tokens for headline with large start space', () => {
    const headline = '*    Hello world';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: '   Hello world' },
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
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Hello world\n' },
      { type: TokenType.Headline, value: '** ' },
      { type: TokenType.Text, value: 'Nested headline\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.Text, value: 'Another one headline!\n' },
      { type: TokenType.Headline, value: '**** ' },
      { type: TokenType.Text, value: 'And 4 headline level\n' },
      { type: TokenType.Headline, value: '***** ' },
      { type: TokenType.Text, value: 'And 5 headline level\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'And again 1 level headline' },
    ]);
  });

  it('Should create tokens for headlines at same level', () => {
    const headline = `* Hello world
* Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Hello world\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 2\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should correct parse tokens for string with first space and aasterisk', () => {
    const headline = ` * Hello world`;
    const result = tokenize(headline);
    expect(result).toEqual([{ type: TokenType.Text, value: ' * Hello world' }]);
  });

  it('Should correct parse tokens for string with first space and aasterisk before word', () => {
    const headline = ` *Hello world`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '*' },
      { type: TokenType.Text, value: 'Hello world' },
    ]);
  });

  it('Should create tokens for todo keywords inside headlines', () => {
    const headline = `* TODO Hello world
*** DONE Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Keyword, value: 'TODO' },
      { type: TokenType.Text, value: ' Hello world\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.Keyword, value: 'DONE' },
      { type: TokenType.Text, value: ' Headline 2\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should note create tokens for todo keywords in middle of headline', () => {
    const headline = `* This is not a TODO
* And this is not a DONE
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'This is not a TODO\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'And this is not a DONE\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should create tokens for headline with priority', () => {
    const headline = `* [#A] Most important headline
*** [#B] Third most important headline
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#A' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Most important headline\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#B' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Third most important headline\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should create tokens for headlines with todo keyword and priority', () => {
    const headline = `* TODO [#A] Most important headline
*** DONE [#B] Third most important headline
* HOLD [#c] Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Keyword, value: 'TODO' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#A' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Most important headline\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.Keyword, value: 'DONE' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#B' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Third most important headline\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Keyword, value: 'HOLD' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#c' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Headline 3' },
    ]);
  });

  it('Should create tokens for statistics cookies', () => {
    const headline = `* TODO [50%] [#A] Most important headline`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Keyword, value: 'TODO' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: '50%' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#A' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Most important headline' },
    ]);
  });

  it('Should create statistic with delimiter', () => {
    const headline = `* [1/2] Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: '1' },
      { type: TokenType.Bracket, value: '/' },
      { type: TokenType.Text, value: '2' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Headline 123' },
    ]);
  });

  it('Should tokenize statistic without trailing space', () => {
    const headline = `* [1/2]Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: '1' },
      { type: TokenType.Bracket, value: '/' },
      { type: TokenType.Text, value: '2' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: 'Headline 123' },
    ]);
  });

  it('Should tokenize statistic with large trailing space', () => {
    const headline = `* [1/2]      Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: '1' },
      { type: TokenType.Bracket, value: '/' },
      { type: TokenType.Text, value: '2' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: '      Headline 123' },
    ]);
  });

  it('Should tokenize headline after simple text', () => {
    const headline = `Some text
* Headline 123`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Text, value: 'Some text\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 123' },
    ]);
  });

  it('Should tokenize simple text after headline', () => {
    const headline = `* Headline 123
Some text`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.Text, value: 'Headline 123\n' },
      { type: TokenType.Text, value: 'Some text' },
    ]);
  });

  // Simple text
  it('Should tokenize simple text', () => {
    const headline = `Some text`;
    const result = tokenize(headline);
    expect(result).toEqual([{ type: TokenType.Text, value: 'Some text' }]);
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
      { type: TokenType.Operator, value: '- ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item\n' },
      { type: TokenType.Operator, value: '- ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item 2\n' },
      { type: TokenType.Operator, value: '- ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item 3' },
    ]);
  });

  it('Should tokenize the dash as text when the dash is not at the start position', () => {
    const orgData = `Some text - with dash`;
    const result = tokenize(orgData);
    expect(result).toEqual([{ type: TokenType.Text, value: 'Some text - with dash' }]);
  });

  it('Should tokenize list values with plus', () => {
    const orgDoc = `+ [ ] List item
+ [ ] List item 2
+ [ ] List item 3`;
    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: '+ ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item\n' },
      { type: TokenType.Operator, value: '+ ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item 2\n' },
      { type: TokenType.Operator, value: '+ ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' List item 3' },
    ]);
  });

  it('Should tokenize properties keywords', () => {
    const orgDoc = `:PROPERTIES:
:ID:      123
:END:`;
    const result = tokenize(orgDoc);
    expect(result).toEqual([
      { type: TokenType.Operator, value: ':' },
      { type: TokenType.Text, value: 'PROPERTIES' },
      { type: TokenType.Operator, value: ':' },
      { type: TokenType.Text, value: '\n' },
      { type: TokenType.Operator, value: ':' },
      { type: TokenType.Text, value: 'ID' },
      { type: TokenType.Operator, value: ':' },
      { type: TokenType.Text, value: '      123\n' },
      { type: TokenType.Operator, value: ':' },
      { type: TokenType.Text, value: 'END' },
      { type: TokenType.Operator, value: ':' },
    ]);
  });
});
