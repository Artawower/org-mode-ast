import { TokenType } from './types';
import { tokenize } from './tokenizer';

describe('Tokenizer', () => {
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

  it('Should create tokens for todo keywords inside headlines', () => {
    const headline = `* TODO Hello world
*** DONE Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.TodoKeyword, value: 'TODO' },
      { type: TokenType.Text, value: ' Hello world\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.TodoKeyword, value: 'DONE' },
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
      { type: TokenType.TodoKeyword, value: 'TODO' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#A' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Most important headline\n' },
      { type: TokenType.Headline, value: '*** ' },
      { type: TokenType.TodoKeyword, value: 'DONE' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#B' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Third most important headline\n' },
      { type: TokenType.Headline, value: '* ' },
      { type: TokenType.TodoKeyword, value: 'HOLD' },
      { type: TokenType.Text, value: ' ' },
      { type: TokenType.Bracket, value: '[' },
      { type: TokenType.Comment, value: '#c' },
      { type: TokenType.Bracket, value: ']' },
      { type: TokenType.Text, value: ' Headline 3' },
    ]);
  });
});
