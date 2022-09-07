import { NodeType } from './types';
import { tokenize } from './tokenizer';

describe('Tokenizer', () => {
  it('Should create tokens for simple headline', () => {
    const headline = '* Hello world';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Hello world' },
    ]);
  });

  it('Should create tokens for simple headline with space at the end', () => {
    const headline = '* Hello world ';
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Hello world ' },
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
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Hello world\n' },
      { type: NodeType.Headline, value: '** ' },
      { type: NodeType.Text, value: 'Nested headline\n' },
      { type: NodeType.Headline, value: '*** ' },
      { type: NodeType.Text, value: 'Another one headline!\n' },
      { type: NodeType.Headline, value: '**** ' },
      { type: NodeType.Text, value: 'And 4 headline level\n' },
      { type: NodeType.Headline, value: '***** ' },
      { type: NodeType.Text, value: 'And 5 headline level\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'And again 1 level headline' },
    ]);
  });

  it('Should create tokens for headlines at same level', () => {
    const headline = `* Hello world
* Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Hello world\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Headline 2\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should create tokens for todo keywords inside headlines', () => {
    const headline = `* TODO Hello world
*** DONE Headline 2
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.TodoKeyword, value: 'TODO' },
      { type: NodeType.Text, value: ' Hello world\n' },
      { type: NodeType.Headline, value: '*** ' },
      { type: NodeType.TodoKeyword, value: 'DONE' },
      { type: NodeType.Text, value: ' Headline 2\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Headline 3' },
    ]);
  });

  it('Should note create tokens for todo keywords in middle of headline', () => {
    const headline = `* This is not a TODO
* And this is not a DONE
* Headline 3`;
    const result = tokenize(headline);
    expect(result).toEqual([
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'This is not a TODO\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'And this is not a DONE\n' },
      { type: NodeType.Headline, value: '* ' },
      { type: NodeType.Text, value: 'Headline 3' },
    ]);
  });
});
