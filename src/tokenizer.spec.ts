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
***** And 5 headline level`;
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
      { type: NodeType.Text, value: 'And 5 headline level' },
    ]);
  });
});
