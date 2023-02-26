import { ParserConfiguration } from '../models/index.js';

/** Default parser configuration. */
export const parserConfiguration: ParserConfiguration = {
  todoKeywords: ['TODO', 'DONE', 'HOLD', 'CANCELED'],
  latexEnvironmentBlocks: { begin: '\\begin', end: '\\end' },
};
