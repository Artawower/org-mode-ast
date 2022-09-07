import { NodeType, OrgData } from './types';

class Parser {
  constructor() {}

  private nodeTree: OrgData = {
    type: NodeType.Root,
    start: 0,
    end: 13,
    children: [
      { type: NodeType.Operator, value: '* ', start: 0, end: 2 },
      {
        type: NodeType.Headline,
        level: 1,
        start: 2,
        end: 13,
        children: [{ type: NodeType.Text, value: 'Hello world', start: 2, end: 13 }],
      },
    ],
  };

  public parse(text: string): OrgData {
    return this.nodeTree;
  }
}

export function parse(text: string): OrgData {
  const parser = new Parser();
  return parser.parse(text);
}
