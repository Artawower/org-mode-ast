export enum NodeType {
  Root = 'root',
  Operator = 'operator',
  Headline = 'headline',
  Text = 'text',
}

export interface Token {
  type: NodeType;
  value: string;
}

interface WithRange {
  start: number;
  end: number;
}

interface WithChildren {
  children: OrgData[];
}

export interface Headline extends WithRange, WithChildren {
  type: NodeType.Headline;
  level: number;
}

export interface Operator extends WithRange {
  type: NodeType.Operator;
  value: string;
}

export interface OrgRoot extends WithRange, WithChildren {
  type: NodeType.Root;
}

export interface Text extends WithRange {
  type: NodeType.Text;
  value: string;
}

export type OrgData = Headline | Operator | OrgRoot | Text;

export interface Node {
  type: NodeType;
  children: OrgData[];
}
