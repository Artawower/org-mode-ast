import { OrgRepeater, RepeaterType, TimeUnit } from '../../models/index.js';

// Org timestamp body: YYYY-MM-DD DAY [HH:MM] [repeater/warning ...]
// Both regexes share the same structure — keep day names in sync if Org spec changes.
const ACTIVE_TIMESTAMP_RE =
  /^<\d{4}-\d{2}-\d{2} (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)(?: \d{2}:\d{2})?(?: (?:\.\+|\+\+?|--?)\d+[hdwmy])*>$/;
const INACTIVE_TIMESTAMP_RE =
  /^\[\d{4}-\d{2}-\d{2} (?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)(?: \d{2}:\d{2})?(?: (?:\.\+|\+\+?|--?)\d+[hdwmy])*]$/;

// Explicit day names align with ACTIVE/INACTIVE_TIMESTAMP_RE above.
const DATE_INNER_RE =
  /^(\d{4}-\d{2}-\d{2})\s+(?:Mon|Tue|Wed|Thu|Fri|Sat|Sun)(?:\s+(\d{2}:\d{2}))?(.*)$/;

const REPEATER_RE = /(\+\+|\.\+|\+)(\d+)([hdwmy])/;
const WARNING_RE = /(--?)(\d+)([hdwmy])/;

export interface ParsedOrgTimestamp {
  active: boolean;
  date: string;
  hasTime: boolean;
  repeater?: OrgRepeater;
  warning?: OrgRepeater;
}

export function isOrgTimestamp(raw: string): boolean {
  const trimmed = raw?.trim() ?? '';
  return (
    ACTIVE_TIMESTAMP_RE.test(trimmed) || INACTIVE_TIMESTAMP_RE.test(trimmed)
  );
}

export function parseOrgTimestamp(raw: string): ParsedOrgTimestamp | null {
  const trimmed = raw?.trim() ?? '';
  if (!isOrgTimestamp(trimmed)) {
    return null;
  }
  const active = trimmed.startsWith('<');
  const inner = trimmed.slice(1, -1);
  const match = DATE_INNER_RE.exec(inner);
  if (!match) {
    return null;
  }
  const [, datePart, timePart, rest] = match;
  const hasTime = !!timePart;
  const date = timePart ? `${datePart}T${timePart}` : datePart;
  return {
    active,
    date,
    hasTime,
    ...buildOptional('repeater', parseRepeater(rest)),
    ...buildOptional('warning', parseWarning(rest)),
  };
}

function buildOptional<K extends string, V>(
  key: K,
  value: V | undefined
): Partial<Record<K, V>> {
  return value !== undefined ? ({ [key]: value } as Partial<Record<K, V>>) : {};
}

function buildRepeater(match: RegExpExecArray): OrgRepeater {
  return {
    type: match[1] as RepeaterType,
    value: parseInt(match[2], 10),
    unit: match[3] as TimeUnit,
  };
}

function parseRepeater(rest: string): OrgRepeater | undefined {
  const match = REPEATER_RE.exec(rest);
  return match ? buildRepeater(match) : undefined;
}

function parseWarning(rest: string): OrgRepeater | undefined {
  const match = WARNING_RE.exec(rest);
  return match ? buildRepeater(match) : undefined;
}
