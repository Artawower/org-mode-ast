import {
  ClockEntry,
  Heading,
  MetaInfo,
  NodeType,
  OrgChildrenList,
  OrgDate,
  OrgNode,
} from '../models/index.js';
import { parseOrgTimestamp } from '../parser/timestamp/index.js';
import { walkTree } from './tree-walker.js';
import { findNextSibling } from './find-next-sibling.js';

export function collectFromKeywords(
  orgNode: OrgNode,
  metaInfo: MetaInfo
): void {
  if (
    orgNode.isNot(NodeType.Keyword) ||
    orgNode.parent.is(NodeType.BlockHeader, NodeType.BlockFooter)
  ) {
    return;
  }
  const [key, val] = extractKeyValue(orgNode.children, 2, -1);

  const existingValue = metaInfo[normalizeKey(key)];
  if (existingValue && Array.isArray(existingValue) && Array.isArray(val)) {
    metaInfo[normalizeKey(key)] = [
      ...(existingValue as string[]),
      ...(val as string[]),
    ];
    return;
  }
  metaInfo[normalizeKey(key)] = val;
}

function normalizeKey(key: string): string {
  const keys = { filetags: 'fileTags' };
  return keys[key] ?? key;
}

function normalizeKeywordValue(
  key: string,
  value: string
): string | string[] | undefined | boolean {
  const handlers = {
    filetags: () => value.split(':').filter((v) => !!v.trim()),
    published: () => value.trim().toLowerCase() === 'true',
  };
  const handler = handlers[key] ?? (() => value.trim());
  return handler();
}

export function collectFromProperties(
  orgNode: OrgNode,
  metaInfo: MetaInfo
): void {
  if (
    orgNode.isNot(NodeType.Property) ||
    orgNode.parent?.isNot(NodeType.PropertyDrawer) ||
    orgNode.children.length < 2
  ) {
    return;
  }
  const [key, val] = extractKeyValue(orgNode.children, 1, -1);
  metaInfo[key] = val;
}

function extractKeyValue(
  nodes: OrgChildrenList,
  ...range: [number, number]
): [string, string | string[] | undefined | boolean] {
  const key = nodes.first.value
    .trim()
    .slice(...range)
    .toLowerCase();
  const isNoKeywordValue = nodes.length <= 1;
  const rawKeywordValue = isNoKeywordValue ? '' : nodes.last.rawValue;

  const val = normalizeKeywordValue(key, rawKeywordValue);
  return [key, val];
}

export function collectImages(orgNode: OrgNode, metaInfo: MetaInfo): void {
  if (orgNode.isNot(NodeType.Link) || orgNode.meta.linkType !== 'image') {
    return;
  }
  metaInfo.images ??= [];
  metaInfo.images.push(orgNode.children.get(1).children.get(1).value);
}

function getTitleChildren(node: OrgNode): OrgNode[] {
  return node.title?.childrenList ?? [];
}

function findTitleChild(node: OrgNode, type: NodeType): OrgNode | undefined {
  return getTitleChildren(node).find((child) => child.is(type));
}

function collectHeadingPriority(orgNode: OrgNode): string | undefined {
  const priorityNode = findTitleChild(orgNode, NodeType.Priority);
  if (!priorityNode) return undefined;
  const textChild = priorityNode.childrenList.find((c) => c.is(NodeType.Text));
  return textChild?.value?.replace('#', '') || undefined;
}

function collectHeadingTags(orgNode: OrgNode): string[] | undefined {
  const tagListNode = findTitleChild(orgNode, NodeType.TagList);
  if (!tagListNode) return undefined;
  const tags = tagListNode.childrenList
    .filter((c) => c.is(NodeType.Text))
    .map((c) => c.value ?? '')
    .filter(Boolean);
  return tags.length > 0 ? tags : undefined;
}

function collectHeadingTodoKeyword(orgNode: OrgNode): string | undefined {
  return (
    findTitleChild(orgNode, NodeType.TodoKeyword)?.value?.trim() || undefined
  );
}

export function collectHeadings(orgNode: OrgNode, metaInfo: MetaInfo): void {
  if (orgNode.isNot(NodeType.Headline)) {
    return;
  }

  const heading: Heading = {
    level: orgNode.level,
    title: orgNode.title.cleanValue,
    start: orgNode.start,
    end: orgNode.end,
    priority: collectHeadingPriority(orgNode),
    tags: collectHeadingTags(orgNode),
    todoKeyword: collectHeadingTodoKeyword(orgNode),
  };

  collectPlanningIntoHeading(orgNode, heading);
  collectClocksIntoHeading(orgNode, heading);

  metaInfo.headings ??= [];
  metaInfo.headings.push(heading);
}

export function collectConnectedNotes(
  orgNode: OrgNode,
  metaInfo: MetaInfo
): void {
  if (orgNode.isNot(NodeType.Link)) {
    return;
  }

  const linkAddress = orgNode.children.get(1).children.get(1).value;
  if (!linkAddress.startsWith('id:')) {
    return;
  }

  const linkName =
    orgNode.children.length === 4
      ? orgNode.children.get(2).children.get(1).value
      : '';

  metaInfo.connectedNotes ??= {};
  metaInfo.connectedNotes[linkAddress.slice(3)] = linkName;
}

const PLANNING_KEYWORD_MAP: Record<
  string,
  keyof Pick<Heading, 'deadline' | 'scheduled' | 'closed'>
> = {
  'DEADLINE:': 'deadline',
  'SCHEDULED:': 'scheduled',
  'CLOSED:': 'closed',
};

function collectPlanningIntoHeading(headline: OrgNode, heading: Heading): void {
  const planningNode = findDirectChild(headline.section, NodeType.Planning);
  if (!planningNode) {
    return;
  }

  planningNode.children.forEach((child) => {
    if (child.isNot(NodeType.PlanningKeyword)) {
      return;
    }
    const field = PLANNING_KEYWORD_MAP[child.value];
    if (!field) {
      return;
    }
    const dateNode = findNextSiblingDate(child);
    if (!dateNode) {
      return;
    }
    heading[field] = parseOrgDateNode(dateNode);
  });
}

function findDirectChild(
  node: OrgNode | undefined,
  type: NodeType
): OrgNode | null {
  if (!node) {
    return null;
  }
  return node.children?.find((child) => child.is(type)) ?? null;
}

function findNextSiblingDate(node: OrgNode): OrgNode | null {
  return findNextSibling(
    node,
    (n) => n.is(NodeType.Date, NodeType.DateRange),
    (n) => n.is(NodeType.PlanningKeyword, NodeType.NewLine)
  );
}

function collectClocksIntoHeading(headline: OrgNode, heading: Heading): void {
  collectClocksFromNode(headline.section, heading);
}

function collectClocksFromNode(
  node: OrgNode | undefined,
  heading: Heading
): void {
  if (!node?.children) {
    return;
  }
  node.children.forEach((child) => {
    if (child.is(NodeType.Headline)) {
      return;
    }
    if (child.is(NodeType.Clock)) {
      heading.clocks ??= [];
      heading.clocks.push(parseClockNode(child));
      return;
    }
    collectClocksFromNode(child, heading);
  });
}

function parseClockNode(clockNode: OrgNode): ClockEntry {
  const dateNode = findDirectDateChild(clockNode);
  if (!dateNode) {
    return { start: clockNode.start, end: clockNode.end };
  }

  if (dateNode.is(NodeType.DateRange)) {
    const startDate = dateNode.children.first;
    const endDate = dateNode.children.last;
    return {
      date: extractIsoFromDateNode(startDate),
      to: extractIsoFromDateNode(endDate),
      start: clockNode.start,
      end: clockNode.end,
    };
  }

  return {
    date: extractIsoFromDateNode(dateNode),
    start: clockNode.start,
    end: clockNode.end,
  };
}

function findDirectDateChild(node: OrgNode): OrgNode | null {
  return (
    node.children.find((child) =>
      child.is(NodeType.Date, NodeType.DateRange)
    ) ?? null
  );
}

function extractIsoFromDateNode(dateNode: OrgNode): string | undefined {
  return parseOrgTimestamp(dateNode.rawValue)?.date;
}

function parseOrgDateNode(dateNode: OrgNode): OrgDate {
  if (dateNode.is(NodeType.DateRange)) {
    const startDateNode = dateNode.children.first;
    const endDateNode = dateNode.children.last;
    const parsed = parseSingleDateNode(startDateNode);
    return {
      ...parsed,
      to: extractIsoFromDateNode(endDateNode),
      start: dateNode.start,
      end: dateNode.end,
    };
  }

  return {
    ...parseSingleDateNode(dateNode),
    start: dateNode.start,
    end: dateNode.end,
  };
}

function parseSingleDateNode(
  dateNode: OrgNode
): Omit<OrgDate, 'start' | 'end'> {
  const parsed = parseOrgTimestamp(dateNode.rawValue);
  return parsed ?? { date: '', active: false, hasTime: false };
}

export function withMetaInfo(orgNode: OrgNode): OrgNode {
  return withOptionalMetaInfo(
    orgNode,
    null,
    collectFromKeywords,
    collectFromProperties,
    collectImages,
    collectHeadings,
    collectConnectedNotes
  );
}

export function withSuperficialInfo(orgNode: OrgNode): OrgNode {
  const stopCallback = (node: OrgNode) => {
    if (node.parent && node.parent.isNot(NodeType.Root)) {
      return true;
    }
  };
  return withOptionalMetaInfo(orgNode, stopCallback, collectFromKeywords);
}

export type MetaInfoHandler = (orgNode: OrgNode, metaInfo: MetaInfo) => void;

export function withOptionalMetaInfo(
  orgNode: OrgNode,
  stopCallback?: (orgNode: OrgNode) => boolean,
  ...handlers: MetaInfoHandler[]
): OrgNode {
  const metaInfo: MetaInfo = {};

  walkTree(orgNode, (node) => {
    handlers.forEach((handler) => handler(node, metaInfo));
    if (stopCallback) {
      return stopCallback(node);
    }
  });

  orgNode.meta = metaInfo;
  return orgNode;
}
