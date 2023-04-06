import { MetaInfo, NodeType, OrgNode } from '../models/index.js';
import { walkTree } from './tree-walker.js';

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
  const key = orgNode.children.first.value.slice(2, -1).toLowerCase();
  const value = orgNode.children.last.rawValue;
  const normalizedValue = normalizeKeywordValue(key, value);

  const existingValue = metaInfo[normalizeKey(key)];
  if (existingValue && Array.isArray(existingValue) && Array.isArray(normalizedValue)) {
    metaInfo[normalizeKey(key)] = [...existingValue, ...normalizedValue] as any;
    return;
  }
  metaInfo[normalizeKey(key)] = normalizedValue;
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
    filetags: () => value.split(':').filter((v) => !!v),
    published: () => !!eval(value.trim()),
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
  const key = orgNode.children.first.value.slice(1, -1).trim().toLowerCase();
  const val = normalizeKeywordValue(key, orgNode.children.last.rawValue);
  metaInfo[key] = val;
}

export function collectImages(orgNode: OrgNode, metaInfo: MetaInfo): void {
  if (orgNode.isNot(NodeType.Link) || orgNode.meta.linkType !== 'image') {
    return;
  }
  metaInfo.images ??= [];
  metaInfo.images.push(orgNode.children.get(1).children.get(1).value);
}

export function collectHeadings(orgNode: OrgNode, metaInfo: MetaInfo): void {
  if (orgNode.isNot(NodeType.Headline)) {
    return;
  }

  metaInfo.headings ??= [];
  metaInfo.headings.push({
    level: orgNode.level,
    title: orgNode.title.cleanValue,
  });
}

/*
 * Collect all available meta info
 */
export function withMetaInfo(orgNode: OrgNode): OrgNode {
  return withOptionalMetaInfo(
    orgNode,
    null,
    collectFromKeywords,
    collectFromProperties,
    collectImages,
    collectHeadings
  );
}

/*
 * Get info from top level nodes
 */
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
