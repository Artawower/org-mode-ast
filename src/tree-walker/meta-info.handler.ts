import { MetaInfo, NodeType, OrgNode } from '../models/index.js';
import { walkTree } from './tree-walker.js';

export function collectFromKeywords(orgNode: OrgNode): Partial<MetaInfo> {
  if (orgNode.isNot(NodeType.Keyword)) {
    return;
  }
  const key = orgNode.children.first.value.slice(2, -1).toLowerCase();
  const value = orgNode.children.last.rawValue;
  const normalizedValue =
    key === 'filetags' ? value.split(':').filter((v) => !!v) : value.trim();

  return {
    [key]: normalizedValue,
  };
}

export function collectFromProperties(orgNode: OrgNode): Partial<MetaInfo> {
  if (
    orgNode.isNot(NodeType.Property) ||
    orgNode.parent?.isNot(NodeType.PropertyDrawer) ||
    orgNode.children.length < 2
  ) {
    return;
  }
  const key = orgNode.children.first.value.slice(1, -1).trim().toLowerCase();
  const val = orgNode.children.last.rawValue.trim();
  return { [key]: val };
}

export function withMetaInfo(orgNode: OrgNode): OrgNode {
  let metaInfo: MetaInfo = {};

  walkTree(orgNode, (node) => {
    const keywordsInfo = collectFromKeywords(node);
    const propertiesInfo = collectFromProperties(node);
    metaInfo = { ...metaInfo, ...keywordsInfo, ...propertiesInfo };
    if (node.parent && node.parent.isNot(NodeType.Root)) {
      return true;
    }
  });

  orgNode.meta = metaInfo;
  return orgNode;
}
