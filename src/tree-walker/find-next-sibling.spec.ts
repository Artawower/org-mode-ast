import { NodeType } from 'models';
import { parse } from 'parser/parser';
import { findNextSibling } from './find-next-sibling';

describe('findNextSibling', () => {
  it('should find next sibling matching predicate', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>`;
    const root = parse(orgDoc);
    const planning = root.children.first.section.children.first;
    const keyword = planning.children.first;

    const dateNode = findNextSibling(keyword, (n) =>
      n.is(NodeType.Date, NodeType.DateRange)
    );

    expect(dateNode).not.toBeNull();
    expect(dateNode.is(NodeType.Date)).toBe(true);
  });

  it('should return null when stop predicate is met before match', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon> SCHEDULED: <2026-05-01 Fri>`;
    const root = parse(orgDoc);
    const planning = root.children.first.section.children.first;
    const firstKeyword = planning.children.first;
    const secondKeyword = planning.children.find(
      (n) => n.is(NodeType.PlanningKeyword) && n !== firstKeyword
    );

    const result = findNextSibling(
      secondKeyword,
      (n) => n.is(NodeType.Date),
      (n) => n.is(NodeType.NewLine)
    );

    expect(result).not.toBeNull();
    expect(result.is(NodeType.Date)).toBe(true);
  });

  it('should return null when no sibling matches predicate', () => {
    const orgDoc = `* Task\nsome text`;
    const root = parse(orgDoc);
    const textNode = root.children.first.section.children.first;

    const result = findNextSibling(textNode, (n) => n.is(NodeType.Date));

    expect(result).toBeNull();
  });

  it('should return null when stop predicate fires first', () => {
    const orgDoc = `* TODO Task
DEADLINE: <2026-05-10 Mon>
Body text`;
    const root = parse(orgDoc);
    const planning = root.children.first.section.children.first;
    const keyword = planning.children.first;

    const result = findNextSibling(
      keyword,
      (n) => n.is(NodeType.SrcBlock),
      (n) => n.is(NodeType.NewLine)
    );

    expect(result).toBeNull();
  });
});
