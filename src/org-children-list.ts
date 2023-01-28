import { OrgNode } from 'org-node';

const customInspectSymbol = Symbol.for('nodejs.util.inspect.custom');

/**
 * This class need to provide way to iterate over linked list of OrgNode children.
 */
export class OrgChildrenList implements Iterable<OrgNode> {
  private head: OrgNode | null = null;
  private last: OrgNode | null = null;
  public length = 0;

  public push(node: OrgNode): number {
    if (!this.head) {
      this.head = node;
    }
    this.last?.setNext(node);
    node.setPrev(this.last);
    this.last = node;
    this.length++;
    return this.length;
  }

  public get(index: number) {
    let current = this.head;
    let i = 0;
    while (i !== index) {
      current = current?.next;
      i++;
    }
    return current;
  }

  *[Symbol.iterator]() {
    let current = this.head;
    while (current) {
      const res = current;
      current = current.next;
      yield res;
    }
  }

  [customInspectSymbol](depth, inspectOptions, inspect) {
    return `[\n  ${Array.from(this)
      .map((node) => node.toString())
      .join('  ')}]`;
  }
}
