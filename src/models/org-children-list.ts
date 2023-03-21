import { OrgNode } from './org-node.js';

const customInspectSymbol = Symbol.for('nodejs.util.inspect.custom');

export class OrgListChild {
  public next: OrgListChild;
  public prev: OrgListChild;

  constructor(public value: OrgNode) {}

  public setNext(next: OrgListChild) {
    this.next = next;
  }

  public setPrev(prev: OrgListChild) {
    this.prev = prev;
  }
}

/**
 * This class need to provide way to iterate over linked list of OrgNode children.
 */
export class OrgChildrenList implements Iterable<OrgNode> {
  private header: OrgListChild | null = null;
  private tail: OrgListChild | null = null;

  public length = 0;

  get last() {
    return this.tail?.value;
  }

  get first() {
    return this.header?.value;
  }

  get isEmpty(): boolean {
    return this.length === 0;
  }

  constructor(...nodes: OrgNode[]) {
    nodes.forEach((node) => this.push(node));
  }

  public push(node: OrgNode): number {
    const child = new OrgListChild(node);
    if (!this.header) {
      this.header = child;
    }
    this.tail?.setNext(child);
    child.setPrev(this.tail);
    this.tail = child;
    this.length++;
    return this.length;
  }

  public append(...nodes: OrgNode[]): void {
    nodes.forEach((node) => this.push(node));
  }

  public merge(other: OrgChildrenList): void {
    other.forEach((node) => this.push(node));
  }

  public filter(predicate: (node: OrgNode) => unknown): OrgChildrenList {
    const newList = new OrgChildrenList();
    this.forEach((node) => {
      if (predicate(node)) {
        newList.push(node);
      }
    });
    return newList;
  }

  public replaceNodes(
    nodes: OrgChildrenList | OrgNode[],
    ...newNodes: OrgNode[]
  ) {
    const firstNode = this.#find(
      nodes instanceof OrgChildrenList ? nodes.first : nodes[0]
    );

    let prevNode = firstNode.prev ?? this.header;

    this.removeNodes(nodes);

    newNodes.forEach((n) => {
      const childList = new OrgListChild(n);
      this.insertAfter(prevNode, childList);
      prevNode = childList;
    });
  }

  #find(node: OrgNode): OrgListChild | null {
    let current = this.header;
    while (current) {
      if (current.value === node) {
        return current;
      }
      current = current.next;
    }
    return null;
  }

  public insertAfter(prevNode: OrgListChild, insertedNode: OrgListChild): void {
    const next = prevNode.next;
    prevNode.setNext(insertedNode);
    insertedNode.setPrev(prevNode);
    insertedNode.setNext(next);
    next?.setPrev(insertedNode);
    this.length++;
  }

  public get(index: number) {
    let current = this.header;
    let i = 0;
    while (i !== index) {
      current = current?.next;
      i++;
    }
    return current?.value;
  }

  *[Symbol.iterator](): IterableIterator<OrgNode> {
    let current = this.header;
    while (current) {
      const res = current;
      current = current.next;
      yield res.value;
    }
  }

  [customInspectSymbol](depth, inspectOptions, inspect) {
    if (!this.length) {
      return '[]';
    }
    return `[\n  ${Array.from(this)
      .map((node) => node.toString())
      .join('  ')}]`;
  }

  public toString() {
    const res =
      '\n' +
      Array.from(this).reduce((acc, node) => {
        return `${acc}\n${node.toString()}`;
      }, '');
    return res + '\n-----------------';
  }

  /**
   * Replaces the old node with a new node in the organizational tree.
   *
   * @param {OrgNode} oldNode - The node that needs to be replaced.
   * @param {OrgNode} newNode - The node that will replace the old node.
   */
  public replaceNode(oldNode: OrgNode, newNode: OrgNode): void {
    let current = this.header;
    while (current) {
      const orgNode = current.value;
      if (orgNode === oldNode) {
        current.value = newNode;
        newNode.setNext(orgNode.next);
        newNode.setPrev(orgNode.prev);
        orgNode.prev?.setNext(newNode);
        orgNode.next?.setPrev(newNode);
        return;
      }
      current = current.next;
    }
  }

  // TODO: i was so lazy to write this method
  // all implementation was made by ChatGPT, check it
  public map<T>(callback: (node: OrgNode) => T): T[] {
    let current = this.header;
    const result: T[] = [];
    while (current) {
      result.push(callback(current.value));
      current = current.next;
    }
    return result;
  }

  public join(separator = ','): string {
    let current = this.header;
    let result = '';
    while (current) {
      result += current.toString();
      current = current.next;
      if (current) {
        result += separator;
      }
    }
    return result;
  }

  public reduce<T>(
    callback: (accumulator: T, node: OrgNode) => T,
    initialValue: T
  ): T {
    let current = this.header;
    let accumulator = initialValue;
    while (current) {
      accumulator = callback(accumulator, current.value);
      current = current.next;
    }
    return accumulator;
  }

  public forEach(
    callback: (node: OrgNode, index: number, last: boolean) => void
  ): void {
    let current = this.header;
    let i = 0;
    while (current) {
      callback(current.value, i, current === this.tail);
      current = current.next;
      i++;
    }
  }

  public find(
    predicate: (value: OrgNode, index: number, obj: OrgChildrenList) => boolean
  ): OrgNode | undefined {
    let current = this.header;
    let index = 0;
    while (current) {
      if (predicate(current.value, index, this)) {
        return current.value;
      }
      current = current.next;
      index++;
    }
    return undefined;
  }

  public findLast(predicate: (node: OrgNode) => boolean): OrgNode | undefined {
    let current = this.tail;
    while (current) {
      if (predicate(current.value)) {
        return current.value;
      }
      current = current.prev;
    }
    return undefined;
  }

  public pop(): OrgNode | undefined {
    const last = this.tail;
    if (last) {
      this.removeNode(last);
      return last.value;
    }
    return;
  }

  /**
   * @function getNodesBetweenPairs
   * @desc Return nodes between two nodes
   * @param startNode - The starting node to begin the search from
   * @param endNode - The ending node to finish the search at
   * @param [includeParis=false] - Whether to include the startNode and endNode in the returned list
   * @returns {OrgChildrenList} - A new OrgChildrenList instance containing the nodes between the startNode and endNode
   */
  public getNodesBetweenPairs(
    startNode: OrgNode,
    endNode?: OrgNode,
    includeParis = false
  ): OrgChildrenList {
    const foundNodes = new OrgChildrenList();
    let currentOrgListChild = this.header;
    let startNodeFound = false;

    while (currentOrgListChild) {
      const currentNode = currentOrgListChild.value;
      currentOrgListChild = currentOrgListChild.next;

      if (endNode && currentNode === endNode) {
        if (includeParis) {
          foundNodes.push(currentNode);
        }
        return foundNodes;
      }

      if (currentNode === startNode) {
        startNodeFound = true;
        if (includeParis) {
          foundNodes.push(currentNode);
        }
        continue;
      }

      if (startNodeFound) {
        foundNodes.push(currentNode);
      }
    }
    // foundNodes.clear();
    return foundNodes;
  }

  public slice(from: number, to?: number): OrgChildrenList {
    if (to < 0) {
      to = this.length + to;
    }
    const newList = new OrgChildrenList();
    let current = this.header;
    let i = 0;
    to = to ?? this.length;
    while (current) {
      if (i >= from && i < to) {
        newList.push(current.value);
      }
      current = current.next;
      i++;
    }
    return newList;
  }

  /**
   * Removes one or multiple nodes from the OrgChildrenList.
   * Instead of completely removing the nodes, their connections with neighboring nodes are preserved.
   *
   * @param nodes - The nodes to be removed from the OrgChildrenList
   */
  public removeNodes(nodes: OrgNode[] | OrgChildrenList): void {
    let orgListChild = this.header;

    while (orgListChild) {
      const orgNode = orgListChild.value;
      if (nodes.find((n) => n === orgNode)) {
        this.removeNode(orgListChild);
      }
      orgListChild = orgListChild.next;
    }
  }

  private removeNode(node: OrgListChild): void {
    node.prev?.setNext(node.next);
    node.next?.setPrev(node.prev);
    if (node === this.header) {
      this.header = node.next;
    }
    if (node === this.tail) {
      this.tail = node.prev;
    }
    this.length--;
  }

  public clear(): void {
    this.header = null;
    this.tail = null;
    this.length = 0;
  }

  public asArray(): OrgNode[] {
    return Array.from(this);
  }
}
