import { NodeType, OrgData, OrgRoot, OrgText } from 'types';

export class BracketHandler {
  private bracketsStackPositions: Array<{ childIndex: number; node: OrgData }> = [];

  constructor() {}

  public handleBracket(startPos: number, bracket: string): OrgData {
    const orgData: OrgData = {
      type: NodeType.Unresolved,
      value: bracket,
      start: startPos,
      end: startPos + bracket.length,
    };

    return orgData;
  }

  private clearBracketsPairs(): void {
    let childIndexOffset = 0;

    this.bracketsStackPositions.forEach((bracket) => {
      const neighbors = (bracket.node.parent as OrgRoot).children;
      let childIndex = bracket.childIndex + childIndexOffset;
      const leftChild = neighbors[childIndex - 1];
      const rightChild = neighbors[childIndex + 1];

      // Offset after position changed
      bracket.node.type = NodeType.Text;

      if (leftChild?.type === NodeType.Text) {
        neighbors.splice(childIndex - 1, 1);
        bracket.node.start = leftChild.start;
        (bracket.node as OrgText).value = (leftChild as OrgText).value + (bracket.node as OrgText).value;
        childIndexOffset--;
        childIndex--;
      }

      if (rightChild?.type === NodeType.Text) {
        neighbors.splice(childIndex + 1, 1);
        bracket.node.end = rightChild.end;
        (bracket.node as OrgText).value += (rightChild as OrgText).value;
        childIndexOffset--;
      }
    });

    this.bracketsStackPositions = [];
  }
}
