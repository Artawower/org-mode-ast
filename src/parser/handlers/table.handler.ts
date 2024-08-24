import {
  NodeType,
  OrgHandler,
  OrgNode,
  ParserConfiguration,
  TokenType,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class TableHandler implements OrgHandler {
  #lastPipeOperator: OrgNode;
  #lastTableRow: OrgNode;
  #lastTable: OrgNode;
  #tableLine: boolean;

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {}

  public handle(): OrgNode {
    this.astBuilder.checkContext();

    if (this.#lastPipeOperator) {
      return this.#createTableCell();
    }

    this.#lastPipeOperator = this.astBuilder.createOperatorNode(
      this.tokenIterator.currentValue
    );

    const lastNode = this.astBuilder.lastNode;

    const table = this.#createTableIfNotExist();

    if (lastNode.is(NodeType.Indent)) {
      lastNode.parent.removeNode(lastNode);
      table.addChild(lastNode);
    }

    const tableRow = this.#getOrCreateTableRow();

    tableRow.addChild(this.#lastPipeOperator);
    const tableCell = this.astBuilder.createTableCellNode();

    tableRow.addChild(tableCell);
    return tableCell;
  }

  public handleDelimiter(): OrgNode {
    const table = this.#createTableIfNotExist();
    const delimiterNode = this.astBuilder.createTableDelimiterNode(
      this.tokenIterator.currentValue
    );
    table.addChild(delimiterNode);
    this.#lastTableRow = delimiterNode;
    return delimiterNode;
  }

  public isTableLine(): boolean {
    if (this.#tableLine === false) {
      this.#mergeCurrentToken();
      return this.#tableLine;
    }

    const lastNode = this.astBuilder.lastNode;
    const newLineStartedWithIndent =
      lastNode.is(NodeType.Indent) &&
      (!lastNode?.prev || lastNode.prev?.is(NodeType.NewLine, NodeType.Root));

    this.#tableLine ||=
      lastNode.is(NodeType.NewLine, NodeType.Root) || newLineStartedWithIndent;

    if (!this.#tableLine) {
      this.#mergeCurrentToken();
    }

    return this.#tableLine;
  }

  #mergeCurrentToken(): void {
    const lastNode = this.astBuilder.lastNode;
    if (lastNode.is(NodeType.Text)) {
      lastNode.appendValue(this.tokenIterator.currentValue);
      return;
    }

    this.astBuilder.attachToTree(
      this.astBuilder.createTextNode(this.tokenIterator.currentValue)
    );
  }

  #createTableCell(): OrgNode {
    this.#lastPipeOperator = this.astBuilder.createOperatorNode(
      this.tokenIterator.currentValue
    );

    const tableRow = this.#getOrCreateTableRow();
    tableRow.addChild(this.#lastPipeOperator);

    if (
      this.tokenIterator.nextToken &&
      !this.tokenIterator.nextToken.isType(TokenType.NewLine)
    ) {
      const nextTableCell = this.astBuilder.createTableCellNode();
      tableRow.addChild(nextTableCell);
      return nextTableCell;
    }

    return this.#lastPipeOperator;
  }

  #createTableIfNotExist(): OrgNode {
    if (this.#lastTable) {
      return this.#lastTable;
    }
    if (this.#lastTableRow) {
      return this.#lastTableRow.parent;
    }

    const table = this.astBuilder.createTableNode();
    this.astBuilder.attachToTree(table);
    this.astBuilder.saveLastNode(table);

    return table;
  }

  #getOrCreateTableRow(): OrgNode {
    if (!this.#lastTableRow) {
      this.#lastTableRow = this.astBuilder.createTableRowNode();
      this.astBuilder.attachToTree(this.#lastTableRow);
      this.astBuilder.saveLastNode(this.#lastTableRow);
    }
    return this.#lastTableRow;
  }

  public handleNewLine(): void {
    if (this.#lastTableRow) {
      this.#lastTable = this.#lastTableRow.parent;
    } else {
      this.#lastTable = null;
    }
    this.#lastTableRow = null;
    this.#lastPipeOperator = null;
    this.#tableLine = null;
  }
}
