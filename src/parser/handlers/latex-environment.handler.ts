import {
  OrgHandler,
  OrgNode,
  ParserConfiguration,
} from '../../models/index.js';
import { AstBuilder } from '../ast-builder.js';
import { TokenIterator } from '../../tokenizer/index.js';

export class LatexEnvironmentHandler implements OrgHandler {
  #latexEnvironmentBlocks: {
    begin: string;
    end: string;
  };

  #beginLatexEnvironmentKeyword: OrgNode;
  #endLatexEnvironmentKeyword: OrgNode;

  constructor(
    private readonly configuration: ParserConfiguration,
    private readonly astBuilder: AstBuilder,
    private readonly tokenIterator: TokenIterator
  ) {
    this.initConfiguration();
  }

  get isBeginLatexEnvironmentKeyword(): boolean {
    return (
      this.tokenIterator.currentValue === this.#latexEnvironmentBlocks.begin
    );
  }

  get isEndLatexEnvironmentKeyword(): boolean {
    return this.tokenIterator.currentValue === this.#latexEnvironmentBlocks.end;
  }

  private initConfiguration(): void {
    this.#latexEnvironmentBlocks = this.configuration.latexEnvironmentBlocks;
  }

  public handle(): OrgNode {
    const orgNode = this.astBuilder.createTextNode(
      this.tokenIterator.currentValue
    );

    if (this.isBeginLatexEnvironmentKeyword) {
      this.#beginLatexEnvironmentKeyword = orgNode;
    }

    if (this.isEndLatexEnvironmentKeyword) {
      this.#endLatexEnvironmentKeyword = orgNode;
    }
    return this.#endLatexEnvironmentKeyword;
  }

  public isLatexEnvironmentKeyword(keyword: string): boolean {
    return Object.values(this.#latexEnvironmentBlocks).includes(keyword);
  }
}
