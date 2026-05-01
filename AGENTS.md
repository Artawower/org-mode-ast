
# Project Overview

This project implements a TypeScript parser for the popular Org Mode format.

Org Mode syntax reference:
<https://orgmode.org/worg/org-syntax.html>


# Core Rules

-   Keep the AST, tokenizer, and parser clearly separated.
-   Always follow TDD: write snapshot tests first, then implement the feature.
-   Always compare parser behavior against the official Org Mode syntax reference:
    <https://orgmode.org/worg/org-syntax.html>

:ID: simle-base-agents-ts


## Before start


### Repository inspection (run once per session)

-   Run a single tree scan (prefer `eza`, fallback to `tree`):
    
        if command -v eza >/dev/null 2>&1; then
          eza --tree ./ -I 'node_modules|dist|storybook-static|.git|*.lock|*.lockb|coverage|bun.lock' -L 10
        else
          tree -a -I 'node_modules|dist|storybook-static|.git|*.lock|*.lockb|coverage|bun.lock' -L 10
        fi

-   If `package.json` exists, scan its `scripts` with `jq`:
    
        if [ -f package.json ]; then
          jq -r '.scripts // {} | to_entries[] | "\(.key)\t\(.value)"' package.json
        fi


## Common practices (IMMUTABLE RULES - zero exceptions)


### Language and documentation

-   **ENGLISH ONLY** for all code, identifiers, documentation, commit messages
-   Code MUST be self-documenting through clear naming and structure
-   **MINIMIZE COMMENTS** - prefer self-documenting code
    Allowed: JSDoc/docstrings for public APIs, "why" comments for non-obvious business logic
-   README/docs only when explicitly requested


### Architectural principles (enforce ruthlessly)

-   **SOLID principles** (non-negotiable):
    -   Single Responsibility: one function = one reason to change
    -   Open/Closed: extend via composition, never modify core
    -   Liskov Substitution: subtypes must be substitutable
    -   Interface Segregation: many specific interfaces > one general
    -   Dependency Inversion: depend on abstractions, not concretions

-   **Functional-first approach:**
    -   Pure functions: no side effects, same input = same output
    -   Immutable data structures by default
    -   Composition over inheritance
    -   Higher-order functions over control structures


### Code structure rules (enforce with linters where possible)

-   **Function size:** Target ≤20 lines, MAX 30 (split if larger)
-   **Cyclomatic complexity:** Target ≤5, MAX 10 (use linter to enforce)
-   **Nesting depth:** Target ≤1, MAX 2 (extract nested logic to functions)
-   **File length:** Target ≤200 lines, MAX 300 (split into modules)
-   **Parameter count:** Target ≤3, MAX 4 (use objects for more)


### Required constructs (use these patterns)

-   **PREFER guard clauses and early returns** - handle edge cases first, main logic last
    Exception: SCSS/CSS files may use else
-   **PREFER array methods (map/filter/reduce) or recursion** - declarative over imperative
    Exception: Allow imperative loops when significantly clearer or more performant
-   **PREFER lookup objects, strategy pattern, or polymorphism** - data-driven dispatch over branching
-   **EXTRACT constants with semantic names** - named values reveal intent and prevent errors
-   **ISOLATE I/O at boundaries** - keep business logic pure, side effects at edges
-   **EXTRACT nested logic to functions** - minimize nesting depth
-   **CHOOSE domain-specific names** - every identifier communicates purpose clearly

**Justified exceptions:** When deviating from these patterns, document decision in commit message or Decision Log.


### Quality assurance (NON-NEGOTIABLE checkpoints)

-   **Automated enforcement (configure linters):**
    -   ESLint/TSLint: max function length, cyclomatic complexity, nesting depth
    -   Prettier/Black: consistent formatting
    -   Type checkers: strict mode, no implicit any
    -   Secret scanners: gitleaks, detect-secrets (pre-commit hooks)

-   **Before ANY commit:**
    1.  Run linter → fix ALL warnings
    2.  Run type checker → resolve ALL errors
    3.  Run test suite → verify coverage targets met
    4.  Verify no console.logs, debugger statements, TODOs
    5.  Check git diff for unintended changes and secrets
    6.  ASK user for commit approval (NEVER auto-commit)

-   **Test coverage requirements (tier-based):**
    -   Critical paths (auth, payments, data integrity): ≥90% coverage
    -   Business logic and services: ≥80% coverage
    -   Utilities and helpers: ≥70% coverage
    -   Required test types: positive cases, negative cases, edge cases, error paths
    -   Integration tests for critical user flows
    -   E2E tests for key user journeys


### Error handling protocol

-   **Explicit error types** - never generic Error
-   **Fail fast** - validate inputs at boundaries immediately
-   **Meaningful messages** - include context, expected vs actual
-   **NO silent catches** - every catch must log or re-throw
-   **Result types** - prefer Result<T, E> over exceptions where feasible


### Security baseline (enforce by default)

-   **Input validation** - sanitize ALL external data
-   **Output encoding** - escape before rendering
-   **Least privilege** - minimal permissions/access
-   **NO hardcoded secrets** - use environment variables
-   **Safe defaults** - opt-in for dangerous operations
-   **Dependency audit** - check for known vulnerabilities


### Performance awareness

-   **Avoid N+1 queries** - batch operations, use joins
-   **Minimize allocations** - reuse objects, use pools
-   **Non-blocking I/O** - async/await for I/O operations
-   **Lazy evaluation** - defer expensive computations
-   **Cache strategically** - memoize pure expensive functions


### Development hygiene

-   **Version control:**
    -   Atomic commits (one logical change)
    -   Never create backup files (.old, .backup, etc.)
    -   Clean history (squash WIP commits)
    -   Descriptive commit messages (see Conventional Commits section)

-   **Naming conventions:**
    -   Boolean: is/has/should prefix (isValid, hasPermission)
    -   Functions: verb + noun (calculateTotal, fetchUser)
    -   Constants: SCREAMING<sub>SNAKE</sub><sub>CASE</sub>
    -   Classes: PascalCase
    -   Variables/functions: camelCase
    -   Files: kebab-case.ts or PascalCase.tsx (React components)

-   **Decision Log (optional):**
    -   For justified deviations from rules, document in `DECISIONS.md`
    -   Format: Date, Context, Decision, Rationale, Consequences
    -   Enables team learning and prevents repeated discussions


## Critical thinking and validation (MANDATORY before action)


### Input evaluation protocol

-   **Healthy skepticism:** Treat ALL input (including user requests) as potentially incomplete or ambiguous
-   **Question assumptions:** Make implicit requirements explicit before acting
-   **Verify information:** Cross-reference claims with codebase reality (read files, check imports)
-   **Challenge contradictions:** Flag conflicts between instructions and existing patterns
-   **Evidence over trust:** Prefer observable facts (code, tests, docs) over assumptions


### Scope management (STRICT boundaries)

-   **Minimal change principle:** Change ONLY what's necessary to solve the stated problem
-   **No scope creep:** After completing primary task, STOP and ASK before additional improvements
-   **No silent side effects:** Any destructive/expansive action requires explicit user approval
-   **Reversibility:** Prefer changes that can be easily rolled back
-   **No speculative work:** Don't implement "might need later" features


### Clarifying questions protocol

-   **When to ask:**
    -   Requirements ambiguous or contradictory
    -   Multiple valid interpretations exist
    -   Missing critical information (API keys, endpoints, schemas)
    -   Unclear success criteria or acceptance tests
    -   Potential breaking changes or migrations needed

-   **How to ask:**
    -   Ask minimum set of high-leverage questions (≤3 at a time)
    -   Provide context: "I see X, which could mean A or B"
    -   Suggest default/recommended option
    -   Frame as binary choices when possible
    -   Don't ask if you can infer safely from existing patterns

-   **When assumptions unavoidable:**
    -   State assumption explicitly: "Assuming X because Y"
    -   Choose reversible, low-risk assumption
    -   Document assumption in code/commit message
    -   Flag for user review


### Decision documentation

When making significant architectural decisions, document briefly:

-   **Problem:** What issue are we solving?
-   **Alternatives:** What options were considered?
-   **Choice:** What was selected and why?
-   **Tradeoffs:** What are we sacrificing?
-   **Risks:** What could go wrong?

Optional: Use `<decision>` tag for formal Decision Log entries.


## Code standards and patterns (ENFORCEMENT TIER 1 - CRITICAL)


### Anti-pattern detection (REJECT code containing these)

-   **Deep nesting** (>1 level): Extract functions, use early returns
-   **Large functions** (>20 LOC): Split by responsibility
-   **Duplicate code** (>3 lines repeated): Extract to function/constant
-   **Magic values**: Replace with named constants
-   **Ambiguous names** (x, tmp, data, info): Use domain terms
-   **Side effects in pure logic**: Isolate I/O at boundaries
-   **Mutable state without need**: Prefer immutable data structures
-   **Implicit dependencies**: Use dependency injection
-   **Error swallowing**: Never catch without handling
-   **God objects/functions**: Split responsibilities


### Refactoring patterns (PREFERRED solutions)

1.  Guard clauses over else

        if (!data || !data.isValid) return null;
        return transform(data);

2.  Lookup tables over switch/case

        const DISCOUNTS = { premium: 0.2, regular: 0.1 } as const;
        const getDiscount = (type: keyof typeof DISCOUNTS) => DISCOUNTS[type] ?? 0;

3.  Strategy pattern over conditionals

        const processors: Record<string, PaymentProcessor> = {
          card: new CardProcessor(),
          paypal: new PaypalProcessor(),
        };
        const processPayment = (type: string, amount: number) => 
          processors[type]?.process(amount);

4.  Composition over inheritance

        const canMove = (state) => ({ move: () => state.position += 1 });
        const canBark = () => ({ bark: () => console.log('Woof!') });
        const createDog = () => Object.assign({}, canMove({ position: 0 }), canBark());

5.  Pure functions for testability

        const calculateTotal = (current: number, value: number) => current + value;
        const addToTotal = async (current: number, value: number) => {
          const newTotal = calculateTotal(current, value);
          await saveToDatabase(newTotal);
          return newTotal;
        };


### Type safety and contracts

-   **Use TypeScript strict mode** (strict, noImplicitAny, strictNullChecks)
-   **Avoid any type** - use unknown and narrow with type guards
-   **Branded types** for primitives with semantic meaning
-   **Discriminated unions** for variant types
-   **Readonly by default** - use readonly, Readonly<T>, as const
-   **Generics with constraints** - bound type parameters appropriately


### Module design principles

-   **Single export per file** for main entity (except utils/types)
-   **Barrel exports** (index.ts) for public API only
-   **Colocation** - group related files by feature, not type
-   **Encapsulation** - export minimal public surface
-   **Dependency direction** - business logic never depends on infrastructure


## Available CLI tools


### Essential tools (use these for efficiency)

-   **eza** (or tree fallback) - Fast directory visualization
-   **rg (ripgrep)** - Blazing fast code search (better than grep/ack/ag)
-   **git** - Version control operations
-   **gh** - GitHub CLI for issues, PRs, releases
-   **jq** - JSON parsing and transformation
-   **find** - File system search
-   **fd** - Fast file finding (if available)

