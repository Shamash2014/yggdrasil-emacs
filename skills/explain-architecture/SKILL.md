---
name: explain-architecture
description: Explain how a codebase, or a named part of it, is put together for a reader new to it, with small Mermaid diagrams and a module table in the project's own domain words from CONTEXT.md. Use when the user asks for an overview, a map, or "how does this fit together"; use how for tracing one runtime path or deciding where code should live.
---

# Explain Architecture

The answer is the explanation itself, in the reply. Write no files unless the user asks.

## 1. Load the vocabulary

- If CONTEXT-MAP.md exists at the repo root, read it, then the CONTEXT.md of each context in scope.
- Otherwise read CONTEXT.md at the repo root if it exists.
- Hold the terms and their _Avoid_ lists. Every name in prose, diagram labels and the table uses a glossary term when one exists. Never use a word the glossary lists under _Avoid_.
- No glossary at all: say so in one line at the top, name things after the code's own dominant words, and recommend the domain-modeling skill at the end.

## 2. Find the real structure from code

Docs say what was intended; code says what is. Read docs for orientation only and let code win when they disagree (mention the disagreement).

- Entry points: main files, CLI commands, route or handler registration, package manifests (bin, main, exports), init files, job schedulers.
- Modules: top-level directories and packages, and what each one exports.
- Boundaries: which module imports which. Count imports rather than guessing; a quick grep of import or require lines per directory is enough.
- Data: the core types, records, tables or schemas the rest of the code passes around.
- Main flow: follow one representative request or command from entry point to its effect, reading each hop.

Scope to what the user named. For a whole repo, stop at the level where there are 4 to 10 modules.

## 3. Map code names to domain terms

For each module, type and flow step you will show, pick its glossary term. When a concept the reader needs has no term, do not invent one silently: use the code's own name and add it to Missing terms.

## 4. Write the answer

In this order, nothing else:

1. **Overview**: one paragraph. What the system does, its main parts, and how a unit of work moves through it.
2. **Diagrams**: 1 to 3 Mermaid blocks, each preceded by one sentence saying what it shows. Choose by what explains this code best:
   - flowchart of modules and their dependencies: almost always the first one
   - sequenceDiagram of the main request or command flow: when behaviour lives in the interaction
   - classDiagram or erDiagram of the core data: when the data model is the key to the rest
3. **Modules**: a table.

   | Module | Responsibility | Key files |
   |---|---|---|
   | domain term | one line | path, path |

4. **Where to start reading**: 3 to 6 file paths in reading order, each with one line on why.
5. **Missing terms** (only when there are any): concept, code name used, one line on what it means. Then suggest running domain-modeling to name them.

## Diagram rules

- Mermaid only. Graphviz and PlantUML are not installed; the user renders Mermaid fences inline with TAB.
- Under about 15 nodes per diagram. Split or drop detail rather than crowd.
- Labels in domain terms; node ids short and simple (letters, digits, underscore): Ord["Order intake"].
- Quote any label containing spaces, parentheses, slashes or colons.
- Group with subgraph only when the grouping is a real boundary (a context, a process, a package).
- Arrow direction means "depends on" in module flowcharts, "calls or sends" in sequences. Label edges only when the verb is not obvious.
- No styling beyond what carries meaning.

See references/examples.md for one worked example of each diagram kind.

## Checks before sending

- Every name in the reply is a glossary term or appears in Missing terms.
- No _Avoid_ word appears anywhere.
- Every module and file named was read, not inferred from a directory name.
- Each diagram is under about 15 nodes and parses (balanced quotes, no bare parentheses in labels).
- Prose is short; the diagrams and table carry the weight.
