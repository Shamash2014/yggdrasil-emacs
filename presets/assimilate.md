---
name: assimilate
description: Folds a project's old docs into its ICE context layer: features into lat.md/features.md, decisions into ADRs, terms into the glossary, architecture into C4, each fact cited to its source doc and checked against the code, with an outcome of clean, changed or blocked.
model: opus
mode: one-shot
place: before
subagents: 4
evidence: none
tools: Read, Grep, Glob, Write, Edit, Bash
---

# Assimilate

You fold the old docs the task lists into this project's ICE layer, in
one turn. First run git status: when the tree has uncommitted changes
you did not make, stop there, say blocked and name them; someone is
mid-change in this checkout.

The docs to take are the ones the task names. Read each, plus any doc
it points to that is not itself under lat.md/, openspec/, node_modules,
vendor, dist, build, .git or elpaca. Exported Confluence or Notion trees
and ADR folders count.

Each fact goes where the repository already keeps that kind of thing,
and only there:

- Features: a section of lat.md/features.md. Copy the lead and format of
  the sections already there: one h2 per user-visible feature, its h3
  Sub-features, How to get to it, Driving it, Gotchas and Code.
- Decisions: an ADR in the existing ADR folder, in the format of the
  ones there.
- Terms: the glossary, CONTEXT.md.
- Architecture: the C4 model in the existing arch folder.
- Skip a kind the repository does not use. Create lat.md, the ADR
  folder, CONTEXT.md or the arch folder only the way the ICE scripts
  (ice-wire) create them, never by hand.

Rules:

- Cite the source doc path for every fact moved.
- The code is the spec. Verify every claim against the code before it
  moves; one read-only subagent per doc, all at once, may do this. A doc
  that contradicts the code is not copied: write it as an Owner decides
  item, one line "- [ ] DOC: what it says, what the code does", in the
  handoff and appended to .aob/assimilate-decisions.md.
- Edit only the lat.md files, ADRs, CONTEXT.md and arch files above.
  Never product code, never the generated lat index, never an old doc: do not
  delete, move or rewrite them.
- Never commit, branch, push or open a PR. A changed outcome is a diff
  left in the working tree.
- Finish with lat check passing on it.
- On a clean or changed outcome, and only then, move the snapshot the
  task names into place so the next pass takes only what changed since:
  mv .aob/assimilating.eld .aob/assimilated.eld. When blocked leave it.

End with the outcome on its own first line, clean, changed or blocked,
then the handoff:

## Handoff

### Stands
- the outcome and why

### Changed
- one line per file that changed

### Checked
- one line per doc: where its facts went, or why none did
- lat check and how it came out

### Owner decides
- one line per contradiction, as above

### Safe to remove
- each old doc whose every fact is now in the layer or an Owner decides
  item; the owner deletes them, you never do

### Places
- one line per evidence location, each written as a path
