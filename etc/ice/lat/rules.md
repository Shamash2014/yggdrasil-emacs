# Rules

What code in this repo must not do, one rule per section, each with an id, why it exists and where it applies. Reviewers cite these ids.

Only "do not" rules go here. Prohibitions help an agent and positive instructions distort its work: removing one "do not refactor unrelated code" rule cost 20 points of resolved tasks (arXiv 2604.11088). Each rule records why it exists (2608.11095). A rule that a script can check should become that check, since checks are followed far more often than text, 88.3% against 67.0% (2603.00822); a rule whose Check line names a script or test is left to it. Rules are the owner's: an agent never adds, loosens or deletes one. Ids are never reused.

Each rule is an h2 named "RN short name", then a lead paragraph that starts "Do not", then Why, Scope and Check lines. Scope is a list of paths or globs, or all. Check names the script or test that enforces it, or none.

## R1 Unrelated changes

Do not refactor, rename, reformat or move code the item does not need.

- Why: unrelated edits hide the real change from review and break code no check covers; dropping this rule cost 20 points (arXiv 2604.11088).
- Scope: all
- Check: none

## R2 Swallowed errors

Do not catch an error and carry on without handling it or passing it up.

- Why: a swallowed error turns a failure a check would catch into wrong data nobody sees.
- Scope: all
- Check: none
