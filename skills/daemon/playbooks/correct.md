# Playbook: correct

Pick when: the owner says /correct, or corrects agents for a mistake
they made before: a class worth making impossible, not one more fix.

1. correct, quick level: find the mistake classes from commits,
   reverts, review comments, agent rules and workaround comments; a
   class counts once it happened twice. Evidence: each class with
   two dated instances.
2. Per class, highest level that works: architecture, then types,
   then a lint or check whose error names the fix, then a test, docs
   last. Architecture or type moves go to playbooks/architecture.md
   first when they move a boundary.
3. build, build level, one class per item: the change plus its check.
   Evidence: the check run against a real past mistake, failing, then
   green on the fix.
4. prove-it-works, build level: the same command locally and in CI.
5. Rule table in the agent instruction file: each rule beside what
   enforces it; rules whose mistake can no longer happen are dropped.

Report: each class with its evidence, the level picked, and why a
higher level did not work.

Rule: a correction mid-task is fixed in place and its rule added; a
rule already listed with nothing enforcing it is a repeat, so step 2
runs on it in the same change. Ends on: one commit per class, each
with a check proven on a past mistake.
