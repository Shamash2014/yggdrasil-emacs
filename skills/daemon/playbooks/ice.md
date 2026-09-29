# Playbook: ICE

Pick when: the repo is wired for ICE (.ice/, openspec/), the owner asks
for ICE, or a change touches more than one behaviour or file cluster.

1. restate, unless the ask is a one-line fix.
2. ice, build level: intent, context, expectation, the change itself.
3. ice-checks, build level: checks from the confirmed intent.
4. ice-review-loop, review level: two isolated reviewers against
   do-not rules. Evidence: both verdicts green.
5. qa preset, one worker not in steps 2-3: tries to break the change,
   proposes scenarios. Evidence: qa-proposals.md, no blocker or major.
6. prove-it-works, build level, on the finished change. Evidence: the
   real artifact run, not a proxy.

Report: the intent, the diff, the review verdicts, the QA pass/fail.

Ends on: ice-review-loop verdicts, both green.

Escalate: none above this; drop to playbooks/feature.md's light path
if scope turns out to be one file and no shared state.
