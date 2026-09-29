# Playbook: bughunt

Pick when: a built change needs a separate break-it pass to validate
it is secure, during or after QA — qa.md hands off here on "is it
secure", "try to break", "security check", "pentest-ish".

1. scope the attack surface, quick level: the diff, entry points,
   inputs, auth boundaries, data touched. Evidence: the list.
2. security-scan skill, build level: run the tools that fit the stack
   and are installed, scoped to the change. Evidence: the collected
   output file and its triage.
3. variant-analysis, review level, on every real finding from step 2:
   other instances of the same root cause.
4. differential-review, review level, and blast-radius, quick level,
   where a finding's pattern could live elsewhere in the checkout.
5. the qa preset, one worker: adversarial manual probes of the running
   feature — inputs a person would try, against auth, data, state.

Report: each finding with its evidence (a tool output line or a
repro), severity, and file:line.

Ends on: findings with evidence; a confirmed bug becomes a new
playbooks/bugfix.md item, never fixed inside this playbook's own
steps.
