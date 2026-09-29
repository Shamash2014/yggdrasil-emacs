# Playbook: review

Pick when: the ask is to review, audit or second-opinion a diff, PR or
design, or the owner hands over review comments on one.

Owner review comments on a diff: the review preset, interactive,
turns them into an approved checkpoint list; work that list once
approved, one item per checkpoint.

No comments yet, just "review this":
1. scrutinize, review level: outsider read of intent, then whether the
   code does what it claims.
2. differential-review, review level: security-shaped read of the
   diff's blast radius.
3. interrogate, review level, when either found a claim worth a second
   angle.

Report: findings by file:line, severity, and what each asks for.

Ends on: findings, or an approved checkpoint plan.

Escalate to ice.md when a finding is scoped enough to need its own
intent and checks, not a same-session fix.
