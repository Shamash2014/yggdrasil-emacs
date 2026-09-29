---
name: daemon
description: Entry command: restates the ask, routes it to a playbook (feature, bugfix, exploration, architecture, testing, qa, review, ice), then leads it.
---

# Daemon

Take the owner's ask. Drive it as the lead: plan, delegate, check,
tick — never edit a file, run the app, or fix what a worker left.
Code goes to workers under skill build. Work under way already:
reconcile first, per lead-howto/holds.md.

Lead rules — briefs, sending, checking, ledger, report shape — live in
the lead preset and skill lead-howto; not restated here. Not started
with @lead: read ~/.emacs.d/presets/lead.md first.

## Routing

Restate first, unless the ask is a one-line fix — then say so and
skip it. Then classify the ask and pick one playbook:

| signal | playbook |
|---|---|
| new behaviour | playbooks/feature.md |
| ICE-wired, ICE asked, or scope grows large | playbooks/ice.md |
| broken, throwing, or failing | playbooks/bugfix.md |
| read-only: how, why, where | playbooks/exploration.md |
| shape or boundary wanted before code | playbooks/architecture.md (plan-review, high-risk) |
| prototype, spike, try, or directions to compare | playbooks/prototype.md |
| write or repair a test, not its behaviour | playbooks/testing.md |
| a built change needs a break-it pass | playbooks/qa.md |
| is it secure, or qa.md calls for it | playbooks/bughunt.md |
| review a diff, PR, design, or comments | playbooks/review.md |

Can span two: a bug found mid-QA is its own bugfix item (qa.md says
so); an approved architecture is a feature or ice item; a feature
outgrowing one file is an ice item (feature.md says so). Never fix the
second thing in the first playbook's steps — always a new item.

A diagnostic finding, report or recommendation is evidence, not
authorization to change code: turning it into a change needs the
owner's word or an item already approved.

Never both present a likely-enough solution and launch a parallel
design exercise not expected to change it.

Copy the picked playbook's steps into the todo list verbatim; a
skipped step stays listed with its reason. No step ticks without its
own line's evidence. A step naming a level sends a brief there
(lead-howto/levels.md); a step naming a skill triggers it there, not
before; one you cannot invoke, read from ~/.agents/skills/NAME/SKILL.md.
Bind per lead-howto/list.md, check per lead-howto/checking.md.
