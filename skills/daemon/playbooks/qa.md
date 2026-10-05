# Playbook: QA

Pick when: a change is built and needs an independent break-it pass
before the owner reviews it, ICE or not.

1. the qa preset with qa-health, one worker who built none of it and
   wrote none of its checks. Evidence: qa-proposals.md.
2. prove-it-works, build level: the feature run end to end as a
   person would, real artifact, not a proxy.
3. interrogate, review level, on any claim the QA pass could not
   settle itself.
4. differential-review, review level: what changed over the whole call
   graph, before and after, intended or not.
5. blast-radius, quick level, when a finding names a pattern that
   could live elsewhere in the checkout.
6. regression, quick level, when .aob/qa/baseline.json exists: diff
   findings, perf and canary against it.

Report: regressions first, findings by severity, scenarios, claims
last, pass or fail per the qa preset's closing line.

Ends on: findings with evidence; a confirmed bug becomes a new
playbooks/bugfix.md item. QA workers never fix; the recheck after a
fix is a fresh QA worker (lead-howto/checking.md).

Escalate: any other blocker goes back to the playbook that built the
change. Secure question: hand off to playbooks/bughunt.md as its own
item.
