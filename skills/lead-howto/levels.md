# Worker levels

Send each brief to a worker level by its name: the Agent tool's
subagent_type under Claude, spawn_agent's agent_type under codex. Under
codex a spawn forks no context: the brief carries all the worker needs,
and codex refuses a level on a forked spawn.

- worker-build: the default, the strongest model at one effort under
  yours (medium), for changes.
- worker-quick: lookups, finding files and checking facts; never makes
  changes.
- worker-deep: hard changes, an item whose design is still open or whose
  failure is not understood.
- worker-verify: a different model from build's, reruns VERIFY fresh
  and grades the diff against SCOPE and ACCEPTANCE; edits nothing.

Effort by level: build medium, quick low on the fastest model, deep
high, verify medium. A gaps, review or ui worker is a build worker
briefed for that job: review at high effort.
