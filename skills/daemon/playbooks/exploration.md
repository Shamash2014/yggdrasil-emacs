# Playbook: exploration

Pick when: the ask is read-only: how something works, why it was built
that way, where something lives, or an investigation with no edit
asked for.

1. restate, unless the question is already a one-liner.
2. how, quick level, for runtime flow or placement questions; why,
   quick level, for rationale, history or a regression.
3. recall, quick level, when the question is about a past change or
   period, not the code as it stands.
4. explore-do, deep level, when the question needs code read across
   several files to answer, not one lookup.

Report: the answer with evidence (file:line, a command's output), and
each claim that could not be confirmed, marked as such.

Ends on: an answer with evidence, no edits.

Rule: no edits, ever, on this playbook. A finding that wants a change
hands off to playbooks/feature.md or playbooks/bugfix.md as a new item,
never fixed in place.

Escalate only as a new item the owner approves, never in place.
