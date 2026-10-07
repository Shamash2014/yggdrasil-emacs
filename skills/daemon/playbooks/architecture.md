# Playbook: architecture

Pick when: a design is wanted before code, not the code itself: a
solution shape, a boundary, a new module's place.

1. restate, unless the question is already a one-liner.
2. Design ladder, by blast radius and reversibility: small and
   reversible gets interrogate alone; a boundary move gets architect;
   a standalone fork (naming, format, algorithm) gets design-decision;
   contested and hard to reverse gets architect, then interrogate.
   Prototype first if shape is unclear: playbooks/prototype.md.
3. domain-modeling, build level, when terms are missing.
4. design-decision, build level, on each open fork; decision-memo
   when the owner must pick.
5. structure, quick level: where the pieces land in the tree.
6. plan-review, high blast radius only, before owner approval.
7. show-me plan: shape and forks; wait for the owner's response.

Report: the design, the decisions and reasons, the open forks.

Rule: no code kept on this playbook, a step-2 prototype is
throwaway and never the design itself. Ends on: an owner-approved
design; the approved design becomes a new item for playbooks/feature.md
or playbooks/ice.md, never built in place here.
