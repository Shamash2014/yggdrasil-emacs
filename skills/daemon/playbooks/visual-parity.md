# Playbook: visual-parity

Pick when: a UI must match Figma or another build, pixel for pixel.

1. Baseline first, build level: a harness screenshotting the current
   or target component across its states. No baseline, no parity
   claim. Figma source: figma-build-design. Evidence: the baseline
   path.
2. Held for every item: no harness edits, no baseline tampering, no
   restructuring a component to pass a diff. A baseline that looks
   wrong goes to the owner.
3. One component per item, shared primitives first; parallel items
   each in their own worktree.
4. argent-screenshot-diff, build level, on the matching surface; web
   or review passes with ice-ui-review or visual-pr. A nonzero diff
   fails; investigate the delta and loop until zero. Evidence: the
   diff output per component.
5. a diff check: only the component's code changed.

Report: components done, diff per component, baseline path, what is
left.

Ends on: a zero diff per component against an untouched baseline.

Escalate to playbooks/ice.md when UI scope spans many components.
PR steps: skill create-pr.
