# Playbook: refactor

Pick when: restructure, rename, extract or dedupe is asked and
behaviour must not change. A missing feature or real bug found on the
way is its own item; ship the structural change first.

1. how, explore level: the affected subsystem's contract. Evidence:
   the contract in a few lines.
2. build level: a characterization test, snapshot or equivalence
   harness pinning current behaviour before any structure moves.
   Type check and lint are not a pin. Evidence: the pin, green.
3. architect, plan-review level, when the target shape crosses a
   function boundary. Name the target shape and what it deletes.
4. build level, small steps, the pin green after each: delete dead
   code and one-caller wrappers first; migrate every caller and delete
   the old API in the same wave, no shims or parallel paths; check each
   rename against the files, strings and prose included.
5. prove-it-works, build level: the real artifact behaves as before,
   by an old-vs-new output diff or a replayed baseline. "It compiles"
   is not proof.
6. a diff check: reader load dropped somewhere, else revert the
   change. Evidence: what got simpler, and what was reverted.

Report: structure changed, the pin, the equivalence proof, reverted.

Ends on: a diff that passed VERIFY with the pin unchanged.

Escalate to playbooks/ice.md or architecture.md when it is
cross-cutting or the shape is undecided; a redesign is a new
playbooks/feature.md item. PR steps: skill create-pr.
