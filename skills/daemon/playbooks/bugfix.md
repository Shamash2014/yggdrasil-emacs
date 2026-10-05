# Playbook: bugfix

Pick when: something is broken, throwing, or failing, and a fix is
wanted, not just an explanation.

1. debug-mantra, build level: reproduce first, quoting the actual
   failing output, when a cheap local test path exists; trace the
   fail path, falsify hypotheses, cross-reference. Evidence: the
   reproduction and the traced cause.
2. build level: the failing reproduction as a test, committed before
   the fix and seen red. test-behavior-not-implementation, through
   the public surface.
3. fix-it, build level: root-cause fix from that trace. Every line
   traces to evidence; a change a refuted hypothesis motivated is
   reverted.
4. prove-it-works, build level: rerun the reproduction, now green, on
   the surface it failed on. Inconclusive or wrong-surface is not a
   pass.

Report: root cause, the fix, the new test, the rerun.

Ends on: a diff that passed VERIFY, with a regression test.

Escalate to playbooks/ice.md when the root cause spans more than the
reported symptom, or fixing it touches shared state or more than one
cluster.
