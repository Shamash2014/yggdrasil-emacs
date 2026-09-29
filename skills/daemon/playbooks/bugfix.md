# Playbook: bugfix

Pick when: something is broken, throwing, or failing, and a fix is
wanted, not just an explanation.

1. debug-mantra, build level: reproduce first, quoting the actual
   failing output, when a cheap local test path exists; trace the
   fail path, falsify hypotheses, cross-reference. Evidence: the
   reproduction and the traced cause.
2. fix-it, build level: root-cause fix from that trace.
3. test-behavior-not-implementation, build level: a test that would
   have caught it, through the public surface, always a regression
   test since it changes existing behaviour.
4. prove-it-works, build level: rerun the reproduction, now green.

Report: root cause, the fix, the new test, the rerun.

Ends on: a diff that passed VERIFY, with a regression test.

Escalate to playbooks/ice.md when the root cause spans more than the
reported symptom, or fixing it touches shared state or more than one
cluster.
