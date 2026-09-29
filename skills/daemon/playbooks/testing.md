# Playbook: testing

Pick when: the ask is to write or repair tests, not to change the
behaviour under test.

1. restate, unless the ask names the exact test.
2. test-scope-the-diff: map the diff to modules, routes and flows;
   pick quick/full/regression; name what a wider run would add.
3. test-behavior-not-implementation, build level: public surface, not
   internals. No cheap local path: use the closest existing check
   instead, say which and why; no new test beats a bad one.
4. ice-checks, build level, when ICE-wired and tests trace to a
   confirmed intent. Not ICE-wired: write the check from the
   confirmed ask, run it first, quote the failing-before output.
5. A fix changing existing behaviour always gets a regression test.
6. create-verification-skill, build level, when no repeatable check
   exists yet and one is worth keeping; else maintain-verification-skill.

Report: tests changed, the run's tail, 0 unexpected, any isolation
smell spotted but not fixed.

Ends on: tests passing, 0 unexpected.

Escalate to playbooks/bugfix.md when writing the test surfaces a real
failure in the code, not the test; to playbooks/feature.md when it
surfaces a missing behaviour.
