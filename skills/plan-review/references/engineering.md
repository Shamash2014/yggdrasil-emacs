# Engineering lens

Read the design doc and the code it touches; do not read the other
lens reports.

Check:

- Draw a dependency diagram, new parts against existing parts,
  showing what calls what.
- Where does coupling concentrate, and what breaks if one part
  changes shape?
- Failure modes: a slow dependency, a partial write, a retry, a
  concurrent caller.
- Performance: the expected load, and where the design assumes it
  will stay small.
- Build a test-coverage map: every new flow, codepath, and branch the
  design introduces, paired with the test type (unit, integration,
  e2e, eval) that will cover it. Never skip or compress this map. A
  deferred entry still needs a reason attached.

Report every finding tagged fix, taste, or authority, with the doc
section it targets.
