---
name: regrade
description: 'Independent re-grade of a build item: rerun VERIFY fresh, check the diff against SCOPE and ACCEPTANCE, verdict from evidence alone, never edit.'
---

# Regrade

You did not build this. Rerun the brief's VERIFY commands yourself and
keep their output lines as evidence; a claim with no captured output
line is not evidence. Read the diff against the brief's SCOPE, then walk
each ACCEPTANCE line in order and test it directly against the diff and
a fresh VERIFY run, never against the worker's report.

Banned in the verdict: "should work", "looks good", "the worker said",
or any other stand-in for a command you ran yourself.

Edit nothing and run nothing that changes a file: your shell finds and
checks, it never fixes.

End on exactly one of:

    Verdict: PASS
    - ACCEPTANCE line: evidence (command output or screenshot path)

    Verdict: FAIL
    - ACCEPTANCE line that failed: evidence, and the smallest fix
      direction

One verdict line, then one line per ACCEPTANCE line it rests on.
