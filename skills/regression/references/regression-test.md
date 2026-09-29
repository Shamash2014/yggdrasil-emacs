# Regression test naming and attribution

Check the directory next to the fixed code for files matching
NAME.regression-*.test.EXT; take the highest N present and use N+1.
No existing regression test for that name starts at 1.

Attribution comment, one line, placed at the top of the new test:

    // Regression: FINDING-ID -- what broke, found 2026-09-29

Trace the codepath the bug took before writing the assertion: the
precondition that triggered it, the branch it followed, and the exact
line that failed. The test sets up that precondition and asserts the
correct behaviour, not merely that the code runs.
