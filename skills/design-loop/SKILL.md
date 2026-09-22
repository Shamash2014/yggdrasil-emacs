---
name: design-loop
description: Turn a goal that names a direction into criteria a shell command can prove. Use when the goal says "reduce", "no more", "stop using", "migrate off", "raise coverage", or any target the codebase drifts toward rather than reaches in one change.
---

A goal like "reduce the old pattern" cannot be graded, so nothing can
accept it for you. Give it back as commands that exit non-zero today and
zero when the increment is done.

- **Set point.** One line: the property, and the paths where it holds.
- **Sensor.** What already measures the gap here — a linter, a type
  checker, a test, ripgrep, a script the repo ships. Prefer what is
  installed over what would be written, and what cannot be disabled from
  inside the code it measures. Run it and read its output before you
  quote it.
- **Baseline.** Count now. Write the criterion against that number, so a
  direction becomes a condition. Count occurrences, not matching files:
  `test $(rg --count-matches OLD_PATTERN src | awk -F: '{s+=$2} END
  {print s+0}') -le 12`. A criterion that already passes proves nothing
  and will be dropped; the one that fails is the one that is kept.
- **Increment.** How much of the gap one run closes, and what is out of
  scope. Small enough to read in one diff.
- **Disturbance.** What changes these paths without this work — other
  work in flight, generated files, dependency bumps — and whether a
  criterion has to tolerate it.

Read only. Change no files except to add a failing test.

## Output

Answer in exactly these headings, nothing before or after:

```
## Understanding
what the work is, including the set point in one line

## Approach
- one increment per line

## Questions
- question text (default: answer)

## Criteria
- `shell command` - what it proves

## Touched
- path/to/file - why
```

Every criterion is a single shell command runnable from the checkout
root, with the baseline number written into it. Put the sensor's raw
count in Understanding so the number can be checked. Anything the code
cannot settle goes in Questions with your own default, not in prose.
