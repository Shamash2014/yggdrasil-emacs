# Playbook: perf

Pick when: slow, janky, slow startup, a large bundle or memory growth
is reported and a fix is wanted. A trace, profile or heap already
captured and only a diagnosis wanted: the read-only branch below.

1. State the metric, the surface and the target as the done line.
2. Baseline, build level, on that surface: a trace or timing, median
   of N. Mobile: argent-react-native-profiler or argent-native-profiler
   (Expo, RN), flutter-mobile-testing (Flutter DevTools); claiming-a-
   device first on shared devices. Evidence: the numbers and artifact.
3. Vet the baseline and every later number: warm vs cold, noise
   floor, same build, device and data. A number that could not have
   failed is not a measurement.
4. how, explore level: hypotheses tied to the baseline, cheapest
   first: don't do it, don't repeat it, do it less, later, unseen,
   concurrently, cheaper. No ceiling claimed unrun.
5. One hypothesis at a time, build level, then re-measure on the same
   surface. Kept only when past noise and tests green, else revert.
   Inconclusive or wrong-surface is not a pass.
6. Sustained climbing against a metric: autoresearch, build level,
   with the same vetting and the same done line.

Read-only trace branch: load the trace or heap into sqlite, one row
per sample, frame or node; query the hot path, or a leak's retainer
chain to a GC root; attribute to file, symbol and line, an unmapped
frame is not a diagnosis; confirm on a paired capture or mark the
finding a hypothesis. No fix unless asked.

Report: metric, baseline, final, delta, artifact paths, each kept or
reverted change.

Ends on: a measured delta on the same surface, or a cited diagnosis.

Escalate to playbooks/bugfix.md when the slowness is a defect.
