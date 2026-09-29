# Mobile (Flutter, native Android/Kotlin, iOS/Swift)

Claim a simulator or emulator through simbroker before driving
anything, never boot or borrow one directly: simbroker claim --class
ios or --class android prints the claim id and the UDID or AVD name;
simbroker release ID when done, simbroker renew ID for a long run.
Never touch a device the owner holds (simbroker list shows who holds
what); a full pool is reported, not worked around. Never shut down or
erase a simulator. Drive the claimed UDID or serial explicitly; do not use
flutter-mobile-testing's "booted" shorthand here, since "booted" can
resolve to a device someone else is holding. See
skills/flutter-mobile-testing for the interaction commands
themselves; this file only adds what regression needs on top.

## What to drive

Screens and flows from the repo's own verify skill feature map
(lat.md/features.md, one h2 per user-visible feature). No feature
map: fall back to the app's own screen/route names.

## What regresses

- Screenshot per screen and state, compared against
  .aob/qa/screens/mobile/. Use the repo's own screenshot-testing
  harness (Flutter golden tests, or an equivalent already wired in)
  when one exists; otherwise capture with flutter-mobile-testing's
  commands and compare visually, flagging any difference as WARNING
  for owner review rather than auto-failing (UNCONFIRMED as a
  diff method; no pixel-diff tool is installed for this).
- Cold start time: flutter drive or integration_test's own timeline,
  Android macrobenchmark, or adb shell am start -W on the claimed
  device/emulator; xcodebuild test / XCTest launch metrics on iOS.
  Read the metric name from that tool's own output rather than
  assuming a field name.
- Frame build/jank stats where the tooling reports them (the same
  timeline or macrobenchmark output, or XCTest's measure API). These
  are frame-scale (single-digit ms) numbers, not the 500ms-absolute
  leg from SKILL.md's Performance section; judge them only against
  the 50%/20% relative legs, never the absolute one.
- App size per build, against perf-baseline.json's "mobile" key:
  REGRESSION over 10%, WARNING over 5%. UNCONFIRMED as our own
  choice; gstack has no mobile app-size rule to inherit.

## Canary

None. Mobile has no live canary check (no running process to poll
between deploys); a post-release smoke on the store or TestFlight
build is the owner's call, not something this skill runs on its own.
