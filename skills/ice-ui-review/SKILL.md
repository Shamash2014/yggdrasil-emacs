---
name: ice-ui-review
description: 'ICE UI review gate: compare screenshots to the reference. Apply to every ICE item that changes something a user sees, after its checks pass.'
---

# ICE UI review

Whether a screen looks right is almost impossible to write down, and a
pixel diff fails whenever two builds render differently. A reviewer who
looks at both screenshots can see a title too small or a divider too
dark, say where it is, and send the item back. The first rendering does
not need to be right; it cannot pass until it is.

**Why:** Shopify's Helix lands its native rebuild close to one to one
with the old app through this gate: a model acting as a perfectionist
design reviewer on matched screenshots, judging sizes in proportion,
with every fixable difference a blocker. A logic-only change skips it.

## If you are the reviewer

You are read-only. You write nothing and run nothing; your reply is the
review, and a build worker saves it as it stands. Your brief holds:

- the scope: what this checkpoint built, for example "the navigation bar
  and the title only";
- pairs of screenshots, one pair per state, as paths: built-STATE.png and
  ref-STATE.png;
- the reference kind: prototype, design, design.md or the old app. With
  design.md and no image, compare the built screenshot with the values
  it states.

Open each pair. First decide whether the two show the same state: the
same screen, the same data, the same step of the flow. An unfulfilled
order beside a fulfilled one, a loaded list beside an empty one, two
different scroll positions: that pair is INVALID. Say which state each
one shows and stop judging that pair.

In a valid pair, judge only what the scope names. The reference shows a
full screen and the checkpoint may have built a part of it; a missing
part outside the scope is not a difference.

Look at structure, order, spacing, alignment, size, weight, colour,
icons, text and truncation. Judge sizes in proportion to each
screenshot's own width and height, never in pixels: the two may differ in
density or window size. Never diff pixels.

List every difference, one line each:

    - blocker, header, top left: title is about 60% of the reference's height relative to screen width

The severity, where on screen, what differs. Severities:

- blocker: anything code can fix. This is the default for any difference;
- minor: code could fix it, but the owner has named it as allowed in the
  brief;
- platform: the difference comes from the platform itself and code
  cannot change it, such as a system font or a status bar.

Do not wave a difference through because it is small. If you are unsure
whether two things differ, list it as a blocker and say what you are
unsure of.

End on one verdict line, alone and exactly one of:

    Verdict: approve
    Verdict: blockers N
    Verdict: INVALID

approve when no blocker is listed, INVALID when any pair is invalid.

## Asking for the screenshots

Screenshots come from the repo's verification skill, skills/verify-APP,
driving the real app, and the feature map in lat.md/features.md says
how to reach each state. The reviewer cannot take them. Whoever runs ICE
sends one build worker a brief that names:

- the states to capture, taken from the scenarios this item covers, each
  with a short name such as empty, loaded or error;
- for each state: drive the built app there with the verification skill
  and save CHANGE/reviews/ui-N/built-STATE.png; bring the reference
  to the same state and save ref-STATE.png beside it, at the same window
  size;
- how to reach the reference: the old app through its own verification
  skill; a prototype by opening docs/prototypes/CHANGE/SCREEN-STATE.html
  in the browser the verification skill drives; a design as the exported
  frame; design.md as text only, with no ref image;
- "report the paths and the state each shows; change no code".

## Running the gate

1. After .ice/ice-verify CHANGE passes the item, capture as above.
2. Send the ui worker the scope, the pairs and the reference kind.
3. Hand its reply to a build worker that saves it unchanged to
   CHANGE/reviews/ui-N.md, N the slice number, under "## Round R",
   then fixes every blocker inside the item's Files.
4. INVALID: capture the named pair again in matching states and review
   again. This does not count as a round, but after two INVALID results
   in a row the item goes to the owner with both pairs.
5. After fixes, re-run .ice/ice-verify CHANGE, capture again, review
   again. Stop on approve. At most three rounds; past that the item goes
   to the owner with the file and the open blockers.

The scope grows with each checkpoint: the first may judge a skeleton,
the last the whole screen in every state.

## A second opinion

Spatial judgment varies by model family. Helix uses Gemini for this gate
because it catches margin and padding differences and estimates their
size; GPT is the next family to ask. The ui level runs on Claude opus
because a Claude lead can send only Claude subagents. For a second
opinion the owner opens a gemini or codex session from Emacs with this
skill as its prompt and the same pairs, and its reply is saved as a
second reviewer in the same file. When the two disagree, the stricter
one stands.

## Done

The item's UI is reviewed when CHANGE/reviews/ui-N.md exists and
its last round ends on approve, or it has gone to the owner past the
cap. This prints approve:

    grep "^Verdict: " CHANGE/reviews/ui-N.md | tail -1 | cut -d' ' -f2
