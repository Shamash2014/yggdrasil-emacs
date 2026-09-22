---
name: decision-memo
description: "A technical decision memo, adversarially verified before the owner reads it: a proposer tags every load-bearing claim with a probe or a primary source, red-teamers try to refute each claim, a synthesizer rewrites from the evidence alone. Use when a turn must decide between options and the answer will be acted on."
disable-model-invocation: true
---

# Decision memo

The owner wants a technical decision memo verified before reading it,
not corrected afterward. Run this whenever a turn has to choose between
options and the choice will be built on. Do not run it for a question
one probe answers; answer that directly.

Question: the decision the turn is asked to make, stated in one line at
the top of the memo, with the options named.

## Protocol

1. Spawn a PROPOSER helper on your own tier that drafts the memo.
   Every load-bearing claim is tagged CLAIM-N and cites either a
   runnable probe, code it actually compiled or ran with the command
   and the raw output, or primary-source documentation with the link.
   No claim rests on model priors alone; a claim without a probe or a
   source is written as an assumption, not a claim.
2. In ONE message spawn three RED-TEAM helpers in parallel, on the
   lowest tier available to you, each given a disjoint subset of the
   claims. Their only job is falsification: write and run the smallest
   probe that would disprove the claim. Each writes
   YGG_EVIDENCE/claims/CLAIM-N.md holding the verdict on its first
   line, CONFIRMED or REFUTED or UNVERIFIABLE, then the exact commands
   and the raw output. When YGG_EVIDENCE is unset, use a claims folder
   under the system temp directory and name it in the memo.
3. Spawn a SYNTHESIZER helper on the lowest tier that reads only the
   evidence files, never the proposer's prose, and rewrites the memo.
   REFUTED claims are removed and the reasoning that rested on them is
   redone or dropped. UNVERIFIABLE claims move to a section headed
   Assumptions, labeled as such.
4. Report each helper's final verdict only, read from the verdict line
   of its evidence file, never from a heading or a summary written
   before the helper concluded.

## What you hand back

The final memo, then a table with one row per claim: the tag, the
claim in one line, the evidence kind (probe, source, inference), the
verdict, and the evidence file path. A memo with no table is not done.
Name in your closing words that this skill ran and how many claims
survived.
