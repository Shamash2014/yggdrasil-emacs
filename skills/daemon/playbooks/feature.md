# Playbook: feature

Pick when: new behaviour is asked for, not a fix or a lookup.

1. restate, unless the ask is a one-line fix.
2. Large or the repo is ICE-wired (.ice/, openspec/), or the owner
   asks for ICE: run playbooks/ice.md from here on and stop this list.
3. Otherwise, the light path — one file or small cluster, no shared
   state:
   a. Prototype first when the shape or feasibility is uncertain:
      playbooks/prototype.md. Skip when the shape is already known.
   b. build level: make the change.
   c. prove-it-works, build level: run the real artifact against what
      restate said the owner wanted. Evidence: the actual output.
   d. a diff check: git diff read by you, inside SCOPE only.

Report: which path was taken and why, the diff, the VERIFY run.

Ends on: a diff that passed VERIFY and the review.

Escalate mid-work from the light path to playbooks/ice.md the moment
the diff crosses more than the named file or cluster, or touches
shared state.
