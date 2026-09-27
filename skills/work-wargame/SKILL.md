---
name: work-wargame
description: How to attack a WORK plan before anyone agrees to it, with subagents on one angle each. Read when asked to attack a plan.
---

# work-wargame

You are given a plan nobody has agreed to yet. **Your job is to break it.** Not
to improve it, not to approve it, and not to implement any part of it.

A plan comes back from the agent that wrote it having been agreed with by
exactly one party. This is the other one.

## One subagent per angle, dispatched together

Give each the contract, the plan and the tree — and one angle only. They do not
get each other's findings and they do not get a narrative:

- **Failure.** Take each step's assumption and make it false. What happens?
- **Ordering and concurrency.** Two of these at once; these in the other order;
  this one interrupted halfway.
- **Compatibility.** Existing callers, data already written, older clients,
  anything persisted in the shape this changes.
- **Security and permissions.** Who can reach this that could not before, and
  what does it now let them do.
- **Operations.** What this looks like when it fails at three in the morning:
  what is observable, what is recoverable, what is silent.

Require the same thing back from each: what actually breaks, named at a file
and a line, or nothing. "Looks reasonable", "consider adding", and a list of
general good practice are nothing. An angle that found nothing found nothing —
say so and move on.

Add an angle this plan obviously needs and this list does not have.

## Then judge, and drop most of it

An attack the plan already answers is dropped, silently. An attack on something
outside the scope the card names is dropped. An attack that amounts to "this
could be more general" is dropped. What is left is what genuinely breaks the
plan as written.

**At most three.** If everything survived, say so — a plan that cannot be
broken by five subagents on five angles is a plan worth running, and reporting
three weak objections to look thorough is how a real one gets lost among them.

## What survives is a question, not a finding

A break in the plan is a decision somebody has to make, and it is not yours.
Write each as a question with the ways it could go, indented and lettered:

```
UNCERTAIN
- <what breaks, in one line, with the place it breaks>
  a) accept it — what that costs, and when it would bite
  b) <the mitigation> — what it costs
  c) <the different approach> — what it costs
  pick: a — <one line: why, in this codebase's terms>
```

`pick:` is its own last line, naming a letter, because the human can take every
`pick:` at once and get on with the work — what is recorded then is your
reading, marked as yours, and the daemon reads it: the option you name leads
the answer menu and is the default the park would take. So do not name a letter
you would not defend. Where
the choice is genuinely not yours to make, `pick: none — <why>` stops the plan
until somebody decides, which is what it is for.

`a)` is a real option and often the right one: a risk taken deliberately is not
the same as a risk nobody saw.

Answer with that block and nothing around it. If nothing survived, answer
`NOTHING SURVIVED` and nothing else.
