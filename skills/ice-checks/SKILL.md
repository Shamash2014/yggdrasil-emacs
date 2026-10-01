---
name: ice-checks
description: 'ICE Expectation step: checks from confirmed intent. Apply whenever a check or test is written for an ICE change, or tests for code you did not write.'
---

# ICE checks

Checks come third in ICE: after exploration, after the intent is written
and confirmed, before the plan. They are the owner's expectations made
runnable. The agent that builds the change runs them and never writes or
changes them.

**Why:** a test written from the code agrees with the code, bugs
included. Handed buggy code, models write about eight times as many tests
that assert the bug and a third as many that catch it; handed a written
specification instead, bug-catching tests rise by four fifths (arXiv
2607.22883). Writing the contract first lifts bug detection by about ten
points (2608.17177). Tests written before the code find faults 25% of the
time, after it 14% (2607.05139).

## 1. The contract before any check

For each unit the change touches, write down before any test exists:

- pre-conditions: the exact state or input the unit requires;
- post-conditions: the value it returns, the error it raises for each bad
  input, the state it changes, the side effects it has;
- undefined inputs: what the spec leaves open, named so nobody tests it.

Give each pre- and post-condition line an id, C1, C2 and on, and each law
L1, L2 and on. A line an existing test already covers ends "(tested:
path)". Write one check per untested line, and nothing a line does not
ask for; every scenario names the lines it covers on a Covers line.

The contract lives in the change's Expectations as GIVEN / WHEN / THEN
scenarios, each with an id such as cart#promo-before-tax, and every check
carries its scenario id in its name.

A spec missing a contract is where checks miss bugs: caught 55% of the
time when the spec held the violated contract, 19% when it did not.

## 2. Expected values from the spec, never from the code

Reading code to understand what to specify is fine. Expected values for
behaviour the change adds or alters come from the intent, the spec
deltas, docs, the issue, a worked example or a reference implementation,
never from running or reading the code under test. Behaviour the change
keeps is specified by the code and its tests: mark it (tested: path) or
(code: path#symbol).

When the code may be wrong, because you wrote it or it is under a fix,
the checks are written by a fresh subagent given only the intent, the
contract and the public interface, with no path to the implementation.
The code must be out of view, not beside the spec: a spec added next to
buggy code barely helps, and looking for bugs with the code in view is
worse than not looking.

## 3. Today's code is the spec; a fix needs the intent

When a contract has to be written from existing code, audit it first:

1. logic mistakes: does it produce the right result even on the happy
   path;
2. robustness omissions: missing input validation, null and boundary
   handling, error handling, escaping.

Each finding goes to the owner as a proposed intent line, with its
default; it never goes into the contract on the audit's word. Only a
confirmed intent line may make a contract line differ from what the code
does today, and that contract line quotes it. A check that fails on
correct behaviour goes back to the owner, not into the code.

## 4. Property checks where a law holds

Beside the example checks, add one property check per law the contract
states:

- roundtrip: decode(encode(x)) equals x;
- reference model: the result equals a simple, obviously right version;
- invariants that survive every operation;
- idempotence, commutativity, associativity where the contract says so;
- monotonicity and ordering;
- boundaries: empty, single element, min and max.

One property per check, derived from the contract, not from reading the
source. Never filter the generator in a way that excludes the region a
bug would live in: a filter that skips empty or boundary inputs is the
commonest way a property check passes a bug. Property checks found 5 to
24 points more bugs for most models (2605.15229); they add to example
checks, they do not replace them.

## 5. Never bend a check to pass

Neither the writer nor the builder ever does any of these:

- invert, narrow, delete, skip or comment out an assertion or its
  expected value, or widen a tolerance to let a result through;
- wrap a failing check in something that swallows the failure, or return
  early before an assertion;
- detect which check is running and return what it wants;
- use mock or fake data to pass a check that real data would fail;
- remove the behaviour that makes a check fail instead of fixing it;
- write a comment or report that describes intended rather than actual
  behaviour.

If a check contradicts the confirmed intent, or cannot be passed without
one of the above, stop: name the check, quote the line of the intent it
contradicts, and hand it to the owner. Stopping to report a wrong check
is not failing the task.

A listed policy like this cut reward hacking from 23.6% to 9.7%, where a
one-paragraph warning managed 16.9%; with a way to stop and report it
fell to 5.3% (2608.29460). Telling an agent only to pass all tests, or
only not to modify them, left more than 85% cheating (2510.20270).

Text is weakest against editing the check files themselves, so an ICE
change locks its checks and verifies the lock outside the agent. This
skill is the rule; the lock is what holds it. Before the owner locks,
.ice/ice-fail-on-base CHANGE --red must pass: every scenario has a check
and every check fails now, with no implementation in the tree. A check
counts only from inside a test: its name, decorator, docstring or body
names the scenario id; an id in a helper or at module level counts for
nothing.

## Done

ice-check expect enforces coverage and ids, not the checks themselves.
It reads contract lines under these h3 headings, spelled with or without
the hyphen, in any case: Pre-conditions, Post-conditions, Laws, and
Undefined inputs, whose lines it skips. It fails on:

- a contract line under any other h3 inside ## Contract;
- a contract line without an id, or an id with nothing after it;
- a pre- or post-condition no scenario names on its Covers line;
- a law no scenario with a Check: property line covers;
- a scenario without a capability#slug id or a GIVEN, WHEN or THEN.

A line ending "(tested: path)" needs no scenario. Whether each check
exists, is named by its scenario id and traces to the spec is for the
owner to confirm. A change's checks are ready when:

- every untested contract line has one check named by its scenario id;
- every expected value for changed behaviour traces to the intent, the
  contract or a named reference, never to the code; kept behaviour
  traces to the code or an existing test;
- each law the contract states has a property check;
- the owner has confirmed them.
