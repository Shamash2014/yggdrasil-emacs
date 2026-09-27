---
name: ice-prototype
description: Throwaway static HTML prototype for an ICE change with user-visible UI and no reference. Apply when planning its checkpoints; skip if a design exists.
---

# ICE prototype

The UI review gate needs something to compare with. When a change shows
the user something new and nobody has drawn it, the first checkpoint
draws it: a prototype the owner can open in a browser and approve in
minutes, before any of the real system is built against it.

**Why:** a question a prototype can settle is settled that way, not
asked. A picture the owner has approved is a reference the UI gate can
hold the build to; a paragraph of taste is not. Helix builds against a
reference app or designs; this is what stands in when neither exists.

## When

Build one when all three hold:

- the change has user-visible UI: a screen, a panel, a dialog, a state a
  user sees change;
- there is no design, no reference app and no design.md that covers it;
- the owner has not said to skip it.

Skip it for a logic-only change, a change whose UI already has a
reference, and a one-sentence diff. When a reference covers some screens
and not others, prototype only the others.

## What it is

Static HTML and CSS, nothing else:

- under docs/prototypes/CHANGE/, one file per screen and state, named
  SCREEN-STATE.html, such as cart-empty.html and cart-error.html;
- one shared style.css beside them, the repo's own tokens and fonts
  where it has them;
- no backend, no build step, no framework and no script beyond what a
  state needs to look right; each file opens straight from disk;
- fixed sample data written into the page, the same data the scenarios
  name, so the built app can be brought to the same state later;
- an index.html listing every file, one line each, with the scenario id
  it shows.

## What it must show

Every state the change's scenarios name, and at least:

- the loaded state with realistic data, long names and all;
- the empty state;
- the error state for each failure scenario the user can see;
- loading, when the user waits long enough to see it;
- each state a user action leads to: open, selected, disabled, confirm.

A state the scenarios name and the prototype lacks is a gap the UI gate
cannot judge; name it in the checkpoint report.

## Approval

The prototype is the first checkpoint of the change. A build worker
writes it; the owner opens index.html and approves it, or says what to
change, and the worker changes it. Once approved:

- it is the reference kind prototype for every later ui review of this
  change, captured by opening SCREEN-STATE.html in the browser the
  verification skill drives, at the window size the app runs at;
- the owner's changes to it are feedback lines under ice-learnings;
- the main system is built against it, and it is not edited again
  without the owner.

When the change is archived, docs/prototypes/CHANGE/ is deleted: the
built app is the reference from then on.

## Done

The prototype is ready for the owner when every scenario that names a
visible state has a file. This prints nothing:

    for id in $(sed -n 's/^## Scenario:[[:space:]]*//p' CHANGE/expectations.md); do
      grep -q "$id" docs/prototypes/CHANGE/index.html || echo "missing $id"
    done

A scenario with no visible state is listed in index.html as "no screen".
