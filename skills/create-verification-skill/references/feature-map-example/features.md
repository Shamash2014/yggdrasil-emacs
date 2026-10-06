# Features

One section per user-visible feature with five h3 subsections: Sub-features, How to get to it, Driving it, Gotchas and Code.

Every recipe starts from the baseline in skills/verify-notes/SKILL.md: Notes at http://127.0.0.1:4173 on a disposable data directory, seeded with Quarterly plan and Grocery list, and control-notes doctor passing. Each feature is an h2 named the way a user names it; intents link to it as "Feature: <name>". Its h3s are Sub-features, How to get to it, Driving it, Gotchas and Code, in that order, each opening with a paragraph of at most 250 characters. The archive step adds a Changes h3; leave it to that step.

## Create a note

Create note lets a user save a titled note from the browser or CLI, cancel an unfinished draft, and confirm the saved note from a second view.

### Sub-features

The behaviours a full pass proves, each with a short id the evidence names.

- create-open opens a blank editor from each browser entry point.
- create-save persists a title and body.
- create-cancel discards an unfinished browser draft.
- create-cli creates the same note shape from the terminal.

### How to get to it

Every entry point a user has, from the user's side.

- Choose the New note button in the browser toolbar.
- Press n in the browser while focus is outside an editable field.
- Run notes create --title TITLE --body BODY in a terminal.

### Driving it

Each step pairs the user action with the exact control-notes command and the result that proves it.

Preconditions:

- Notes is healthy at http://127.0.0.1:4173.
- No note is titled Release checklist.
- control-notes doctor reports the expected URL and disposable data directory.

Steps:

- **Open editor.** Choose New note. Run control-notes browser click --role button --name "New note". A form named Note editor appears with focus in the Title textbox.
- **Enter content.** Run control-notes browser fill --role textbox --name "Title" --value "Release checklist" and control-notes browser fill --role textbox --name "Body" --value "Tag and publish". The Save note button becomes enabled.
- **Save note.** Run control-notes browser click --role button --name "Save note". A status named Note saved appears and the heading reads Release checklist.
- **Confirm persistence.** Run control-notes browser click --role link --name "All notes", then control-notes browser click --role link --name "Release checklist". The editor shows both saved values.
- **Cancel draft.** Open a new note, fill the title Discard me, and run control-notes browser click --role button --name "Cancel". The note list returns and has no Discard me link.
- **CLI entry.** Run control-notes cli -- notes create --title "CLI note" --body "Created from terminal" --format json. Exit code 0 and stdout hold the new note id and title.
- **Proof.** Run control-notes browser snapshot --aria --path artifacts/create-note/list.aria.txt and control-notes browser screenshot --path artifacts/create-note/list.png. Both show Release checklist and CLI note.

### Gotchas

Traps that waste a run or make its proof worthless.

- Pressing n while a textbox has focus types the character instead of opening a new editor.
- Titles are trimmed on save. Assert the rendered title, not the draft input value.
- A save status alone is not proof. Reopen the note from the list.
- Remove Release checklist and CLI note in cleanup, but keep their proof artifacts.

### Code

The main files, then the test files that map to them by name.

- `src/ui/note-editor.tsx`
- `src/cli/notes-create.ts`

Tests:

- `src/ui/note-editor.test.tsx`
- `src/cli/notes-create.test.ts`

## Search notes

Search lets a user find notes by title or body text, open a match, and tell no matches apart from an unavailable search.

### Sub-features

The behaviours a full pass proves, each with a short id the evidence names.

- search-open opens search from each browser entry point.
- search-match returns title and body matches without changing note data.
- search-open-result opens a result in the note editor.
- search-empty shows a complete empty state for a query with no matches.
- search-clear removes the query and restores the recent-notes view.
- search-cli returns the same matching notes from the terminal.

### How to get to it

Every entry point a user has, from the user's side.

- Choose the Search button in the browser toolbar.
- Press / in the browser while focus is outside an editable field.
- Run notes search QUERY in a terminal.

### Driving it

Each step pairs the user action with the exact control-notes command and the result that proves it.

Preconditions:

- Notes is healthy at http://127.0.0.1:4173.
- The data directory holds Quarterly plan with body text Draft budget.
- control-notes doctor reports the expected URL and data directory.

Steps:

- **Toolbar entry.** Run control-notes browser click --role button --name "Search". A dialog named Search notes appears with focus in its searchbox.
- **Keyboard entry.** Close the dialog, focus the page, and run control-notes browser press --key "/". The same dialog appears and no slash is typed.
- **Title match.** Run control-notes browser fill --role searchbox --name "Search notes" --value "quarterly". The Search results list holds Quarterly plan and not Grocery list.
- **Body match.** Run control-notes browser fill --role searchbox --name "Search notes" --value "budget". Quarterly plan stays with a body-match excerpt.
- **Open result.** Run control-notes browser click --role link --name "Quarterly plan". The dialog closes and the editor heading reads Quarterly plan.
- **Empty state.** Reopen search and fill volcano. A status named No matching notes appears once search completes.
- **Clear query.** Run control-notes browser click --role button --name "Clear search". The searchbox is empty and Recent notes replaces the results.
- **CLI match.** Run control-notes cli -- notes search "quarterly" --format json. Exit code 0 and stdout hold one object titled Quarterly plan.
- **CLI miss.** Run control-notes cli -- notes search "volcano" --format json. Exit code 0 and stdout is [].
- **Proof.** Run control-notes browser snapshot --aria --path artifacts/search/results.aria.txt and control-notes browser screenshot --path artifacts/search/results.png. Both show Notes, the query and Quarterly plan.

### Gotchas

Traps that waste a run or make its proof worthless.

- Pressing / while the editor or searchbox has focus types text instead of opening search.
- Results update after a debounce. Wait for the results list or the empty status, never a fixed sleep.
- Archived notes are left out unless the user turns on Include archived.
- The CLI prints for humans by default. Use --format json for stable assertions.
- Opening a result changes browser state. Reopen search before proving another query.

### Code

The main files, then the test files that map to them by name.

- `src/ui/search-dialog.tsx`
- `src/cli/notes-search.ts`

Tests:

- `src/ui/search-dialog.test.tsx`
- `src/cli/notes-search.test.ts`
