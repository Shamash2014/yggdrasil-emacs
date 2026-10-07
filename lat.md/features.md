# Features

The user-visible features of the yggdrasil config (modal editing, spaces, agents, git compare review, ICE tooling), one section per feature. Keys are the real bindings, checked against the code by test/ice-feature-map-tests.el.

## Modal editing

Helix-first modal editing with a vim blend: normal, visual and insert states, selection before verb, match mode for pairs and surrounds, ex command line, and a tutor.

### Sub-features

Motions, selections, verbs, match mode, action groups, quickscope hints, block selection and the ex line.

- Motions and goto maps: words, finds, paragraphs, scrolling, views.
- Selection model: multiple selections, split, keep, rotate, align.
- Verbs: delete, change, yank, paste, indent, join, registers, increment.
- Match mode `m`: jump, inside, around, surround, tree-sitter expand.
- Ex line, quickscope f/t hints and rectangle selection.

### How to get to it

Normal state is the default in every file buffer; the keys below are normal-state bindings unless noted.

- `x` — select the line, repeat to extend (`ygg-select-line`)
- `d` — delete the selection (`ygg-delete-dwim`)
- `m s` — surround every selection with a pair (`ygg-match-surround`)
- `s` — select every regex match inside the selection (`ygg-select-regex`)
- `.` — repeat the last change (`ygg-repeat`)
- `SPC :` — ex command line (`ygg-ex`)
- `M-x ygg-tutor` — hands-on tutor buffer (`ygg-tutor`)

### Driving it

Results below are read from docstrings and key labels, not driven live (unverified).

1. In a file buffer press `x`: the current line becomes the selection (`ygg-select-line`).
2. Press `d`: the selection is deleted (`ygg-delete-dwim`); `u` undoes it (`ygg-undo`).
3. Select a word, then `m s` and a pair character: each selection is wrapped (`ygg-match-surround`).
4. `SPC :` opens the ex line in the minibuffer and runs the command typed (`ygg-ex`).
5. `M-x ygg-tutor` opens `tutor/yggdrasil-tutor.txt` in a scratch buffer; edits never touch the source file.

### Gotchas

What the code shows about modal state and special buffers.

- `ygg-undo` is whole-buffer undo; it never undoes inside a region.
- Special-mode buffers get the leader on `SPC` too, set per mode in `lisp/yggdrasil-leader.el` (magit, dired, special-mode).
- Modes outside the modal set are listed in `ygg-modal-special-modes`.

### Code

The main files, then the test files that map to them by name.

- `lisp/yggdrasil.el`
- `lisp/yggdrasil-core.el`
- `lisp/yggdrasil-motions.el`
- `lisp/yggdrasil-selection.el`
- `lisp/yggdrasil-verbs.el`
- `lisp/yggdrasil-match.el`
- `lisp/yggdrasil-actions.el`
- `lisp/yggdrasil-quickscope.el`
- `lisp/yggdrasil-rect.el`
- `lisp/yggdrasil-ex.el`

Tests:

- `test/keys-live-tests.el`
- `test/ygg-ex-tests.el`
- `test/ygg-helix-motions-tests.el`
- `test/ygg-helix-selection-tests.el`
- `test/ygg-helix-verbs-tests.el`
- `test/ygg-increment-tests.el`
- `test/ygg-insert-repeat-tests.el`
- `test/ygg-match-tests.el`
- `test/ygg-modal-consistency-tests.el`
- `test/ygg-mode-keys-tests.el`
- `test/ygg-motions-tests.el`

## Leader keys and help

The SPC leader tree (also M-SPC everywhere), per-mode localleader on backslash, a searchable key cheatsheet, and embark context actions.

### Sub-features

Leader prefixes, localleader maps, key browser, embark.

- Leader prefixes: f files, b buffers, w windows, q quit, p zones, a agents, c code, d debug, g git, o open, r repl, s search, u ui.
- Localleader: `\` opens the map registered for the current major mode.
- `SPC ?` lists every binding and describes the one picked.
- Embark acts on the things the config draws.

### How to get to it

The leader is bound in normal and visual state and as M-SPC globally.

- `SPC ?` — browse every yggdrasil binding (`ygg-keys`)
- `SPC SPC` — M-x (`execute-extended-command`)
- `SPC h` — help prefix (`help-command`)
- `SPC .` — context menu on the thing at point (`embark-act`)
- `SPC q q` — quit (`save-buffers-kill-terminal`)
- `SPC q r` — restart Emacs (`restart-emacs`)

### Driving it

Results below are read from code, not driven live (unverified).

1. Press `SPC` and wait: which-key lists the prefixes with their labels.
2. `SPC ?` opens a completing-read of every binding with its command; choosing one describes the function.
3. In a major mode with a localleader, `\` opens that mode's map, for example `\ e` in an Emacs Lisp buffer (`eval-last-sexp`).
4. `SPC .` opens embark on the thing at point.

### Gotchas

Where leader maps come from.

- `yggdrasil-leader-def` binds under `ygg-leader-map`; each prefix map such as `ygg-leader-workspace-map` is a separate keymap that a layer fills with `yggdrasil-define-keys`.
- A localleader map is found by walking `major-mode` parents, so derived modes inherit.

### Code

The main files, then the test files that map to them by name.

- `lisp/yggdrasil-leader.el`
- `lisp/yggdrasil-localleader.el`
- `lisp/ygg-embark.el`

## Spaces and the tab bar

Nestable workspaces called spaces (zones): a tree of tab-bar tabs with children and siblings, a picker, a sidebar tree, and a tab bar that is hidden until asked for.

### Sub-features

Space tree navigation, picker, sidebar, buffer scoping, tab bar visibility.

- Create child and sibling spaces, move up, down and across siblings.
- Pick any space or agent from an indented tree.
- Spaces on a folder, a host or a worktree.
- Buffer switching scoped to the current space.

### How to get to it

All zone commands live under SPC p.

- `SPC p z` — pick a zone or agent (`ygg-space-pick`)
- `SPC p Z` — pick a zone or agent, subagents too (`ygg-space-pick-everything`)
- `SPC p c` — new child space (`ygg-space-child`)
- `SPC p s` — new sibling space (`ygg-space-sibling`)
- `SPC p j` — down to the first child (`ygg-space-down`)
- `SPC p k` — up to the parent (`ygg-space-up`)
- `SPC p l` — next sibling (`ygg-space-next-sibling`)
- `SPC p d` — close the space and its subtree (`ygg-space-close`)
- `SPC p t` — tree sidebar (`ygg-space-tree`)
- `SPC TAB` — toggle to the last space (`ygg-space-toggle`)
- `SPC b b` — buffers scoped to this space (`ygg-buffer-space`)
- `SPC u T` — show or hide the space tab bar (`ygg-space-toggle-tab-bar`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC p c` creates a child of the current space and switches to it; `SPC p s` creates a sibling.
2. `SPC p z` opens an indented tree of spaces and agents; choosing a row switches to it.
3. `SPC p t` toggles the vertical sidebar; there `j`, `k`, `h`, `l` move, `a` adds a child, `r` renames, `d` closes.
4. `SPC u T` shows the tab bar for this session.
5. `SPC TAB` ping-pongs to the most recently used space.

### Gotchas

What the code shows.

- The tab bar is hidden by default; `ygg-space-tab-bar-visible` and `SPC u T` control it.
- `SPC p Z` also lists rows that `SPC p z` hides, such as subagents.
- `SPC p d` closes the whole subtree, not only the current space.

### Code

The main files, then the test files that map to them by name.

- `lisp/yggdrasil-spacetree.el`
- `lisp/layer-sessions.el`
- `lisp/ygg-tile.el`

Tests:

- `test/idle-cost-tests.el`
- `test/space-pick-agents-tests.el`
- `test/space-tab-bar-visibility-tests.el`

## Sessions and restore

Saved sessions per project and global, restored on start: save, save as, pick by project name, resume, and delete.

### Sub-features

Quick save, named save, project sessions, picker, restore on start.

- Session save under the existing name or a new one.
- Project sessions named from the project, saved and resumed from SPC p.
- Restore on start, controlled by `ygg-session-restore-on-start`.

### How to get to it

Saving lives under SPC q, project sessions under SPC p.

- `SPC q s` — save the session (`ygg-session-save`)
- `SPC q S` — save the session as a new name (`ygg-session-save-as`)
- `SPC q d` — delete a session (`easysession-delete`)
- `SPC p m` — session picker (`ygg-session-pick`)
- `SPC p w` — save the project session (`ygg-session-save-project`)
- `SPC p r` — resume the project session (`ygg-session-load-project`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC q s` saves under the current session name; `SPC q S` prompts for a name.
2. `SPC p w` saves a session named from the current project.
3. Later, `SPC p r` loads that project's session if one was saved.
4. `SPC p m` lists sessions by project name with path and save time.

### Gotchas

What the code shows.

- Whether Emacs restores a session at startup is the option `ygg-session-restore-on-start`.
- Session branching and startup behaviour have their own tests: `test/sessions-branch-tests.el`, `test/ygg-session-restore-tests.el`, `test/ygg-session-startup-tests.el`.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-sessions.el`
- `early-init.el`

Tests:

- `test/sessions-branch-tests.el`
- `test/ygg-session-restore-tests.el`
- `test/ygg-session-startup-tests.el`

## Agent sessions

Agent objects (aob): ACP agent sessions in their own spaces, each shown as a trace of operations, with modes, models, subagents, running shells and a todo list.

### Sub-features

Spawning, trace buffer, subagents, modes and models, shells, todo, transcripts, activity.

- Spawn an agent on a project with a model, resume, fork, archive, delete.
- Trace: the conversation as operations, with a queue of prompts waiting to send.
- Subagents: a delegation you can hold, list and open.
- Session modes, model, goal and worker effort per session.
- Running shell commands across agents, with a way to stop one.
- Todo list kept by you and the agent; transcripts of ended conversations.

### How to get to it

Agents live under SPC a; the trace buffer has its own keys and localleader.

- `SPC a c c` — spawn agent, project, model (`aob-acp-spawn-with`)
- `SPC a c C` — talk to an existing agent (`ygg-aob-talk-existing`)
- `SPC a c o` — go to an agent in this space (`ygg-aob-pick`)
- `SPC a c R` — resume a stored session in a folder (`ygg-aob-resume-pick`)
- `SPC a c r` — answer the first pending decision (`ygg-aob-resolve-next`)
- `SPC a c p` — running commands of all agents (`aob-shells`)
- `SPC a c q` — kill the session (`aob-kill-session`)
- `SPC a v` — conversations of the open project (`ygg-conversations`)
- `SPC p z` — pick a zone or agent (`ygg-space-pick`)
- `\ d` — todo list of the session, in a trace buffer (`aob-todo`)
- `\ l` — model of the session (`aob-acp-model`)
- `\ m` — mode of the session (`ygg-compose-transient`)
- `\ S` — cancel the running turn (`aob-cancel`)

### Driving it

Results below are read from docstrings and key labels, not driven live (unverified).

1. `SPC a c c` asks for agent, project and model and spawns the session with a first prompt; its trace opens in a space.
2. In the trace, `a`, `i` or `o` opens a compose buffer (in the plan and subagents lists `\ p` does); `c` steers the turn already running (`aob-steer`).
3. `RET` on a pending question or decision answers it (`aob-trace-answer`).
4. `SPC a c o` flashes labels over the agents in this space and jumps to the chosen one.
5. `\ $` lists every command the agents have running (`aob-shells`); `\ k` kills this session (`aob-kill-session`).

### Gotchas

What the code shows.

- A queued prompt can be edited, moved earlier or later, sent now or dropped from its line in the trace (`aob-trace-queue-edit`, `aob-trace-queue-earlier`, `aob-trace-queue-later`, `aob-trace-queue-send-now`, `aob-trace-queue-drop`).
- Finished subagents in the space sidebar are governed by `ygg-aob-finished-subagents-shown` and `ygg-aob-tree-toggle-finished` (unverified).

### Code

The main files, then the test files that map to them by name.

- `lisp/agent-objects/aob.el`
- `lisp/agent-objects/aob-acp.el`
- `lisp/agent-objects/aob-trace.el`
- `lisp/agent-objects/aob-trace-vui.el`
- `lisp/agent-objects/aob-workflow.el`
- `lisp/layer-aob.el`
- `lisp/aob-subagent.el`
- `lisp/aob-shells.el`
- `lisp/aob-todo-view.el`
- `lisp/aob-transcript.el`
- `lisp/ygg-todo.el`

Tests:

- `lisp/agent-objects/aob-tests.el`
- `test/aob-acp-conformance-tests.el`
- `test/aob-acp-session-conformance-tests.el`
- `test/aob-compact-reserve-tests.el`
- `test/aob-dedupe-tests.el`
- `test/aob-native-cancel-tests.el`
- `test/aob-question-prompt-tests.el`
- `test/aob-reject-reason-tests.el`
- `test/aob-steer-outcomes-tests.el`

## Talking to agents

Prompting beyond one reply: answer an agent's questions as a form, side questions, shell lines in a draft, context, delivery targets, scheduled prompts, handoffs and diagnostics pushed to the agent.

### Sub-features

Answers, btw, bang lines, context, deliver, schedule, handoff, diag push.

- Answer: the questions of the last reply as a form.
- Btw: a side question whose answer never enters the conversation.
- Bang: shell lines in a compose draft.
- Context: files and regions the agent is given, kept visible.
- Deliver: where an answer lands besides the trace.
- Schedule: prompts sent later, once or on a repeat.
- Handoff: a fresh session that starts where another left off.
- Diag push: tell the agent what its edits broke.

### How to get to it

Context, delivery and scheduling hang under SPC a c; per-trace actions use the localleader.

- `SPC a c x` — add region or file to the context (`aob-context-add`)
- `SPC a c X` — list the context (`aob-context-list`)
- `SPC a c s` — schedule a prompt (`aob-schedule`)
- `SPC a c S` — list schedules (`aob-schedule-list`)
- `SPC a e` — say where the next answer goes (`aob-deliver-to`)
- `SPC a c W` — ask, answer goes elsewhere (`aob-ask-to`)
- `\ A` — answer the last reply's questions (`aob-answer`)
- `\ H` — hand off to a fresh session (`aob-handoff`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. Select code, `SPC a c x` adds it to the agent's context; `SPC a c X` shows what the agent holds.
2. `SPC a c s` takes a prompt and a moment or repeat and sends it to the session then.
3. `\ A` in a trace opens the questions of the last reply in the compose box.
4. `\ H` writes the next task for a fresh session that takes over from this one.

### Gotchas

What the code shows.

- `aob-btw` answers never enter the conversation of the source session.
- `aob-handoff` starts a new session; it does not continue the old process.

### Code

The main files, then the test files that map to them by name.

- `lisp/aob-answer.el`
- `lisp/aob-bang.el`
- `lisp/aob-btw.el`
- `lisp/aob-context.el`
- `lisp/aob-deliver.el`
- `lisp/aob-schedule.el`
- `lisp/aob-handoff.el`
- `lisp/aob-diag-push.el`

Tests:

- `test/aob-answer-tests.el`
- `test/aob-bang-tests.el`
- `test/aob-btw-tests.el`
- `test/aob-diag-push-tests.el`
- `test/aob-handoff-tests.el`
- `test/aob-schedule-tests.el`

## Agent skills and presets

The skills agents get (installed from the repo skills directory), a searchable index of every reachable skill, per-project agent config homes, and presets for words you retype.

### Sub-features

Skill install and uninstall, skill index, config homes, presets.

- Install every skill under `ygg-agent-skills-root` into the agent CLIs.
- Searchable index of every skill the agents can reach.
- Per-project agent config homes, refreshed and logged in separately.
- Presets: named prompts kept per project, per user or in the config.

### How to get to it

Skills and presets sit under SPC a.

- `SPC a m` — repo map now, and a feature map agent session (`ygg-agent-maps-generate`)
- `SPC a s` — install skills (`ygg-agent-skill-install`)
- `SPC a S` — uninstall a skill (`ygg-agent-skill-uninstall`)
- `SPC a U` — new preset (`ygg-preset-new`)
- `SPC a u` — edit a preset (`ygg-preset-edit`)
- `M-x ygg-agent-refresh-config-homes` — redo the share bootstrap for every project home

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC a s` installs the skills found under `skills/` into the agent CLIs.
2. `SPC a U` asks for a name and a scope (project, yours, or the config's) and opens the preset's file.
3. `SPC a u` opens the file of an existing preset.
4. `SPC a S` removes one skill by name.

### Gotchas

What the code shows.

- A preset's scope decides which file it lives in; `ygg-preset-migrate` moves presets out of the old homes.
- `ygg-agent-skill-core` names the skills kept in the model-visible listing; the others are written as name-only (unverified).

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-agent-skills.el`
- `lisp/ygg-skill-index.el`
- `lisp/ygg-agent-conf.el`
- `lisp/ygg-preset.el`
- `skills/create-verification-skill/SKILL.md`
- `skills/spec-mikado/mikado.py`
- `skills/tcr/scripts/tcr.sh`

Tests:

- `test/build-skill-tests.el`
- `test/lead-lazy-tests.el`
- `test/plugin-mcp-adopt-tests.el`
- `test/regrade-qa-tests.el`
- `test/regression-skill-tests.el`
- `test/skills-description-tests.el`
- `test/ygg-skill-index-tests.el`

## Agent MCP sidecar

An MCP server that lives beside Emacs: a headless Emacs that serves tools to agents, and the questions it puts back to the editing Emacs.

### Sub-features

Sidecar host, MCP tools, server refresh.

- The host starts the headless Emacs and hands it to the agents.
- The tools are the questions the sidecar asks of the editing Emacs.
- Per session: list MCP servers, refresh, restart.

### How to get to it

The sidecar starts on its own for agents; the controls are commands.

- `M-x aob-mcp-host-start` — start the headless Emacs that serves MCP (`aob-mcp-host-start`)
- `M-x aob-acp-mcp-refresh` — ask every MCP server again (`aob-acp-mcp-refresh`)
- `M-x aob-acp-mcp-restart` — reload this conversation into a process that gets the servers (`aob-acp-mcp-restart`)
- `\ c` — MCP servers this session can reach (`aob-acp-mcp`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `\ c` in a trace lists every MCP server the session can reach and whether it was given.
2. After changing servers, `M-x aob-acp-mcp-refresh` asks each again.
3. If a session never got a server, `M-x aob-acp-mcp-restart` reloads the conversation into a new process.

### Gotchas

What the code shows.

- The sidecar is a separate headless Emacs; the running editing Emacs answers its questions through `lisp/aob-mcp-tools.el`.

### Code

The main files, then the test files that map to them by name.

- `lisp/aob-mcp.el`
- `lisp/aob-mcp-host.el`
- `lisp/aob-mcp-tools.el`
- `etc/pi/aob-pi-mcp.js`
- `etc/pi/aob-pi`
- `lisp/ygg-pi.el`

Tests:

- `test/aob-mcp-review-submit-tests.el`
- `test/ygg-pi-harness-tests.el`
- `test/aob-steal-small-tests.el`
- `test/aob-trim-tests.el`

## Git status and worktrees

Magit as the git front end, with worktrees that open as spaces, a staged-line selection mode, inline blame, merge-conflict keys, and difftastic diff.

### Sub-features

Magit status and menus, worktree commands, conflicts, blame, diff.

- Magit status, log, branch, commit, push and pull under SPC g.
- Worktrees: switch, list, merge, remove, run at point.
- Merge-conflict resolution with smerge keys.
- Inline blame toggle and difftastic diff.

### How to get to it

Everything git sits under SPC g.

- `SPC g g` — magit status (`magit-status`)
- `SPC g ?` — magit dispatch menu (`magit-dispatch`)
- `SPC g c` — commit (`magit-commit`)
- `SPC g d` — diff with difftastic (`ygg-git-diff`)
- `SPC g w w` — switch or create a worktree (`ygg-wt-switch`)
- `SPC g w r` — run a branch, commit or PR in a worktree (`ygg-git-worktree-run`)
- `SPC g w x` — remove the worktree, its space and buffers (`ygg-git-worktree-remove`)
- `SPC g W` — worktree status (`ygg-git-worktree-status`)
- `SPC g x o` — keep ours in a conflict (`smerge-keep-upper`)
- `SPC u b` — inline blame (`ygg-inline-blame-mode`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC g g` opens magit status for the project.
2. `SPC g w w` asks for a worktree, creating its branch if new, and opens it in magit.
3. `SPC g w r` checks the chosen spec out in a worktree and opens it as a space, offering a task.
4. `SPC g x n` jumps to the next conflict; `SPC g x o` keeps ours.

### Gotchas

What the code shows.

- Magit buffers take the leader on SPC and have their own `j`/`k` section movement.
- `SPC g w x` removes the worktree, its space and its buffers together.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-git.el`
- `lisp/ygg-git.el`
- `lisp/ygg-git-worktree.el`

Tests:

- `test/git-worktree-tests.el`
- `test/ygg-git-worktree-tests.el`

## Pull requests in status

Open pull and merge requests of the repository, listed in magit status in three groups (review requested, mine, open; capped by `ygg-git-review-requests-limit`), from the forge, cached on disk and fetched in the background.

### Sub-features

Listing in magit status, open, browse, copy URL, refetch.

- The list is a magit status section controlled by `ygg-git-review-requests`; heading "Pull requests (N)", "Merge requests (N)" on GitLab; empty groups are hidden and drafts are marked.
- A forge failure shows the first meaningful line of the tool's own stderr (for example an unresolvable host).
- Open a request as a compare; open in the browser; copy its URL.
- Merge a request from its row or its compare: the forge is asked in the background for the request and the repository's settings (re-read up to three times while GitHub is still computing mergeability), you pick the target (the request's base, the default branch or another offered branch, retargeted first), a method the repository allows (GitLab fast-forward and semi-linear projects show as such), whether to auto-merge when the checks or pipeline pass (default yes while they run, no otherwise) and whether to delete the remote branch, then one question confirms with the pinned head commit, checks, review, mergeable state and any blocked state. The reviewed head is pinned (`--match-head-commit` / `--sha`), GitLab always gets `--auto-merge` explicitly, GitHub gets `--auto`, and a branch is deleted through the API only after an immediate GitHub merge (auto-merge leaves it to the repository setting). Drafts, conflicts and closed requests are refused, a second merge of the same request is refused while one runs, the forge's own words show on failure (tokens scrubbed), and the result says merged, auto-merge enabled or queued before the list is fetched again.

### How to get to it

The section appears in magit status; from it the keys act on the request row.

- `SPC g g` — magit status, where the section lists the requests (`magit-status`)
- `SPC g r` — review a branch's PR or MR directly (`ygg-git-compare-review-branch`)
- `RET` — open the request on the row (`ygg-git-review-requests-open`)
- `o` — open it in the browser (`ygg-git-review-requests-browse`)
- `y` — copy its URL (`ygg-git-review-requests-copy-url`)
- `r` — fetch again (`ygg-git-review-requests-refetch`)
- `m` — merge the request on the row (`ygg-git-pr-merge`)

### Driving it

Results below are read from code, not driven live (unverified).

1. `SPC g g`, find the pull-requests section.
2. `RET` on a row opens that request as a compare review.
3. `r` refetches from the forge.

### Gotchas

What the code shows.

- Forge calls run in the background and are cached on disk (recent commit "compare: forge calls run in the background, cached on disk").

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-git-review-requests.el`

Tests:

- `test/ygg-git-review-requests-tests.el`

## Stacked pull requests

Branches that build on one another, shown bottom to top in magit status, restacked and pushed with one key, native to git, magit and the forge calls already here (no git-spice, git-town or Graphite).

### Sub-features

Stack section, restack, branch on top, retarget after a merge, compare against the parent.

- A branch's parent is `branch.X.ygg-parent` in git config, else the base of its pull request from the cached request list; the list also feeds the review and CI state (`reviewDecision` and the check rollup, GitHub only).
- The section "Stack (N)" sits after Worktrees and before the requests, controlled by `ygg-git-stack`; each row shows the branch, `#number`, draft, approved, changes or review, CI ok, failed or running, `+N on parent` and `behind M`. It is hidden for one branch alone and on the default branch.
- The stack is the current branch's parents up to the default branch plus the children above it; a fork follows the first child by name.
- Restack walks the stack from the first branch that no longer contains its parent and rebases the rest onto that parent with `--update-refs`, in the background, the old base being the reflog fork point. Only branches whose tip moved, and that the remote already has, are listed in one question and pushed with `--force-with-lease=<ref>:<expected sha>`; the default branch is never pushed.
- A dirty worktree or a rebase in progress refuses the restack; a conflict stops it, opens magit status on the rebase, and the push question comes when the rebase is over, found on the next refresh, whether it was continued or aborted.
- Make a branch on top of the current one with `magit-branch-and-checkout`, recording `branch.<name>.ygg-parent`.
- After a merge from `ygg-git-pr-merge`, or when a request the section saw open is gone and the forge says it was merged, one question asks "Retarget #N to <target> and restack?": it edits the request's base through the merge command's retarget step, fetches, sets the child's parent to the target and restacks the stack above the merged branch onto it.
- A compare of a stacked branch's request diffs against its recorded parent rather than the default branch (`ygg-git-compare--parent-base`).
- v1 does not open pull requests.

### How to get to it

The section appears in magit status; the keys act on the section or a branch row.

- `R` — restack the stack (`ygg-git-stack-restack`)
- `b` — new branch on top of the current one (`ygg-git-stack-branch`)
- `RET` — check out the branch on the row (`ygg-git-stack-visit`)
- `m` — merge the request of the branch on the row (`ygg-git-stack-merge`)

### Driving it

Results below are read from code and driven only in temp repositories (unverified live).

1. `SPC g g`, find "Stack".
2. `b` on a stack row, name the branch; it is made on top and its parent recorded.
3. Amend a lower branch, `R`; the stack is rebased and one question lists the force-pushes.
4. After the bottom request is merged, answer the retarget question.

### Gotchas

What the code shows.

- A branch with no remote counterpart is rebased but never pushed.
- The lower branch you amended is not pushed by the restack unless the rebase moved it.
- Merge detection of a vanished request only knows requests the section saw open in this session.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-git-stack.el`

Tests:

- `test/ygg-git-stack-tests.el`

## Compare review

Compare any two sides (worktrees, branches, commits, pull requests) read-only in the whole frame, with line, hunk and file comments, review marks, interdiff, and an explain action.

### Sub-features

Compare buffer, comments, marks, interdiff, explain, dispatch menu.

- Compare A with B; swap sides, switch base, toggle A...B and A..B, log of each side.
- Comments on a line, lines, hunk, file or the whole review; edit, append, delete, accept an agent's proposal.
- Marks: check files and hunks off as reviewed; jump to the next unreviewed.
- Interdiff: what B changed since it was last seen.
- Explain: ask a new agent session to explain the change.
- Guided tour: an agent orders the change into steps by behaviour; walk them one at a time, the rest folded, each step left marked reviewed, the hunks no step covers last.
- Dispatch transient bound to `;` and `?`.

### How to get to it

Open from git, then work in the compare buffer; the second group are compare-buffer keys.

- `SPC g C` — compare two sides (`ygg-git-compare`)
- `SPC g r` — review a branch's PR or MR (`ygg-git-compare-review-branch`)
- `= =` — compare at point in a magit buffer (`ygg-git-compare-at-point`)
- `= r` — review the branch's PR or MR from a magit buffer (`ygg-git-compare-review-branch`)
- `c` — comment on the line or selected lines (`ygg-git-compare-comment`)
- `C` — comment on the file (`ygg-git-compare-comment-file`)
- `r` — mark the file reviewed (`ygg-git-compare-mark-file-reviewed`)
- `R` — mark the hunk reviewed (`ygg-git-compare-mark-hunk-reviewed`)
- `] u` — next unreviewed hunk (`ygg-git-compare-next-unreviewed`)
- `I` — interdiff since last review (`ygg-git-compare-interdiff`)
- `t` — walk the branch's guided tour from step 1, or ask an agent to order one when there is none, a step went stale or the tour is already being walked (`ygg-git-compare-tour`)
- `] t` — next tour step, marking the step left reviewed (`ygg-git-compare-tour-next`)
- `[ t` — previous tour step (`ygg-git-compare-tour-previous`)
- `T` — leave the tour, hunks folded to their files again (`ygg-git-compare-tour-leave`)
- `;` — dispatch menu (`ygg-git-compare-dispatch`)
- `@` — send the review to an agent (`ygg-git-compare-review`)
- `P` — merge this compare's pull or merge request, choosing target, method and branch deletion (`ygg-git-pr-merge`)
- `~` — swap sides (`ygg-git-compare-swap`)
- `q` — leave the compare (`ygg-git-compare-quit`)

### Driving it

Results below are read from docstrings and key maps, not driven live (unverified).

1. `SPC g C` asks for side A and side B and opens their diff in the whole frame.
2. `] c` and `] f` step hunks and files; `] m` steps comments.
3. `c` on a line opens a draft; `C-c C-c` saves it and `C-c C-k` cancels (`ygg-git-compare-draft-save`, `ygg-git-compare-draft-cancel`).
4. `r` marks the file reviewed; `] u` jumps to the next unreviewed hunk.
5. `;` opens the dispatch menu listing every comment, mark, send and view action.
6. `q` leaves, bringing back the windows from before.
7. `t` asks an agent to order the change; it answers through the `review_tour` MCP tool, kept per branch beside its review in the common git directory. `] t` and `[ t` walk its steps, the header showing step, title, risk and check; the last step is the hunks no step covers.

### Gotchas

What the code shows.

- A compare is read-only: `s`, `S`, `u`, `U` refuse with "stage, discard and apply from magit status" (`ygg-git-compare-read-only`).
- `SPC g w c` is the same compare opened from the worktree menu; the `g w` worktree map exists only when `wt` is on the path, otherwise `SPC g w` is plain `magit-worktree` (`lisp/layer-git.el`).
- A tour follows a pushed branch by each hunk's mark key: a step that lost a hunk is stale and its hunks go to the last step until `t` asks again; steps carry notes only, comments still come through `review_submit`.
- A compare buffer's keys are the bare keys in `ygg-git-compare-mode-map`, not leader keys.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-git-compare.el`
- `lisp/ygg-git-compare-comments.el`
- `lisp/ygg-git-compare-marks.el`
- `lisp/ygg-git-compare-interdiff.el`
- `lisp/ygg-git-compare-explain.el`
- `lisp/ygg-git-compare-tour.el`

Tests:

- `test/ygg-git-compare-async-tests.el`
- `test/ygg-git-compare-comments-tests.el`
- `test/ygg-git-compare-explain-tests.el`
- `test/ygg-git-compare-glab-host-tests.el`
- `test/ygg-git-compare-interdiff-tests.el`
- `test/ygg-git-compare-marks-tests.el`
- `test/ygg-git-compare-tests.el`
- `test/ygg-git-compare-tour-tests.el`

## Review threads and submit

The forge's comments on a pull request shown inside its compare, and sending your review back: comment, approve, request changes, draft, to an agent, or as markdown.

### Sub-features

Thread display and replies, submit transient, markdown export.

- Threads: open, copy, reply, fold, hide resolved.
- Submit transient: comment, approve, request changes, draft, to an agent.
- Export the review as markdown to the kill ring, a buffer or a file.

### How to get to it

Both are reached from a compare buffer.

- `&` — submit transient (`ygg-git-compare-submit`)
- `y` — copy the review as markdown (`ygg-git-compare-export-markdown`)
- `; H` — hide or show resolved threads (`ygg-git-compare-threads-toggle-resolved`)
- `r` — reply to the thread on this line, by `ygg-git-compare-threads-reply`
- `TAB` — fold the thread, by `ygg-git-compare-threads-toggle-fold`

### Driving it

Results below are read from code, not driven live (unverified).

1. Open a pull request compare with `SPC g r`; the forge's threads appear under the lines they anchor to.
2. `r` on a thread replies; `TAB` folds it.
3. After commenting, `&` opens the submit menu: `c` comment, `a` approve, `r` request changes, `d` draft, `@` to an agent.

### Gotchas

What the code shows.

- The thread keys `o`, `y`, `r`, `TAB` act on thread rows, not on diff lines.
- The submit transient has a `-p` argument; what it sets is not read here (unverified).

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-git-compare-threads.el`
- `lisp/ygg-git-compare-submit.el`

Tests:

- `test/ygg-git-compare-post-tests.el`
- `test/ygg-git-compare-submit-tests.el`
- `test/ygg-git-compare-threads-tests.el`

## Review file

A compare review as one markdown file: front matter for the review, the diff quoted with "> ", and every comment or thread as a fenced div under the line it anchors to.

### Sub-features

Printer, parser, live-diff check.

- Printer and parser between a review's comment and thread plists and the markdown file.
- Long context runs far from any comment print as a snip.
- A live-diff check aligns the file to the current diff by file and hunk.

### How to get to it

Reached from the compare submit menu export entries.

- `&` — submit menu, with the export entries (`ygg-git-compare-submit`)
- `y` — copy the review as markdown (`ygg-git-compare-export-markdown`)

### Driving it

Results below are read from code, not driven live (unverified).

1. In a compare with comments, `&` then `w` writes the review to a file (`ygg-git-compare-export-markdown-file`).
2. `b` in the same menu shows it in a buffer (`ygg-git-compare-export-markdown-buffer`).

### Gotchas

What the code shows.

- `ygg-review-file-snip-threshold` and `ygg-review-file-snip-margin` decide which context runs are shortened.
- `ygg-review-file--bad` signals an error for a malformed file.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-review-file.el`

Tests:

- `test/ygg-review-file-tests.el`

## Projects and project setup

A projects sidebar of every repo and its agent conversations, repo scanning, import, add and forget, umbrella child repos, and installing the CLIs a project's files declare.

### Sub-features

Sidebar, scanning, import, folders, setup, commands.

- Sidebar: sessions, schedules, context rows, pin, archive, resume.
- Scan every repo on disk, not just the opened ones.
- Add or drop folders a project's agents are given.
- Install the toolchain the project's own files declare.

### How to get to it

Project commands are under SPC p; the sidebar under SPC a.

- `SPC a d` — projects sidebar (`ygg-projects-sidebar`)
- `SPC p p` — switch project (`project-switch-project`)
- `SPC p u` — switch repo in this umbrella (`ygg-project-switch-child`)
- `SPC p i` — import or re-import a project (`ygg-project-import`)
- `SPC p P` — add a project to the sidebar (`ygg-projects-add`)
- `SPC p f` — add a folder to this project (`ygg-project-add-folder`)
- `SPC p F` — drop a folder (`ygg-project-remove-folder`)
- `SPC p T` — install the project's toolchain (`ygg-project-setup`)
- `SPC p D` — forget a project (`ygg-project-remove`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC a d` toggles the sidebar; `j`/`k` move, `RET` visits, `TAB` folds a project.
2. `SPC p P` imports a directory and lists it in the sidebar.
3. `SPC p T` installs the toolchain the project's files declare.
4. `SPC p u` opens another repository of the umbrella you are in.

### Gotchas

What the code shows.

- `ygg-project-setup-never-file` records tools never to install (unverified).
- Zone commands share the same SPC p prefix, so project keys and space keys sit together.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-projects.el`
- `lisp/ygg-project-scan.el`
- `lisp/ygg-project-setup.el`
- `lisp/ygg-project-commands.el`

Tests:

- `test/ygg-project-setup-tests.el`
- `test/ygg-project-umbrella-tests.el`
- `test/ygg-projects-tests.el`

## ICE and lat.md

Intent, context, expectation tooling: lat.md and OpenSpec changes, ADRs, glossary, C4 views, checks, and the maintain pass that keeps the feature map honest.

### Sub-features

Changes list, task views, connections, lat search, checks, wiring, maintain.

- List open OpenSpec changes with task progress; tasks of one change.
- Connections of a lat section: what it links to and what links to it.
- Check a change's intent, expectations and plan.
- Wire a repository for ICE; preview the C4 views.
- Maintain: keep the verification skill and feature map honest.

### How to get to it

ICE has a leader submenu under SPC a k built at load time (not listed by the static facts), and every other command is an M-x away.

- `M-x ygg-ice-changes-list` — open OpenSpec changes with task progress (`ygg-ice-changes-list`)
- `M-x ygg-ice-lat-search` — search lat.md and jump to a section (`ygg-ice-lat-search`)
- `M-x ygg-ice-connections` — links of a section (`ygg-ice-connections`)
- `M-x ygg-ice-confirm-intent` — confirm a change's restated intent (`ygg-ice-confirm-intent`)
- `M-x ygg-ice-approve-checkpoints` — approve a change's checkpoints (`ygg-ice-approve-checkpoints`)
- `M-x ygg-ice-c4-preview` — preview the C4 views (`ygg-ice-c4-preview`)
- `M-x ygg-ice-maintain` — keep the verification skill and map honest (`ygg-ice-maintain`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `M-x ygg-ice-changes-list` shows the changes; in the view `RET` visits the row, `t` shows its tasks, `c` adds the row to context, `Q` sends it to quickfix, `g r` refreshes.
2. `M-x ygg-ice-lat-search` asks a query and jumps to the section picked.
3. `M-x ygg-ice-maintain` runs the maintain pass on a root; `ygg-ice-maintain-daily` runs it once a day when set.

### Gotchas

What the code shows.

- `ygg-ice-maintain-daily` arms an idle timer only when projects are named and Emacs is not in batch mode.
- The scripts under `etc/ice/` are run through the `ygg-ice-*-script` options.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-ice.el`
- `etc/ice/ice-check`
- `etc/ice/ice-lat-drift`
- `etc/ice/ice-wire.sh`
- `skills/maintain-verification-skill/SKILL.md`
- `etc/ice/ice-archive-to-lat`
- `etc/ice/ice-c4-drift`
- `etc/ice/ice-compact`
- `etc/ice/lat-init-agents.py`

Tests:

- `test/ice-layer-tests.el`

## ICE gates and hooks

Command-line gates that keep an ICE change honest: the locked checks, the fail-on-base proof, scenario coverage, the one-command verify, the commit gate and its git hook, the runner detector, and the archive tools.

### Sub-features

Lock, scenarios, fail-on-base, coverage, verify, commit gate, hook install, runner, archive and compact, C4 drift, agent init.

- Lock a change's checks and detect any later change to them.
- List scenario ids and the tests that name them; require each to have a passing test.
- Prove every check fails on the locked base and passes on the working tree.
- Verify a change in one command and record the verdict in a ledger.
- Refuse a commit that touches a locked change without a verified ledger row.
- File archived changes into lat.md, drop the old ones, and report C4 code paths that vanished.

### How to get to it

These are command-line tools under `etc/ice/`, run from the repo root; ice-wire copies them into the .ice directory of a wired repo.

- `etc/ice/ice-lock CHANGE lock` — record the locked checks; `verify` compares them later
- `etc/ice/ice-scenarios CHANGE --tests` — scenario ids, each with the tests that name it
- `etc/ice/ice-fail-on-base CHANGE` — every check fails on the locked base and passes on the working tree
- `etc/ice/ice-coverage CHANGE` — every scenario id names a test that passed in the report
- `etc/ice/ice-verify CHANGE` — the lead's VERIFY steps in order, one verdict line
- `etc/ice/ice-commit-gate` — the pre-commit check for a locked change
- `etc/ice/ice-hook-install ROOT MARKER GATE_LINE` — install or upgrade the pre-commit hook
- `etc/ice/ice-runner detect ROOT` — print the test runner the repo uses
- `etc/ice/ice-compact ROOT` — file archived changes into lat.md; `--apply` removes the old ones
- `etc/ice/ice-c4-drift` — report C4 `code` paths that no longer exist

### Driving it

Results below are read from the scripts' help text and source, not driven live (unverified).

1. `etc/ice/ice-lock CHANGE lock` writes the lock record under the wired repo's .ice/locks and the git tag ice-expect/CHANGE; `etc/ice/ice-lock CHANGE verify` exits 1 naming every locked path that changed.
2. `etc/ice/ice-verify CHANGE` runs intent, plan, lock, base, unit, coverage and any configured sec, live, perf and mutate steps, and stops at the first that fails; it prints `verdict: unit-verified`, `live-verified`, `failed STEP` or `blocked STEP` and appends a row to the ledger in the wired repo's .ice directory.
3. `etc/ice/ice-verify --status CHANGE` exits 0 only when the newest ledger row for the branch matches the content hash of the working tree.
4. `etc/ice/ice-runner detect ROOT` prints the runner it would write into the .ice config of the wired repo, and `etc/ice/ice-runner baseline ROOT` records one run of the whole suite.
5. `etc/ice/ice-compact ROOT` files every archived change with `etc/ice/ice-archive-to-lat`; with `--apply` it removes folders older than `--older-than` days whose lat.md section exists.
6. `etc/ice/ice-c4-drift` exports the likec4 model and prints each element whose `code` path is gone, exiting 1.

### Gotchas

What the code shows.

- The commit gate never runs tests: it only reads the ledger in .ice and hashes the staged tree.
- `etc/ice/ice-hook-install` never overwrites a foreign hook; it chains it as `pre-commit.legacy`.
- Only the owner runs `ice-lock`; moving a lock needs `--force` and `ICE_LOCK_OWNER=1`.
- The commit gate also refuses a staged `lat.md/lat.md` that holds a line with the `<!-- ice-repo-map:generated -->` marker, and any staged `lat.md/repo-map.md`, even force-added; the `latgen` git filter strips the marker line on `git add`.
- A tampered `.ice` copy of `ice-verify` can skip its own steps, so the judge runs the copy in this checkout.
- `etc/ice/lat-init-agents.py` drives the interactive menu of `lat init` for the named agents, so wiring never needs a person at the prompt.

### Code

The main files, then the test files that map to them by name.

- `etc/ice/ice-lock`
- `etc/ice/ice-scenarios`
- `etc/ice/ice-fail-on-base`
- `etc/ice/ice-coverage`
- `etc/ice/ice-verify`
- `etc/ice/ice-commit-gate`
- `etc/ice/ice-hook-install`
- `etc/ice/ice-runner`
- `etc/ice/ice-compact`
- `etc/ice/ice-archive-to-lat`
- `etc/ice/ice-c4-drift`
- `etc/ice/lat-init-agents.py`

Tests:

- `test/ice-commit-gate-tests.el`
- `test/ice-runner-stacks-tests.el`
- `test/ice-verify-sec-tests.el`

## Repo map and feature map

Two generated maps for agents: the ranked repo map of files and symbols, and the feature map of user-visible features with a condensed summary injected into a fresh chat.

### Sub-features

Repo map, feature facts, feature summary.

- `ice-repo-map` ranks symbols by references and prints the top ones within a token budget.
- `ice-feature-facts` extracts entry points, tests and candidate feature clusters for any repo.
- `ice-feature-summary` prints one line per feature from lat.md/features.md within a budget.

### How to get to it

These are command-line tools, run from the repo root.

- `SPC a m` — regenerate the repo map now and start an agent session that writes or refreshes `lat.md/features.md` with create-verification-skill or maintain-verification-skill; the session is skipped while another agent is live in the project, and the filter install is asked first when `latgen` is missing (`ygg-agent-maps-generate`)
- `M-x ygg-ice-lat-search` — search the map from Emacs (`ygg-ice-lat-search`)

### Driving it

Results below are read from the scripts' help text (unverified).

1. `etc/ice/ice-repo-map --budget 1000` prints the condensed repo map.
2. `etc/ice/ice-feature-facts --keys "SPC p"` lists the key triggers under a prefix, with command and label.
3. `etc/ice/ice-feature-summary` prints the condensed feature summary.

### Gotchas

What the code shows.

- `ice-repo-map --write` writes `lat.md/repo-map.md` and lists it in `lat.md/lat.md`; until then the root index leaves it out, because lat check fails on a listed file that does not exist.
- The written map is generated and gitignored; its index line in `lat.md/lat.md` carries the `<!-- ice-repo-map:generated -->` marker, which the `latgen` git filter (set up by ice-wire) drops from every committed copy, matching the exact marker only.
- Both tools skip vendored and environment directories (`node_modules`, `vendor`, `target`, `build`, `dist`, `.venv`, `venv`, `site-packages`, `.direnv`, `.dart_tool`, `Pods`) even when untracked and not ignored.
- Comments and string literals are blanked before routes and commands are extracted, so commented-out routes never show up.
- Entry-point extractors turn on only when their marker file is present; unknown ecosystems still get files, ranks, tests and clusters.

### Code

The main files, then the test files that map to them by name.

- `etc/ice/ice-repo-map`
- `etc/ice/ice-core.mjs`
- `etc/ice/ice-latgen-filter`
- `lisp/ygg-agent-maps.el`
- `etc/ice/ice-feature-facts`
- `etc/ice/ice-feature-summary`
- `test/ice-repo-map-tests.el`

Tests:

- `test/ice-feature-map-tests.el`
- `test/ygg-agent-maps-tests.el`

## Markdown

Markdown buffers with fenced code rendered inline by language, diagrams and math drawn below their source, clipboard images pasted as files, and show-me plans in `.aob/plans` answered in place.

### Sub-features

Inline fences, diagrams and math, image paste, plan mode.

- Fenced code blocks shown inline by language.
- Diagrams and math as transient images below their source.
- Paste a clipboard image into a markdown or org file.
- Plans open folded to their level-1 claims; decisions are picked with RET, claims struck or commented, and one key sends the response to the agent.

### How to get to it

These are localleader keys in markdown buffers.

- `\ f` — raw code fences on or off (`ygg-markdown-fences-toggle`)
- `\ m` — diagrams and math (`ygg-diagram-toggle`)
- `\ p` — paste a clipboard image (`ygg-md-paste-image`)

In a plan (`ygg-plan-mode`, any `.aob/plans/*.md`) these apply instead, with `\ f` and `\ m` kept.

- `\ s` — send the response to the agent session (`ygg-plan-send`)
- `\ y` — copy the response (`ygg-plan-copy`)
- `\ x` — strike or restore the claim at point (`ygg-plan-strike`)
- `\ c` — comment on the claim at point (`ygg-plan-comment`)
- `\ C` — drop the claim's comments (`ygg-plan-clear-comments`)
- `\ a` — take the suggested option (`ygg-plan-accept`)
- `\ A` — take every suggestion not yet picked (`ygg-plan-accept-all`)
- `\ u` — take a pick back (`ygg-plan-clear-pick`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. In a markdown file `\ f` shows the fence lines as written, or the inline look again.
2. `\ m` draws the diagram or math images below their sources.
3. `\ p` saves the clipboard image next to the file and inserts the link.
4. In a plan, RET on `b) …` shows a check on it, then `\ s` sends the decisions, strikes and comments to the agent whose directory holds the plan, or asks which.

### Gotchas

What the code shows.

- `ygg-diagram-toggle-at-point` and `ygg-diagram-toggle-image-at-point` draw one fence or image, not the whole buffer.
- `ygg-diagram-magit-tab` makes TAB draw the fence at point in magit buffers.
- In a plan Tab draws the mermaid fence at point, else opens the claim a level at a time (`ygg-plan-tab`); Shift-Tab folds the whole plan a level deeper (`ygg-plan-cycle`); Return picks the option on the line, or the suggested one on the pick line (`ygg-plan-ret`).
- Picks, strikes, comments and opened decisions live in `<slug>.answers.eld` beside the plan, never in the plan text.
- A decision never opened is sent as `not opened; default kept`, which is not agreement.
- `src="path" lines="a-b"` fences show the file range as an overlay only when their body is empty.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-markdown.el`
- `lisp/ygg-markdown-fences.el`
- `lisp/ygg-diagram.el`
- `lisp/ygg-md-image.el`
- `lisp/ygg-plan.el`

Tests:

- `test/md-image-tests.el`
- `test/ygg-markdown-fences-tests.el`
- `test/ygg-plan-tests.el`

## Notebooks and data

Jupyter kernels with percent cells, Rmd and Quarto chunks, a REPL, a kernel picker, a variables pane, VisiData, R tooling and database drawer.

### Sub-features

Evaluation, REPL, kernel choice, variables, data viewing, databases.

- Evaluate cells, buffers, defuns, expressions and files; inline results.
- Choose or restart the jupyter kernel session.
- Variables pane; view a kernel object in VisiData.
- R: tree-sitter mode and ark's LSP and DAP from a live kernel.
- Databases through usql: connections, a drawer and query buffers.

### How to get to it

Notebook keys live under SPC r.

- `SPC r r` — REPL, or back to the buffer (`ygg-nb-repl`)
- `SPC r x` — evaluate cell or selection (`ygg-nb-eval-cell`)
- `SPC r n` — evaluate the cell and go to the next (`ygg-nb-eval-cell-and-next`)
- `SPC r b` — evaluate all cells (`ygg-nb-eval-buffer`)
- `SPC r k` — choose the kernel session (`ygg-kernel-picker`)
- `SPC r K` — restart the kernel (`ygg-nb-restart-kernel`)
- `SPC r v` — variables pane (`ygg-kernel-vars-toggle`)
- `SPC r t` — view data in VisiData (`ygg-visidata-view`)
- `SPC o s` — database drawer (`ygg-db-drawer-toggle`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC r k` picks the jupyter session this buffer runs in, starting one if new.
2. `SPC r x` evaluates the cell at point; `SPC r n` does so and moves on.
3. `SPC r v` shows the kernel's variables; `SPC r t` opens one in VisiData.
4. `SPC r o` clears inline results.

### Gotchas

What the code shows.

- `SPC r a` attaches the buffer to an already running REPL (`ygg-nb-associate`).

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-notebook.el`
- `lisp/ygg-kernel-picker.el`
- `lisp/ygg-kernel-vars.el`
- `lisp/ygg-visidata.el`
- `lisp/ygg-ark.el`
- `lisp/ygg-r-mode.el`
- `lisp/ygg-db.el`

Tests:

- `test/ark-tests.el`
- `test/db-tests.el`
- `test/kernel-picker-tests.el`
- `test/kernel-vars-tests.el`
- `test/notebook-tests.el`
- `test/r-mode-tests.el`
- `test/visidata-tests.el`

## Code intelligence

LSP actions, navigation and hierarchies, structural search and rewrite, formatting, one build, run and test verb per language, and servers for JSON, YAML and Rust.

### Sub-features

LSP verbs, call graph, structural search, format, code verbs.

- Rename, code actions, organize imports, fix all, type definition, inlay hints.
- Call hierarchy, type hierarchy, and a text call graph; calls to quickfix.
- ast-grep search and rewrite; search by symbol kind.
- Format buffer or selection; format on save toggle.
- Build, run, test, device, screenshot and record verbs shared across languages.

### How to get to it

Code verbs live under SPC c.

- `SPC c r` — rename (`ygg-lsp-rename`)
- `SPC c a` — code actions (`ygg-lsp-code-actions`)
- `SPC c G` — call graph (`ygg-call-graph`)
- `SPC c I` — call hierarchy (`ygg-lsp-call-hierarchy`)
- `SPC c f` — format buffer (`ygg-format-buffer`)
- `SPC c b` — build (`ygg-code-build`)
- `SPC c l` — build and run (`ygg-code-run`)
- `SPC c u` — test at point (`ygg-code-test-at-point`)
- `SPC s a` — ast-grep (`ygg-ast-grep`)
- `SPC c R` — structural replace (`ygg-ast-grep-rewrite`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC c r` renames the symbol at point through the language server.
2. `SPC c G` prints the callers and callees of the symbol as a text graph.
3. `SPC c b` builds the way the buffer's language does; `SPC c l` builds and runs; `SPC c u` runs the test around point.
4. `SPC s a` searches structurally with ast-grep.

### Gotchas

What the code shows.

- `ygg-format` uses eglot when the buffer is managed, otherwise `indent-region`.
- `SPC c n` reconnects the language server (`ygg-lsp-reconnect`).

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-lsp.el`
- `lisp/ygg-lsp-calls.el`
- `lisp/ygg-call-graph.el`
- `lisp/ygg-ast.el`
- `lisp/layer-astgrep.el`
- `lisp/layer-format.el`
- `lisp/ygg-format-linters.el`
- `lisp/ygg-code-verbs.el`
- `lisp/ygg-json-lsp.el`
- `lisp/ygg-eglot-x.el`
- `lisp/layer-rass.el`
- `lisp/layer-spell.el`
- `lisp/layer-completion.el`
- `etc/rass-harper.py`
- `etc/rass-typescript.py`

Tests:

- `test/call-graph-tests.el`
- `test/code-verbs-tests.el`
- `test/eglot-x-tests.el`
- `test/format-tests.el`
- `test/lsp-calls-tests.el`
- `test/rass-tests.el`
- `test/swift-layer-tests.el`
- `test/tempel-tests.el`
- `test/ts-server-tests.el`
- `test/ygg-treesit-objects-tests.el`

## Debugging

Debug adapter protocol through dape: breakpoints, stepping, a shared drawer with views, and per-language launch configs for Python, JS, Java, Kotlin, C-family and iOS.

### Sub-features

Dape keys, drawer views, language adapters.

- Breakpoints, conditional breakpoints, step in, over and out, REPL, eval.
- Drawer with breakpoints, console, stack, scope, threads and watch views.
- Adapters: debugpy, js-debug, jdtls, kotlin-lsp, lldb-dap, iOS simulator.

### How to get to it

Debug keys live under SPC d.

- `SPC d c` — continue or launch (`ygg-dape-continue`)
- `SPC d b` — toggle breakpoint (`dape-breakpoint-toggle`)
- `SPC d o` — step over (`dape-next`)
- `SPC d i` — step in (`dape-step-in`)
- `SPC d d` — toggle the drawer (`ygg-dape-drawer-toggle`)
- `SPC d v k` — stack view (`ygg-dape-view-stack`)
- `SPC d t` — terminate (`dape-quit`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC d b` toggles a breakpoint on the line.
2. `SPC d c` starts a session via dape if none is running, else continues the stopped one.
3. `SPC d o`, `SPC d i` step; `SPC d d` shows the drawer, `SPC d v k` its stack view.
4. `SPC d t` ends the session.

### Gotchas

What the code shows.

- `SPC d j` launches from a vscode launch.json (`ygg-dape-vscode`).

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-dap.el`
- `lisp/ygg-debugpy.el`
- `lisp/ygg-dap-js.el`
- `lisp/ygg-dap-java.el`
- `lisp/ygg-dap-kotlin.el`
- `lisp/ygg-dap-lldb.el`
- `lisp/ygg-dap-ios.el`

Tests:

- `test/dap-java-tests.el`
- `test/dap-js-tests.el`
- `test/dap-kotlin-tests.el`
- `test/dap-lldb-tests.el`
- `test/debugpy-tests.el`

## Devices and mobile

One device selection for Flutter, Android and iOS, live device logs, screenshots and recordings, Compose previews, and React Native, Swift and JDK project support.

### Sub-features

Device choice, logs, capture, previews, platform layers.

- Choose the device, emulator or simulator a project runs on.
- Live logs, screenshots and screen recordings of the selected device.
- Compose `@Preview` renders in a side window.
- React Native and Expo, Swift (xcodebuild, simctl, SwiftPM), one pinned JDK per project.

### How to get to it

Device verbs sit under SPC c.

- `SPC c m` — choose the device (`ygg-code-device`)
- `SPC c L` — device logs (`ygg-code-logs`)
- `SPC c y` — screenshot (`ygg-code-screenshot`)
- `SPC c v` — record video (`ygg-code-record`)
- `SPC c P` — preview (`ygg-code-preview`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC c m` picks the device the project runs on.
2. `SPC c l` builds and runs on it; `SPC c L` streams its log.
3. `SPC c y` saves, shows and copies the path of a screenshot of the device.

### Gotchas

What the code shows.

- The device selection is shared by Flutter, Android and iOS verbs, so choosing once applies to all three.

### Code

The main files, then the test files that map to them by name.

- `lisp/ygg-device.el`
- `lisp/ygg-device-log.el`
- `lisp/ygg-device-capture.el`
- `lisp/ygg-compose-preview.el`
- `lisp/layer-react-native.el`
- `lisp/layer-swift.el`
- `lisp/ygg-jdk.el`

Tests:

- `test/compose-preview-tests.el`
- `test/device-capture-tests.el`
- `test/device-log-tests.el`
- `test/device-tests.el`
- `test/react-native-tests.el`
- `test/ygg-jdk-tests.el`

## Terminal, tasks and remote

Toggle terminals, a command panel for tasks and background jobs, justfile and package.json task runner, and Helix-style remote editing over TRAMP.

### Sub-features

Terminals, jobs, tasks, remote hosts and containers.

- Terminal toggle, new, pick, rotate split; ghostty here.
- Run any command as a background job at the project root; list and kill jobs.
- Command panel: run a task, jump to a running job, or run any command.
- Remote: ssh host, container, sudo edit, recent remote file, close all connections.

### How to get to it

Terminals and jobs are under SPC o, remote under SPC f.

- `SPC o t t` — toggle the terminal (`ygg-terminal-toggle`)
- `SPC o c` — command panel (`ygg-command-panel`)
- `SPC o !` — run an async command (`ygg-run-async`)
- `SPC o R` — repeat the last task (`ygg-task-repeat`)
- `SPC o j j` — jobs (`ygg-jobs`)
- `SPC f h` — ssh host (`ygg-remote-ssh`)
- `SPC f u` — sudo edit (`ygg-remote-sudo`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC o t t` toggles a terminal split.
2. `SPC o c` lists tasks and running jobs; choosing a task runs it at the project root.
3. `SPC o R` reruns the last task without prompting.
4. `SPC f h` opens a host's home directory over ssh.

### Gotchas

What the code shows.

- `SPC f c` closes all remote connections (`ygg-remote-cleanup`).

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-terminal.el`
- `lisp/layer-tasks.el`
- `lisp/layer-tramp.el`
- `lisp/layer-http.el`

Tests:

- `test/ygg-term-env-tests.el`

## Files, search and quickfix

Files, search and quickfix: an oil-style single-buffer dired, fuzzy file picker, project-wide ripgrep with context, consult pickers, vim-style quickfix lists, regex dialects and wgrep editing.

### Sub-features

Search pickers, quickfix sources, files.

- Search a buffer, word, project (multibuffer with context), marks, imenu, outline.
- Send many things to quickfix: diagnostics, TODOs, symbols, an agent's mentions, process output.
- Fuzzy file picker and recent files.
- Oil-style dired: single buffer, `h` up and `l` open, `i` toggles editable dired, `a` creates a file.

### How to get to it

Search is under SPC s, quickfix under SPC q, files under SPC f.

- `SPC s s` — search the buffer (`consult-line`)
- `SPC s c` — project search with context (`ygg-search-multibuffer`)
- `SPC s w` — grep the word at point (`ygg-search-word-at-point`)
- `SPC f f` — files, fuzzy (`ygg-files-picker`)
- `SPC f j` — jump to dired (`ygg-dired-jump`)
- `SPC q l` — toggle the quickfix panel (`ygg-quickfix-toggle`)
- `SPC q D` — diagnostics to quickfix (`ygg-qf-diagnostics`)
- `SPC q A` — what the agent said to quickfix (`ygg-qf-from-session`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC s c` runs ripgrep over the project and fills the quickfix buffer with context excerpts.
2. `SPC q l` opens the panel and puts point in it; again hides it.
3. `SPC q D` sends diagnostics to the list; `SPC q t` sends TODOs.
4. `SPC f j` opens the current file's directory in dired; `l` opens, `h` goes up, `i` makes the listing editable.

### Gotchas

What the code shows.

- The dired keys `h`, `l`, `-` are rebound to dired-single variants once dired-single loads.
- Stepping through errors is separate: `] q` and `[ q` follow the next-error list.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-quickfix.el`
- `lisp/layer-dired.el`
- `lisp/layer-pcre.el`
- `lisp/layer-completion.el`

## UI and themes

Mode line, sticky scroll, zen mode, floating picker, focus framing, font size, golden ratio, cursorword toggles under SPC u, and a light and dark theme switch.

### Sub-features

Toggles and view helpers.

- Helix-flavoured mode line.
- Sticky scroll via the header line; zen centered view.
- Floating (posframe) minibuffer; focused window framed and others dimmed.
- Font size, line numbers, whitespace, color preview, rainbow delimiters.
- Light and dark theme, one toggle.

### How to get to it

Toggles sit under SPC u.

- `SPC u z` — zen (`ygg-zen-toggle`)
- `SPC u s` — sticky scroll (`ygg-sticky-mode`)
- `SPC u p` — floating picker (`ygg-vertico-posframe-toggle`)
- `SPC u g` — golden ratio (`ygg-golden-ratio-mode`)
- `SPC u l` — line numbers (`display-line-numbers-mode`)
- `SPC u w` — cursorword (`ygg-cursorword-mode`)
- `SPC o T` — theme light or dark (`ygg-theme-toggle`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC u z` toggles a distraction-free centered view of the selected window.
2. `SPC u s` shows the enclosing scopes in the header line.
3. `SPC u p` flips the picker between floating and bottom minibuffer.
4. `SPC o T` flips between the light and the dark theme.

### Gotchas

What the code shows.

- Font size keys are normal-state `g =` and `g -` (`ygg-font-increase`, `ygg-font-decrease`).
- The theme toggle is defined in `init.el`, not in a layer file.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-ui.el`
- `lisp/yggdrasil-modeline.el`
- `lisp/ygg-focus.el`
- `lisp/ygg-ui.el`
- `lisp/layer-editing.el`
- `init.el`

Tests:

- `test/ygg-focus-tests.el`
- `test/ygg-ui-markdown-tests.el`
- `test/ygg-vertico-posframe-fit-tests.el`

## In-buffer browser

A browser in a buffer: WebKit xwidget when available, else eww, embr side by side, and an agent preview pane beside a trace.

### Sub-features

WebKit, embr, agent preview.

- Open a URL in WebKit, or in a new WebKit buffer.
- Embr browser, also incognito.
- Preview of an agent's dev server beside its trace.

### How to get to it

Browsers are under SPC o b.

- `SPC o b w` — webkit (`ygg-browser-open`)
- `SPC o b e` — embr side by side (`ygg-browser-embr`)
- `SPC o b a` — agent preview (`ygg-aob-browser`)

### Driving it

Results below are read from docstrings, not driven live (unverified).

1. `SPC o b w` opens a URL in a WebKit xwidget, or eww when none.
2. `SPC o b a` shows an agent session's preview URL beside its trace.

### Gotchas

What the code shows.

- Browser behavior is tested in `test/aob-browser-tests.el` for the agent preview.

### Code

The main files, then the test files that map to them by name.

- `lisp/layer-browser.el`

Tests:

- `test/aob-browser-tests.el`

