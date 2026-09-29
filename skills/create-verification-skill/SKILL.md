---
name: create-verification-skill
description: "Generate a project-local verification skill that drives your app the way a user does — any language, framework, or platform. Use for /create-verification-skill, \"make a control skill for this repo\", or when a project has no scripted way to prove UI/CLI/service behavior."
disable-model-invocation: true
---

# Create a verification skill

Every serious project needs a scripted way to drive the real app and prove behavior: launch it, exercise a feature the way a user would, and capture evidence. This skill generates that as a project-local skill (`skills/verify-<app>/`) tailored to the repo. You write the generator's output for the next agent, not for a human: it will be read cold, mid-task, by an agent that has never seen the app.

## 1. Interview the repo, not the user

Answer these from the codebase and only ask the user what you cannot observe:

- **Surface:** what does a user actually touch? A web UI, a CLI/TUI, a desktop app, an API, a mobile app, a library? A repo can have several; pick the primary one and note the rest.
- **Run:** how does the app start locally? Prefer the repo's own documented dev command (package scripts, Makefile, README quickstart). Note ports, env vars, seed data, auth.
- **Drive:** how can an agent interact with it programmatically? Existing harnesses first — Playwright/Cypress specs, expect scripts, PTY helpers, curl-able endpoints, a debug port. Only then pick a generic recipe: browser/CDP for web and Electron, a tmux/PTY harness for CLI/TUI, plain HTTP for services.
- **Observe:** what evidence can be captured? Screenshots, terminal transcripts, response bodies, logs, exit codes, DB state.
- **Isolate:** can two instances run side by side (ports, data dirs, profiles)? If not, say so in the generated skill: refusing to double-drive a shared instance beats corrupting the user's session.

If the checkout doesn't build or start as-is, fix that first (or report it precisely) before generating; a skill written against a broken base teaches wrong steps. When an irrelevant missing asset blocks startup (a static dir the API never serves, a sample config), the generated skill may create it, clearly marked as verification scaffolding, and remove it in cleanup.

## 2. Generate the skill

Write `skills/verify-<app>/SKILL.md` with YAML frontmatter (`name: verify-<app>` and a `description` that names the app, the surface, and when to reach for it — without frontmatter the skill never registers) and these sections, each grounded in what the interview actually found (no placeholders left):

- **Launch:** the exact command that starts the app for verification, and how to tell it's ready (a log line, a port answering, a prompt). Include teardown. For a short-lived CLI or TUI there is no server to keep alive: launch means build the binary (or install deps) once, then start each drive in its own isolated PTY or tmux session.
- **Doctor:** one read-only check that answers "is this instance worth driving?" — process up, right version/build, port owned by us, auth valid. An agent runs this first whenever anything looks off.
- **Drive:** the harness recipe with real selectors/commands from this repo, not examples. Prefer stable handles (ARIA labels, data attributes, prompt strings, route paths) over coordinates and tab order.
- **Evidence:** what to capture for a proof and where it goes. State the proof standards: exercise the real user path, not internal setters or test-only endpoints; capture the action and the resulting state, not just the final screen; verify side effects (files written, rows inserted, messages sent) alongside what's visible; mocks only where a production boundary already isolates the external system. When the safe path is a dry-run or test mode, verify what it actually skips by observing (files, network, git refs) rather than trusting its name: some dry-runs still touch the network or open a browser.
- **Cleanup:** how to tear down instances the run created. Never kill by process name; kill what you started. Cleanup removes instances and scratch state, never the evidence: proof artifacts survive the teardown, in a location the skill names.
- **Helpers:** any script the skill ships is executable and its invocation is shown in the skill body. A helper the reader has to reverse-engineer is not a helper.
- **Coverage trace:** a per-change step the generated skill runs on every diff, not just at generation time. List every changed codepath and user flow the diff touches, including interaction edge cases where relevant (double submit, navigate away mid-operation, stale session, slow network, concurrent tabs), and mark each tested or gap. Route by kind: end-to-end for a flow crossing three or more parts and for any auth, payment or destructive action; eval for a prompt or tool-definition change; unit for a pure function. Gate: every gap either gets a test now or a written reason it's deferred — no silent gaps. Cap generation so a run never balloons: at most 30 paths traced, 20 tests generated, 2 passes to get one green, 2 minutes per test; a generated test still failing after one fix attempt is removed, and the gap it covered stays listed as a gap, not silently dropped.
- **Security:** detect the stack the way security-scan does, then choose the scanners that fit: secrets, dependency audit, SAST, and container/IaC when a Dockerfile or *.tf is present; for Flutter/Dart, Kotlin/Android or Swift/iOS that means osv-scanner on the lockfile (pubspec.lock, gradle.lockfile, Package.resolved), semgrep's per-language rules, mobsfscan scoped to the mobile source tree, and detekt for Kotlin. For each, record the exact command scoped to this repo (or to changed files where the tool supports that), where it writes its output, and the severity where the owner's threshold (default high) starts failing it — read the tool's own help for that flag, or record that any finding fails it; never guess a flag from memory. Point at skills/security-scan for triage rather than restating it. A tool that is missing: name it with the project-local way to add it (the repo's own package manager or dev dependencies, mise, a pinned container image), and ask the owner before installing anything; never install silently, and never turn on a cloud or third-party ruleset without the owner's yes. Make the security step runnable on its own, such as a subcommand of the entry command, so it can be named as sec_cmd in .ice/config without driving the whole app; have it exit 75 when a recorded tool is missing, so ICE reads that as blocked rather than failed. In a repo ICE-wired for this app, the security step runs once, as sec_cmd's own lane in ice-verify, and stays out of the entry command (live_cmd) so a live drive never re-runs it; outside ICE, the entry command itself gains a step that runs these recorded commands and fails on a finding at or above the owner's threshold.

## 3. Seed the feature map

The map lives in the repo's lat.md, not in the skill's folder: one file, lat.md/features.md, headed "# Features", with one h2 section per user-facing feature you can identify (aim for the top 3-5 to start, from routes, commands, menus, or docs). Follow the shape in [references/feature-map-example/features.md](references/feature-map-example/features.md). Each section answers, from the user's point of view: what the feature is, how to reach it, how to drive it with the harness, and what observable end state proves it works.

- The h2 is the feature as a user names it. Intents point at it as "Feature: <name>" and the archive step files changes under it by that name, so rename one only on purpose. Nothing but features gets an h2 here: baseline state and driving conventions go in the h1's body or the verify skill's own sections.
- Under it, four h3s in this order: Sub-features, How to get to it, Driving it, and Gotchas. Driving it names the harness in its opening line and pairs each user action with an exact command and the observable result. A Changes h3 belongs to the archive step; leave it alone.
- lat refuses a section without a leading paragraph: every h1, h2 and h3 opens with one plain paragraph of at most 250 characters before any list.
- Where the repo has no lat.md/ yet, create lat.md/lat.md with a one-line lead and an index line "- [[features]] — feature map, one section per user-visible feature". Where lat.md/lat.md exists, add that line if it is missing.
- Run lat check from the repo root and fix what it reports before moving on. Its "No init version recorded" warning is not a failure; never run lat init to clear it, wiring the repo is ice-wire's job.

The skill's SKILL.md points at the map as [[features]] in lat.md/features.md; it keeps no copy. The map is the repo's maintained verification source; a proof that drives one convenient entry point is incomplete when the map lists others.

## 4. Prove the generated skill before handing it over

Run its own instructions end to end once: launch, doctor, drive ONE mapped feature (one is enough; the map exists so later runs can cover the rest), capture evidence, clean up. Also run the security step once and confirm its recorded output lands where the skill says. After cleanup, confirm the evidence still exists at the named location — a cleanup that eats the proof fails this step. Fix what fails, and run the generated cleanup after every failed iteration too, so broken attempts don't strand processes and ports. A generated skill that was never executed is a draft, not a deliverable.

## 5. Offer the maintenance loop

Point the user at `/maintain-verification-skill` for keeping the map honest as the app changes; in Emacs, the maintain preset runs it (M-x ygg-ice-maintain), and ygg-ice-maintain-daily runs it once a day. Suggest a cadence only if they ask.
