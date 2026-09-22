---
name: automation-builder
---

# Automation Builder

## Summary
Generates project automations, build scripts, and task integrations. Use when the user wants to create or wire reusable scripts, custom tasks, or ACP-discoverable automations.

## Goal
Turn a repeated manual workflow into a reusable automation with the smallest viable surface:
- a script when execution logic is needed
- a task registration when the workflow should be selectable
- an ACP-facing automation entry when it should be runnable from neowork

## When to use
- “build a script for this workflow”
- “turn this into an automation”
- “make this a task”
- “wire this into the ACP automation picker”
- “generate a build script / release script / project task”

## When not to use
- one-off shell commands with no reuse value
- purely conversational prompts with no execution surface
- CI-only changes where the user explicitly wants pipeline config and not local automation

## Inputs
- target workflow in plain English
- project context and existing task surfaces
- expected invocation surface:
  - shell script
  - `core.tasks`
  - ACP automation picker
  - more than one of the above

## Default approach
1. Inspect the current project before writing anything.
2. Find existing automation surfaces and reuse them.
3. Choose the narrowest durable implementation:
   - script only if logic must live outside Lua
   - task registration if users need a runnable command
   - ACP automation integration if it should feel like a code action
4. Make the automation idempotent and explicit about cwd, env, and failure handling.
5. Link the created automation to the real discovery surface instead of stopping at implementation.
6. Validate by running or dry-running the exact entrypoint users will invoke.

## Decision points
- If the workflow is mostly shellable, prefer a checked-in script plus task registration.
- If the workflow is editor-native, prefer Lua integration over wrapper shell.
- If the workflow should be discoverable in chat/session UX, expose it through the automation picker path.
- If the project already has task conventions, match them exactly instead of inventing new ones.

## Linking rules
- Do not treat "automation created" as complete until it is reachable from the intended user-facing surface.
- For task-backed automations, make sure `core.tasks.get_tasks()` can return the new entry and that `djinni.automations.collect()` will include it.
- For ACP-published automations, make sure the active neowork session can see the command through `availableCommands` or `available_commands_update`, because the picker reads `neowork.stream.get_available_commands()`.
- If the new automation depends on ACP command advertisement and an existing session will not refresh automatically, call that out and wire or document the required refresh behavior.
- Prefer existing picker wiring in `lua/djinni/automations.lua` over adding a second discovery path.

## Repo-specific integration order
For this repo, check these in order:
1. `lua/djinni/automations.lua`
2. `lua/neowork/commands.lua`
3. `lua/djinni/init.lua`
4. `lua/core/tasks.lua`
5. existing `scripts/`, `Makefile`, `package.json`, or project-native task files if present

Prefer:
- `core.tasks` for executable project tasks
- `djinni.automations` for ACP/task picker exposure
- repo scripts only when they carry real execution logic

## Output contract
When you use this skill, produce:
1. the automation implementation
2. any task or picker wiring needed
3. a short usage note
4. validation results or the exact blocker

## Validation loop
- Check that the automation has a single clear entrypoint.
- Check that paths, cwd, and env assumptions are explicit.
- Check that failure output is actionable.
- Check that the created automation is linked to the intended picker/task/session surface.
- Run syntax checks.
- Run the automation, or dry-run if execution would be destructive or too expensive.
- Confirm the automation is reachable from the intended surface.

## Templates

### Automation request template
```text
Build an automation for: <workflow>
Entry surface: <task|ACP picker|script|mixed>
Inputs: <files/args/env>
Expected result: <artifact or side effect>
Constraints: <speed/safety/idempotency>
```

### Implementation checklist
```text
- inspect existing task/automation surfaces
- choose target surface
- implement logic in the narrowest layer
- expose through picker/task registry if needed
- validate real invocation path
```

## Examples

### Example 1
Input: “Create a build-and-test automation for this repo and expose it in the picker.”

Output:
- add a script or Lua task entry for build/test
- register it in `core.tasks`
- ensure it appears in `djinni.automations`
- verify the picker can actually invoke it from the intended session
- run the exact command once to verify

### Example 2
Input: “Turn this release shell sequence into a reusable automation, but keep it repo-local.”

Output:
- create a repo script with strict error handling
- add a task entry pointing at that script
- document required env vars
- validate with a dry-run mode if release is destructive

## Evaluation prompts
1. “Create a skill for generating reusable build scripts and wiring them into a project task runner. It should prefer existing task surfaces over new wrappers.”
2. “User asks for a one-off command explanation. This skill should not trigger; answer normally.”
3. “Generate an automation for a destructive release flow. Require dry-run support, env validation, and explicit failure handling.”
