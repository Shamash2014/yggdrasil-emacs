# Automation Builder Reference

## Target surfaces

### Script
Use when the workflow:
- has meaningful shell logic
- should be runnable outside Neovim
- benefits from versioned executable files

### `core.tasks`
Use when the workflow:
- should be selectable from editor UI
- maps cleanly to a command invocation
- belongs with other project tasks

### `djinni.automations`
Use when the workflow:
- should be discoverable alongside ACP commands
- should feel like a code action or editor automation
- may combine ACP actions and local tasks in one menu

## Build-script rules
- Use explicit shells and strict failure behavior.
- Keep cwd deterministic.
- Validate required tools before heavy work.
- Add a dry-run mode for destructive flows when practical.
- Print short actionable errors.

## Integration rule of thumb
- Put logic in scripts.
- Put discoverability in Lua.
- Put repeated command execution behind `core.tasks`.

## Linkage checklist
- A task-backed automation is not done until `core.tasks` exposes it and `djinni.automations` can collect it.
- An ACP-backed automation is not done until the active neowork session exposes it through `availableCommands` or `available_commands_update`.
- Validate the real picker path, not just the implementation file.
- If a session refresh or restart is required before the automation appears, make that explicit.

## Common mistakes
- writing shell logic directly into picker handlers
- creating a script without exposing it anywhere discoverable
- adding a task entry that assumes implicit cwd or env
- creating an automation but never linking it to the picker/session state that users actually invoke
- skipping validation of the real user-facing entrypoint
