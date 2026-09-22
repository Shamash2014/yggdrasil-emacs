# Presets and pstack

## What pstack is

pstack is a Cursor plugin: a directory at the root of cursor/plugins holding a manifest at
pstack/.cursor-plugin/plugin.json. The manifest is metadata plus glob pointers; its only
content keys are skills, pointing at ./skills/, and agents at ./agents/. The schema
(schemas/plugin.schema.json) allows far more: commands, rules, hooks, variables (a JSON
Schema of user-settable parameters), mcpServers, minClientVersions. pstack declares none.
It ships no code, no tools, no servers: markdown and a manifest.

Inside: one router skill, skills/poteto-mode/SKILL.md, frontmatter carrying mode: true and
disable-model-invocation: true; twenty-one playbook files beside it (twenty-three by
pstack's own count); a dozen leaf skills; two subagent definitions in agents/.

The technique is a router plus playbooks. Given a goal, the mode skill classifies the work
— read-only question, defect, new behaviour, cross-cutting programme — picks one playbook
and copies its steps verbatim into a todo list. Each step names the model and subagent
type that executes it; the default split sends code to a fast model, judgment to a strong
one. The unit of composition is a markdown file with frontmatter, and composition is by
reference: a playbook says read skill X, delegate to subagent type Y. It assumes the host
provides skill discovery, subagent spawn by name, per-call model choice and a todo list.
Nothing is bound at runtime; every choice is a sentence the model obeys.

The README is 22 KB but promotional; the mechanism is in the mode skill and the playbooks.
Unverified: a summary of that skill claimed per-role model overrides live in a setup-pstack
command. No such skill is in the tree, and the guide never names one.

## What gptel presets are

The opposite shape: runtime bindings, not prose. gptel--known-presets is an alist of name
to plist; the defining form:

    (defun gptel-make-preset (name &rest keys) ...)

whose docstring says "A preset is a combination of gptel options intended to be applied and
used together." Named keys are description, parents, pre, post, backend, model, system,
tools — but the set is open: "Any other key (like :foo) corresponds to the value of either
gptel-foo (preferred) or gptel--foo." Temperature, max-tokens, context, stream and cache
ride free. A preset is a patch over gptel's defcustoms.

Scope comes from the setter:

    (defun gptel--apply-preset (preset &optional setter) ...)

It defaults to set (global). The save/restore path passes one calling make-local-variable,
so a preset recorded in a file-local gptel--preset re-applies buffer-locally.
gptel-with-preset let-binds every symbol it touches around a body: one-request scope.

In-buffer, gptel--transform-apply-preset scans the last user prompt for
"@\\([^[:space:]]+\\)\\_>", matching only where the preceding character is whitespace or a
comment ender, deletes the cookie, and applies the preset buffer-locally with point where
it was. It runs as a prompt transform on the temporary copy — hence the warning that :eval
and :function forms "are evaluated in a temporary buffer, and not the buffer from which
the request is sent." Several cookies apply in order.

Composition is three-layered: :parents applies named presets first (depth-first, child
last, so the child wins); (:append …) or (:prepend …) merges rather than replaces;
(:eval form) or (:function fn) computes the value at apply time. That is the part worth
copying.

## What a preset means for aob

Our agent table is already a half-preset: the aob-acp-agents docstring says :worktree and
:mode are "the agent's traits, not spawn-time flags", which is why claude-isolated is a
second entry rather than a modifier. A preset layer generalises that entry.

What belongs: agent (or base plus overrides), model, permission mode, config options
(reasoning effort, collaboration mode, fast), worktree policy as a boolean not a path, the
MCP server set, a tool allowance, a skill scope, a first-turn preamble. What does not: the
project or cwd (pin one and the preset stops being reusable; aob-acp-spawn-with rightly
asks for project separately), session identity (name, :parent-session, :mcp-token),
credentials and the config home, a captured context set. Context is a recipe, not a
payload: gptel's :context '(:eval …) is the precedent.

Where it applies splits along the wire. Changeable mid-session: mode, via
aob-acp--want-mode sending session/set_mode once the id is confirmed advertised; model,
via aob-acp--set-model, which routes to set_config_option with configId "model" when the
agent advertises configOptions and to session/set_model otherwise; anything else via
aob-acp--set-config. Fixed at session/new: cwd and mcpServers — the aob-acp-mcp-servers
docstring says outright that "the servers a session gets are decided by whoever opens it
and are part of the session/new call, so there is no later moment to add one." Fixed lower
still, at the process: the agent binary and its environment, since aob-acp--conn-key files
connections under (agent . project) and every session of that pair shares one. A preset
changing CLAUDE_CONFIG_DIR needs its own, keyed by aob-acp-isolate.

Tool exposure is the one real gap. aob-mcp--listing walks the whole table unfiltered and
aob-mcp--call looks the name up directly, so narrowing must happen in both or the model
calls what it was never shown. The sidecar cannot ask who is calling — the token to
session map lives in the editing Emacs, and tools/list must answer synchronously. The
cheap fix is the URL: aob-mcp-host-spec already builds ...?session=TOKEN, a preset appends
&tools=prefix,prefix, aob-mcp--query already parses arbitrary keys, and aob-mcp-tool-names
already takes a prefix. Prefix narrowing is nearly free; name-exact allowlists cost more.
Neither is a security boundary — it is curation, and should be called that. One catch: a
preset cannot just cons its own entry on. aob-mcp-host--around-spawn already conses the
host spec, and aob-acp--mcp-servers dedupes theirs against mine but never within mine, so
you get two servers named aob. The narrowing belongs inside aob-mcp-host-spec or that
advice, not beside it.

pstack's composition does not map. It is a packaging format for prose plus a router the
model reads; ours is a binding problem — what session/new carries, and which of it a live
session can still be told. gptel is the template for the mechanism. The one thing to take
from pstack is that a preset should carry a named procedure, a skill or a preamble, not
only knobs. "Plan mode, opus, read-only tools, follow the investigation playbook" beats
three variables. aob-acp-before-first-prompt-functions is where that rides.

## What to build first

One defcustom, aob-acp-presets: name to plist of :agent :model :mode :worktree :config
:skill — :skill meaning a skill named in the preamble (prose, cheap), not the skills
directory the agent sees, which is config-home-shaped (ygg-agent--config-homes shares
skills, agents, commands) and needs its own connection. Applying it at spawn needs almost
no new machinery — bind aob-acp-mcp-servers
before session/new, set the :want-mode and :want-model refs, and the handshake in
aob-acp--session-opened applies them before the first prompt flushes. The one new piece is
:want-config, a deferred list of set_config_option calls in the same place, because
reasoning effort has no want-path today. Then one entry point: aob-acp-spawn-with's three
prompts (agent, project, model) become preset plus project.

Left out: parents and append/prepend merging, tool narrowing, mid-session re-presetting.
Merging is the part of gptel that earns its keep, but only once enough presets share
parts. Tool narrowing wants the URL change first. Re-presetting is a one-liner over the
three set calls — but the fixed half will not follow, and a re-preset that silently
applies half of itself is worse than none.

The trade-off: presets and aob-acp-agents will overlap, and keeping both forever is how
this rots. The end state folds claude-isolated and codex-isolated into presets over a base
agent — second step, not first, but it should be the plan.
