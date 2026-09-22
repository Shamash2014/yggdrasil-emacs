# Agent servers on other machines

One local Emacs, agents on several hosts, each host holding its own config.

## 1. The spawn is local, and that is the good news

aob-acp.el:185 calls make-process with no :file-handler, so no file name handler is ever consulted and the agent always runs on this Mac. The default-directory bound to the project at aob-acp.el:171 is decorative for spawning; it reaches the agent only as the session cwd.

The elisp manual, *Asynchronous Processes*, is explicit: make-process looks for a handler only "if file-handler is non-nil"; start-file-process "may invoke a file name handler based on the value of default-directory". Only start-file-process and process-file are handler-aware by default; make-process becomes so with :file-handler t.

The sharper consequence: :command at aob-acp.el:187 is an ordinary argv taken from the aob-acp-agents table (aob-acp.el:23). An entry of the form

    ("claude-remote" :command ("ssh" "-T" "buildhost" "npx" "-y" "@agentclientprotocol/claude-agent-acp"))

runs an agent on another machine **with no change to the process call at all**. Checked: aob-acp-login-shell-command (aob-acp.el:69-73) maps shell-quote-argument over that argv into a valid local command, the backslashes it adds before = and @ being inert in sh. The transport is free; the question is what breaks once you have the pipe.

## 2. Stdio over TRAMP: the known-broken case, exactly

aob-acp.el:191 passes :stderr, a separate buffer — the subject of [bug#47861](https://lists.gnu.org/r/bug-gnu-emacs/2021-04/msg00888.html): a jsonrpc-process-connection over TRAMP fails with "Wrong type argument: inserted-chars" whenever the remote process writes to stderr, because tramp-sh-handle-make-process builds a fifo and then insert-file-contents on it. Every ACP adapter writes to stderr on startup — hermes is documented doing so at aob-acp.el:44. The one-line :file-handler t lands on the failure mode, not around it. [bug#50748](https://lists.gnu.org/archive/html/bug-gnu-emacs/2021-09/msg02027.html) adds that the fifo uses mknod p, unsupported on macOS — it bites when the *remote* is a Mac.

Eglot is the precedent and reads as a bill of costs. Its make-process is our shape — :connection-type pipe, :coding, :noquery, a :stderr buffer — plus :file-handler t. Around it, [bug#61350](https://lists.nongnu.org/archive/html/emacs-diffs/2023-03/msg00018.html) binds tramp-use-ssh-controlmaster-options to suppress and forces ControlMaster=no, commented "Tramp turns on a feature by default that can't (yet) handle very much data": core disabling ssh connection sharing for LSP because the multiplexed channel loses bytes under load. Filters fire, but through TRAMP's shell and connection buffer, and [measurements](https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./) put a round trip at 50-100ms against about 1ms locally. The escape hatch is shut too: [lsp-mode#4573](https://github.com/emacs-lsp/lsp-mode/issues/4573) reports servers dying at once under tramp-direct-async-process, worked around only by stubbing tramp-direct-async-process-p to nil.

Two more snags. aob-acp-login-shell-command uses the *local* SHELL (aob-acp.el:72), which need not exist on a Linux remote. And the session cwd is an expand-file-name'd local path (aob-acp.el:1760, :1912, :2122, :2241); under TRAMP those carry an /ssh: prefix no agent can read, so file-local-name is needed either way.

## 3. Environment does not cross ssh by itself

The central finding, because per-machine config is the whole point. aob-acp-environment-function's output is prepended to process-environment at aob-acp.el:174-179. Over an ssh argv that sets the variable in the *local ssh client*, which discards it: CLAUDE_CONFIG_DIR and CODEX_HOME (ygg-agent-conf.el:27, :39) would be set on the Mac and ignored by the agent. Inline it instead — ssh -T host env CLAUDE_CONFIG_DIR=… npx … — or ssh -o SetEnv= against a remote AcceptEnv.

Under TRAMP the gap differs. The manual's *Remote processes* node says a let-bound process-environment applies only for entries "not present in the global default value", pointing instead at tramp-remote-process-environment and connection-local variables keyed by :application tramp, :protocol, :machine. Per-host config becomes a connection-local profile.

## 4. The four routes

| route | per-machine config | failure, reconnect | cost |
|---|---|---|---|
| ssh-wrapped stdio argv | env inlined in the remote argv; one aob-acp-agents entry per host | inbound death is clean; outbound is not — see below | hours; no make-process change |
| TRAMP :file-handler t | tramp-remote-process-environment, connection-local profiles | bug#47861 on stderr; ControlMaster off; 50-100ms a turn | days, fighting core bugs |
| ssh port-forward to a remote HTTP server | the server's config file | tunnel drops silently; wants autossh | no ACP server to forward to yet |
| remote Emacs daemon, emacsclient | separate, one init per host | a UI session per host; buffers unshared | highest; two Emacsen, one frame lost |
| small remote broker | the broker's config | you own reconnect | you maintain a daemon |

Death is asymmetric, the route's real hazard. aob-acp--kill-tree signals the negative pid — the *local* process group, which over ssh holds only the ssh client. Without a pty the remote agent gets no SIGHUP and keeps talking to a model, holding the throwaway worktree aob-acp--reap-worktree (aob-acp.el:1305) expects to remove. Fix with ssh -tt or a remote wrapper that traps. [acpx#637](https://github.com/openclaw/acpx/issues/637) names this teardown-without-a-local-pid problem; it asked for exactly our contract and was **closed as not planned**, but reports the wrapper working today — acpx --agent 'ssh build-host codex-acp'.

The port-forward row is unavailable for the agent itself. ACP is JSON-RPC with no transport in the spec; the [introduction](https://agentclientprotocol.com/get-started/introduction) says local agents "run as sub-processes of the code editor, communicating via JSON-RPC over stdio" and remote agents over HTTP or WebSocket are "a work in progress". [claude-code#24365](https://github.com/anthropics/claude-code/issues/24365) is the open request for a network transport.

Keep that apart from MCP over HTTP, which already works. aob-acp--mcp-takes-p (aob-acp.el:1567-1577) gates on advertised capability, never reachability, so an http MCP URL that resolves here will not resolve from the remote, and stdio servers read from the project .mcp.json (aob-acp.el:1609) are spawned by the *remote* agent and must exist there. Port-forwarding earns its place tunnelling MCP endpoints, not ACP.

## 5. Secrets

ygg-agent--share-mcp-auth is gated on darwin (ygg-agent-conf.el:196) and reads a login-keychain item named for the sha of the config home (ygg-agent-conf.el:157, :164). None of that exists on Linux. The [Claude Code docs](https://code.claude.com/docs/en/authentication) say credentials there live in a plaintext .credentials.json under the config dir at mode 0600 — which makes the remote case *simpler*: the same JSON blob, a file rather than a keychain item.

The standard answer is per-host credential stores, one login per machine, not shipping tokens. Agent forwarding is the wrong tool — it forwards an ssh key, not an OAuth token, and the socket is usable by anyone with root there. If a secret must be provisioned, use sops or age with per-host recipients.

## 6. Verdict

Build the **ssh-wrapped stdio argv**. No make-process change, the pipe is a plain local subprocess so section 2 does not apply, it is the shape acpx reports working, and it puts the config home in the remote command where it is actually read.

Smallest change to aob-acp.el that makes remote agents work at all: none — a host entry in aob-acp-agents already spawns one. Smallest change that makes it *useful*: give the spec a :host, and at aob-acp.el:174-187 send aob-acp-environment-function's output through the remote argv as env VAR=VAL rather than process-environment. Then map cwd (aob-acp.el:1760, :1912, :2122, :2241) to the remote path. Expect the first spawn to fail on a remote .zshenv printing to stdout — ssh runs a non-interactive login shell and stdout is the ndjson wire, the hazard aob-acp.el:70-71 already guards locally. Isolated agents last: their worktree comes from ygg-git-async, another local make-process at ygg-git.el:122.

## Verified on 31.1

Run on emacs-plus@31 31.1 with -Q --batch. **ssh to localhost is refused on this machine** (Remote Login off; port 22 connection refused), starting a user sshd was blocked, and sudo needs a password — so the remote is /mock::, the tramp-sh method Emacs's own tramp-tests.el uses: a real tramp-sh connection over a login shell, exercising the same handler, without a network. Where that substitution matters it is said so.

**Q1 — :stderr works.** Probe: make-process with :file-handler t, :stderr a buffer, running sh -c 'echo OUT; echo ERR >&2'. Observed, remote case: filter got "OUT\n", stderr buffer got "ERR\n". No error, no merge — identical to the local baseline. tramp-sh.el:3162-3171 makes the fifo, redirects 2> to it, and reads it back with a second make-process that does pass :file-handler t, so the cat runs remotely. bug#47861 does not reproduce on 31.1. bug#50748 is also stale here: tramp-get-remote-mknod-or-mkfifo returned "mknod %s p" and mknod /tmp/x p succeeds on this Darwin. **Verdict: the existing spawn path at aob-acp.el:181-192 can be reused as-is with :file-handler t added.** No stderr workaround is needed. If one is ever wanted, note that appending 2>FILE after a redirect inside the command does not work — the observed merged run put "OUT\nERR\n" on the filter; the form that keeps stdout clean is brace-grouping the whole command, sh -c '{ ...; } 2>/path', which gave filter "OUT\n" and file "ERR\n".

**Q2 — kill-tree is a no-op remotely.** TRAMP sets both remote-pid and tramp-vector on the process. signal-process is routed through signal-process-functions to tramp-signal-process (tramp.el:7538), which goes remote **only** when given a process object, or a number plus a remote file name as its third argument. Probed all four forms, reading the hook's return value:

| form | hook returned | reaches remote |
|---|---|---|
| (signal-process (- pid) 'TERM) — what aob-acp--kill-tree does | nil | no, falls through to the local C signal |
| (signal-process pid 'TERM) | nil | no |
| (signal-process proc 'TERM) | 0 | yes, sends kill -TERM remote-pid |
| (signal-process (- remote-pid) 'TERM default-directory) | 0 | yes, sends kill -TERM -pid, a remote process group |

The last row is the exact working incantation that preserves kill-tree's group semantics. Whether a real ssh remote orphans the agent could not be observed, because under /mock:: the remote is this machine; the routing result above is method-independent.

**Q3 — process-environment does cross, and this corrects section 3 for the TRAMP route.** With the connection already warm, so nothing could leak by inheritance, sh -c 'echo [$CLAUDE_CONFIG_DIR]' returned "[]" at baseline, "[/via-process-environment]" under a let-bound process-environment, and "[/via-tramp-remote]" under a let-bound tramp-remote-process-environment. Both are lists of "VAR=VAL" strings and **both can be let-bound around make-process** — no global setting needed. TRAMP inlines them as env VAR=VAL in the command it builds (tramp-sh.el:3127). So aob-acp-environment-function (aob-acp.el:174-179) keeps working unchanged under TRAMP; it is only the raw-ssh-argv route that discards it.

**Q4 — TRAMP sets the remote cwd itself.** With default-directory "/mock::/tmp/", the spawned process reported pwd "/tmp" — TRAMP prefixes cd to the command. file-local-name gave "/tmp/", which is the right value for the ACP session cwd. expand-file-name gave "/mock:Serial-Killer.local:/tmp/", confirming that the cwd computed at aob-acp.el:1760, :1912, :2122 and :2241 would reach the agent as an unusable TRAMP name.

**Net.** For the TRAMP route the work is smaller than section 2 estimated: add :file-handler t, swap the four cwd sites to file-local-name, and change aob-acp--kill-tree to pass the negative remote-pid with default-directory. Environment needs no change at all. Still untested against a real ssh host, and Eglot's ControlMaster workaround (bug#61350) is a throughput question this probe did not measure.
