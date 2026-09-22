# Asynchrony in Emacs 31 for an in-daemon MCP server

A listener and many streaming agent sessions in one Emacs 31 daemon, without stalling redisplay. Citations: elisp manual nodes, Emacs 31.1 NEWS, or file:line in lisp/jsonrpc.el.

## Decision table

| Want | Use | Costs |
|---|---|---|
| Accept TCP clients | make-network-process :server t, :log | Client inherits TYPE, FILTER, SENTINEL, plist; a :filter means no process buffer |
| Read a stream | Process filter | Arbitrary chunks; errors swallowed; inhibit-quit is t |
| Escape a filter | run-at-time 0 trampoline | One event-loop turn |
| Wait at top level | accept-process-output PROCESS | Other filters and timers run inside it |
| Wait and repaint | sit-for | Returns nil once input is pending |
| Guarantee a delay | sleep-for | No repaint; ignores input |
| Drop speculative work | while-no-input | Aborts BODY like a quit |
| Housekeeping | Rescheduled one-shot run-at-time | Repeating timers drop overdue ticks |
| Heavy CPU-bound Lisp | async.el child Emacs | Fresh Emacs, one result at the end |
| Outliving the daemon | detached.el | Shell-side only |
| Parallelism | Nothing in Emacs | Global lock; threads do not give it |

## 1. Filters and sentinels

Output "may come in chunks of any size", and a string may be "split across two or more batches" ((elisp) Filter Functions): never assume whole lines, frames, or UTF-8 sequences, and keep parse state on the process plist. Decode before framing and a split multibyte character is corrupted and byte-counted framing breaks, so copy jsonrpc — binary/binary coding, set-buffer-multibyte nil, buffer-disable-undo, marker parsing (jsonrpc.el:616-624).

Filter and sentinel errors are "caught automatically" unless debug-on-error is non-nil, so a filter dies silently mid-parse and the connection wedges half-consumed: wrap in condition-case and close deliberately. Quitting is inhibited; match data and buffer are restored for you. Consing per chunk buys a GC pause that also stops redisplay, so append bytes to a buffer rather than concat strings.

The rule to copy: a filter parses and queues, never dispatches. jsonrpc--process-filter detects recursive entry and reschedules itself with run-at-time 0 (bug#60088, jsonrpc.el:790-802), then drains messages through zero-delay timers because a handler "will exit non-locally" (jsonrpc.el:865-888).

process-adaptive-read-buffering is nil by default in 31 — it "leads to wrong results in some cases... no longer useful" (NEWS). Leave it off.

## 2. accept-process-output vs sit-for vs sleep-for vs while-no-input

The first three re-enter the same internal wait, so other filters and timers run inside them. None re-enters the command loop — cold comfort, since your own filter can.

- **accept-process-output** "allows Emacs to read pending output from processes" ((elisp) Accepting Output). No redisplay is promised. With PROCESS nil it "should not be expected to return before the specified timeout expires", so always pass the process.
- **sit-for** "performs redisplay (provided there is no pending input), then waits ... or until input is available" ((elisp) Waiting). The only one that repaints — (sit-for 0) is exactly (redisplay) — and unreliable as a delay.
- **sleep-for** "pauses ... without updating the display. It pays no attention to available input" — use it "when you wish to guarantee a delay", not to service the wire. A loop of 112 (sleep-for 0.05) calls kept serving this session, but nothing documents output being read inside the call.
- **while-no-input** does not yield; it "aborts them (working much like a quit)", returning t on input and nil on a real quit ((elisp) Event Input Misc). Fine for discardable computation, wrong for anything mutating wire state: no unwind rewinds a socket.

Verdict: a correct filter waits for nothing. Only top-level code and timers wait, and only on a specific PROCESS.

## 3. Timers

Timers run "only when Emacs could accept output from a subprocess ... inside certain primitive functions such as sit-for or read-event" ((elisp) Timers). Emacs "binds inhibit-quit to t before calling the timer function", so a slow timer body is worse than a slow filter: equally blocking, additionally unquittable, and fired on a schedule rather than by arriving data. The manual also warns timer bodies off sit-for, since "other timers ... can run while waiting". timer-max-repeats caps how many overdue invocations fire, so never use a repeating timer as a protocol clock; reschedule one-shots, as jsonrpc does per request. run-with-idle-timer fires only once Emacs goes idle: right for repainting a mirror, never for work a client awaits.

## 4. Threads

Concurrency is "mostly cooperative"; switches occur only at thread-yield, waiting for keyboard input or process output, mutex locking, condition-wait, and thread-join ((elisp) Threads). One global lock means zero parallelism: a thread cannot make Lisp faster, only relocate the block. Redisplay runs in the main thread, so a worker holding the lock through a long computation freezes the frame exactly as if you had never forked it.

Worse, thread-signal to the main thread "is not propagated there. Instead, it is shown as message", so worker errors go quiet; let bindings are thread-local and unreproducible by unwind-protect; and 31 adds thread-buffer-killed when another thread kills your buffer, unless you pass make-thread's new BUFFER-DISPOSITION (NEWS). The manual: "correct programs should not rely on cooperative threading."

Verdict: no threads in the server. The only well-behaved thread is one parked in accept-process-output on one process — which a filter gives you for free.

## 5. jsonrpc.el — client-only transport, role-symmetric protocol

jsonrpc-process-connection wraps one process "expected to contain a pre-established connection" ((elisp) Process-based JSONRPC connections). The library has no listen or accept concept; its only :server t is a throwaway port probe (jsonrpc.el:1222). But the message layer is symmetric: jsonrpc-connection carries -request-dispatcher and -notification-dispatcher, and jsonrpc-connection-receive has a remote-request branch that dispatches and replies (jsonrpc.el:264-330).

Two routes. A: own the listener and build one jsonrpc-process-connection per accepted client inside :log, since initialize-instance installs its own filter, sentinel, buffer and coding over the inherited ones (jsonrpc.el:591-628). B, the manual's "API for building JSONRPC transports" ((elisp) JSONRPC Overview): subclass jsonrpc-connection, specialize jsonrpc-connection-send, and call jsonrpc-connection-receive per whole message.

Pick B. jsonrpc-connection-send hardcodes LSP framing — Content-Length plus CRLF, no status line (jsonrpc.el:630-664) — and the filter hardcodes the matching regexp. Since MCP frames as newline-delimited JSON over stdio and as a real response over HTTP, that framing is replaced either way; if it matched LSP's, route A would suffice. Subclassing keeps the parts worth having: ids, continuations, timeouts, deferred requests, and the reentrancy control from bug#67945.

Hard rule: never call jsonrpc-request from a server. It spins (while t (accept-process-output nil 30)) at jsonrpc.el:509 and re-enters your filters. Use jsonrpc-async-request and jsonrpc-notify.

## 6. async.el

async-start forks a fresh Emacs, runs a printable form, and a sentinel reads the printed sexp back. Right for CPU-bound Lisp needing none of the parent's state and yielding one result; wrong for the wire and for streaming — the answer comes only at the end, the child has neither your buffers nor your init, and startup costs tens to hundreds of milliseconds.

## 7. Worth knowing in 31

- Native-comp's async compilation is the in-core precedent: background work by forking child Emacs processes and reaping them with sentinels, not by threading.
- detached.el survives a daemon restart — right for long agent shell runs, irrelevant to in-process protocol.
- aio.el, already in this profile, gives async/await promises over the existing event loop: no threads, no extra process.

## 8. The redisplay trap

"Emacs normally tries to redisplay the screen whenever it waits for input" ((elisp) Forcing Redisplay), so a freeze while the agent streams reduces to: Lisp is running and not returning. Per chunk, in a visible buffer:

- **font-lock / jit-lock** refontifies every arrival. Keep the wire buffer fundamental-mode and unibyte; fontify the mirror on a debounce or not at all.
- **buffer-undo-list** grows a record per insert. buffer-disable-undo on the wire buffer, buffer-undo-list t in the mirror.
- **markers and after-change-functions** scale with insert count, not bytes. Batch.
- **redisplay in a loop** does not keep the UI alive; it re-enters display code per chunk, usually the largest single cost.
- **auto-scroll** with point at end forces a full window update per insert. Follow point only when the buffer is displayed and point was at max.

Do not reach for inhibit-redisplay: its docstring says "This is used for internal purposes", and holding it across a long operation buys a frozen frame showing nothing.

The shape that works: the filter appends bytes and returns; a zero-delay timer drains all complete messages at once; the mirror updates from an idle timer in one insert, under inhibit-read-only with a single undo-boundary. Repaint once per batch, not once per packet.

## Libraries

Which third-party packages are worth depending on. Status is stars plus last push, via the GitHub API.

| Library | Gives you | Real mechanism | True parallelism | Status | Failure mode people hit |
|---|---|---|---|---|---|
| async.el (jwiegley) | async-start, async-let, async dired copy and byte-compile | Forks a child Emacs with -Q -batch, loads async.el, prin1's the result back base64-encoded | Yes — separate OS process | Alive: 913 stars, pushed 2026-07 | Child runs -Q, so none of your init exists; only printable values cross, so buffers, markers, processes and closures over the environment do not; full Emacs startup per call |
| aio (skeeto) | async/await, promises, aio-select, aio-sem | Built-in generator.el (not iter2, despite the common claim) plus callbacks; "they do not block any thread while paused" | No — interleaving on the one event loop | 248 stars, pushed 2026-02; the author treats it as finished, which is not abandoned | Errors surface at the await point, not the call site; tiny ecosystem, so you own every adapter |
| deferred.el / concurrent.el (kiwanami) | Deferred chains, semaphores, dataflow variables | Timers plus callbacks | No | 318 stars, last push 2022-05 | Four years cold; callback-chain stack traces; superseded by aio |
| promise.el and async-await.el (chuntaro) | Promises/A+, async/await over it | generator.el plus timers | No | 63 and 77 stars, last pushed 2021-03 and 2022-08 | Abandonware, and async-await stacks two stale packages |
| iter2 (doublep) | A faster, more correct generator.el | CPS transform at macroexpansion | No | 13 stars, pushed 2025-11; low stars because it is a substrate, not an app | Iteration only. It does nothing for I/O by itself |
| pfuture (Alexander-Miller) | "Futures" over external commands | make-process plus accept-process-output | Only the external command | 57 stars, 2022-09; used by treemacs | The async that is synchronous underneath: pfuture-await blocks the event loop until the process finishes |
| emacs-parallel (daimrod) | Parallel jobs across child Emacsen | Forked Emacsen over a socket | Yes | 5 stars, last push 2014 | Dead twelve years. A liability, not a dependency |
| detached.el (niklaseklund) | Shell sessions that outlive Emacs | dtach(1), an external program | The external command | Sourcehut-hosted and on MELPA; sourcehut 502'd this session, so last activity is unverified | Needs dtach installed; shell commands only, never elisp |
| plz.el (alphapapa) | Async HTTP | curl subprocess plus sentinels | The curl process | 236 stars, pushed 2025-03 | curl must exist; streaming needs the separate plz-event-source (3 stars) |

**1. A long CPU-bound elisp job.** async.el is the best packaged option and is genuinely parallel, because it is a real second Emacs. But it is one-shot-shaped: the child starts with -Q so your configuration does not exist, and you pay a full Emacs startup on every call. For a self-contained printable-in, printable-out function called occasionally, take it. For repeated work, a persistent child Emacs you own — the sidecar pattern already in use here — beats it outright, because the startup is paid once and the child can be configured.

**2. Many concurrent JSON-RPC conversations.** No library beats plain process filters. The evidence is gptel: 3535 stars, pushed this week, the most-used streaming client in Emacs, and it depends on no async library at all — it hand-rolls a state machine over curl process filters and url-retrieve. jsonrpc.el does the same. aio would buy readability, letting each session read as sequential code, at the cost of a generator layer and a dependency; it changes nothing about throughput, since it interleaves on the same event loop the filters already use.

**3. A real worker pool.** Nothing provides one. emacs-parallel is the only attempt and it died in 2014. Everyone hand-rolls, Emacs itself included: native-comp drives its own job pool through native-comp-async-jobs-number, and elpaca runs its own process queue. A pool is a short piece of code you own — a list of persistent child Emacsen, each with a filter, plus a queue and a dispatcher.

Verdict: take no library for the server. async.el is the single conditional dependency, and only for heavy one-shot elisp.
