# Emacs 31.1 audit — libs, layers, config

Profile: ~/emacs-31 on emacs-plus@31 31.1. Daemon e31, startup 0.35 s, 78 elpaca items (73 source repos), 19 layers, zero startup warnings.

## Native compilation — in use

native-comp-available-p is t, native-comp-jit-compilation is t, native-comp-speed is 2, async jobs auto. The eln load path is ~/emacs-31/eln-cache first, then the 31.1 system native-lisp dir, so the profile never writes into the 30.2 cache. Spot check: subr-native-elisp-p on yggdrasil-global-mode returns t, so config code really is running native, not byte-only.

Cache is 248 eln files, 32 MB, against 1154 in the 30.2 tree. That gap is only JIT laziness — packages compile as they are first loaded. To pay it all up front instead of in first-use pauses:

```
emacsclient -s e31 --eval '(native-compile-async (expand-file-name "elpaca/builds" user-emacs-directory) (quote recursively))'
```

## Libraries

All 78 queue entries reach installed on 31.1; no build failures remain. Two needed intervention, both recorded in the profile:

- docker-compose-mode is gone from MELPA. Pinned in layer-lsp.el to github meqif/docker-compose-mode. The 30.2 tree only still has it because that tree predates the removal.
- ghostel's native module will not compile under zig 0.16.0 (libc++ nullability error in zig's bundled string header while building ghostty's simd C++). The working dylib was copied from the 30.2 build; both trees are on commit 402ca63 and the module ABI carries across 30 to 31.

Now shipped by Emacs 31 core, so the elpaca copy is redundant or near-redundant:

- transient — core has lisp/transient.el. Magit generally wants a newer one, so keeping the elpaca build is the safe default, not a defect.
- compat — core ships lisp/emacs-lisp/compat.el, but it is explicitly a stub ("Stub of the Compatibility Library"). Packages needing real compat still need the elpaca build. Keep.
- which-key — core has lisp/which-key.el and the profile correctly installs only which-key-posframe on top.
- use-package, eglot, project, jsonrpc, so-long, xref, flymake are all core and are not duplicated by elpaca here.

Upstreams that have not moved in a long time, worth knowing before relying on them:

| last commit | package |
|---|---|
| 2019-02-08 | shrink-path |
| 2020-08-30 | docker-compose-mode |
| 2022-09-11 | ace-window |
| 2022-09-15 | spinner |
| 2023-03-13 | which-key-posframe |
| 2023-08-30 | rainbow-delimiters |
| 2024-01-31 | dired-single |
| 2024-06-29 | pcre2el |

which-key-posframe is the one to watch: it is the whole hint panel and it is three years stale. It touches only one which-key internal, which-key--buffer, and that still exists in 31.1, so it works today.

## Layers

All 19 load: completion, dired, terminal, browser, ui, editing, lsp, rass, http, sessions, astgrep, git, tasks, format, markdown, quickfix, pcre, dap, tramp.

Dropped as agent-related: layer-agent, layer-aob, layer-review, layer-task. Kept layer-tasks — that is the justfile and package.json runner, unrelated to the agent task system. ygg-doctor was removed too; layer-dap and layer-format now carry their own tables (ygg-dape-adapters, ygg-format-tools) and use executable-find, which is exactly what the doctor did for these entries since none declares a check command.

Layers report these tools absent from PATH. Environment gaps, not config faults:

- lsp: kotlin-lsp, jdtls, emmet-language-server, astro-ls, ngserver
- dap: debug_adapter.sh (Elixir), codelldb (Rust)
- format: stylua, shfmt, prettier

## Config code under 31

Byte-compiled the whole 30.2 lisp tree (108 files, agent modules included) with the 31.1 binary, in a scratchpad copy so nothing was written into the live tree.

Real findings, all warnings, nothing that breaks:

- when-let and if-let became obsolete in 31.1. Seven uses, in three files: ygg-doctor.el (3), ygg-root.el (2), ygg-serena.el (2). Replacements are when-let\* / and-let\* and if-let\*. None of these files is in the 31 profile.
- ygg-task-tree.el line 1957 uses magit-section-visibility-indicator, obsolete as of Magit-Section 4.5.0, now magit-section-visibility-indicators. This is a package deprecation, not a 31 one.
- layer-review.el lines 126 and 139 reference an unknown slot, parent, on a magit-section struct.
- 22 defface forms specify no containing group.

Ignore these: warnings reading "Malformed function", ":not called as a function", ":defaults called as a function" and "ghostel called with 6 arguments" all come from elpaca recipe forms in layer files compiled outside a session. init.el already treats layers as source-only for exactly this reason.

Verified afterwards that no 31-compiled elc landed in ~/.emacs.d/lisp.
