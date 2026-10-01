#!/bin/sh
set -eu

ice_dir=$(cd "$(dirname "$0")" && pwd)
rebaseline=""
if [ "${1:-}" = "--rebaseline" ]; then rebaseline=--force; shift; fi
root=$(git -C "${1:-.}" rev-parse --show-toplevel)
agents=${ICE_AGENTS:-claude,codex,pi}
wired=""
decide=""

kept=0
note() { case "$2" in *"(already"*) kept=$((kept + 1)) ;; *) wired="$wired
- [$1] $2" ;; esac; }
ask() { decide="$decide
- [ ] $1"; }
resolve() { mise which "$1" 2>/dev/null || command -v "$1" || { echo "ice-wire: $1 not found" >&2; exit 1; }; }

lat_bin=$(resolve lat)
openspec_bin=$(resolve openspec)
command -v python3 >/dev/null || { echo "ice-wire: python3 not found" >&2; exit 1; }

iso=$(mktemp -d "${TMPDIR:-/tmp}/ice-wire.XXXXXX")
trap 'rm -rf "$iso"' EXIT
mkdir -p "$iso/codex" "$iso/config" "$iso/cache" "$iso/data" "$iso/state"
if [ -f "$HOME/.config/openspec/config.json" ]; then
  mkdir -p "$iso/config/openspec" && cp "$HOME/.config/openspec/config.json" "$iso/config/openspec/"
fi
isolated() {
  env CODEX_HOME="$iso/codex" XDG_CONFIG_HOME="$iso/config" XDG_CACHE_HOME="$iso/cache" \
      XDG_DATA_HOME="$iso/data" XDG_STATE_HOME="$iso/state" \
      OPENSPEC_TELEMETRY=0 OPENSPEC_NO_COMPLETIONS=1 OPENSPEC_NO_AUTO_CONFIG=1 OPENSPEC_NO_UPDATE_CHECK=1 \
      PATH="$(dirname "$lat_bin"):$(dirname "$openspec_bin"):$PATH" "$@"
}

has() { [ -f "$root/$1" ] && grep -q -- "$2" "$root/$1"; }
copy_if_changed() {
  if [ -f "$2" ] && cmp -s "$1" "$2"; then return 1; fi
  mkdir -p "$(dirname "$2")" && cp "$1" "$2" && return 0
}
append_once() {
  grep -q -- "$2" "$1" 2>/dev/null && return 1
  [ -s "$1" ] && [ "$(tail -c 1 "$1")" != "" ] && printf '\n' >> "$1"
  printf '%s\n' "$3" >> "$1" && return 0
}

cd "$root"

lat_complete=yes
[ -d lat.md ] || lat_complete=no
case ",$agents," in *,claude,*) has CLAUDE.md 'lat:begin' && has .claude/settings.json 'lat hook claude' || lat_complete=no ;; esac
case ",$agents," in *,codex,*) has AGENTS.md 'lat:begin' && has .codex/config.toml 'mcp_servers.lat' || lat_complete=no ;; esac
case ",$agents," in *,pi,*) [ -f .pi/extensions/lat.ts ] || lat_complete=no ;; esac
if [ "$lat_complete" = yes ]; then
  note x "lat.md for $agents (already wired)"
else
  isolated python3 "$ice_dir/lat-init-agents.py" "$root" "$agents" > "$iso/lat-init.log" 2>&1 \
    || { cat "$iso/lat-init.log" >&2; echo "ice-wire: lat init failed" >&2; exit 1; }
  note x "lat.md for $agents: lat init ran"
fi

if [ -f openspec/config.yaml ]; then
  note x "OpenSpec (already initialised)"
else
  isolated openspec init --tools "$agents" --no-animation "$root" < /dev/null > "$iso/openspec-init.log" 2>&1 \
    || { cat "$iso/openspec-init.log" >&2; echo "ice-wire: openspec init failed" >&2; exit 1; }
  note x "OpenSpec for $agents: openspec init ran"
fi

if [ -d openspec/schemas/ice ] && diff -r -q "$ice_dir/schema" openspec/schemas/ice >/dev/null 2>&1; then
  note x "ice schema in openspec/schemas/ice (already current)"
else
  rm -rf openspec/schemas/ice && mkdir -p openspec/schemas && cp -R "$ice_dir/schema" openspec/schemas/ice
  note x "ice schema copied to openspec/schemas/ice"
fi
isolated openspec schema validate ice > "$iso/validate.log" 2>&1 \
  || { cat "$iso/validate.log" >&2; echo "ice-wire: ice schema invalid" >&2; exit 1; }

if grep -q '^schema: ice$' openspec/config.yaml; then
  note x "ice is the default schema (already)"
elif grep -q '^schema: spec-driven$' openspec/config.yaml; then
  sed 's/^schema: spec-driven$/schema: ice/' openspec/config.yaml > "$iso/config.yaml" && cat "$iso/config.yaml" > openspec/config.yaml
  note x "ice set as the default schema in openspec/config.yaml"
else
  ask "openspec/config.yaml names another default schema; pass --schema ice to openspec new change, or change it"
fi

if [ -d docs ]; then docs=docs; elif [ -d doc ]; then docs=doc; else docs=docs; fi
arch="$docs/arch"
if [ -e "$arch" ]; then
  note x "$arch C4 model (already present, left alone)"
else
  likec4_bin=$(resolve likec4)
  mkdir -p "$arch"
  cp "$ice_dir/c4/spec.c4" "$ice_dir/c4/model.c4" "$ice_dir/c4/views.c4" "$arch/"
  project=$(basename "$root" | tr '[:upper:]' '[:lower:]' | tr -c 'a-z0-9\n' '-')
  printf '{ "name": "%s" }\n' "$project" > "$arch/likec4.config.json"
  "$likec4_bin" export markdown "$arch" > "$iso/likec4.log" 2>&1 \
    || { cat "$iso/likec4.log" >&2; echo "ice-wire: likec4 export markdown failed" >&2; exit 1; }
  note x "$arch C4 skeleton created (TODO titles) and README.md generated"
fi

for tool in ice-check ice-archive-to-lat ice-c4-drift ice-lat-drift ice-scenarios ice-fail-on-base ice-lock ice-coverage ice-verify ice-compact ice-commit-gate; do
  if copy_if_changed "$ice_dir/$tool" ".ice/$tool"; then chmod +x ".ice/$tool"; note x ".ice/$tool installed"; else note x ".ice/$tool (already current)"; fi
done
runner() {
  python3 "$ice_dir/ice-runner" "$@" "$root" > "$iso/runner.out" 2>&1 \
    || { cat "$iso/runner.out" >&2; echo "ice-wire: ice-runner $1 failed" >&2; exit 1; }
  while IFS= read -r line; do
    case "$line" in
      "note: "*) note x "${line#note: }" ;;
      "ask: "*) ask "${line#ask: }" ;;
    esac
  done < "$iso/runner.out"
}
runner config

for pair in claude:.claude codex:.agents pi:.pi; do
  case ",$agents," in *",${pair%%:*},"*) ;; *) continue ;; esac
  for skill in "$ice_dir/../../skills/ice" "$ice_dir/../../skills/ice-checks" "$ice_dir/../../skills/ice-review-loop" \
               "$ice_dir/../../skills/ice-ui-review" "$ice_dir/../../skills/ice-prototype" "$ice_dir/../../skills/ice-learnings" \
               "$ice_dir/skills/likec4-dsl"; do
    dest="${pair#*:}/skills/$(basename "$skill")"
    if [ -d "$dest" ] && diff -r -q "$skill" "$dest" >/dev/null 2>&1; then
      note x "$dest (already current)"
    else
      rm -rf "$dest" && mkdir -p "$(dirname "$dest")" && cp -R "$skill" "$dest"
      note x "$dest copied"
    fi
  done
done

if [ ! -f lat.md/features.md ]; then
  printf '%s\n' "# Features" "" \
    "One section per user-visible feature: sub-features, how to get there, driving it and gotchas, each as a subsection: what and why, invariants and code anchors, never the code restated." \
    > lat.md/features.md
  note x "lat.md/features.md feature map created"
else
  note x "lat.md/features.md feature map (already present)"
fi
for seed in rules learnings; do
  if [ -f "lat.md/$seed.md" ]; then
    note x "lat.md/$seed.md (already present, left alone)"
  else
    cp "$ice_dir/lat/$seed.md" "lat.md/$seed.md"
    note x "lat.md/$seed.md seeded from etc/ice/lat"
  fi
done
python3 - "$root/lat.md" "$arch" "$docs" <<'EOF'
import os, re, sys
lat, arch, docs = sys.argv[1:4]

def write_if_changed(path, text):
    old = open(path, encoding="utf-8").read() if os.path.isfile(path) else None
    if old != text:
        open(path, "w", encoding="utf-8").write(text)

readme = os.path.join(lat, "..", arch, "README.md")
if os.path.isfile(readme):
    views = re.findall(r"^###\s+(.+?)\s*$", open(readme, encoding="utf-8").read(), re.M)
    slug = lambda t: re.sub(r"\s", "-", re.sub(r"[^\w\s-]", "", t.lower()))
    body = "".join("- [%s](../%s/README.md#%s)\n" % (v, arch, slug(v)) for v in views)
    write_if_changed(os.path.join(lat, "architecture.md"),
                     "# Architecture\n\nC4 views of this system, drawn by LikeC4 from %s into %s/README.md.\n\n%s"
                     % (arch, arch, body))

path = os.path.join(lat, "lat.md")
text = open(path, encoding="utf-8").read()
entries = {"features": "feature map, one section per user-visible feature",
           "changes": "archived OpenSpec changes, filed by ice-archive-to-lat",
           "architecture": "C4 views, one link per LikeC4 view",
           "rules": "do-not rules the reviewers cite, the owner's",
           "learnings": "lessons from owner feedback and recurring findings, pasted into briefs"}
missing = ["- [[%s]] — %s\n" % (k, v) for k, v in entries.items()
           if os.path.isfile(os.path.join(lat, k + ".md")) and not re.search(r"^- \[\[%s\]\]" % k, text, re.M)]
links = {"CONTEXT.md": "Glossary: [CONTEXT.md](../CONTEXT.md)",
         docs + "/adr": "Decisions: [%s/adr](../%s/adr)" % (docs, docs)}
extra = [v + "\n" for k, v in links.items() if os.path.exists(os.path.join(lat, "..", k)) and v not in text]
if missing or extra:
    lines = text.rstrip("\n").splitlines()
    sep = "\n" if lines and lines[-1].startswith("- [[") else "\n\n"
    text = text.rstrip("\n") + sep + "".join(missing) + ("\n" + "".join(extra) if extra else "")
write_if_changed(path, text)
EOF
note x "lat.md/lat.md indexes the feature map, rules and learnings, and lat.md/architecture.md links each C4 view"
if [ -f CONTEXT.md ]; then note x "CONTEXT.md linked from lat.md/lat.md"; else ask "CONTEXT.md: none yet; the first agreed term creates it (domain-modeling), then rerun ice-wire to link it"; fi
if [ -d "$docs/adr" ]; then note x "$docs/adr linked from lat.md/lat.md"; else ask "$docs/adr: none yet; the first decision that is hard to reverse, surprising and a real trade-off creates it"; fi
ask "$arch: replace the TODO titles and each container's metadata code and lat paths; rerun ice-wire after adding views so lat.md/architecture.md links them"

marker="ice-wire: commit gate, C4, lat check and ice-check"
gate_line=".ice/ice-commit-gate"
check_lines="$gate_line
likec4 validate --no-layout --json $arch
likec4 format --check $arch
.ice/ice-c4-drift $arch
likec4 export markdown $arch && git add $arch/README.md
lat check
.ice/ice-check repo"
placed=no
last_key=$(awk '/^[A-Za-z_][A-Za-z0-9_-]*:/ { key = $1 } END { print key }' .pre-commit-config.yaml 2>/dev/null || true)
if [ -f .pre-commit-config.yaml ] && [ "$last_key" != "repos:" ] && ! grep -q "id: ice-check" .pre-commit-config.yaml; then
  ask ".pre-commit-config.yaml: repos is not its last key, so ice-wire left it alone; add a local repo with these hooks in order: $(echo "$check_lines" | tr '\n' ';')"
  placed=yes
elif [ -f .pre-commit-config.yaml ]; then
  if grep -q '^- repo' .pre-commit-config.yaml; then pc_strip='s/^  //'; else pc_strip='s/^//'; fi
  if append_once .pre-commit-config.yaml "id: ice-check" "$(sed "$pc_strip" <<PRECOMMIT
  - repo: local
    hooks:
      - id: ice-commit-gate
        name: ice commit gate
        entry: $gate_line
        language: system
        pass_filenames: false
      - id: likec4-validate
        name: likec4 validate
        entry: likec4 validate --no-layout --json $arch
        language: system
        pass_filenames: false
      - id: likec4-format
        name: likec4 format check
        entry: likec4 format --check $arch
        language: system
        pass_filenames: false
      - id: ice-c4-drift
        name: C4 code paths exist
        entry: .ice/ice-c4-drift $arch
        language: system
        pass_filenames: false
      - id: likec4-readme
        name: regenerate $arch/README.md
        entry: sh -c 'likec4 export markdown $arch && git add $arch/README.md'
        language: system
        pass_filenames: false
      - id: lat-check
        name: lat check
        entry: lat check
        language: system
        pass_filenames: false
      - id: ice-check
        name: ice-check
        entry: .ice/ice-check repo
        language: system
        pass_filenames: false
PRECOMMIT
)"; then note x ".pre-commit-config.yaml: commit gate, C4, lat check and ice-check hooks added"
  else
    note x ".pre-commit-config.yaml: hooks (already present)"
    grep -q "id: ice-commit-gate" .pre-commit-config.yaml \
      || ask ".pre-commit-config.yaml already has ice-wire's hooks from an earlier run, without the commit gate; add by hand: - id: ice-commit-gate, entry: $gate_line, language: system, pass_filenames: false"
  fi
  placed=yes
fi
if [ -f .husky/pre-commit ]; then
  if grep -q "ice-wire:" .husky/pre-commit; then
    if grep -qxF "$gate_line" .husky/pre-commit; then
      note x ".husky/pre-commit: lines (already present)"
    else
      ask ".husky/pre-commit already has ice-wire's lines from an earlier run, without the commit gate; add by hand: $gate_line"
    fi
  elif append_once .husky/pre-commit "$marker" "# $marker
$check_lines"; then note x ".husky/pre-commit: lines added"
  else note x ".husky/pre-commit: lines (already present)"; fi
  placed=yes
fi
if [ -d .github/workflows ]; then
  if [ -f .github/workflows/ice.yml ]; then
    note x ".github/workflows/ice.yml (already present)"
  else
    cat > .github/workflows/ice.yml <<EOF
name: ice
on: [push, pull_request]
jobs:
  ice:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - uses: actions/setup-node@v4
      - run: npm install -g lat.md@0.12.2 likec4@1.59.3
      - run: likec4 validate --no-layout --json $arch
      - run: likec4 format --check $arch
      - run: .ice/ice-c4-drift $arch
      - run: lat check
      - run: .ice/ice-lat-drift
      - run: .ice/ice-check repo
EOF
    note x ".github/workflows/ice.yml added"
  fi
  placed=yes
fi
if [ -f lefthook.yml ] || [ -f lefthook.yaml ]; then
  ask "lefthook: add these pre-commit commands in order (not edited by ice-wire): $(echo "$check_lines" | tr '\n' ';')"
  placed=yes
fi
if [ "$placed" = no ]; then
  while IFS= read -r line; do
    case "$line" in
      "note: "*) note x "${line#note: }" ;;
      "ask: "*) ask "${line#ask: }" ;;
    esac
  done <<HOOKOUT
$(printf '%s\n' "$check_lines" | "$ice_dir/ice-hook-install" "$root" "$marker" "$gate_line")
HOOKOUT
fi

runner baseline $rebaseline

ask "lat init gitignored .claude, .codex, .pi and .mcp.json (they hold local paths); that also hides OpenSpec's skills and commands there from git. Keep or un-ignore"
ask "lat hooks and MCP call 'lat' from PATH; confirm each agent's environment (Emacs daemon, CI) finds the mise shim"
ask "semantic search uses lat's local model; set LAT_LLM_KEY for hosted embeddings, or leave it"
ask "who writes the scenario checks: you, or a spec-only session you approve and lock"
ask ".ice/ice-check repo in pre-commit blocks every commit while an active change has an incomplete intent.md, expectations.md or tasks.md; keep that gate or drop the line"
ask "expectations.md is owner-only by instruction; once the owner confirms a change's checks, the owner runs .ice/ice-lock CHANGE lock, which tags them (signed when user.signingkey is set) with the .ice scripts, .ice/config, intent.md, conftest.py and runner configs; a re-lock needs ICE_LOCK_OWNER=1 and --force. Running ice-verify itself is not in pre-commit or CI (tests are slow for a hook, and CI checkouts lack the tag); it stays the lead's. .ice/ice-commit-gate is in pre-commit (installed above): it reads .ice/ledger.tsv and refuses a commit touching a locked change's files with no unit-verified or live-verified row for that exact content; set commit_gate = off in .ice/config to turn it off, or --no-verify to bypass one commit"
ask "rerunning ice-wire after etc/ice changes updates the .ice scripts, so every locked change fails its lock until the owner re-locks it (ICE_LOCK_OWNER=1 .ice/ice-lock CHANGE lock --force)"
ask ".ice/state and .ice/evidence hold per-run reports and logs; gitignore them or keep them. .ice/locks and .ice/ledger.tsv are the lock copy and the verdicts"

# decisions repeat every run; print only the ones the last run did not list
previous=""
[ -f .ice/decisions.md ] && previous=$(cat .ice/decisions.md)
printf '# ICE decisions for the owner\n%s\n' "$decide" > .ice/decisions.md
fresh=$(printf '%s\n' "$decide" | while IFS= read -r line; do
  [ -n "$line" ] || continue
  printf '%s\n' "$previous" | grep -Fqx -- "$line" || printf '%s\n' "$line"
done)
[ -n "$wired" ] || wired="
- nothing new"
printf 'ICE wiring for %s\n\nWired:%s\n- %d already current\n\nOwner decides (new since the last run; all in .ice/decisions.md):\n%s\n' \
  "$root" "$wired" "$kept" "${fresh:-- nothing new}"
