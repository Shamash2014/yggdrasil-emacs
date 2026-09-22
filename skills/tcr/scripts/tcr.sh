#!/usr/bin/env bash
# TCR: test && commit || revert
#   tcr.sh "<test cmd>"        run one cycle
#   tcr.sh                     autodetect test cmd, run one cycle
#   tcr.sh --watch "<cmd>"     run a cycle on every file save
# Env: TCR_MSG sets the commit message (default "tcr").
set -uo pipefail

WATCH=0
if [ "${1:-}" = "--watch" ]; then WATCH=1; shift; fi

detect_test_cmd() {
  if [ -f tests/run.sh ] && [ -f init.lua ]; then echo "bash tests/run.sh"; return; fi
  if [ -f package.json ] && grep -q '"test"' package.json; then echo "npm test --silent"; return; fi
  if [ -f justfile ] || [ -f Justfile ]; then echo "just test"; return; fi
  if [ -f Cargo.toml ]; then echo "cargo test"; return; fi
  if [ -f mix.exs ]; then echo "MIX_ENV=test mix test"; return; fi
  if [ -f go.mod ]; then echo "go test ./..."; return; fi
  if ls ./*.csproj >/dev/null 2>&1 || ls ./**/*.csproj >/dev/null 2>&1; then echo "dotnet test"; return; fi
  if [ -f pyproject.toml ] || [ -f pytest.ini ] || [ -f setup.cfg ]; then echo "pytest -q"; return; fi
  echo ""
}

TEST_CMD="${1:-$(detect_test_cmd)}"
if [ -z "$TEST_CMD" ]; then
  echo "TCR: no test command given and none detected. Pass one: tcr.sh \"<cmd>\"" >&2
  exit 2
fi

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "TCR: not inside a git repository." >&2
  exit 2
fi

cycle() {
  echo "TCR ▶ $TEST_CMD"
  if eval "$TEST_CMD"; then
    if git diff --quiet && git diff --cached --quiet && [ -z "$(git status --porcelain)" ]; then
      echo "TCR ✓ green, nothing to commit"
      return 0
    fi
    git add -A
    git commit -m "${TCR_MSG:-tcr}" --quiet
    echo "COMMITTED ✓ $(git rev-parse --short HEAD)"
    return 0
  else
    git reset -q --hard HEAD
    git clean -fdq
    echo "REVERTED ✗ tree restored to $(git rev-parse --short HEAD) — make a smaller change"
    return 1
  fi
}

if [ "$WATCH" -eq 0 ]; then
  cycle
  exit $?
fi

echo "TCR watch ▶ saving any file triggers: $TEST_CMD   (Ctrl-C to stop)"
if command -v fswatch >/dev/null 2>&1; then
  fswatch -o -r --exclude '\.git' . | while read -r _; do cycle; done
elif command -v inotifywait >/dev/null 2>&1; then
  while inotifywait -qr -e modify,create,delete --exclude '\.git' .; do cycle; done
else
  echo "TCR: no fswatch/inotifywait — polling every 2s"
  LAST=""
  while true; do
    NOW=$(find . -path ./.git -prune -o -type f -newer /tmp 2>/dev/null -print 2>/dev/null | git hash-object --stdin 2>/dev/null || echo x)
    SNAP=$(git status --porcelain)
    if [ "$SNAP" != "$LAST" ]; then LAST="$SNAP"; cycle; fi
    sleep 2
  done
fi
