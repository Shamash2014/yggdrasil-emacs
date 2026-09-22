---
name: create-pr
description: Prepare and open a pull request / merge request for the current branch, auto-detecting GitHub (gh) or GitLab (glab)
---

# Create PR / MR

Open a pull request (GitHub) or merge request (GitLab) for the current branch. Follow these steps in order, verifying each before moving on.

## Detect the host

1. `git config --get remote.origin.url` — read the remote.
2. If the URL contains `gitlab` (e.g. `gitlab.com`, self-hosted `git@gitlab.corp:…`) → **GitLab**, use `glab`.
3. Else → **GitHub**, use `gh`.
4. Verify the auth for whichever CLI: `gh auth status` / `glab auth status`. If it fails, report and stop. Do not try to swap CLIs silently.

## Preconditions

1. `git status` is clean — if there are uncommitted changes, surface them and stop. Ask the user whether to commit or stash before continuing.
2. Current branch is not `main` / `master` / `trunk`. If it is, stop and ask the user to create a feature branch first.
3. Remote exists (`origin`).

## Gather context

Run in parallel:
- `git rev-parse --abbrev-ref HEAD` — current branch
- `git config --get remote.origin.url` — already read above
- `git log --oneline origin/<base>..HEAD` — where `<base>` is `main` / `master` / the detected default branch
- `git diff --stat origin/<base>..HEAD`
- Existing-PR/MR check:
  - GitHub: `gh pr list --head <branch> --json number,url`
  - GitLab: `glab mr list --source-branch <branch> --output json`
  Skip creation if one already exists; print its URL.

## Push

If the branch isn't tracking a remote or is behind, `git push -u origin <branch>`. Never `--force` without the user's explicit approval.

## Title

- Pull from the first commit subject on the branch. Strip any trailing issue number.
- Keep under 70 chars. If you have to shorten, prefer cutting the scope qualifier over the verb.

## Body

Use a HEREDOC. Template:

```
## Summary
- <one bullet per meaningful commit, past tense, imperative verb dropped>

## Changes
- <file-area>: <short note>  (group commits by area, not by commit)

## Test plan
- [ ] <specific check the reviewer can run>
- [ ] <another check>

## Notes
<anything the reviewer needs to know: follow-ups, known gaps, risky assumptions>
```

Rules:
- Do not invent checks the code can't actually verify. If there are no tests, say so.
- Do not include the co-author trailer in the body (commits already carry it).

## Create

**GitHub:**
```
gh pr create \
  --title "<title>" \
  --body "$(cat <<'EOF'
<body>
EOF
)" \
  --base <default-branch>
```

**GitLab:**
```
glab mr create \
  --title "<title>" \
  --description "$(cat <<'EOF'
<body>
EOF
)" \
  --target-branch <default-branch> \
  --source-branch <current-branch> \
  --remove-source-branch \
  --squash-before-merge
```

Honor user flags:
- `--draft` / "draft" → `gh pr create --draft` or `glab mr create --draft` (GitLab renames to `--draft` in recent versions; old `--wip` is a fallback).
- Explicit base → `--base <x>` (gh) / `--target-branch <x>` (glab).

## Report

Print only the returned URL. Do not open a browser unless the user asked.
