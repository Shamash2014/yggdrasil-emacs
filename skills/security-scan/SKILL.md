---
name: security-scan
description: Run installed SAST/secrets/dependency/DAST tools by stack, scoped to the change, and triage findings to file:line, severity, fix.
---

# Security scan

A repo whose skills/verify-* skill already carries a Security section runs those recorded commands first, scoped and triaged as below, before anything in this table.

Detect the stack from files present, then run only the matching, already-installed tools:

| file present | tool |
|---|---|
| any repo | secrets: gitleaks, trufflehog |
| package.json | SAST: eslint-plugin-security; deps: npm audit |
| requirements*.txt, pyproject.toml | SAST: bandit; deps: pip-audit |
| go.mod | SAST: gosec; deps: govulncheck |
| Cargo.toml | deps: cargo audit |
| Gemfile | SAST: brakeman |
| Dockerfile | trivy image/fs |
| *.tf | checkov |
| pubspec.yaml | deps: osv-scanner on pubspec.lock; SAST: semgrep dart rules, mobsfscan on lib/ |
| build.gradle(.kts), settings.gradle(.kts) | deps: osv-scanner on gradle.lockfile/verification-metadata.xml; SAST: detekt, semgrep kotlin rules, mobsfscan on app/src |
| Package.swift, *.xcodeproj, *.xcworkspace | deps: osv-scanner on Package.resolved; SAST: semgrep swift rules, mobsfscan on Sources/ |
| any repo, general | semgrep (local rules only, see below) |

Check each with which/--version before use. A tool not installed: name
it in the report as missing, with its install command, and skip it —
never install it yourself, never ask the owner to wait while you do.

Scope every scan to the changed files or the feature's directory when
the tool supports a path argument; a whole-repo scan is a fallback,
named as such.

Never send code to a third-party service without the owner's yes
first. semgrep --config auto, semgrep's cloud rulesets, and any
scanner's SaaS upload mode all count — flag them and get the yes, or
fall back to a local/offline ruleset.

DAST (ZAP baseline, nuclei) only against a running instance that is
local or the owner has named; never a host you were not told is theirs
to test.

Collect every tool's raw output to one file, then triage: dedupe
across tools, drop a false positive with the one-line reason it is
one, and for every real finding give file:line, severity, and a
one-line fix direction. Never fix anything here — a confirmed finding
is a new bugfix item.
