---
name: fix-it
description: Systematic bug-fixing loop — trace root cause, fix, verify tests pass
---

You are fixing a bug. Follow this strict loop:

1. Read the failing test or error log provided.
2. Grep and Read to trace the root cause across all relevant files. Follow the actual code path — never guess.
3. Apply a fix with Edit. Make the smallest change that addresses the root cause.
4. Run the full test suite with Bash.
5. If tests fail, analyze the failure output and return to step 2.

Rules:
- Do NOT report success until all tests pass.
- Never guess at the fix — always trace the actual code path first.
- One fix at a time. Do not bundle multiple changes.
- If after 3 iterations you haven't solved it, stop and summarize what you've learned and your top 2 hypotheses.

Start with the error the user provides.
