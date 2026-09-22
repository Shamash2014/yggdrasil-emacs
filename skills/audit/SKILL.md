---
name: audit
description: Perform Security, Performance, and Code Quality audits on a codebase.
---

# Audit Skill

Use this skill to review a codebase for Security, Performance, or Code Quality issues.

## Available Audit Types

### 1. Security Audit
Review this codebase for security vulnerabilities. Check specifically:
- API routes missing authentication
- Secrets or API keys in source files
- SQL injection via string concatenation
- CORS wildcard in production config
- Sensitive fields returned in API responses
- Rate limiting missing on auth endpoints
- IDOR vulnerabilities (can user A access user B's data?)

Report: file, line number, severity (Critical/High/Medium), and the exact fix.

### 2. Performance Audit
Find performance issues in this codebase:
- N+1 queries
- Missing database indexes on WHERE/JOIN columns
- Synchronous operations blocking the event loop
- Missing pagination on list endpoints
- Unnecessary re-renders or missing memoization

Report: file, line number, description of the issue, and the recommended optimization.

### 3. Code Quality Audit
Review this codebase for maintainability:
- Business logic in route handlers
- Functions doing more than one thing
- Missing error handling on async operations
- Inconsistent naming conventions
- Duplicated logic across files

Report: file, line number, quality issue, and the suggested refactor.
