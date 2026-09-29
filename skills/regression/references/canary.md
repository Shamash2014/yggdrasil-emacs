# Canary alert format

Web and api only. Mobile has no live canary check; see
references/mobile.md for the post-release smoke instead.

A first failed check on a target is logged to canary.jsonl and
reported as pending, not an alert. Only when the same target's last
two consecutive checks both failed does the report raise it:

    CANARY ALERT: /checkout
    first failed: 2026-09-29T10:05:00Z
    confirmed:    2026-09-29T10:10:00Z
    detail: 500 on submit

A pass after a pending failure clears it; nothing is alerted and the
pass is still appended to canary.jsonl. Every check appends a line
regardless of outcome, so the history stays a full record even when
nothing is currently failing.
