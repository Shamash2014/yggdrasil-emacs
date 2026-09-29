# Backend / API

Drive endpoints from the repo's own fixtures, its OpenAPI spec, or
recorded request/response pairs (a cassette or fixture directory the
repo already has). Do not invent request bodies; use what the repo
records or documents.

## Contract regression

A regression is a previously passing request whose response changed
in a way the repo did not intend: HTTP status code differs, the
response fails the OpenAPI schema it used to satisfy, or a key field
(the fixture's or recorded pair's asserted fields) changed value or
type. Report each as a contract regression, with the before/after
value.

## Performance

p50 and p95 latency per endpoint against perf-baseline.json's "api"
key. SKILL.md's timing thresholds apply to p95: REGRESSION over 50%
slower or 500ms absolute, whichever hits first; WARNING over 20%. p50
is reported for context but is not itself gated. Error rate (non-2xx
over total requests) against the baseline's error_rate; no gstack
threshold exists for this field, so flag any increase and let the
report note the delta rather than auto-classifying REGRESSION/WARNING
(UNCONFIRMED as a threshold). Query count per request, where the
stack can report it (an ORM query log, APM span count, or similar):
same treatment, request-count's 30% WARNING line borrowed as a
starting point, UNCONFIRMED.

## Schemathesis

When schemathesis is already installed and the repo has an OpenAPI
spec, run it for property-style contract checks in addition to the
fixture replay above. When it is not installed, ask the owner before
installing it; never install silently. Read schemathesis's own
--help for the run syntax and flags on the installed version rather
than assuming one; flags differ between versions.
