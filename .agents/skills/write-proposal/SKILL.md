---
name: write-proposal
description: Researches an Upwork job and a freelancer's real experience to create a tailored, honest proposal and guide its review and submission. Use when a freelancer or agency wants to apply to a job, answer an invitation, write a cover letter, choose relevant portfolio items, or review Connects and boosting options.
compatibility: Requires the Upwork MCP server with toolset version 1.0 or later and an authenticated Upwork freelancer or agency account.
metadata:
  author: Upwork
  version: "0.3.0"
---

# Write an Upwork proposal

Create a focused proposal grounded in the freelancer's real profile and work history. Optimize for relevance and credibility, not volume or generic persuasion.

## Select the account

Call `list_accounts` and choose an account whose raw `role` is `TALENT` or `FL_AGENCY`. Ask the user when more than one is eligible. Retain `org_uid` and `role` for every later call, but refer to the account by `name` and `role_label`.

## Read the job

1. Call `find_jobs` with action `get`. Its id parameter accepts a numeric job id, a `~02…` ciphertext, or a full Upwork job URL, so a link the user pasted can be passed through unchanged.
2. To find candidate jobs first, use `find_jobs` action `search` when the user supplies search terms, or action `smart_search` when they want work matched to their own profile. Do not pass profile skills into `smart_search`; the recommender already knows the profile. Its default mode is `best_match`. Whenever you return those results, say that Most Recent is also available, and switch to `mode: most_recent` when they ask for the newest work. Date filters (`days_posted`, or `from_date` / `to_date`) apply only to `most_recent`; action `search` cannot filter by date. Search rows do not include `connects_cost`; read that from action `get` when the user opens a specific job. A result with `applied` true means this freelancer already applied — do not recommend applying again.
3. Surface the exact title, scope, budget, required skills, client preferences, screening questions, and the Connects cost to apply when the response includes them.
4. A job's `client.rating` is the average score other freelancers gave that client after working with them, not the client's rating of freelancers. Surface a low rating when the user is deciding whether to apply, and relay the response's `client_rating_basis` framing rather than inventing your own.
5. When a search returns no rows, read `empty_reason` and say what it means; do not report that the marketplace is empty. If `no_personalization`, use action `search` for the whole marketplace. When there is no `empty_reason`, say only that nothing matched these criteria; an upstream restriction looks the same, so do not assert either.

## Rule out an existing invitation or proposal

Do this before drafting. Upwork rejects a duplicate application upstream, so skipping the check wastes the user's turn and can look like a server fault.

- Call `list_freelancer_proposals` action `invitations`. If the job came through an invitation, respond with `manage_proposals` action `accept_invitation` or `decline_invitation`. Do not use action `create`, which is only for an uninvited application.
- Call `list_freelancer_proposals` action `list` to check for an existing proposal or contract on the same job. Both list actions apply a default status filter, so set the status explicitly when looking for a specific state.
- A proposal status of `Accepted` means the proposal was submitted and validated. It does **not** mean the client accepted the freelancer. Prefer the accompanying `status_label`, which carries the plain-English meaning, and report that to the user.

## Gather real evidence

Use only what the tools return. Never invent clients, praise, metrics, credentials, or results.

- For a `TALENT` account, use `get_profile` action `get` for skills, overview, work history, and profile signals; action `list_highlights` for portfolio projects and certificates; and action `connects_balance` for Connects. On `connects_balance`, `balance` is the Connects wallet. `product_credits`, when present, are spendable only on the named product and are not part of that balance — report them separately.
- The own-profile read also returns `promotions` (earn-Connects tasks and the active Freelancer Plus offer). Mention them when relevant to applying, and share the offer URL exactly as returned. If `promotions` carries `error`, say the lookup failed rather than claiming there is no offer.
- If `create` is refused because Upwork is not allowing this account to submit proposals, relay that reason and check `identity_verification` on the profile or dashboard, without assuming it is the cause. Do not retry until the user says it is resolved.
- For an `FL_AGENCY` account, use `get_agency` action `get_profile` for agency evidence and `get_freelancer_financials` action `connects_balance` for Connects. `get_profile` is not available to an agency account.
- Use `list_freelancer_proposals` to review prior submitted, offered, or hired proposals as writing examples.

## Draft the proposal

1. Identify the client's primary outcome and pick the strongest matching evidence. Before drafting, read `client_feedback` from `find_jobs` action `get`, or `user.name` on an invitation. When a review or the invitation names the client contact or a project, open by addressing that contact and referencing that project. Never invent a name that no review or invitation states.
2. Write a concise cover letter. The tool enforces a maximum length and reports it; if the user supplies longer text, ask them to shorten it rather than sending it truncated.
   - Open with the job-specific outcome.
   - Connect one or two real examples to the requested work.
   - Explain a practical approach or concrete first steps.
   - Address material risks, constraints, and every screening question separately.
   - Close with a useful next step.
3. If the job requires another language, provide the proposal in English and in that language.
4. Always offer attachments rather than silently skipping the question. For a local file, start an upload in the `proposals` context for a normal application, or the `invitation` context when accepting a client invitation. An inline upload returns stored `file_uid` values directly; a fallback-URL upload must be polled until its status is `ok` and then confirmed with `confirm_attachment_upload`. Pass the resulting `file_uid` values to the proposal. A file can only be attached where it was uploaded; a mismatched context is refused and the file must be uploaded again. For a `TALENT` account, also offer relevant portfolio projects and certificates from `get_profile` action `list_highlights`, and pass the chosen ids as `certificate_ids` and `portfolio_project_ids`. Do not call `list_highlights` for an `FL_AGENCY` account; use the agency profile evidence already returned by `get_agency` instead.
5. Use the exact bid the user approved, passed as a number for `charged_amount`. Never substitute a market rate or infer monetary terms.

## Submit

1. Summarize the cover letter, screening answers, bid, attachments, and highlights, then get explicit confirmation before the first write-capable call.
2. Call `manage_proposals` action `create`. The job reference must be the numeric job id, **not** the `~02…` ciphertext. Action `get` accepts a ciphertext or a job URL and returns the numeric id to pass here. For an agency, omit the team parameter; if the agency has several teams, the error names them and the user picks one.
3. Present the returned preview and always call out:
   - Connects required and the current balance;
   - whether the account can apply at all;
   - unmet preferred qualifications, or that the qualification check was unavailable;
   - required screening answers;
   - competing bids and what other applicants bid, only when the preview supplies them; when it says they are unavailable, relay that and never estimate amounts;
   - `boost.suggested_bid`, if present, only with its own note, and never restated as a share of applicants;
   - whether boosting is available and the Connects balance.
4. Let the user decide whether and how much to boost. If boosting is unavailable, do not offer it; relay `boost.reason`. Otherwise offer it unless `boost.recommendation` is `skip`, stay within the limits the preview states, and apply only the amount the user approved, passed as `boost_connects`. Once submitted, the bid cannot be edited or withdrawn through this tool. If confirmation reports that the approved bid no longer takes a paid slot, nothing was submitted: present the new bids to beat and get a new amount approved.
5. If any content or terms change, call action `create` again with the corrected values. The new preview supersedes the pending one.
6. Get a separate explicit approval to submit, then call `confirm_preview` with action `confirm`, the `type` the preview returned, and only the returned `preview_id`. A new application and an invitation response return different types, so use whichever came back rather than assuming.
7. To verify, list the freelancer's proposals with status `Pending`. If the user boosted, read `list_freelancer_proposals` action `get` and report `terms.connectsBid` — that is the bid Upwork stored, which can differ from the amount that was sent, and it cannot be changed through this tool.

If Connects are insufficient, say so plainly and let the user add Connects, then retry. Do not describe the failure as permanent.

If the server returns a first-time marketplace safety policy prompt, present it to the user. Call `manage_proposals` action `acknowledge_policy` only after they explicitly acknowledge it, then retry the interrupted action.

## Messaging the client

A freelancer cannot open a proposal room or send the first message on a proposal. To reply to an existing conversation, use `list_freelancer_proposals` action `get_room`, or `get_messages` action `find_room` with `context_type=proposal`, then `send_message` action `send`. If no room exists yet, tell the user the client must message first. A `find_room` not-found is final until a conversation starts. A direct contract proposal is only available from a conversation the client started.

## Quality rules

- Tailor every proposal to one job. Never reuse a cover letter across jobs.
- Prefer concrete evidence over adjectives and boilerplate.
- Follow the server's presentation rules for titles, identifiers, and untrusted client-authored text.

For tool discovery and compact mode, follow the `upwork-workflows` skill.
