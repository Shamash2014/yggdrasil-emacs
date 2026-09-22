# The UI measured against the monospace TUI system

Measured live in the e31 daemon, 2026-09-22: modus-vivendi, ground #080808, JetBrains Mono 13
at 8x17 px, global line-spacing 5 px. Colours are face-attribute resolved (nil t), deltas are
per-channel counts off the ground, chroma is the max-min channel spread (0 = untinted), and
pixel heights come from buffer-text-pixel-size over the real render functions — the one live
trace buffer held 118 bytes. The grayscale increment is #0A = 10 counts.

## 1. Sidebar — lisp/ygg-projects.el

| System rule | Measured | Verdict | Fix |
|---|---|---|---|
| sidebar 8-20 cols | 34 | deliberate | — |
| selected row, reverse video, full width | hl-line overlay, ygg-projects-current #1c1c1c, +20, chroma 0, extend t | deliberate (fill, not inverse); +20 clears the #0A floor | — |
| one body fill only | ygg-projects-card sets :extend t, no fill | conforms | — |
| neutrals untinted | chroma 0 throughout | conforms | — |
| value 1 cell before the edge | ygg-projects--right holds back 2 (line 364) | **ACCIDENTAL** | (+ 2 ...) to (1+ ...); measured overflow is +0 px |
| nothing paints the ground | ygg-projects-label resolves bg #080808 via :inherit default (measured); hl-line is an overlay so it still wins today | **ACCIDENTAL**, latent: any text-property fill under it gets punched through, which is how ygg-projects-card is applied | drop :inherit default (line 99); lines 797-799 warn against exactly this |
| list rows, 0 gap | line-spacing 0, consecutive | conforms | — |
| gutter always occupied | dot or icon on every row | conforms | — |
| scale 0 1 2 3 4 6 8 | head 1+dot+2, rows 4+icon+3 | conforms | — |
| U+22EF in data cells | name truncates trailing U+2026 (line 405) | **ACCIDENTAL** | pass "⋯" as the ellipsis arg |
| U+2026 leading in paths | "…/" (line 408) | conforms | — |
| no size axis | base :height 1.05 → 18 px line, advance still 8.00 px | **ACCIDENTAL**: buys 1 px, no width | drop it |
| no visible border | divider and internal-border = #080808 = ground | deliberate, working | — |
| — | ygg-projects-gutter #000000, -8, 10 px right fringe | **ACCIDENTAL**: below the step so invisible, and against the no-borders ruling | #080808 |
| green healthy | success = #7e9cd8: live dot and open bar are blue | **ACCIDENTAL**, minor — or a palette choice outside the enumerated list | confirm the blue is intended before touching it |
| max 2 attributes | current row name: bold + fill (+ size) | conforms in SGR | — |

## 2. Trace — lisp/agent-objects/aob-trace.el

| System rule | Measured | Verdict | Fix |
|---|---|---|---|
| prose stays monospace | aob-trace-prose = SF Pro Text 1.05, 0.864x mono on a sentence | deliberate | — |
| no size axis | aob-trace-icon 1.6 → 32 px row | deliberate | — |
| no size axis | aob-trace-done 0.8 → 18 px row | deliberate | — |
| tool rows are table rows, no fill | aob-trace-card #1a1a1a, +18, extend t | **ACCIDENTAL**, the worst self-contradiction here: the sidebar dropped its card fill by ruling, the trace kept one | drop the fill as the sidebar did; at minimum #1c1c1c |
| neutrals untinted | aob-trace-comment #1d2027, chroma 10 | **ACCIDENTAL**: the last tinted neutral | #1c1c1c |
| — | aob-trace-anchor #3a3222 + underline | conforms: semantic, 2 attributes | — |
| value 1 cell before the edge | (- right (1+ ...)) at line 334 | conforms, and disagrees with the sidebar | — |
| gutter 2 cells (3 and 4 legal) | left-margin-width 4, margins (4 . 12), text held to 78 | conforms | — |
| collapsed thinking = dim only | aob-trace-aside = shadow alone | conforms; the doc's named violation is gone | — |
| truncate output with U+22EF | explore fold trailing U+2026 (650); --bound leading (212) | **ACCIDENTAL** for the fold, a data cell | "⋯" at line 650 |
| glyph followed by a word | ✓ ✗ ⟳ … bare; only queued and cancelled carry words | **ACCIDENTAL**, minor | — |
| one ellipsis family | aob-trace--status mixes "…" and "⋯" (233-234) | **ACCIDENTAL** | one character |
| at most 1 blank row between sections | paragraph-space 0.9 (15.3 px) lands on every newline, not on paragraph breaks (444-448) | **ACCIDENTAL**: a five-line block measures 145 px against an intended 105, 38% inflation, against a docstring that says "under a paragraph break" | gate on the next character also being a newline |
| — | code spans render in SF Pro Text: markdown-inline-code-face has no :family, and fenced blocks get that face too | **ACCIDENTAL**: shell commands and paths drawn proportional | give code a :family from default in the trace |
| max 2 attributes | measured prose+bold, prose+code, prose+markup — none over 2 SGR | conforms; every over-count is a :height | — |

## 3. Reopened session — lisp/aob-transcript.el

No face, layout, spacing or truncation of its own: it reads the CLI jsonl, builds an ordinary
aob session and hands it to aob-trace-buffer, so section 2 applies verbatim. It never
streams, so aob-trace-live-markdown-max never fires.

## The four open questions

**Elevation.** Neutral fills against the ground: -8 #000000 (sidebar gutter); +7 #0f0f0f
(mode-line, tab-bar); +10 #121212 (floats); +15 #171717 (hl-line); +18 #1a1a1a (trace card);
+20 #1c1c1c (active sidebar row); +21 #1d2027 (trace comment, tinted); +22 #1e1e1e
(header-line); +24 #202020 (region). Direction is up, and forced rather than sloppy: #080808 is three steps off black, so
there is no room to go down the way #1c1c1c goes down from #262626 — settled. Step size is
the failure: no two adjacent rungs are a full #0A apart, the gaps running 3, 5, 3, 2, 2, 2.
Eight rungs where the system ships one, and only +20 is a clean lift off the ground. The
"float #0f0f0f is a non-step" complaint was fixed for the float (now #121212, exactly #0A)
and left standing on the mode line.

**Proportional font plus :align-to.** Measured: no visible misalignment. The only :align-to
in a prose buffer is aob-trace--card line 334, and both its body and its meta are
default-family monospace, so the arithmetic is exact. In the sidebar, base at 1.05 still
measures 8.00 px/char against a frame column of 8 px, so ygg-projects--right overflows by
+0 px at 6, 10, 14 and 20 characters. The other three numeric uses (aob.el 367, aob-acp.el
512, 1809) are monospace minibuffer annotations. The prose font costs elsewhere: prose rows
measure 21 px against 22 for mono, a 1 px jitter between a message and the card under it,
and code spans drawn proportional.

**Rhythm.** The scale is coherent: app default 5 px, trace 0.3 resolves to 5.1, 0.9 to 15.3 —
exactly 3x, and 0/1/3 sit on the permitted 0 1 2 3 4 6 8 scale. The sidebar's 0 is correct
list density, not an outlier. You never see the scale because the paragraph unit lands on
every newline and a 1.6 icon makes any row with a glyph 32 px against 21-22. Measured rows:
sidebar 18, prose 21, mono 22, card 22, done 18, icon 32.

**Truncation.** U+22EF appears nowhere as a truncation mark; its one use is the queued status
glyph. Everything truncates with U+2026 — sidebar name and explore fold trailing,
aob-trace--bound leading. Paths conform on both surfaces. One ASCII three-dot survives at
yggdrasil-verbs.el 49, outside these surfaces.

**Two attributes.** Nothing exceeds it, counting the way the system counts: weight, slant,
underline and inverse plus colour, with :height excluded because the system has no size axis
to count against. Measured composites are prose+bold, prose+code and prose+markup, each one
SGR over the prose family. The named violation, a dim-bold-coloured thinking header, is now
dim alone. Nearest: aob-trace-anchor (background plus underline) and nerd-icon spans.

## Deliberate — noted, not reopened

Icons 1.6 and done 0.8: no size axis in the system, both would be Labels; overridden. Prose
in SF Pro Text: the system would keep prose monospace or forbid column alignment inside it —
we keep prose and confine :align-to to monospace rows, the second half of its own advice.
Fill on the active row only, no card fill: the ruling stands, the sidebar follows it, the
trace does not. Sidebar 34
columns: outside the 8-20 band, kept. No visible borders: every ring resolves to the surface
it borders.

## Ranked, cheapest first

1. **One ellipsis family in one status set.** aob-trace.el 234: "…" becomes "⋯". One character.
2. **Collapse the two raised surfaces 2 counts apart.** aob-trace.el 310: #1a1a1a becomes
   #1c1c1c. One hex. The principled version is that edit with :background removed, so the two
   surfaces agree rather than merely match.
3. **Clear the last sub-step neutral.** init.el mode-line #0f0f0f becomes #121212, the value
   the float already moved to; the +7 rung leaves the ladder.
4. **Gate the paragraph space.** aob-trace.el 444-448: mark only a newline whose next
   character is also a newline. 145 px to 105 px on a five-line block, and the largest
   measured effect on this list.
5. **Make the two right edges agree.** ygg-projects.el 364: (+ 2 (string-width text)) becomes
   (1+ (string-width text)), matching aob-trace--card and the 1-cell rule.

Then: drop :inherit default from ygg-projects-label, untint aob-trace-comment, give code
spans a monospace :family.

Fourteen accidental divergences: six in the sidebar, seven in the trace, one shared (the mode
line). None in the reopened session that is not already a trace row.
