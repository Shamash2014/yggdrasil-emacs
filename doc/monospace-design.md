# Monospace Design TUI, applied to Emacs buffers

Source: github.com/coreyt/monospace-design-tui v0.3.0. Substantive on archetypes,
spacing, elevation, colour and form widgets; silent on prose, avatars, transcripts and
collapsible disclosure. INFERRED items are mine, not shipped. Bare `§` refs are
`monospace-tui-design-standard.md`; `§R` refs are `monospace-tui-rendering-reference.md`.

## Token table

| Token | Value (source) | Emacs mechanism |
|---|---|---|
| `space-scale` | 0 1 2 3 4 6 8 cells; 5 and 7 forbidden (§1.2) | every gap, pad, indent |
| `gap-1`/`gap-0` | 1 blank row around a section; 0 within a list | bare `"\n"` / consecutive lines |
| `pad-inline` | 1 cell inside a border (§6.3) | `(space :align-to 1)` |
| `pad-cell` | 1 cell each side of a table cell (`measurements.md`) | `(space :align-to (- right 1))` |
| `gap-shortcut` | 2 cells before right border, menus only (§R4.9) | `(space :align-to (- right 2))` |
| `indent-level` | 2 cells per level — INFERRED from the marker gutter | `line-prefix` |
| `sidebar` | 8-20 cols; 12-16 at 120-159 (§1.3, §1.6) | `window-width` |
| `ground` | 235 `#262626` "Neutral bg" (`color-palette.md`) | frame `:background` |
| `surface` | 234 `#1c1c1c` "Surface" — one step **darker** | `:background` + `:extend t` |
| `emphasis-bg` | 236 `#303030` secondary/tertiary role bg. **Not an elevation token** | `:background` + `:extend t` |
| `fg`/`bright` | 252 `#d0d0d0` / 231 `#ffffff` | `:foreground` |
| `dim` | SGR 2; inactive 240 `#585858` | `:foreground`, never `:height` |
| `ok/warn/err` | 40 `#00d700` / 220 `#ffd700` / 196 `#ff0000` | `:foreground` |
| `selected` | reverse video SGR 7, full row | `:inverse-video t` + `:extend t` |
| `ellipsis` | U+2026 leading, U+22EF midline (§R1.7) | `truncate-string-to-width` |

## 1. Spacing

The atomic unit is one character cell (`standard/layout.md`). Permitted scale
`0 1 2 3 4 6 8`; intermediate values "MUST NOT be used" (§1.2) — no 5-cell indent, no
half step. Border padding is 1 cell each side (§6.3). List and table rows are
consecutive with **zero** blank rows between them ("0-row vertical gap"). Sections are
divided by a rule line with at most 1 blank row either side. Nothing uses 2 blank rows.

## 2. Type scale and weight

Confirmed: hierarchy is weight, case and dimming only, no size axis. Exactly **four**
treatments (`typography.md`) — Display (bold + optional UPPERCASE) for titles and hero
metrics; Title (bold) for section headers; Body (plain) for content; Label (dim) for
secondary text. Hard cap: **max 2 SGR attributes per span** — two of `:weight`,
`:slant`, `:underline`, `:inverse-video`, plus colour. "Bold + dim + coloured" violates.

## 3. Colour roles

Five roles assigned by name, never by literal index in layout code (§5.1): primary,
secondary, tertiary, error, neutral — plus four statuses: green healthy, red error,
yellow warning, dim gray inactive (§5.2). Seven palettes ship.

**How a raised surface differs from the ground — our exact problem.** Both findings
cut against the web intuition:

1. Elevation is carried by **border style, shadow and dim — never fill**: §6.1 defines
   all five levels by border and shadow alone. There is no ladder of surface fills.
2. Exactly **one** body fill differs from the ground: `Surface` 234 `#1c1c1c` against
   `Neutral bg` 235 `#262626` — one xterm grayscale step **darker**, delta `#0A` (~4%
   lightness). The light palette inverts it (`#ffffff` on gray): the surface moves
   *away* from the midpoint, never toward light.

Contrast floors 4.5:1 body, 3:1 bold (§9.5); colour is never the sole indicator (§5.3).

## 4. Borders and dividers

Box-drawing characters, not background shifts. Five levels (§6.1): 0 inline, no
border; 1 panels `─│┌┐└┘`; 2 menus single-line + 2x1 shadow; 3 dialogs `═║╔╗╚╝` +
shadow; 4 modals double + dim scrim over every non-modal cell (§6.5). Double-line
borders are forbidden below Level 3. Focused overlapping windows go double, unfocused
single (§6.2), but panels in a non-overlapping layout stay single regardless of focus.
Titles centre in the top border row, one space each side (§6.3).

The rule that bites us: rounded corners `╭╮╰╯` "MAY be used for cosmetic,
non-interactive containers... MUST NOT be used for interactive windows, dialogs, or
panels that participate in the elevation system" (§6.6).

## 5. Component patterns

- **List row** — 1 row, no gap. Min box width 20 cols, height 5-17 rows.
- **Selected row** — reverse video across the full row, or `[▸ item ◂]`. Exactly one
  element holds focus at all times (`state.md`).
- **Disclosure** — `▸` U+25B8 in a 2-cell gutter, kept blank on unmarked rows so the
  name column never shifts.
- **Status dots** — `◉` healthy, `○` inactive, `✓` success, `✗` error, `⚠` warning,
  always followed by a word (`◉ OK`, `⚠ DEGRADED`, `✗ DOWN`).
- **Table** — header in Title weight, one `─` rule row, data rows, values right-aligned
  1 cell before the border. **Disabled** — dim, visible in place.

## 6. Density and alignment

Numeric right, labels left, column width fixed at longest value + 2 so nothing reflows
between frames. Overflow truncates with U+22EF midline in data cells, U+2026 leading in
paths (`… › Node 7 › Containers`). Overflowing lists show `↑`/`↓` in the border.

## WHAT TO CHANGE HERE

**Sidebar**

1. **34 columns is out of range.** The sidebar band is 8-20 cols, 12-16 at our frame
   width (§1.3, §1.6). Either cut to **20**, or accept it is not a sidebar under this
   taxonomy — it is region B — and stop styling it as navigation chrome.
2. **Drop the card fill, or fix the ground.** `#191c23` on `#080808` is a lighter,
   blue-tinted raised surface; the system has no such token. Its one surface literal is
   `#1c1c1c` *below* a `#262626` ground. Either adopt the shipped pair — ground
   `#262626`, open-project fill `#1c1c1c` — or keep `#080808` and give the open project
   a **Level 1 single-line border**, name centred in the top border row, 1 cell inner
   padding, no fill. The second is what the system actually does. Any fill must be a
   pure gray; every neutral background it ships is untinted. Square the corners either
   way: rounded `╭╮╰╯` on a clickable card violates §6.6.
3. **Float `#0f0f0f` is a non-step.** `+#07` is below the `#0A` grayscale step and
   reads as ground. A float is Level 2: single-line border + 2x1 dim shadow, **no
   fill** (§6.1). If it must have a fill, use `#1c1c1c`, the one surface literal —
   there is no third rung.
4. Counts: the sidebar row is a table column, so right-align **1 cell** before the edge
   (`(space :align-to (- right 1))`), not flush; 2 is the menu-shortcut rule. Pair every
   coloured count with a glyph.
5. Selected row: `:inverse-video t` + `:extend t` so the bar spans all 20 cols, and
   keep the icon gutter occupied on unselected rows so names stay column-aligned.

**Trace** (INFERRED — the repo has no transcript or prose pattern)

1. **SF Pro Text breaks the atomic unit.** A proportional font makes `:align-to`
   meaningless. Either keep prose monospace, or put it outside the grid and forbid
   every column alignment and right-aligned value inside it.
2. **Avatar gutter = 2 cells, constant**, kept blank on continued paragraphs so the
   prose column never shifts. 3 and 4 are legal; 5 is not.
3. **Tool-call rows are table rows, not cards.** Glyph + word (`◉ OK`), name left,
   duration right-aligned 1 cell from the edge, 0 blank rows between consecutive
   calls, 1 before the next prose block. No fill, no box.
4. **Collapsed thinking** takes Label treatment (dim only, one attribute) with a
   `▸`/`▾` marker, via an `invisible` overlay — never `:height`; there is no size axis.
5. Truncate tool output with U+22EF, not `...`; paths with a leading U+2026. Cap every
   span at two attributes — a dim, bold, coloured thinking header is three.
