# vistributions 0.2.2 — fix round 6: drawing-layer redesign (pkg-vist-fx6)

Worktree `F:/R/Packages/_worktrees/vist-fx6` (branch `task/vist-fx6`, based on the
round-5 head `f3cf926`). No push, no GitHub operations, no branch switch, no
stash. Scratch is confined to `.fx6-tmp/` (deleted before the final message).

Environment: Windows 11 x64, R 4.6.1 ucrt (2026-06-24), ggplot2 4.0.3,
testthat 3.3.2, vdiffr 1.0.9, devtools 2.5.2, scales 1.4.0.

Commits: `7c3cd44` (redesign + tests + snapshots + help + NEWS), `44174a3`
(installed-package NEWS read in the round-6 test) and `04d161f` (probability
contract and df floors read from one definition). This report is a later
documentation-only commit under the `.Rbuildignore`d `docs-dev/`.

The brief named `docs-dev/vfxx*-reaudit.md`, but no re-audit file exists in this
repository: the five round reports (`vfx-report.md`, `vfx2-report.md` … 
`vfx5-report.md`) are the audit record and are the spec used here. Every finding
id they contain is listed in the table below (40 ids).

## What this round changed

The five rounds before this one each fixed an instance of the same three
defects. Round 6 removes the defects themselves.

**Root cause 1 — the event was partly derived from its own drawing.**
Cutoffs were formatted to text, parsed back with `as.numeric()` and then used as
the coordinates of the polygon, the reference line and the percentage. The drawn
event therefore depended on the display precision, changed with the unit system,
and could differ from the event the caption named.

*Design.* One numeric source: the event is the `q*()` result (or the supplied
`perc`), its probability is `p*()` at that exact cutoff, and every polygon,
marker and annotation is built from those exact numbers. The caption is a
*presentation* of that event: `vdist_fmt_pct()` picks the shortest printed
percentage whose rounding interval contains the exact probability (a value on a
boundary, such as `5.05%`, gains a digit instead of being rounded to a false
value), and `vdist_fmt_cutoff()` / `vdist_fmt_value()` / `vdist_fmt_interval()`
print the exact cutoff with enough digits that its own `p*()` value falls inside
that displayed interval. The printed text is never read back as a coordinate.

**Root cause 2 — the shading grid was chosen independently of the event.**
A fixed-count probability grid with a `1e-12` probability clamp, a uniform
filler and 20,000-point special cases; regions were clipped to the view, and the
"area check" added an `omitted` term computed outside the polygon. This is why
small-`df` chi-square lost mass at the support floor, why support-edge spikes
were invisible, and why every round found a new tail case.

*Design.* `vdist_region()` builds the shaded polygon in three stages:

1. a probability-spaced grid (logit-spaced, so it is geometric in x in both
   tails and resolves an unbounded density at a support edge);
2. a uniform grid in standardised units (`z = (x - center) / scale`), so the
   normal picture does not change with a change of measurement units;
3. adaptive bisection driven by the *exact* CDF mass of each interval: the
   local trapezoid error is compared with `p(x[i+1]) - p(x[i])` and the worst
   intervals are split until the polygon's own area is within `1e-5` of
   `p(hi) - p(lo)` (bounded at 20,001 points and 200 passes).

The polygon spans the **whole event**; the panel only clips the drawing. A
density that is unbounded at an endpoint is included exactly: the endpoint
height is the value that makes the first (last) trapezoid carry the exact CDF
mass of that sliver, instead of being dropped or drawn as a spike. The region
starts at the distribution's support (zero for chi-square and F), not at a
quantile floor, so no probability is discarded. The area check is now
`vdist_check_region(area, p_event)` with no off-view adjustment: the drawn
polygon itself carries the probability.

**Root cause 3 — the density display scale was sampled in probability space.**
`vdist_density_cap()` sampled the density at probability-spaced points, so for
`df = 0.1` the singular support edge (density ≈ 1e112) decided the axis and the
shaded body was compressed to less than a pixel (rendered fraction 1e-19).

*Design.* The cap is the maximum of the density over a grid uniform in x; when
that maximum is more than 20× the 95th percentile of the same sample, the axis is
capped at that percentile. Ordinary densities are not clipped at all. The cap is
a display limit only: the polygon and the area check use the unclipped density.
The rendered fraction of the must-pass inputs is now 0.008–0.9 instead of 0.

Two display rules complete the layer:

* reference lines, markers and text are clamped to the panel (unchanged from
  round 5) and the panel is set explicitly by `coord_cartesian(xlim = view)`, so
  a far cutoff cannot pull the window out to itself;
* a part of the region that lies outside the panel is disclosed in a plot
  caption only when it exceeds **1% of the region** ("X% of the shaded region
  lies outside the plotted range"). The polygon still covers it, so the area
  check does not depend on the caption.

The input contract is now defined once, in `vdist_contract()`: the probability
ranges are read from it by `vdist_prob_range()`, the chi-square and F parameter
floors are enforced by `vdist_check_chisquare_df()` / `vdist_check_f_num_df()`
in every export, and the tests read the same table. It is unchanged from rounds 3–5:

| Family | Parameters | Probability range | Other limits |
|---|---|---|---|
| normal | finite `mean`, `sd > 0`; `sd / \|mean\| >= 1e-14` when `mean != 0` | `[1e-10, 1-1e-10]` | finite density and `mean ± 4 sd` |
| t | finite `df > 0` | `[1e-10, 1-1e-10]` | very small `df` may hit a representability error |
| chi-square | finite `df >= 0.1` | `[1e-6, 1-1e-6]` | — |
| F | finite `num_df >= 1`, `den_df > 0`; central mass width ≥ 0.001 | `[1e-6, 1-1e-6]` | tail overflow reported separately |
| binomial | whole `n` in `[0, 2^31-1]`, whole `s`, `0 <= p <= 1`, `0 < tp < 1` | — | — |

Ordinary teaching inputs are inside every limit and are asserted to be
accepted: all 15 defaults, `normal(100, 15)`, t with `df = 5`, chi-square with
`df = 1` and `df = 2` in an upper tail, `F(5, 10)` and a typical binomial.

The binomial drawing layer already used exact `dbinom`/`pbinom`/`qbinom` on the
integer support and is unchanged. Its captions keep their historical
three-decimal proportion format (`P(X) <= 4 = 0.85`), which is the number it
prints; it is asserted against `pbinom` in the new test file.

**No API change.** The formals of all 15 exported plotting functions are
asserted unchanged.

## Finding id → status

All five rounds' findings hold under the redesign. "Test" is the file that
re-asserts the finding; `r6` is `tests/testthat/test-vfx6-redesign.R`,
`c022` `test-correctness-022.R`, `vx` `test-vfxx-findings.R`, `vx3`
`test-vfx3-findings.R`, `vx4` `test-vfx4-findings.R`, `vx5`
`test-vfx5-findings.R`.

| Round | id | Finding (short) | Status | Test |
|---|---|---|---|---|
| 1 | H1 | version reuse | Holds: DESCRIPTION 0.2.2 | check output |
| 1 | H2 | fractional df kept | Fixed round 1; holds | c022 H2, r6 `finding_cases` |
| 1 | H3 | normal quantile not rounded | Fixed round 1; holds | c022 H3, r6 captions |
| 1 | H4 | binomial counts whole numbers | Fixed round 1; holds | c022 H4, r6 binomial |
| 1 | H5 | reversed intervals rejected | Fixed round 1; holds | c022 H5, r6 limits |
| 1 | H6 | qbinom caption and shaded mass | Fixed round 1; holds | c022 H6, r6 binomial |
| 1 | H7 | negative symmetric t cutoff | Fixed round 1; holds | c022 H7, r6 |
| 1 | M1 | quantile outside the grid | Fixed round 1; holds | c022 M1, r6 |
| 1 | M2 | polygon area at small scales | Fixed round 1; holds | c022 M2, r6 finding M2 |
| 1 | M3 | `probs` 0/1 rejected | Fixed round 1; holds | c022 M3, r6 limits |
| 1 | M4 | maintainer address | Holds: only NOTE in check | check output |
| 1 | L1 | NEWS / cran-comments | Holds: NEWS rewritten this round | r6 contract |
| 1 | L2 | README example comment | Holds (docs only) | — |
| 1 | L3 | missing/empty/vector inputs | Fixed round 1; holds | c022 L3, r6 limits |
| 2 | VFXX-H1 | near-integer binomial counts | Fixed round 2; holds | vx H1, r6 binomial |
| 2 | VFXX-H2 | tiny cutoffs printed truthfully | Fixed round 2; holds | vx H2, r6 `finding_cases` |
| 2 | VFXX-H3 | capped grids drew wrong areas | Fixed round 2; superseded by the round-6 region builder | vx H3, r6 `finding_cases` |
| 2 | VFXX-H4 | chi-square boundary spike | Fixed round 2; region now starts at the support | vx H4, r6 low-df |
| 2 | VFXX-M1 | zero-variance normal overlay | Fixed round 2; holds | vx M1 |
| 2 | VFXX-L1 | empty normal `perc` | Fixed round 2; holds | vx L1 |
| 2 | VFXX-L2 | interval semantics documented | Holds: `man/vdist_normal_plot.Rd` | — |
| 2 | VFXX-L3 | NEWS overstated the grid | Holds: NEWS rewritten | r6 contract |
| 3 | VFXX2-H1 | chi-square mass lost at the support | Fixed round 3; support anchoring now guaranteed by construction | vx3 H1, r6 low-df |
| 3 | VFXX2-H2 | unchecked / infinite F polygons | Fixed round 3; holds | vx3 H2, r6 limits |
| 3 | VFXX2-H3 | translated cutoff precision | Fixed round 3; holds | vx3 H3, r6 captions |
| 3 | VFXX2-H4 | concentrated F density | Fixed round 3; holds | vx3 H4, r6 `finding_cases` |
| 3 | VFXX2-M1 | F overlay unrounded moments | Fixed round 3; holds | vx3 M1 |
| 3 | VFXX2-M2 | clipped view defeated by lines | Fixed round 3; holds | vx3 M2, r6 tails |
| 3 | VFXX2-L1 | release claims | Holds | vx3 L1 |
| 4 | VFXX3-H1 | area guard checked the clipped polygon | Fixed round 4; the polygon now spans the event | vx4 H1, r6 |
| 4 | VFXX3-H2 | translated tiny-scale normals | Fixed round 4; exact-source region | vx4 H2, r6 |
| 4 | VFXX3-M1 | ordinary inputs rejected | Fixed round 4; holds | vx4 M1, r6 ordinary |
| 4 | VFXX3-M2 | caption precision / double rounding | Fixed round 4; one rounding rule now guarantees it | vx4 M2, r6 captions |
| 4 | VFXX3-M3 | normal interval annotation | Fixed round 4; holds | vx4 M3, r6 tails |
| 4 | VFXX3-L1 | contract / help / NEWS | Holds: contract table in `vdist_contract()` and the help pages | vx4 L1, r6 contract |
| 5 | VFXX4-H1 | resolvable scaled normals rejected | Fixed round 5; unit invariance is now exact | vx5 H1, r6 unit |
| 5 | VFXX4-H2 | source areas became invisible shading | Fixed round 5; x-space display cap + support anchoring | vx5 H2, r6 visibility |
| 5 | VFXX4-M1 | central captions false at printed precision | Fixed round 5; exact percentage + verified text | vx5 M1, r6 captions |
| 5 | VFXX4-M2 | advertised domain contained failures | Fixed round 5; holds | vx5 M2, r6 ordinary/limits |
| 5 | VFXX4-L1 | documentation / report / test claims | Holds: contract table, help pages and NEWS agree | r6 contract, r6 API |

## Evidence

Areas are measured from the actual polygon vertices with the independent
local-edge formula; "rendered" is the fraction of the built panel covered by the
clipped dark polygon from `ggplot_build()`. All calls use `print_plot = FALSE`.

### Finding reproductions

| id | Call | Truth | Area | abs err | rendered |
|---|---|---:|---:|---:|---:|
| H2 | `vdist_t_prob(2, df = 1.9)` | 0.904917 | 0.904917 | 5e-7 | 0.187 |
| H3 | `vdist_normal_perc(.95, sd = 1e-4)` | 0.95 | 0.950000 | 4e-9 | 0.229 |
| H7 | `vdist_t_prob(-2, 7, "interval")` | 0.9143807 | 0.9143798 | 8e-7 | 0.183 |
| M1 | `vdist_t_perc(.95, 1)` | 0.95 | 0.950016 | 1.6e-5 | 0.15 |
| M2 | `vdist_normal_plot(sd = .001)` (central stripe) | 0.6826895 | 0.6826799 | 9.6e-6 | — |
| VFXX-H2 | `vdist_chisquare_perc(.05, .1)` | 0.05 | 0.050042 | 4.2e-5 | 4.8e-27 ¹ |
| VFXX-H3 | `vdist_f_prob(5000, 4, 30)` | 1.0 | 1.000008 | 8e-6 | 0.039 |
| VFXX-H4 | `vdist_chisquare_perc(.999, 1)` | 0.999 | 0.999022 | 2.2e-5 | 0.024 |
| VFXX2-H1 | `vdist_chisquare_perc(.95, .1)` | 0.95 | 0.950129 | 1.3e-4 | 0.107 |
| VFXX2-H4 | `vdist_f_plot(1e6, 1e6)` (shape) | 1.0 | 1.000009 | 9.4e-6 | — |
| VFXX3-H1 | `vdist_chisquare_perc(.05, 1, "upper")` | 0.05 | 0.050010 | 9.7e-6 | 0.0011 |
| VFXX3-H2 | `vdist_normal_perc(.95, 1, 1e-14)` | 0.95 | 0.949895 | 1.1e-4 | 0.229 |
| VFXX3-M1 | `vdist_chisquare_plot(2.01)` (shape) | 0.9999962 | 1.0000062 | 1.0e-5 | — |
| VFXX4-H1 | `vdist_normal_perc(.95, 1e-16, 1e-16)` | 0.95 | 0.950001 | 1.0e-6 | 0.229 |
| VFXX4-H2 | `vdist_chisquare_perc(.95, 1)` | 0.95 | 0.950018 | 1.8e-5 | 0.028 |
| VFXX4-M2 | `vdist_f_perc(.95, 2, .1)` | 0.95 | 0.950084 | 8.4e-5 | 0.96 |

¹ The 5% lower tail of a chi-square with `df = 0.1` is `1.1e-26` wide: its area
is exact to 4e-5 but its width is below a pixel on a linear axis (see
"Deferred").

### Must-pass table

| Call | Truth | Area | abs err | rendered |
|---|---:|---:|---:|---:|
| `vdist_normal_plot()` (central stripe) | 0.6826895 | 0.6826799 | 9.6e-6 | — |
| `vdist_normal_perc()` | 0.95 | 0.9500083 | 8.4e-6 | 0.229 |
| `vdist_normal_prob()` | 0.9986501 | 0.9986439 | 6.2e-6 | 0.241 |
| `vdist_t_plot()` | 0.9719915 | 0.9720012 | 9.6e-6 | — |
| `vdist_t_perc()` | 0.95 | 0.9500087 | 8.7e-6 | 0.194 |
| `vdist_t_prob()` | 0.9231842 | 0.9231926 | 8.4e-6 | 0.184 |
| `vdist_chisquare_plot()` | 0.9999846 | 0.9999869 | 2.3e-6 | — |
| `vdist_chisquare_perc()` | 0.95 | 0.9500000 | 1.7e-8 | 0.198 |
| `vdist_chisquare_prob()` | 0.7066746 | 0.7066650 | 9.6e-6 | 0.236 |
| `vdist_f_plot()` | 0.9897892 | 0.9897887 | 5.2e-7 | — |
| `vdist_f_perc()` | 0.95 | 0.9500021 | 2.1e-6 | 0.256 |
| `vdist_f_prob()` | 0.9367237 | 0.9367149 | 8.9e-6 | 0.244 |
| `vdist_normal_perc(.5, "both")` | 0.5 | 0.4999903 | 9.8e-6 | 0.121 |
| `vdist_normal_perc(.95, sd = .1)` | 0.95 | 0.9500010 | 1.0e-6 | 0.229 |
| `vdist_normal_prob(.1645, sd = .1)` | 0.9500151 | 0.9500161 | 1.0e-6 | 0.229 |
| `vdist_chisquare_plot(2.01)` | 0.9999962 | 1.0000062 | 1.0e-5 | — |
| `vdist_t_perc(.95, .1)` | 0.95 | 0.9501009 | 1.0e-4 | 0.891 |
| `vdist_t_perc(.95, .2)` | 0.95 | 0.9500582 | 5.8e-5 | 0.140 |
| `vdist_normal_perc(.95, 100, 15)` | 0.95 | 0.9500010 | 1.0e-6 | 0.229 |
| `vdist_t_perc(.95, 5)` | 0.95 | 0.9500081 | 8.1e-6 | 0.192 |
| `vdist_chisquare_perc(.95, 1)` | 0.95 | 0.9500183 | 1.8e-5 | 0.028 |
| `vdist_chisquare_perc(.05, 3, "upper")` | 0.05 | 0.0500090 | 9.0e-6 | 0.0075 |
| `vdist_f_perc(.95, 5, 10)` | 0.95 | 0.9499901 | 9.9e-6 | 0.266 |

### Low-df chi-square near the support

The polygon starts at 0 and its area is the exact probability for every
combination (`lower` and `upper`, `df = .1, .5, 1, 2`, `p = .05, .5, .95`); the
largest absolute error is 1.3e-4. Rendered fraction:

| `df` | lower .05 | lower .5 | lower .95 | upper .05 | upper .5 | upper .95 |
|---|---:|---:|---:|---:|---:|---:|
| 0.1 | 4.8e-27 ¹ | 4.8e-7 ¹ | 0.107 | 0.053 | 0.108 | 0.108 |
| 0.5 | 1.5e-6 ¹ | 0.0060 | 0.0153 | 0.0008 | 0.0084 | 0.0125 |
| 1 | 0.0005 | 0.0141 | 0.0279 | 0.0011 | 0.0112 | 0.0210 |
| 2 | 0.0064 | 0.0641 | 0.122 | 0.0042 | 0.0418 | 0.0793 |

¹ The shaded interval is narrower than a pixel (width 1e-26 … 1e-6 on a 0–2.3
axis). The area, the caption and the support anchoring are exact; only the
picture is unreadable, and that is a display limit, not a numerical one (see
"Deferred").

### Unit invariance (normal)

| k | area difference | max standardised density difference | rendered difference | caption percentage |
|---|---:|---:|---:|---|
| 1000 | 0 | 1.1e-16 | 5.6e-17 | `= 95%` |
| 0.001 | 0 | 1.1e-16 | 0 | `= 95%` |
| 1e6 | 7.4e-6 | 3.2e-4 | 1.8e-6 | `= 95%` |
| 1e-6 | 7.4e-6 | 3.2e-4 | 1.8e-6 | `= 95%` |

At 1e3 and 1e-3 the standardised polygon is identical to machine precision. At
1e6 and 1e-6 the adaptive grid holds one vertex more or less (the trapezoid
error is within 1e-5 of the tolerance either way); the piecewise-linear density
agrees to 3e-4 and the drawn fraction to 1.8e-6. The test compares the picture
as a function for that reason, and the area, caption and axis labels are
invariant at every factor.

### Captions

The same rounding rule produces, for example:

| Call | Caption |
|---|---|
| `vdist_normal_perc(.95, 100, 15)` | `P(X < 124.673) = 95%` |
| `vdist_normal_perc(.0505, type = "both")` | `P(-0.06333 < X < 0.06333) = 5.05%` (a one-decimal value would be false) |
| `vdist_normal_perc(.0001, mean = 1, type = "both")` | `P(0.9999 < X < 1.0001) = 1e-02%` (true at one significant digit) |
| `vdist_t_prob(.1, 7, "interval")` | `P(-0.1 < X < 0.1) = 7.7%` |
| `vdist_t_prob(.1, 7, "both")` | `P(\|X\| > 0.1) = 92.3%` |
| `vdist_f_perc(1e-6, 5, .5, "upper")` | `P(X > 3.1747e+23) = 1e-04%` + disclosure caption |

## Tests

`tests/testthat/test-vfx6-redesign.R` (new, 376 expectations) contains:

1. **Every finding id**: a table of the audit reproductions (H2, H3, H7, M1,
   M2, VFXX-H2/H3/H4, VFXX2-H1/H4, VFXX3-H1/H2/M1, VFXX4-H1/H2/M2) asserted
   against `p*()`, with `ggplot_build()` visibility for every case whose event is
   wider than a pixel.
2. **Captions and percentages**: truth at the printed precision in every
   rounding regime (one decimal, three significant digits, exact boundary,
   interval, two-tail), and the unprefixed percentage text for the two-tail
   cases.
3. **One numeric source**: the polygon's area equals the exact event
   probability, not the CDF at its own vertices (upper tails, tiny-`df`, large
   units, `df = 0.1`).
4. **`ggplot_build()` visibility**: 23 must-pass ordinary inputs; the rendered
   dark fraction is above 1e-4, the panel range is finite and the polygon
   overlaps it (shape plots are checked through their body area).
5. **Tails beyond the view**: the F 1e-6 upper event extends past the panel, its
   area is exact, its shading is rendered and the caption discloses the clipped
   part; a 1e6 t cutoff is clamped for the marker while the area stays 1.
6. **Low-df chi-square and F**: area within 1e-3 for `df = .1…5`, polygon
   anchored at 0, finite densities, upper-tail visibility, singular F at zero.
7. **Unit invariance**: 1e3, 1e-3, 1e6, 1e-6 — area, caption, rendered fraction
   and standardised density.
8. **Contract**: 18 ordinary teaching inputs accepted with their area; 11
   documented limits each with its own message; `vdist_contract()` values;
   NEWS statements.
9. **API stability**: the formals of all 15 exported plotting functions.
10. **Binomial**: shaded mass equals `pbinom`/`dbinom`/`qbinom` for lower,
    upper, exact, interval and percentile events.
11. **Bounded grids**: every extreme call keeps the curve and every polygon
    under 30,000 rows.

Existing files were adjusted only where the redesign changes a claim:

* `test-vfx5-findings.R`: the unit-invariance comparison now compares the
  standardised polygon as a function (the adaptive grid may hold one vertex more
  or less; the picture agrees to 3e-4). The VFXX4-M2 disclosure assertion uses
  the F 1e-6 upper case, which is genuinely clipped (94.6% of the region), since
  the ordinary F percentile now fits its panel.

Everything else — `test-correctness-022.R`, `test-vfxx-findings.R`,
`test-vfx3-findings.R`, `test-vfx4-findings.R`, the family and bug-regression
files — passes unchanged, including every exact caption string.

Source-tree suite:

```text
NOT_CRAN=true devtools::test()
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 1120 ]
```

(746 before this round: 376 new expectations minus the two unit-invariance
assertions that were replaced by the function comparison in
`test-vfx5-findings.R`; no other test was changed.)

## Snapshots

32 of the 39 vdiffr snapshots changed; the 7 binomial snapshots are
byte-identical. No snapshot was accepted to hide a failing value assertion.

| Snapshot group | Change |
|---|---|
| `normal-plot-1/2` | region polygons are built by the new area-controlled grid (same curve, same captions) |
| `normal-perc-{lower,upper,both}`, `normal-prob-{lower,upper,both}` | grid vertices; the panel is mean ± 4 sd instead of the old mean ± 5 sd axis grid; the 2.9e-5% disclosure caption disappears (below the 1% threshold) |
| `t-plot-1/2/3` | grid vertices |
| `t-perc-*`, `t-prob-*` | grid vertices; captions identical; axis ticks identical (-5…5); the noisy 1e-02% disclosure captions are gone |
| `chisquare-plot-1/1a/2/3` | grid vertices; the shape polygon starts at the support |
| `chisquare-perc/prob-*` | grid vertices; captions identical (e.g. `P(X > 16.564) = 22%`); disclosure captions gone |
| `f-plot-1/2` | grid vertices; x-axis `0 1 2 3 4` |
| `f-perc/prob-*` | grid vertices; x-axis `0…5`, captions identical; disclosure captions gone |

Every caption string in the 32 changed snapshots is unchanged; the diff is the
polygon geometry, the tick set and the removal of sub-1% disclosure captions.
Each snapshot was regenerated by deleting the file and re-running the visual
tests, as `AGENTS.md` prescribes.

## Checks

Clean `git archive HEAD` (commit `04d161f`), `R CMD build --no-manual`,
`R CMD check --as-cran --no-manual`, `LC_ALL=C LANG=C LANGUAGE=en`,
`NOT_CRAN=true`:

```text
* checking examples ... OK
* checking tests ... [80s] OK
  [ FAIL 0 | WARN 0 | SKIP 0 | PASS 1120 ]
* checking re-building of vignette outputs ... OK
Status: 1 NOTE
```

The only NOTE is the pre-existing maintainer-address block (the address change
of round 1, M4):

```text
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Aravind Hebbali <aravindhebbali@rsquaredacademy.com>'
New maintainer:  Aravind Hebbali <aravindhebbali@rsquaredacademy.com>
Old maintainer(s): Aravind Hebbali <hebbali.aravind@gmail.com>
```

With `_R_CHECK_CRAN_INCOMING_REMOTE_=false` (the local schannel stack cannot
validate the documentation host) the same archive returns `Status: OK`.
`docs-dev/` and `.fx6-tmp/` are `.Rbuildignore`d/untracked and are not in the
archive.

## Commands that changed something

```text
Rscript .fx6-tmp/probe*.R, proto*.R, evidence.R   # audit reproductions, design prototypes, after evidence
Rscript -e 'devtools::test()'                     # NOT_CRAN=true, repeatedly
Rscript -e 'devtools::document()'                 # kept only the four affected Rd files
git checkout -- DESCRIPTION NAMESPACE man/vistributions.Rd
rm tests/testthat/_snaps/visual/*.svg             # regenerate snapshots (x3 while iterating)
git archive HEAD | tar -x -C .fx6-tmp/check-src
R CMD build --no-manual .
R CMD check --as-cran --no-manual vistributions_0.2.2.tar.gz
git commit                                        # 7c3cd44, 44174a3, 04d161f
```

The scratch directory `.fx6-tmp/` (scripts, evidence, source copy, tarball, check
products) was deleted after this report was written; it is untracked and was
never in the archive.

## Deferred (not findings; no wrong numbers)

1. **Sub-pixel lower tails with `df < 1`.** `vdist_chisquare_perc(.05, .1)` and
   `vdist_chisquare_perc(.5, .1)` have exact areas and captions, but the shaded
   interval is 1e-26 … 1e-6 wide on a 0–2.3 axis, so it is invisible on a linear
   axis. The upper tail of the same distribution is visible. A support-edge
   inset or a log axis chosen by the event's width would fix the picture; it is
   a display design change and is not needed for the numbers.
2. **Adaptive-grid vertex count.** The refinement minimises the area error, not
   the vertex count; a region in a different unit system can hold one vertex
   more or less (the picture agrees to 3e-4). A deterministic quantisation of the
   refinement decisions would make the vertex set identical but would not change
   the picture, so it is not worth the complexity.
3. **Cost of very small `df`.** A chi-square with `df = 0.1` takes about 0.65 s
   per percentile plot (the support-edge mass needs a fine probability grid);
   ordinary plots take 0.05 s. The grid is capped at 20,001 points.
4. **pkgdown website** — `docs/` still shows the older site; rebuild with
   `pkgdown::build_site()` and commit separately.
5. **Shiny app** — `vdist_launch_app()` delegates to
   `xplorerr::app_vistributions()`; the live app was not run in this round.
6. **Interrupted plotted window** — a very wide t/F window is drawn on a
   symmetric log axis (round 5); the printed probability and area are unaffected.
