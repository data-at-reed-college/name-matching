# namematch

An R package for matching messy name files. It matches each row of one table
(`x`, e.g. campaign contributions) to at most one row of a reference table
(`y`, e.g. a voter registry), using first and last names plus any other
variables the two tables share, such as zip code.

It grew out of `fuzzy_name_match.R` (kept in this repo for reference). That
script matched contribution files to the voter registry using first name, last
name and zip code.

## Install

```r
install.packages("pak")
pak::pak("data-at-reed-college/name-matching")
library(namematch)
```

You can check the install with `packageVersion("namematch")`.

### Installing from a local clone

If you're developing the package, install it from a local clone instead:

```r
devtools::install()
```

## How it works

1. **Set up** the match with `nm_setup()`. Columns are mapped the same way as
   in a dplyr join, `c("x_column" = "y_column")`. Use a single unnamed string
   when the column has the same name in both tables.
2. **Pipe through the steps** you want. Each step only looks at rows that are
   still unmatched, and accepts a match only when exactly one candidate is
   left.
3. **Check** what each step did with `nm_summary()`, and get the matched data
   with `nm_results()`.

### Names and optional variables

`first`, `last` and `id` are required. Names are normalized before matching:
accents become ASCII, everything is upper case, hyphens become spaces,
punctuation is removed, and suffixes and titles (JR, MRS, ...) are dropped.
See `normalize_name()`.

Any other shared variable can be given in one of two roles:

| role       | what it does |
|------------|--------------|
| `block_by` | Must agree whenever both values are present. A candidate with a different value is never considered. Good for reliable variables such as state. |
| `check_by` | Not required. When a step has several candidates, the one that agrees on the most check variables wins. Steps can also use check variables to **verify** a match, and `nm_summary()` reports how often they agree. Good for zip code, since people move. |

`missing_values` lists placeholder values that count as missing (they never
block, break a tie or fail verification), such as the zip codes 97000, 97200
and 00000 in the example files.

### Steps

Exact matching always runs first. If you skip `match_exact()`, the first
other step (or `nm_summary()` / `nm_results()`) runs it for you. Every other
step is optional and can run in any order.

| step | matches | default `verify` |
|------|---------|------------------|
| `match_exact()` | same first and last name | `"none"` |
| `match_tokens()` | same words in any order or split: FRANKENSTEIN, BARON VON / VON FRANKENSTEIN, BARON | `"no_conflict"` |
| `match_nickname()` | last name exact, first names in a nickname group: BOB / ROBERT (see `?nicknames`) | `"none"` |
| `match_typo()` | one name exact, the other within a Jaro-Winkler distance: KATHERYN / KATHRYN | `"none"` |
| `match_first_word()` | last name exact, first word of the first name agrees: LISA E / LISA | `"no_conflict"` |
| `match_initial()` | last name exact, one first name is only initials: D FRANKENSTEIN / DRACULA FRANKENSTEIN | `"no_conflict"` |
| `match_compound_last()` | first name exact, one last name's words are a subset of the other's: GARCIA LOPEZ / GARCIA | `"no_conflict"` |
| `match_soundex()` | first and last names sound alike: JON SMYTH / JOHN SMITH. **Not recommended** (see below) | `"no_conflict"` |

`verify` has three modes:

- `"none"`: accept the single remaining candidate.
- `"no_conflict"`: reject it if a check variable is present on both sides and
  disagrees.
- `"agree"`: accept only if at least one check variable is present on both
  sides and agrees.

Without `check_by` variables, all three modes accept. The default for
`match_compound_last()` was my own call, since it is a loose match; change it
if it rejects too much.

Two rules carry over from the original script:

- If someone's exact name appears more than once in `y` and the tie can't be
  broken, the row is held back from the fuzzy steps. One of those people is
  almost certainly the right match.
- A typo candidate counts towards "is there only one candidate?" even when it
  is too far away to be accepted (`jw_cutoff` vs `jw_accept`).

## Worked example

Match the 2022 contribution file to the voter registry, with zip code as a
check variable:

```r
library(namematch)

contrib  <- read_names("example_2022.csv")    # all columns read as text
registry <- read_names("voter_registry.csv")

m <- nm_setup(
  contrib, registry,
  first = c("firstName" = "FIRST_NAME"),
  last  = c("lastName"  = "LAST_NAME"),
  id    = c("id"        = "VOTER_ID"),
  check_by = c("zip" = "ZIP_CODE"),
  missing_values = c("97000", "97200", "00000")
) |>
  match_tokens() |>          # match_exact() runs automatically first
  match_nickname() |>
  match_typo() |>
  match_first_word() |>
  match_initial() |>
  match_compound_last()

nm_summary(m)
#> Matched 8,929 of 12,211 rows (73.1%); 3,282 unmatched, of which 75 ambiguous and 85 missing a name.
#>           step      verify  pool matched ambiguous rejected cum_matched cum_pct zip_agree
#>          exact        none 12126    8016        42        0        8016   65.6%     83.4%
#>         tokens no_conflict  4068       4         0        8        8020   65.7%    100.0%
#>       nickname        none  4064     573        13        0        8593   70.4%     81.5%
#>           typo        none  3491     261        20      245        8854   72.5%     66.7%
#>     first_word no_conflict  3230      42         1       15        8896   72.9%    100.0%
#>        initial no_conflict  3188       2         1        8        8898   72.9%    100.0%
#>  compound_last no_conflict  3186      31         4       17        8929   73.1%    100.0%
```

Column guide:

- `pool`: rows still unmatched when the step ran.
- `ambiguous`: rows with several candidates the step could not tell apart.
- `rejected`: rows with one candidate that failed verification, or a typo
  above `jw_accept`.
- `zip_agree`: the share of the step's matches where both zips are present
  and equal. It is 100% by construction for `no_conflict` steps.

```r
matched   <- nm_results(m)               # x columns, then y columns, then match info
unmatched <- nm_results(m, "unmatched")  # x columns plus unmatched_reason
everyone  <- nm_results(m, "all")

table(unmatched$unmatched_reason)
#>    ambiguous missing_name no_candidate     rejected
#>           75           85         2875          247
```

Matched rows also have `match_step`, `n_candidates` and `zip_agree`. Columns
of `y` whose names clash with `x` get the suffix `_ref`.

## Comparison with the original script

`analysis/compare_baseline.R` runs three configurations on each year file
against the registry. The results are saved to `analysis/comparison.csv`.

- **legacy**: the original six steps in the original order, names only. On
  2022 it reproduces `fuzzy_name_match.R` exactly (9,139 matched, 7,803 exact).
- **legacy + zip**: the same steps, with zip used only to break ties.
- **recommended**: the pipeline in the worked example (no Soundex).

| year | config | matched | % | exact | ambiguous left | zip agree (fuzzy steps) | Soundex matches | Soundex zip agree |
|------|--------|--------:|--:|------:|---------------:|------------------------:|----------------:|------------------:|
| 2020 | legacy       | 15,790 | 69.5 | 13,166 | 1,141 | 41.3% | 1,240 | 13.6% |
| 2020 | legacy + zip | 16,265 | 71.6 | 13,523 |   664 | 43.8% | 1,343 | 20.3% |
| 2020 | recommended  | 15,176 | 66.8 | 13,523 |   207 | 71.9% |     – | –     |
| 2022 | legacy       |  9,139 | 74.8 |  7,803 |   555 | 48.7% |   581 | 16.5% |
| 2022 | legacy + zip |  9,401 | 77.0 |  8,016 |   293 | 50.5% |   625 | 22.4% |
| 2022 | recommended  |  8,929 | 73.1 |  8,016 |    75 | 78.9% |     – | –     |
| 2024 | legacy       | 38,222 | 72.3 | 32,555 | 2,340 | 50.3% | 2,611 | 16.9% |
| 2024 | legacy + zip | 39,330 | 74.4 | 33,446 | 1,232 | 52.2% | 2,815 | 23.0% |
| 2024 | recommended  | 37,284 | 70.5 | 33,446 |   325 | 82.6% |     – | –     |

The table shows four things:

- **Using zip to break ties** resolves most of the exact-name ties. In 2022,
  213 of the 255 are resolved, which adds 213 exact matches; across all steps
  it adds 262 matches. Zip agreement among exact matches stays the same
  (82.9% → 83.4%).
- **Soundex is left out.** Only 16.5% of the original Soundex matches in 2022
  had a matching zip, against 83% for exact matches, so most were probably
  false. Even when restricted to matches that agree on zip, it still confuses
  relatives (see Caveats).
- **The recommended pipeline** matches 1.7–2.7 points fewer rows than
  legacy, mostly because Soundex is gone. Zip agreement on fuzzy matches rises
  from about 50% to about 80%. Rows left ambiguous fall sharply (555 → 75 in
  2022), because Soundex produced most of the ambiguous candidates.
  The 2020 file agrees on zip less often than the others even for exact
  matches (70%), probably because people moved between 2020 and the registry
  snapshot.
- **The new steps** add 2 / 31 matches (initial / compound last) in 2022, and
  2 / 284 in 2024.

### Caveats

- **`match_soundex()` is still in the package but should be used with care.**
  With the zip check, it adds 75 matches in 2022, and roughly 20 of them look
  wrong even though the zip agrees. Most are relatives at the same address
  with the same last name: MICHELLE → MICHAEL, DON → DEANNA,
  RUSSELL → RACHEL. A zip check can't catch these. If you run it, review its
  matches by hand.
- **Token-set matches are verified against zip by default.** Without a check,
  only 25–35% of them agreed on zip. Many names work as either a first or a
  last name (CRAIG TAYLOR / TAYLOR CRAIG), so a swapped match is often a
  different person. With `no_conflict` the step keeps 4 of 16 matches in
  2020, 4 of 12 in 2022 and 17 of 49 in 2024.
- **Moving `match_nickname()` before `match_typo()` matters.** Nicknames
  match more rows (and agree on zip more often) when they get the first pick.

## What it needs (original to-do list)

- [x] a part for reading in files: `read_names()`
- [x] a part for specifying overlapping variables: `nm_setup(..., block_by =, check_by =)`
- [x] using the optional variables in the steps (blocking, tie-breaking, verifying): `verify =` on every step
- [x] a summary of matches per step: `nm_summary()`
- [x] make it a package: `namematch`
- [x] independent, optional steps (exact always runs), plus new ones: `match_initial()`, `match_compound_last()`

## Development

```r
devtools::document()
devtools::test()
devtools::check()
```

`data-raw/nicknames.R` rebuilds the `nicknames` dataset.
