# Score tests and compute classical psychometrics

Scores one or more scales from an item dictionary using
[`psych::scoreItems()`](https://rdrr.io/pkg/psych/man/score.items.html)
(or
[`psych::scoreFast()`](https://rdrr.io/pkg/psych/man/score.items.html))
and computes per-scale reliability statistics. Unlike
[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md),
this function works on a single data frame and does not require an
[`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md)
object, so it can be used to score either raw or acquiescence-recoded
data.

## Usage

``` r
score_tests(
  df,
  item_dic,
  scr_tot = FALSE,
  filename = "item_stats.xlsx",
  save_item_stat = FALSE,
  reversed = FALSE,
  score_fast = FALSE,
  min_values = NULL,
  max_values = NULL
)
```

## Arguments

- df:

  A data frame of item responses.

- item_dic:

  Item dictionary with at least `coditem`, `scale` and `pole` columns.
  See
  [`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md)
  for the expected format.

- scr_tot:

  Logical. Passed to `totals`: if `TRUE`, total (summed) scores are
  returned instead of means. Defaults to `FALSE`.

- filename:

  Path of the Excel file written when `save_item_stat = TRUE`.

- save_item_stat:

  Logical. If `TRUE`, item and scale statistics are written to
  `filename`. Defaults to `FALSE`.

- reversed:

  Logical. Passed to
  [`dic2keys()`](https://rprimi.github.io/noisecanceling/reference/dic2keys.md):
  whether negatively keyed items have already been reverse-scored.
  Defaults to `FALSE`.

- score_fast:

  Logical. If `TRUE`, use the faster
  [`psych::scoreFast()`](https://rdrr.io/pkg/psych/man/score.items.html)
  instead of
  [`psych::scoreItems()`](https://rdrr.io/pkg/psych/man/score.items.html).
  Defaults to `FALSE`.

- min_values, max_values:

  Optional numeric vectors of the minimum and maximum possible value of
  each scored item. If `NULL` (the default) they are taken from the
  observed range of the scored items.

## Value

A list with the scoring object (`psicom`), the `keys` matrix, the nested
`alpha` results, and tidy `alpha_scale_stat` / `alpha_item_stat` data
frames, plus the `item_dic`.

## See also

[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md),
[`dic2keys()`](https://rprimi.github.io/noisecanceling/reference/dic2keys.md)

## Examples

``` r
data(data_senna)
data(senna_dic)
res <- score_tests(data_senna, senna_dic)
res$alpha_scale_stat
#> # A tibble: 6 × 10
#>   scale raw_alpha std.alpha `G6(smc)` average_r `S/N`     ase  mean    sd
#>   <chr>     <dbl>     <dbl>     <dbl>     <dbl> <dbl>   <dbl> <dbl> <dbl>
#> 1 O         0.801     0.806     0.854    0.133  4.16  0.00585  2.95 0.474
#> 2 C         0.833     0.846     0.896    0.109  5.50  0.00489  3.13 0.386
#> 3 E         0.711     0.727     0.787    0.0899 2.67  0.00869  3.22 0.402
#> 4 A         0.765     0.778     0.830    0.0886 3.50  0.00705  3.02 0.381
#> 5 N         0.740     0.755     0.810    0.103  3.09  0.00787  3.02 0.430
#> 6 OvCl      0.375     0.381     0.304    0.170  0.616 0.0226   2.85 0.771
#> # ℹ 1 more variable: median_r <dbl>
```
