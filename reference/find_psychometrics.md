# Compute classical psychometrics for original and acquiescence-recoded scores

Takes the output of
[`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md),
reverse-scores negatively keyed items, and runs a classical psychometric
analysis (scale scores, Cronbach's alpha, item-total correlations) on
both the original responses and the acquiescence-recoded responses.
Comparing the two shows how much acquiescence inflated or distorted the
original results.

## Usage

``` r
find_psychometrics(obj, likert = 5, center = 0)
```

## Arguments

- obj:

  A list as returned by
  [`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md).

- likert:

  Integer. The number of response categories of the Likert scale, used
  to reverse-score negatively keyed items in the original data
  (`likert + 1 - x`). Defaults to `5`.

- center:

  Numeric. Value added back to the centered responses after
  reverse-scoring, so recoded scores sit on an interpretable scale.
  Defaults to `0`.

## Value

A list with:

- scores:

  Data frame of scale scores (original `_ori` and recoded `_rec`)
  together with the acquiescence indices.

- psicom_orig, psicom_recoded:

  The
  [`psych::scoreItems()`](https://rdrr.io/pkg/psych/man/score.items.html)
  objects for the original and recoded data.

- alpha_orig_scale_stat, alpha_rec_scale_stat:

  Per-scale reliability statistics.

- alpha_orig_item_stat, alpha_rec_item_stat:

  Per-item statistics (including `r.drop`).

- item_dic:

  The item dictionary.

- keys:

  The scoring keys matrix.

## See also

[`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md),
[`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md)

## Examples

``` r
data(data_senna)
data(senna_dic)
recoded <- recode_for_acq(data_senna, senna_dic)
psicom <- find_psychometrics(recoded, likert = 5, center = 3)
#> Number of categories should be increased  in order to count frequencies. 
#> In smc, smcs > 1 were set to 1.0
#> Number of categories should be increased  in order to count frequencies. 
#> Number of categories should be increased  in order to count frequencies. 
#> Number of categories should be increased  in order to count frequencies. 
#> Number of categories should be increased  in order to count frequencies. 
#> Number of categories should be increased  in order to count frequencies. 
#> Number of categories should be increased  in order to count frequencies. 
psicom$alpha_orig_scale_stat
#> # A tibble: 6 × 10
#>   scale raw_alpha std.alpha `G6(smc)` average_r  `S/N`     ase  mean    sd
#>   <chr>     <dbl>     <dbl>     <dbl>     <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1 O         0.879     0.882     0.902     0.217  7.49  0.00360  3.44 0.579
#> 2 C         0.938     0.939     0.951     0.256 15.5   0.00185  3.54 0.575
#> 3 E         0.818     0.824     0.853     0.148  4.68  0.00547  3.44 0.491
#> 4 A         0.842     0.850     0.879     0.136  5.66  0.00471  3.47 0.453
#> 5 N         0.853     0.856     0.878     0.180  5.95  0.00441  3.22 0.545
#> 6 OvCl      0.375     0.381     0.304     0.170  0.616 0.0226   2.85 0.771
#> # ℹ 1 more variable: median_r <dbl>
```
