# Correcting questionnaires for acquiescence bias

``` r

library(noisecanceling)
```

## Acquiescence as noise

**Acquiescence** is the tendency to agree with questionnaire items
regardless of their content. It adds a person-specific offset to every
response and distorts scale scores, reliabilities and correlations.

`noisecanceling` treats acquiescence the way noise-cancelling headphones
treat ambient sound. The headphones record the noise and play back an
inverted copy (180° out of phase); the noise and its inverted copy
cancel, leaving the signal. In a **balanced scale**:

- the **trait** is the signal;
- **acquiescence** is the noise, and it pushes responses *upward* on
  both positively keyed (PK) and negatively keyed (NK) items;
- reverse-scoring the NK items realigns the trait signal while leaving
  acquiescence split into equal positive and negative parts that cancel.

The package estimates a per-person acquiescence index from the balanced
items and centers responses on it, so the remaining variance is a
purified measure of the trait.

## The item dictionary

Every function is driven by an **item dictionary**: a data frame with
one row per item describing how it should be scored. The bundled
`senna_dic` is an example.

``` r

data(senna_dic)
head(senna_dic[, c("coditem", "scale", "pole", "seman_pairs", "item_text")])
#>   coditem scale pole seman_pairs
#> 1 Sv1.040     O    1           1
#> 2 sv2.662     O    0           1
#> 3 sv2.590     O    1           2
#> 4 sv2.593     O    0           2
#> 5 sv2.477     O    1           3
#> 6 sv2.663     O    0           3
#>                                                    item_text
#> 1                                 I like artistic activities
#> 2                                         I find art useless
#> 3                       I apreciate the beauty in all things
#> 4 I don't see the beauty in things until other comment on it
#> 5                            I like to see art presentations
#> 6         I don't see the fun in museums and art exhibitions
```

The required columns are:

| Column | Meaning |
|----|----|
| `coditem` | Item code; matches a column name in the response data. |
| `scale` | Scale the item is scored on. |
| `pole` | `1` = positively keyed, `0` = negatively keyed. |
| `seman_pairs` | Identifier of the PK/NK semantic pair; `NA` if the item is not paired. |
| `item_text` | Item text (optional, carried through for reporting). |

Items with a non-missing `seman_pairs` value are the **balanced items**
used to estimate acquiescence.

## Step 1 — Recode for acquiescence

[`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md)
computes the acquiescence index (the within-person mean of all paired
items) and centers every response on it.

``` r

data(data_senna)

recoded <- recode_for_acq(data_senna, senna_dic)
str(recoded, max.level = 1)
#> List of 5
#>  $ data            :'data.frame':    2300 obs. of  165 variables:
#>  $ data_acq_recoded:'data.frame':    2300 obs. of  165 variables:
#>  $ acq_index       :'data.frame':    2300 obs. of  2 variables:
#>  $ item_dic        : tibble [165 × 10] (S3: tbl_df/tbl/data.frame)
#>  $ item_dic_acq    : tibble [94 × 11] (S3: tbl_df/tbl/data.frame)
```

`acq_index` holds the per-person index and within-subject standard
deviation:

``` r

head(recoded$acq_index)
#>   acq_index     ws_sd
#> 1  2.638298 1.1345130
#> 2  3.180851 1.8431084
#> 3  2.787234 1.2689200
#> 4  2.265957 1.2019297
#> 5  2.755319 0.9581139
#> 6  2.712766 1.0637966
```

`data_acq_recoded` holds the centered responses — each value is the
original response minus that person’s acquiescence index.

## Step 2 — Classical psychometrics

[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
reverse-scores the NK items and runs a classical psychometric analysis
(scale scores, Cronbach’s alpha, item-total correlations) on **both**
the original and the acquiescence-corrected data.

``` r

psicom <- find_psychometrics(recoded, likert = 5, center = 3)
```

Compare the reliabilities side by side:

``` r

orig <- psicom$alpha_orig_scale_stat[, c("scale", "raw_alpha")]
rec  <- psicom$alpha_rec_scale_stat[, c("scale", "raw_alpha")]
merge(orig, rec, by = "scale", suffixes = c("_original", "_recoded"))
#>   scale raw_alpha_original raw_alpha_recoded
#> 1     A          0.8422867         0.8560083
#> 2     C          0.9376881         0.9445510
#> 3     E          0.8179270         0.8289296
#> 4     N          0.8527912         0.8683583
#> 5     O          0.8794080         0.8890068
#> 6  OvCl          0.3753193         0.3120026
```

The `scores` element contains the scale scores (`_ori` and `_rec`)
together with the acquiescence indices, ready for further analysis.

``` r

head(psicom$scores)
#>      O_ori    C_ori    E_ori    A_ori    N_ori OvCl_ori    O_rec    C_rec
#> 1 3.370370 3.333333 3.740741 3.888889 3.074074 3.333333 3.490938 3.453901
#> 2 4.888889 4.755556 4.777778 4.583333 4.814815 3.666667 4.828605 4.695272
#> 3 2.814815 3.066667 3.629630 3.055556 2.703704 2.000000 2.885737 3.137589
#> 4 3.111111 3.111111 3.000000 2.916667 3.000000 2.666667 3.355792 3.355792
#> 5 3.000000 2.666667 2.407407 2.944444 2.444444 1.666667 3.081560 2.748227
#> 6 3.629630 4.000000 3.518519 3.833333 3.481481 2.666667 3.725374 4.095745
#>      E_rec    A_rec    N_rec OvCl_rec acq_index     ws_sd
#> 1 3.861308 4.009456 3.194641 3.695035  2.638298 1.1345130
#> 2 4.717494 4.523050 4.754531 3.485816  3.180851 1.8431084
#> 3 3.700552 3.126478 2.774626 2.212766  2.787234 1.2689200
#> 4 3.244681 3.161348 3.244681 3.400709  2.265957 1.2019297
#> 5 2.488968 3.026005 2.526005 1.911348  2.755319 0.9581139
#> 6 3.614263 3.929078 3.577226 2.953901  2.712766 1.0637966
```

## Scoring without an acquiescence object

[`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md)
scores scales directly from a data frame and an item dictionary, which
is handy for scoring either raw or already-recoded data.

``` r

scored <- score_tests(data_senna, senna_dic)
scored$alpha_scale_stat[, c("scale", "raw_alpha", "std.alpha")]
#> # A tibble: 6 × 3
#>   scale raw_alpha std.alpha
#>   <chr>     <dbl>     <dbl>
#> 1 O         0.801     0.806
#> 2 C         0.833     0.846
#> 3 E         0.711     0.727
#> 4 A         0.765     0.778
#> 5 N         0.740     0.755
#> 6 OvCl      0.375     0.381
```

## Exporting results

[`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md)
writes the full
[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
output to a multi-sheet Excel workbook, and
[`save_item_psicom_si()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom_si.md)
does the same for
[`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md)
output:

``` r

save_item_psicom(psicom, filename = "senna_psychometrics.xlsx")
```

## Summary

1.  Build an **item dictionary** describing scales, poles and semantic
    pairs.
2.  [`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md)
    — estimate acquiescence and center the responses.
3.  [`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
    or
    [`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md)
    — analyse the corrected scores.
4.  [`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md)
    — export the results.

See the [function
reference](https://rprimi.github.io/noisecanceling/reference/index.md)
for the full API.
