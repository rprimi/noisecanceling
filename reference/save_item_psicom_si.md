# Export single-set psychometric results to an Excel workbook

A simplified counterpart of
[`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md)
for results that have a single set of scores (no original/recoded
split), such as the output of
[`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md).
Writes item statistics, item-level alpha statistics, scale statistics
and the scale correlation matrix.

## Usage

``` r
save_item_psicom_si(obj, filename)
```

## Arguments

- obj:

  A list with `psicom`, `alpha_item_stat`, `alpha_scale_stat` and
  `item_dic` elements, as returned by
  [`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md).

- filename:

  Path of the `.xlsx` file to create.

## Value

Invisibly returns `filename`. Called for the side effect of writing a
workbook with the sheets `item_stats`, `alpha_item_stat`, `scale_stat`
and `scale_cor`.

## See also

[`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md),
[`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(data_senna)
data(senna_dic)
res <- score_tests(data_senna, senna_dic)
save_item_psicom_si(res, filename = tempfile(fileext = ".xlsx"))
} # }
```
