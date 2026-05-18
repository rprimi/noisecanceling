# Export psychometric results to an Excel workbook

Writes the results of
[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
to a multi-sheet Excel file, combining item-level statistics,
scale-level statistics and scale correlations for both the original and
the acquiescence-recoded scores.

## Usage

``` r
save_item_psicom(obj, filename)
```

## Arguments

- obj:

  A list as returned by
  [`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md).

- filename:

  Path of the `.xlsx` file to create.

## Value

Invisibly returns `filename`. Called for the side effect of writing the
workbook, which has the sheets `item_stats`, `scale_stats_ori`,
`scale_stats_rec`, `scale_cor_o`, `scale_cor_r`, `alpha_ori` and
`alpha_rec`.

## See also

[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md),
[`save_item_psicom_si()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom_si.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data(data_senna)
data(senna_dic)
recoded <- recode_for_acq(data_senna, senna_dic)
psicom <- find_psychometrics(recoded, likert = 5, center = 3)
save_item_psicom(psicom, filename = tempfile(fileext = ".xlsx"))
} # }
```
