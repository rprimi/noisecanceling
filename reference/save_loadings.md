# Export factor loadings to an Excel workbook

Extracts factor loadings from a
[`psych::fa()`](https://rdrr.io/pkg/psych/man/fa.html) or
[`psych::omega()`](https://rdrr.io/pkg/psych/man/omega.html) object and
appends them as a new sheet to an Excel workbook (creating the workbook
if it does not yet exist). For `fa` objects, item complexity and
communality are included; a `sort_load` column lets you sort items by
loading in Excel.

## Usage

``` r
save_loadings(
  obj,
  item_dic = NULL,
  filename,
  digits = 3,
  sheetName = "factor_loadings"
)
```

## Arguments

- obj:

  A factor-analysis object of class `fa` or `omega` (from the psych
  package).

- item_dic:

  Optional item dictionary; when supplied, item metadata is joined to
  the loadings by `coditem`.

- filename:

  Path of the `.xlsx` file. Created if it does not exist; otherwise a
  new sheet is appended.

- digits:

  Integer. Number of decimal places to round to. Defaults to `3`.

- sheetName:

  Name of the worksheet to add. Defaults to `"factor_loadings"`.

## Value

Invisibly returns the data frame of loadings that was written.

## See also

[`write_sheet_in_excelfile()`](https://rprimi.github.io/noisecanceling/reference/write_sheet_in_excelfile.md)
