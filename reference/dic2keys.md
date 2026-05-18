# Build a scoring keys matrix from an item dictionary

Converts an item dictionary into a scoring keys matrix in the format
expected by
[`psych::scoreItems()`](https://rdrr.io/pkg/psych/man/score.items.html).
Each column is a scale; each cell is `+1`, `-1` or `0` indicating how
the item loads on that scale.

## Usage

``` r
dic2keys(item_dic, reversed = TRUE)
```

## Arguments

- item_dic:

  A data frame with at least the columns `scale`, `pole` and `coditem`.
  An `order` column is added automatically if absent.

- reversed:

  Logical. If `TRUE` (the default), negatively keyed items are assumed
  to have already been reverse-scored, so all items keep a positive sign
  in the keys. If `FALSE`, negatively keyed items (`pole == 0`) are
  given a negative sign so
  [`psych::scoreItems()`](https://rdrr.io/pkg/psych/man/score.items.html)
  reverses them.

## Value

A numeric matrix with one row per item and one column per scale.
