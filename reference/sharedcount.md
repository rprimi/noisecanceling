# Count pairwise complete cases between all column pairs

For every pair of columns in a data frame, counts the number of rows
with no missing value on either column. Useful for inspecting the
overlap of responses across items.

## Usage

``` r
sharedcount(x, ...)
```

## Arguments

- x:

  A data frame.

- ...:

  Unused, for extensibility.

## Value

A data frame with one row per column pair and the count of complete
cases.

## Examples

``` r
sharedcount(data.frame(a = c(1, NA, 3), b = c(1, 2, NA), c = 1:3))
#>   X1 X2 out
#> 1  a  b   1
#> 2  a  c   2
#> 3  b  c   2
```
