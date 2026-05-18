# Map factor level codes to their labels

Map factor level codes to their labels

## Usage

``` r
map_factor_levels(x)
```

## Arguments

- x:

  A factor.

## Value

A data frame with the integer level codes (`levels`) and their string
labels (`labels`).

## Examples

``` r
map_factor_levels(factor(c("low", "high", "low")))
#>   levels labels
#> 1      1   high
#> 2      2    low
```
