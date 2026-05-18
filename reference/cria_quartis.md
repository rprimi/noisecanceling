# Cut a numeric vector into quantile-based bins

Splits a numeric vector into ordered groups defined by sample quantiles
(by default quartiles).

## Usage

``` r
cria_quartis(
  x,
  probs = c(0, 0.25, 0.5, 0.75, 1),
  labels = c("<P25", "P25-P50", "P50-P75", ">P75")
)
```

## Arguments

- x:

  A numeric vector.

- probs:

  Numeric vector of probabilities defining the cut points. Defaults to
  quartiles, `c(0, .25, .5, .75, 1)`.

- labels:

  Character vector of labels for the resulting bins.

## Value

A factor of the same length as `x`.

## Examples

``` r
cria_quartis(rnorm(100))
#>   [1] <P25    P50-P75 <P25    P25-P50 P50-P75 >P75    <P25    P25-P50 P25-P50
#>  [10] P25-P50 <P25    >P75    >P75    <P25    P50-P75 <P25    <P25    P25-P50
#>  [19] P50-P75 <P25    P50-P75 P50-P75 <P25    >P75    >P75    P25-P50 <P25   
#>  [28] P25-P50 <P25    <P25    >P75    P50-P75 P50-P75 >P75    P50-P75 P25-P50
#>  [37] <P25    P25-P50 P25-P50 >P75    P25-P50 <P25    P25-P50 P25-P50 P50-P75
#>  [46] >P75    P25-P50 P50-P75 P50-P75 <P25    >P75    P25-P50 P25-P50 P25-P50
#>  [55] P25-P50 P50-P75 <P25    >P75    <P25    P50-P75 >P75    <P25    >P75   
#>  [64] P25-P50 <P25    <P25    >P75    P50-P75 P50-P75 <P25    <P25    P50-P75
#>  [73] >P75    P50-P75 >P75    P50-P75 P50-P75 P25-P50 P50-P75 P25-P50 >P75   
#>  [82] >P75    >P75    P50-P75 <P25    >P75    <P25    P25-P50 P50-P75 <P25   
#>  [91] P50-P75 >P75    >P75    P25-P50 P50-P75 >P75    P25-P50 >P75    >P75   
#> [100] P25-P50
#> Levels: <P25 P25-P50 P50-P75 >P75
```
