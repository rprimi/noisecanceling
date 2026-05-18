# Faceted boxplots of scale scores with standardized regressions

Draws faceted boxplots of one or more scale scores against a grouping
variable and, for each scale, fits a linear model predicting the scale
score from a predictor and a grouping variable, returning the
standardized regression coefficients and R-squared.

## Usage

``` r
boxplot_descritivo(df, scales, x_var, grid_x_var, breaks, intercept)
```

## Arguments

- df:

  A data frame containing the scale scores and the grouping variables.

- scales:

  Character vector of scale-score column names in `df`.

- x_var:

  Name of the variable on the x axis (and a model predictor).

- grid_x_var:

  Name of the variable used for row facetting (and the second model
  predictor).

- breaks:

  Numeric vector of y-axis breaks.

- intercept:

  Numeric. Where to draw a horizontal reference line.

## Value

A list with `plot` (a ggplot2 object) and `regressions` (a data frame of
standardized betas and R-squared, one row per scale).
