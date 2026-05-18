# Diverging bar plot of Likert item responses

Produces a centered ("diverging") stacked bar plot of Likert item
responses using the likert package, with items ordered by their location
(e.g. a Thurstone threshold). Low, neutral and high response categories
are shown in distinct colors.

## Usage

``` r
describe_likert(
  data,
  coditem,
  item_text,
  pole,
  item_location,
  item_text_max = 28,
  center = 3,
  categ_levels = c("1", "2", "3", "4", "5"),
  categ_labels = c("nada", "pouco", "moderad.", "muit.", "totalm."),
  low.color = "#DF4949",
  neutral.color = "#EEE657",
  high.color = "#2CCA90"
)
```

## Arguments

- data:

  A data frame of raw item responses (one column per item).

- coditem:

  Character vector of item codes.

- item_text:

  Character vector of item labels (used on the y axis).

- pole:

  Vector of item poles (`1` positively keyed, `0` negatively keyed).

- item_location:

  Numeric vector of item locations used to order items.

- item_text_max:

  Integer. Maximum width of item labels before wrapping. Defaults to
  `28`.

- center:

  Numeric. The neutral response category. Defaults to `3`.

- categ_levels:

  Character vector of the response category codes.

- categ_labels:

  Character vector of the response category labels.

- low.color, neutral.color, high.color:

  Fill colors for the low, neutral and high response categories.

## Value

A ggplot2 object.
