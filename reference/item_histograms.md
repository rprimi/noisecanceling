# Plot item response distributions by scale and pole

Builds a faceted bar chart of the proportion of respondents choosing
each response category, with one facet per scale and per item pole.
Useful for inspecting whether positively and negatively keyed items
behave symmetrically before correcting for acquiescence.

## Usage

``` r
item_histograms(df, coditems, scales, poles, r_levels)
```

## Arguments

- df:

  A data frame of item responses.

- coditems:

  Character vector of item column names in `df`.

- scales:

  Character vector (same length as `coditems`) giving the scale of each
  item.

- poles:

  Vector (same length as `coditems`) giving the pole of each item (`1`
  positively keyed, `0` negatively keyed).

- r_levels:

  Vector of the ordered response categories (e.g. `1:5`).

## Value

A ggplot2 object.

## Examples

``` r
data(data_senna)
data(senna_dic)
d <- senna_dic[senna_dic$scale == "C", ]
item_histograms(data_senna, d$coditem, d$scale, d$pole, r_levels = 1:5)
```
