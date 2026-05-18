# Recode a data frame and compute the acquiescence index

Estimates a per-person acquiescence index from a set of balanced
(paired) items and recodes every item response by subtracting that
index. This is the core "noise-cancelling" step: acquiescence is the
noise that occurs with both positively keyed (PK) and negatively keyed
(NK) items, and centering each response on the person's acquiescence
index removes it.

## Usage

``` r
recode_for_acq(data, item_dic, acq_index_by_domain = FALSE)
```

## Arguments

- data:

  A data frame of raw item responses. Columns are items, rows are
  respondents. Must contain every item listed in `item_dic$coditem`.

- item_dic:

  A data frame describing the items (the item dictionary). One row per
  item, with at least these columns:

  coditem

  :   Character. Column name of the item in `data`.

  scale

  :   Character. Name of the scale the item is scored on.

  pole

  :   Integer. `1` for a positively keyed (high pole) item, `0` for a
      negatively keyed (low pole) item.

  seman_pairs

  :   Integer or character identifying the semantic pair an item belongs
      to. Items with a non-missing value are treated as paired and used
      to compute the acquiescence index.

  item_text

  :   Character. The text of the item (optional, carried through for
      reporting).

- acq_index_by_domain:

  Logical. If `TRUE`, an acquiescence index and a within-subject
  standard deviation are also computed separately for each scale (using
  only scales with an even number of paired items). Defaults to `FALSE`.

## Value

A list with five elements:

- data:

  The original item responses, restricted to dictionary items.

- data_acq_recoded:

  Item responses centered on the acquiescence index.

- acq_index:

  A data frame with the global acquiescence index (`acq_index`), the
  within-subject standard deviation (`ws_sd`) and, if
  `acq_index_by_domain = TRUE`, the per-scale indices.

- item_dic:

  The item dictionary (column names lower-cased).

- item_dic_acq:

  The dictionary restricted to paired items, with a `cntrst` contrast
  code (`+1` for PK, `-1` for NK).

## Details

The acquiescence index is the within-person mean of all items that
belong to a semantic pair (a PK item and an NK item measuring the same
content). With balanced pairs, agreeing equally with an item and its
opposite can only reflect content-independent agreement, so that mean
estimates acquiescence.

## See also

[`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
to analyse the recoded data.

## Examples

``` r
data(data_senna)
data(senna_dic)
recoded <- recode_for_acq(data_senna, senna_dic)
head(recoded$acq_index)
#>   acq_index     ws_sd
#> 1  2.638298 1.1345130
#> 2  3.180851 1.8431084
#> 3  2.787234 1.2689200
#> 4  2.265957 1.2019297
#> 5  2.755319 0.9581139
#> 6  2.712766 1.0637966
```
