# SENNA item dictionary

Metadata describing the SENNA items, including the scale each item is
scored on, its keying direction, and the semantic pair it belongs to.
This is the `item_dic` argument expected by
[`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md).

## Usage

``` r
senna_dic
```

## Format

A data frame with 165 rows and 10 columns:

- coditem:

  Item code; matches a column name in
  [data_senna](https://rprimi.github.io/noisecanceling/reference/data_senna.md).

- scale:

  Scale the item is scored on (the Big Five domains A, C, E, N, O, plus
  an overclaiming scale OvCl).

- pole:

  Keying direction: `1` positively keyed (high pole), `0` negatively
  keyed (low pole).

- seman_pairs:

  Identifier of the semantic pair the item belongs to; `NA` for unpaired
  items.

- item_text:

  Item text in English.

- item_text_pt:

  Item text in Portuguese.

- domain:

  Higher-order domain the scale belongs to.

- facet:

  Facet within the domain.

- pers_selfeff:

  Indicator distinguishing personality from self-efficacy framed items.

- seman_pairs0:

  An earlier version of the semantic-pair coding.

## Source

SENNA project, Ayrton Senna Institute / EduLab21.

## See also

[data_senna](https://rprimi.github.io/noisecanceling/reference/data_senna.md)
