# SENNA socio-emotional skills item responses

Item responses to the SENNA inventory of social and emotional skills,
collected from Brazilian students. The inventory uses balanced scales
(paired positively and negatively keyed items) so that acquiescence can
be estimated and corrected. Most items are answered on a 1-5 Likert
scale.

## Usage

``` r
data_senna
```

## Format

A data frame with 2,300 rows and 205 columns:

- `ExamineeID`, `TestingSessionID` — respondent and testing-session
  identifiers.

- 195 item-response columns (such as `C001_1`, `sv001.v2` or `sv2.725`)
  whose names match the `coditem` values in
  [senna_dic](https://rprimi.github.io/noisecanceling/reference/senna_dic.md).
  Most items use a 1–5 Likert scale.

- `age1`, `Education`, `sex`, `ses`, `educ_oprtnty`, `race_min` —
  demographic variables.

- `profic_lp_15`, `profic_mat_15` — Portuguese-language and mathematics
  proficiency criteria.

## Source

SENNA project, Ayrton Senna Institute / EduLab21.

## See also

[senna_dic](https://rprimi.github.io/noisecanceling/reference/senna_dic.md)
