# Changelog

## noisecanceling 0.1.0

- First packaged release.
- Core acquiescence-correction workflow:
  [`recode_for_acq()`](https://rprimi.github.io/noisecanceling/reference/recode_for_acq.md)
  estimates a per-person acquiescence index from balanced (paired) items
  and centers responses on it;
  [`find_psychometrics()`](https://rprimi.github.io/noisecanceling/reference/find_psychometrics.md)
  compares the psychometrics of the original and acquiescence-corrected
  scores.
- [`score_tests()`](https://rprimi.github.io/noisecanceling/reference/score_tests.md)
  scores scales directly from an item dictionary.
- Item visualisation with
  [`item_histograms()`](https://rprimi.github.io/noisecanceling/reference/item_histograms.md)
  and
  [`describe_likert()`](https://rprimi.github.io/noisecanceling/reference/describe_likert.md).
- Excel export helpers:
  [`save_item_psicom()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom.md),
  [`save_item_psicom_si()`](https://rprimi.github.io/noisecanceling/reference/save_item_psicom_si.md)
  and
  [`save_loadings()`](https://rprimi.github.io/noisecanceling/reference/save_loadings.md).
- Bundled example data: `data_senna` and the `senna_dic` item
  dictionary.
