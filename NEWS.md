# noisecanceling 0.1.0

* First packaged release.
* Core acquiescence-correction workflow: `recode_for_acq()` estimates a
  per-person acquiescence index from balanced (paired) items and centers
  responses on it; `find_psychometrics()` compares the psychometrics of the
  original and acquiescence-corrected scores.
* `score_tests()` scores scales directly from an item dictionary.
* Item visualisation with `item_histograms()` and `describe_likert()`.
* Excel export helpers: `save_item_psicom()`, `save_item_psicom_si()` and
  `save_loadings()`.
* Bundled example data: `data_senna` and the `senna_dic` item dictionary.
