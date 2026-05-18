test_that("find_psychometrics returns original and recoded results", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  rec <- recode_for_acq(data_senna, senna_dic)
  ps <- find_psychometrics(rec, likert = 5, center = 3)

  expect_true(all(c(
    "scores", "psicom_orig", "psicom_recoded",
    "alpha_orig_scale_stat", "alpha_rec_scale_stat",
    "alpha_orig_item_stat", "alpha_rec_item_stat", "keys"
  ) %in% names(ps)))

  expect_equal(nrow(ps$scores), nrow(data_senna))
  expect_s3_class(ps$psicom_orig, "psych")
  expect_true(any(grepl("_ori$", colnames(ps$psicom_orig$scores))))
  expect_true(any(grepl("_rec$", colnames(ps$psicom_recoded$scores))))
})

test_that("dic2keys builds a keys matrix with one column per scale", {
  data(senna_dic, package = "noisecanceling")

  keys <- dic2keys(senna_dic)
  expect_true(is.matrix(keys))
  expect_setequal(colnames(keys), unique(senna_dic$scale))
  expect_equal(nrow(keys), length(unique(senna_dic$coditem)))
})

test_that("score_tests scores every scale in the dictionary", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  res <- score_tests(data_senna, senna_dic)
  expect_setequal(colnames(res$psicom$scores), unique(senna_dic$scale))
  expect_s3_class(res$alpha_scale_stat, "data.frame")
})
