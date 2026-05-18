test_that("recode_for_acq returns the documented structure", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  rec <- recode_for_acq(data_senna, senna_dic)

  expect_named(
    rec,
    c("data", "data_acq_recoded", "acq_index", "item_dic", "item_dic_acq")
  )
  expect_s3_class(rec$data_acq_recoded, "data.frame")
  expect_equal(dim(rec$data), dim(rec$data_acq_recoded))
  expect_equal(nrow(rec$acq_index), nrow(data_senna))
  expect_true(all(c("acq_index", "ws_sd") %in% names(rec$acq_index)))
})

test_that("recoded responses are centered on the acquiescence index", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  rec <- recode_for_acq(data_senna, senna_dic)
  item <- names(rec$data)[1]

  expect_equal(
    rec$data_acq_recoded[[item]],
    rec$data[[item]] - rec$acq_index$acq_index,
    tolerance = 1e-8
  )
})

test_that("acq_index_by_domain adds per-scale columns", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  rec <- recode_for_acq(data_senna, senna_dic, acq_index_by_domain = TRUE)
  expect_gt(ncol(rec$acq_index), 2)
})

test_that("recode_for_acq validates its inputs", {
  data(data_senna, package = "noisecanceling")
  data(senna_dic, package = "noisecanceling")

  bad_dic <- senna_dic[, c("coditem", "scale")]
  expect_error(recode_for_acq(data_senna, bad_dic), "missing required column")

  expect_error(
    recode_for_acq(data_senna[, 1:3], senna_dic),
    "missing item column"
  )
})
