test_that("cria_quartis returns a factor with the requested bins", {
  q <- cria_quartis(1:100)
  expect_s3_class(q, "factor")
  expect_length(q, 100)
  expect_setequal(levels(q), c("<P25", "P25-P50", "P50-P75", ">P75"))
})

test_that("map_factor_levels maps codes to labels", {
  out <- map_factor_levels(factor(c("low", "high", "low")))
  expect_equal(nrow(out), 2)
  expect_named(out, c("levels", "labels"))
})

test_that("sharedcount counts pairwise complete cases", {
  df <- data.frame(a = c(1, NA, 3), b = c(1, 2, NA), c = 1:3)
  out <- sharedcount(df)
  expect_equal(nrow(out), 3)
  expect_true("out" %in% names(out))
})
