test_that("summarize_vector is NA-aware for numeric data", {
  result <- summarize_vector(c(1, 2, 2, NA, 10), name = "score")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$variable, "score")
  expect_equal(result$n, 5)
  expect_equal(result$n_missing, 1)
  expect_equal(result$mode, "2")
  expect_equal(result$mode_count, 2)
  expect_equal(result$minimum, 1)
  expect_equal(result$maximum, 10)
})

test_that("summarize_vector returns stable tied modes in one row", {
  result <- summarize_vector(c(1, 1, 2, 2), name = "tie")

  expect_equal(nrow(result), 1)
  expect_equal(result$mode, "1, 2")
  expect_true(result$mode_ties)
})

test_that("summarize_vector handles all missing and empty data", {
  all_missing <- summarize_vector(c(NA_real_, NA_real_))
  empty <- summarize_vector(numeric())

  expect_equal(all_missing$n_missing, 2)
  expect_true(is.na(all_missing$mean))
  expect_match(all_missing$warning, "All values are missing", fixed = TRUE)
  expect_equal(empty$n, 0)
  expect_match(empty$warning, "Vector is empty", fixed = TRUE)
})

test_that("normality diagnostics do not crash on small or constant vectors", {
  small <- summarize_vector(c(1, 2))
  constant <- summarize_vector(rep(5, 10))

  expect_equal(small$normality_decision, "Not tested")
  expect_equal(constant$normality_decision, "Not tested")
  expect_true(is.na(constant$normality_p_value))
})

test_that("summarize_vector handles common non-numeric vectors", {
  character_result <- summarize_vector(c("b", "a", "b", NA))
  factor_result <- summarize_vector(factor(c("yes", "no", "yes")))
  logical_result <- summarize_vector(c(TRUE, FALSE, TRUE, NA))
  date_result <- summarize_vector(as.Date(c("2026-01-01", "2026-01-02", NA)))

  expect_equal(character_result$type, "character")
  expect_equal(character_result$mode, "b")
  expect_equal(factor_result$type, "factor")
  expect_equal(factor_result$mode, "yes")
  expect_equal(logical_result$type, "logical")
  expect_equal(logical_result$mode, "TRUE")
  expect_equal(date_result$type, "date")
  expect_equal(date_result$n_missing, 1)
})
