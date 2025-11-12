test_that("getmode works for numeric, including multimodal", {
  x <- c(1, 2, 2, 3, 3, NA)
  # default collapse=TRUE returns comma-separated string of modes
  expect_true(is.character(getmode(x)))
  expect_true(grepl("2", getmode(x)))
  expect_true(grepl("3", getmode(x)))
  # collapse=FALSE returns first mode deterministically
  expect_true(getmode(x, collapse = FALSE) %in% c(2, 3))
})

test_that("Datum handles numeric with NAs (NA-safe stats)", {
  set.seed(1)
  x <- c(rnorm(100), NA, NA)
  d <- Datum(x)
  expect_equal(d$DataType, "Numeric")
  expect_equal(d$Nmissing, 2)
  # mean/median should be finite numbers (na-safe), not NA
  expect_true(is.finite(d$Mean))
  expect_true(is.finite(d$Median))
  # Range = Max - Min
  expect_equal(d$Range, round(d$Maximum - d$Minimum, 2))
  # Normality should be either "Normal" or "Not Normal"
  expect_true(is.na(d$Normality) || d$Normality %in% c("Normal", "Not Normal"))
})

test_that("Datum handles character and factor", {
  ch <- c("a", "b", "b", NA, "c")
  df_ch <- Datum(ch)
  expect_equal(df_ch$DataType, "Character")
  expect_equal(df_ch$Nmissing, 1)
  expect_equal(df_ch$Mode, "b")
  expect_true(all(is.na(df_ch[, c("Mean", "Median", "Variance", "StDev", "Maximum", "Minimum", "Range", "Skewness", "Kurtosis", "Normality")])))

  fa <- factor(c("x", "x", "y", NA))
  df_fa <- Datum(fa)
  expect_equal(df_fa$DataType, "Factor")
  expect_equal(df_fa$Mode, "x")
})

test_that("shapiro_normality_test returns NA for n<3, character scalar otherwise", {
  expect_true(is.na(shapiro_normality_test(c(1, 2))))
  set.seed(42)
  # n in [3,5000] uses Shapiro
  out <- shapiro_normality_test(rnorm(50))
  expect_true(is.character(out) && length(out) == 1)
  expect_true(out %in% c("Normal", "Not Normal"))
  # n > 5000 uses AD
  out2 <- shapiro_normality_test(rnorm(6000))
  expect_true(is.character(out2) && length(out2) == 1)
  expect_true(out2 %in% c("Normal", "Not Normal"))
})

test_that("DataSumm validates input and summarizes iris", {
  expect_error(DataSumm(1:5), "requires a data.frame")
  res <- DataSumm(iris)
  expect_true(is.data.frame(res))
  expect_equal(nrow(res), ncol(iris))
  expect_true(all(c("DataType", "n", "Mean", "Mode", "Median", "Variance", "StDev", "Maximum", "Minimum", "Range", "Skewness", "Kurtosis", "Normality", "Nmissing", "Source") %in% names(res)))
})
