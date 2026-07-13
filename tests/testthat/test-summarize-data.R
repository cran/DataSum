test_that("summarize_data summarizes each column", {
  data <- data.frame(
    score = c(1, 2, NA, 4),
    group = c("a", "a", "b", "b"),
    stringsAsFactors = FALSE
  )

  result <- summarize_data(data)

  expect_equal(nrow(result), 2)
  expect_setequal(result$variable, c("score", "group"))
  expect_equal(result$n_missing[result$variable == "score"], 1)
})

test_that("summarize_data supports grouped summaries", {
  result <- summarize_data(iris, by = "Species", digits = 2)

  expect_true("Species" %in% names(result))
  expect_true("variable" %in% names(result))
  expect_equal(length(unique(result$Species)), 3)
  expect_true(any(result$variable == "Sepal.Length"))
})

test_that("summarize_data validates grouping columns", {
  expect_error(summarize_data(iris, by = "missing"), "Grouping column")
})
