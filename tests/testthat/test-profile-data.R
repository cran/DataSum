test_that("profile_data returns dataset, summary, and warnings", {
  data <- data.frame(
    x = c(1, 2, NA, 100),
    y = c("a", "a", NA, "b"),
    stringsAsFactors = FALSE
  )

  profile <- profile_data(data)

  expect_s3_class(profile, "datasum_profile")
  expect_named(profile, c("generated_at", "alpha", "dataset", "summary", "warnings"))
  expect_equal(profile$dataset$rows, 4)
  expect_equal(profile$dataset$columns, 2)
  expect_true(nrow(profile$summary) == 2)
  expect_true(is.data.frame(profile$warnings))
})
