test_that("datasum_report writes a Quarto source file", {
  path <- tempfile(fileext = ".qmd")
  result <- datasum_report(iris, path = path, format = "qmd", render = FALSE)

  expect_true(file.exists(result))
  content <- readLines(result, warn = FALSE)
  expect_true(any(grepl("Dataset Overview", content, fixed = TRUE)))
  expect_true(any(grepl("Formula Definitions", content, fixed = TRUE)))
})

test_that("run_datasum_app returns a Shiny app object when shiny is installed", {
  skip_if_not_installed("shiny")
  app <- run_datasum_app(iris)
  expect_s3_class(app, "shiny.appobj")
})
