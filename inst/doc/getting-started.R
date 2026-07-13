## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(DataSum)

## ----vector-------------------------------------------------------------------
summarize_vector(c(1, 2, 2, NA, 10), name = "score")

## ----data---------------------------------------------------------------------
summarize_data(iris)

## ----grouped------------------------------------------------------------------
summarize_data(iris, by = "Species")

## ----profile------------------------------------------------------------------
profile <- profile_data(iris)
profile$dataset
profile$warnings

## ----report-------------------------------------------------------------------
report_path <- datasum_report(iris, format = "qmd", render = FALSE)
file.exists(report_path)

