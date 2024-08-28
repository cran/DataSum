#' Get Mode of a Numeric Vector
#'
#' This function calculates the mode of a numeric vector.
#'
#' @param data A numeric vector.
#' @return The mode of the numeric vector.
#' @examples
#' getmode(c(1, 2, 2, 3, 4))
#' @export
getmode <- function(data) {
  u <- unique(data)
  tab <- tabulate(match(data, u))
  u[tab == max(tab)]
}

#' Perform Normality Test
#'
#' This function performs the Shapiro-Wilk test if the sample size is between 3 and 5000. Otherwise, it performs the Anderson-Darling test.
#'
#' @param data A numeric vector.
#' @return A character string indicating whether the data is "Normal" or "Not Normal".
#' @examples
#' shapiro_normality_test(rnorm(100))
#' @importFrom stats shapiro.test
#' @importFrom nortest ad.test
#' @export
shapiro_normality_test <- function(data) {
  if (length(data) >= 3 & length(data) <= 5000) {
    shapiro_result <- shapiro.test(data)
    Normality <- shapiro_result$p.value

    if (Normality > 0.05) {
      return("Normal")
    } else {
      return("Not Normal")
    }
  } else {
    ad_result <- ad.test(data)  # Perform Anderson-Darling test
    Normality <- ad_result$p.value

    if (Normality > 0.05) {
      return("Normal")
    } else {
      return("Not Normal")
    }
  }
}

#' Summarize a Single Vector
#'
#' This function summarizes a single vector by calculating various statistics.
#'
#' @param data A numeric, character, or factor vector.
#' @return A data frame with summary statistics.
#' @examples
#' Datum(rnorm(100))
#' @importFrom stats median var sd
#' @importFrom moments skewness kurtosis
#' @importFrom dplyr mutate
#' @export
Datum <- function(data) {
  if (is.numeric(data)) {
    DataType <- "Numeric"
    n <- length(data)
    Mean <- round(mean(data), 2)
    Mode <- round(getmode(data), 2)
    Median <- round(median(data), 2)
    Variance <- round(var(data), 2)
    StDev <- round(sd(data), 2)
    Maximum <- round(max(data), 2)
    Minimum <- round(min(data), 2)
    Range <- round(max(data) - min(data), 2)
    Skewness <- round(skewness(data), 2)
    Kurtosis <- round(kurtosis(data), 2)
    Normality <- shapiro_normality_test(data)
    nmiss <- sum(is.na(data))
  } else if (is.character(data)) {
    DataType <- "Character"
    n <- length(data)
    Mode <- as.character(names(sort(-table(data)))[1])
    Mean <- NA
    Median <- NA
    Variance <- NA
    StDev <- NA
    Maximum <- NA
    Minimum <- NA
    Range <- NA
    Skewness <- NA
    Kurtosis <- NA
    Normality <- NA
    nmiss <- sum(is.na(data))
  } else if (is.factor(data)) {
    DataType <- "Factor"
    n <- length(data)
    Mode <- as.character(names(sort(-table(data)))[1])
    Mean <- NA
    Median <- NA
    Variance <- NA
    StDev <- NA
    Maximum <- NA
    Minimum <- NA
    Range <- NA
    Skewness <- NA
    Kurtosis <- NA
    Normality <- NA
    nmiss <- sum(is.na(data))
  }

  summary_df <- data.frame(DataType = DataType, n = n, Mean = Mean, Mode = Mode, Median = Median, Variance = Variance,
                           StDev = StDev, Maximum = Maximum, Minimum = Minimum, Range = Range, Skewness = Skewness, Kurtosis = Kurtosis, Normality = Normality, Nmissing = nmiss)

  summary_df <- dplyr::mutate(summary_df, Source = "Datum Function")

  return(summary_df)
}

#' Summarize an Entire Data Frame
#'
#' This function summarizes each column of a data frame by calculating various statistics.
#'
#' @param data A data frame.
#' @return A data frame with summary statistics for each column.
#' @examples
#' DataSumm(iris)
#' @export
DataSumm <- function(data) {
  summaries <- lapply(data, Datum)
  summaries_df <- do.call(rbind, summaries)
  return(summaries_df)
}
