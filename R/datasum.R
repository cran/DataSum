#' Get Mode of a Vector (numeric/character/factor)
#'
#' Returns the mode(s) of a vector. By default, returns a single string with
#' all modes collapsed by comma when there are ties. If you need only one mode,
#' set \code{collapse = FALSE} to return the first mode deterministically.
#'
#' @param x A vector (numeric, character, factor, etc.).
#' @param collapse Logical; if TRUE (default), return all modes as a single
#'   comma-separated string. If FALSE, return the first mode only.
#' @return A single value (first mode) or a comma-separated string of modes.
#' @examples
#' getmode(c(1, 2, 2, 3, 4))
#' getmode(c("a", "b", "b", "a"), collapse = TRUE)
#' getmode(c("a", "b", "b", "a"), collapse = FALSE)
#' @export
getmode <- function(x, collapse = TRUE) {
  x2 <- x[!is.na(x)]
  if (length(x2) == 0) {
    return(NA)
  }
  # use match+tabulate so it works for characters as well
  u <- unique(x2)
  tab <- tabulate(match(x2, u))
  modes <- u[tab == max(tab)]
  # For factors, coerce to character to avoid factor printing surprises
  if (is.factor(modes)) modes <- as.character(modes)
  if (length(modes) == 1L) {
    return(modes)
  }
  if (collapse) {
    return(paste(as.character(modes), collapse = ", "))
  }
  modes[[1L]]
}

#' Shapiro/Anderson-Darling Normality Decision
#'
#' Performs Shapiro-Wilk for sample sizes between 3 and 5000 (inclusive),
#' otherwise uses Anderson–Darling. Returns "Normal" if p > 0.05, else "Not Normal".
#'
#' @param data A numeric vector.
#' @return Character scalar: "Normal", "Not Normal", or \code{NA} if not applicable.
#' @examples
#' shapiro_normality_test(rnorm(100))
#' @importFrom stats shapiro.test
#' @importFrom nortest ad.test
#' @export
shapiro_normality_test <- function(data) {
  # ensure numeric and drop NAs
  if (!is.numeric(data)) {
    data <- suppressWarnings(as.numeric(data))
  }
  data2 <- stats::na.omit(data)
  n <- length(data2)
  if (n < 3) {
    return(NA_character_)
  }
  if (n >= 3 && n <= 5000) {
    p <- stats::shapiro.test(data2)$p.value
  } else {
    p <- nortest::ad.test(data2)$p.value
  }
  if (is.na(p)) {
    return(NA_character_)
  }
  if (p > 0.05) "Normal" else "Not Normal"
}

#' Summarize a Single Vector
#'
#' Summarizes a single vector by calculating a consistent set of statistics.
#' Numeric vectors include mean/median/variance/sd/min/max/range/skewness/kurtosis
#' and a normality decision. Character/factor vectors report the mode only.
#'
#' @param data A numeric, character, factor, or other vector.
#' @return A one-row \code{data.frame} with summary statistics.
#' @examples
#' Datum(rnorm(100))
#' Datum(factor(sample(letters[1:3], 20, TRUE)))
#' @importFrom stats median var sd
#' @importFrom moments skewness kurtosis
#' @importFrom dplyr mutate
#' @export
Datum <- function(data) {
  # Count missings before removing them
  nmiss <- sum(is.na(data))
  data_no_na <- data[!is.na(data)]

  if (is.numeric(data)) {
    DataType <- "Numeric"
    n <- length(data_no_na)
    if (n == 0L) {
      Mean <- Median <- Variance <- StDev <- Maximum <- Minimum <- Range <- Skewness <- Kurtosis <- Normality <- NA_real_
      Mode <- NA
    } else {
      Mean <- round(mean(data_no_na), 2)
      Mode <- getmode(data_no_na, collapse = TRUE) # string if multiple modes
      Median <- round(stats::median(data_no_na), 2)
      Variance <- round(stats::var(data_no_na), 2)
      StDev <- round(stats::sd(data_no_na), 2)
      Maximum <- round(max(data_no_na), 2)
      Minimum <- round(min(data_no_na), 2)
      Range <- round(Maximum - Minimum, 2)
      Skewness <- round(moments::skewness(data_no_na), 2)
      Kurtosis <- round(moments::kurtosis(data_no_na), 2)
      Normality <- shapiro_normality_test(data_no_na)
    }
  } else if (is.character(data)) {
    DataType <- "Character"
    n <- length(data_no_na)
    if (n == 0L) {
      Mode <- NA_character_
    } else {
      tab <- sort(table(data_no_na), decreasing = TRUE)
      Mode <- as.character(names(tab)[1L])
    }
    Mean <- Median <- Variance <- StDev <- Maximum <- Minimum <- Range <- Skewness <- Kurtosis <- Normality <- NA
  } else if (is.factor(data)) {
    DataType <- "Factor"
    n <- length(data_no_na)
    if (n == 0L) {
      Mode <- NA_character_
    } else {
      tab <- sort(table(data_no_na), decreasing = TRUE)
      Mode <- as.character(names(tab)[1L])
    }
    Mean <- Median <- Variance <- StDev <- Maximum <- Minimum <- Range <- Skewness <- Kurtosis <- Normality <- NA
  } else {
    DataType <- class(data)[1L]
    n <- length(data_no_na)
    if (n == 0L) {
      Mode <- NA_character_
    } else {
      tab <- sort(table(data_no_na), decreasing = TRUE)
      Mode <- as.character(names(tab)[1L])
    }
    Mean <- Median <- Variance <- StDev <- Maximum <- Minimum <- Range <- Skewness <- Kurtosis <- Normality <- NA
  }

  summary_df <- data.frame(
    DataType = DataType,
    n = n,
    Mean = Mean,
    Mode = Mode,
    Median = Median,
    Variance = Variance,
    StDev = StDev,
    Maximum = if (exists("Maximum")) Maximum else NA,
    Minimum = if (exists("Minimum")) Minimum else NA,
    Range = if (exists("Range")) Range else NA,
    Skewness = if (exists("Skewness")) Skewness else NA,
    Kurtosis = if (exists("Kurtosis")) Kurtosis else NA,
    Normality = Normality,
    Nmissing = nmiss,
    stringsAsFactors = FALSE
  )

  summary_df <- dplyr::mutate(summary_df, Source = "Datum Function")
  return(summary_df)
}

#' Summarize an Entire Data Frame
#'
#' Applies \code{Datum()} to each column of a data frame and binds the results.
#'
#' @param data A data frame (tibble is also ok).
#' @return A data frame, one row per input column.
#' @examples
#' DataSumm(iris)
#' @export
DataSumm <- function(data) {
  if (!is.data.frame(data)) {
    stop("DataSumm requires a data.frame or tibble.", call. = FALSE)
  }
  summaries <- lapply(data, Datum)
  summaries_df <- do.call(rbind, summaries)
  rownames(summaries_df) <- names(data)
  as.data.frame(summaries_df, stringsAsFactors = FALSE)
}
