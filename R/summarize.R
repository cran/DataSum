#' Summarize a Single Vector
#'
#' Computes a one-row, NA-aware diagnostic summary for one vector. Numeric
#' vectors receive robust and classical statistics, outlier counts, and a
#' normality diagnostic. Non-numeric vectors receive safe type, missingness,
#' uniqueness, and mode summaries.
#'
#' @param x A vector.
#' @param name Optional variable name to store in the `variable` column.
#' @param alpha Significance level for the normality decision. Defaults to 0.05.
#' @param digits Optional number of digits used to round numeric output. By
#'   default, numeric values are not rounded.
#'
#' @return A one-row `data.frame` with summary statistics and diagnostics.
#' @examples
#' summarize_vector(c(1, 2, 2, NA, 5), name = "score")
#' summarize_vector(factor(c("control", "treatment", "control")))
#' @export
summarize_vector <- function(x, name = NA_character_, alpha = 0.05, digits = NULL) {
  .validate_alpha(alpha)
  if (!is.null(digits) && (!is.numeric(digits) || length(digits) != 1 || digits < 0)) {
    stop("`digits` must be NULL or a non-negative numeric scalar.", call. = FALSE)
  }

  n <- length(x)
  missing <- .missing_index(x)
  values <- x[!missing]
  type <- .column_type(x)
  mode <- .mode_details(values)
  numeric_values <- .finite_numeric_values(x)
  n_missing <- sum(missing)
  n_finite <- length(numeric_values)
  normality <- .normality_diagnostics(numeric_values, alpha = alpha)
  stats <- .numeric_summary(numeric_values)
  warnings <- .vector_warnings(type, n, n_missing, n_finite, stats, normality)

  out <- data.frame(
    variable = as.character(name),
    type = type,
    n = n,
    n_complete = n - n_missing,
    n_missing = n_missing,
    missing_pct = if (n == 0) NA_real_ else 100 * n_missing / n,
    n_unique = .safe_unique_count(values),
    mode = mode$value,
    mode_count = mode$count,
    mode_ties = mode$ties,
    mean = stats$mean,
    median = stats$median,
    sd = stats$sd,
    variance = stats$variance,
    minimum = stats$minimum,
    q25 = stats$q25,
    q75 = stats$q75,
    maximum = stats$maximum,
    range = stats$range,
    iqr = stats$iqr,
    mad = stats$mad,
    skewness = stats$skewness,
    excess_kurtosis = stats$excess_kurtosis,
    outlier_count = stats$outlier_count,
    outlier_pct = if (n_finite == 0) NA_real_ else 100 * stats$outlier_count / n_finite,
    normality_test = normality$test,
    normality_statistic = normality$statistic,
    normality_p_value = normality$p_value,
    normality_alpha = alpha,
    normality_decision = normality$decision,
    warning = warnings,
    stringsAsFactors = FALSE
  )

  .round_numeric_columns(out, digits)
}

#' Summarize a Data Frame
#'
#' Applies [summarize_vector()] to every column in a data frame. Optional grouped
#' summaries are supported by passing one or more grouping column names to `by`.
#'
#' @param data A data frame or tibble.
#' @param by Optional character vector of grouping columns.
#' @param alpha Significance level for normality decisions.
#' @param digits Optional number of digits used to round numeric output.
#'
#' @return A `data.frame`, one row per summarized variable and group.
#' @examples
#' summarize_data(iris)
#' summarize_data(iris, by = "Species")
#' @export
summarize_data <- function(data, by = NULL, alpha = 0.05, digits = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }
  .validate_alpha(alpha)

  if (is.null(by)) {
    rows <- Map(
      function(column, column_name) summarize_vector(column, name = column_name, alpha = alpha, digits = digits),
      data,
      names(data)
    )
    return(.bind_rows(rows))
  }

  by <- as.character(by)
  missing_by <- setdiff(by, names(data))
  if (length(missing_by) > 0) {
    stop("Grouping column(s) not found: ", paste(missing_by, collapse = ", "), call. = FALSE)
  }

  target_names <- setdiff(names(data), by)
  if (length(target_names) == 0) {
    stop("At least one non-grouping column is required.", call. = FALSE)
  }
  if (nrow(data) == 0) {
    return(data.frame())
  }

  group_keys <- interaction(data[by], drop = TRUE, lex.order = TRUE, sep = " | ")
  split_rows <- split(seq_len(nrow(data)), group_keys, drop = TRUE)
  rows <- lapply(split_rows, function(row_index) {
    group_values <- data[row_index[1], by, drop = FALSE]
    group_values[] <- lapply(group_values, as.character)
    summary <- summarize_data(data[row_index, target_names, drop = FALSE], alpha = alpha, digits = digits)
    cbind(group_values, summary, stringsAsFactors = FALSE)
  })

  .bind_rows(rows)
}

#' Profile a Data Frame
#'
#' Builds a dataset-level profile containing variable summaries, dataset shape,
#' missingness, duplicate-row counts, type counts, and warnings that deserve
#' analyst attention.
#'
#' @param data A data frame or tibble.
#' @param by Optional grouping columns passed to [summarize_data()].
#' @param alpha Significance level for normality decisions.
#' @param digits Optional number of digits used to round numeric output.
#'
#' @return A `datasum_profile` list with `dataset`, `summary`, and `warnings`.
#' @examples
#' profile <- profile_data(iris)
#' profile$dataset
#' @export
profile_data <- function(data, by = NULL, alpha = 0.05, digits = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame or tibble.", call. = FALSE)
  }

  summary <- summarize_data(data, by = by, alpha = alpha, digits = digits)
  type_values <- vapply(data, .column_type, character(1))
  type_counts <- table(type_values)
  total_cells <- nrow(data) * ncol(data)
  total_missing <- sum(vapply(data, function(column) sum(.missing_index(column)), integer(1)))

  dataset <- data.frame(
    rows = nrow(data),
    columns = ncol(data),
    complete_rows = sum(stats::complete.cases(data)),
    duplicated_rows = sum(duplicated(data)),
    total_missing = total_missing,
    missing_pct = if (total_cells == 0) NA_real_ else 100 * total_missing / total_cells,
    type_profile = paste(names(type_counts), as.integer(type_counts), sep = "=", collapse = ", "),
    stringsAsFactors = FALSE
  )
  dataset <- .round_numeric_columns(dataset, digits)

  profile <- list(
    generated_at = Sys.time(),
    alpha = alpha,
    dataset = dataset,
    summary = summary,
    warnings = .profile_warnings(summary, dataset)
  )
  class(profile) <- "datasum_profile"
  profile
}

#' @export
print.datasum_profile <- function(x, ...) {
  cat("DataSum profile\n")
  cat("Generated:", format(x$generated_at), "\n\n")
  print(x$dataset, row.names = FALSE)
  cat("\nVariable summary:\n")
  print(x$summary, row.names = FALSE)
  if (nrow(x$warnings) > 0) {
    cat("\nWarnings:\n")
    print(x$warnings, row.names = FALSE)
  }
  invisible(x)
}

.validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be a numeric scalar between 0 and 1.", call. = FALSE)
  }
}

.column_type <- function(x) {
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    "datetime"
  } else if (inherits(x, "Date")) {
    "date"
  } else if (is.numeric(x)) {
    "numeric"
  } else if (is.factor(x)) {
    "factor"
  } else if (is.character(x)) {
    "character"
  } else if (is.logical(x)) {
    "logical"
  } else {
    "other"
  }
}

.missing_index <- function(x) {
  missing <- tryCatch(is.na(x), error = function(...) rep(FALSE, length(x)))
  if (is.matrix(missing)) {
    missing <- rowSums(missing) > 0
  }
  as.logical(missing)
}

.safe_unique_count <- function(x) {
  if (length(x) == 0) {
    return(0L)
  }
  length(unique(as.character(x)))
}

.mode_details <- function(x, sep = ", ") {
  if (length(x) == 0) {
    return(list(value = NA_character_, count = 0L, ties = FALSE))
  }
  labels <- as.character(x)
  labels <- labels[!is.na(labels)]
  if (length(labels) == 0) {
    return(list(value = NA_character_, count = 0L, ties = FALSE))
  }

  tab <- table(labels, useNA = "no")
  max_count <- max(tab)
  modes <- names(tab)[tab == max_count]
  if (suppressWarnings(all(!is.na(as.numeric(modes))))) {
    modes <- as.character(sort(as.numeric(modes)))
  } else {
    modes <- sort(modes)
  }

  list(value = paste(modes, collapse = sep), count = as.integer(max_count), ties = length(modes) > 1)
}

.finite_numeric_values <- function(x) {
  if (!is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(numeric())
  }
  x[!is.na(x) & is.finite(x)]
}

.numeric_summary <- function(x) {
  empty <- list(
    mean = NA_real_, median = NA_real_, sd = NA_real_, variance = NA_real_,
    minimum = NA_real_, q25 = NA_real_, q75 = NA_real_, maximum = NA_real_,
    range = NA_real_, iqr = NA_real_, mad = NA_real_, skewness = NA_real_,
    excess_kurtosis = NA_real_, outlier_count = NA_integer_
  )
  if (length(x) == 0) {
    empty$outlier_count <- 0L
    return(empty)
  }

  quantiles <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7)
  iqr <- unname(diff(quantiles))
  outlier_count <- if (length(x) < 4 || is.na(iqr) || iqr == 0) {
    0L
  } else {
    lower <- quantiles[1] - 1.5 * iqr
    upper <- quantiles[2] + 1.5 * iqr
    sum(x < lower | x > upper)
  }

  list(
    mean = mean(x),
    median = stats::median(x),
    sd = if (length(x) < 2) NA_real_ else stats::sd(x),
    variance = if (length(x) < 2) NA_real_ else stats::var(x),
    minimum = min(x),
    q25 = quantiles[1],
    q75 = quantiles[2],
    maximum = max(x),
    range = max(x) - min(x),
    iqr = iqr,
    mad = stats::mad(x, constant = 1.4826, na.rm = TRUE),
    skewness = .moment_skewness(x),
    excess_kurtosis = .moment_excess_kurtosis(x),
    outlier_count = as.integer(outlier_count)
  )
}

.moment_skewness <- function(x) {
  if (length(x) < 3) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  mean(((x - mean(x)) / s)^3)
}

.moment_excess_kurtosis <- function(x) {
  if (length(x) < 4) {
    return(NA_real_)
  }
  s <- stats::sd(x)
  if (is.na(s) || s == 0) {
    return(NA_real_)
  }
  mean(((x - mean(x)) / s)^4) - 3
}

.normality_diagnostics <- function(x, alpha) {
  if (length(x) < 3) {
    return(.normality_row(NA_character_, NA_real_, NA_real_, "Not tested", "Normality requires at least 3 finite numeric values."))
  }
  if (length(unique(x)) < 3) {
    return(.normality_row(NA_character_, NA_real_, NA_real_, "Not tested", "Normality is not meaningful for constant or near-constant data."))
  }

  if (length(x) <= 5000) {
    result <- tryCatch(stats::shapiro.test(x), error = identity)
    test_name <- "Shapiro-Wilk"
  } else {
    result <- tryCatch(nortest::ad.test(x), error = identity)
    test_name <- "Anderson-Darling"
  }

  if (inherits(result, "error")) {
    return(.normality_row(test_name, NA_real_, NA_real_, "Not tested", conditionMessage(result)))
  }

  p_value <- unname(result$p.value)
  decision <- if (is.na(p_value)) {
    "Not tested"
  } else if (p_value > alpha) {
    "No evidence against normality"
  } else {
    "Evidence against normality"
  }

  .normality_row(test_name, unname(result$statistic[1]), p_value, decision, NA_character_)
}

.normality_row <- function(test, statistic, p_value, decision, warning) {
  list(test = test, statistic = statistic, p_value = p_value, decision = decision, warning = warning)
}

.vector_warnings <- function(type, n, n_missing, n_finite, stats, normality) {
  messages <- character()
  if (n == 0) {
    messages <- c(messages, "Vector is empty.")
  }
  if (n > 0 && n_missing == n) {
    messages <- c(messages, "All values are missing.")
  }
  if (type == "numeric" && n_finite == 0 && n > n_missing) {
    messages <- c(messages, "No finite numeric values are available.")
  }
  if (!is.na(normality$warning)) {
    messages <- c(messages, normality$warning)
  }
  if (length(messages) == 0) NA_character_ else paste(unique(messages), collapse = " ")
}

.profile_warnings <- function(summary, dataset) {
  warnings <- list()
  add_warning <- function(variable, level, message) {
    warnings[[length(warnings) + 1L]] <<- data.frame(variable = variable, level = level, message = message, stringsAsFactors = FALSE)
  }

  if (nrow(dataset) > 0 && !is.na(dataset$missing_pct) && dataset$missing_pct > 10) {
    add_warning("<dataset>", "missingness", "More than 10% of dataset cells are missing.")
  }
  if (nrow(dataset) > 0 && dataset$duplicated_rows > 0) {
    add_warning("<dataset>", "duplicates", "Duplicate rows were detected.")
  }

  if (nrow(summary) > 0) {
    for (i in seq_len(nrow(summary))) {
      variable <- summary$variable[i]
      if (!is.na(summary$warning[i])) {
        add_warning(variable, "data-quality", summary$warning[i])
      }
      if (!is.na(summary$missing_pct[i]) && summary$missing_pct[i] > 20) {
        add_warning(variable, "missingness", "More than 20% of this variable is missing.")
      }
      if (!is.na(summary$outlier_pct[i]) && summary$outlier_pct[i] > 5) {
        add_warning(variable, "outliers", "More than 5% of finite numeric values are IQR-rule outliers.")
      }
      if (!is.na(summary$normality_decision[i]) && identical(summary$normality_decision[i], "Evidence against normality")) {
        add_warning(variable, "normality", "Normality test suggests evidence against a normal distribution.")
      }
    }
  }

  .bind_rows(warnings)
}

.round_numeric_columns <- function(data, digits) {
  if (is.null(digits) || ncol(data) == 0) {
    return(data)
  }
  numeric_columns <- vapply(data, is.numeric, logical(1))
  data[numeric_columns] <- lapply(data[numeric_columns], round, digits = digits)
  data
}

.bind_rows <- function(rows) {
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}
