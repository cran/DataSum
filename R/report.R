#' Create a DataSum Diagnostic Report
#'
#' Creates a self-contained Quarto source file containing dataset diagnostics,
#' variable summaries, warnings, and formula definitions. Set `render = TRUE` to
#' render the report through the optional `quarto` package.
#'
#' @param data A data frame or tibble.
#' @param path Output path. When `render = FALSE`, this should usually end in
#'   `.qmd`. If omitted, a temporary `.qmd` file is created.
#' @param format One of `"qmd"`, `"html"`, `"pdf"`, or `"docx"`.
#' @param title Report title.
#' @param by Optional grouping columns passed to [profile_data()].
#' @param alpha Significance level for normality decisions.
#' @param digits Optional number of digits used to round numeric output.
#' @param render Logical; if `TRUE`, render the Quarto document. Rendering
#'   requires the optional `quarto` package and a working Quarto installation.
#'
#' @return The generated file path, invisibly.
#' @examples
#' report <- datasum_report(iris, render = FALSE)
#' file.exists(report)
#' @export
datasum_report <- function(data,
                           path = NULL,
                           format = c("qmd", "html", "pdf", "docx"),
                           title = "DataSum Diagnostic Report",
                           by = NULL,
                           alpha = 0.05,
                           digits = 3,
                           render = FALSE) {
  format <- match.arg(format)
  profile <- profile_data(data, by = by, alpha = alpha, digits = digits)

  if (is.null(path)) {
    path <- tempfile("datasum-report-", fileext = if (render && format != "qmd") paste0(".", format) else ".qmd")
  }
  qmd_path <- if (tolower(tools::file_ext(path)) == "qmd") {
    path
  } else {
    paste0(tools::file_path_sans_ext(path), ".qmd")
  }

  output_format <- if (format == "qmd") "html" else format
  content <- .report_content(profile, title = title, output_format = output_format)
  writeLines(content, qmd_path, useBytes = TRUE)

  if (isTRUE(render) && format != "qmd") {
    if (!requireNamespace("quarto", quietly = TRUE)) {
      stop("Rendering requires the optional `quarto` package. Install it or call with `render = FALSE`.", call. = FALSE)
    }
    rendered <- quarto::quarto_render(qmd_path, output_format = output_format, quiet = TRUE)
    return(invisible(normalizePath(rendered, mustWork = FALSE)))
  }

  invisible(normalizePath(qmd_path, mustWork = FALSE))
}

.report_content <- function(profile, title, output_format) {
  formulas <- data.frame(
    measure = c("Missing percent", "IQR", "MAD", "IQR outlier rule", "Skewness", "Excess kurtosis", "Normality decision"),
    definition = c(
      "100 * missing values / total values",
      "75th percentile minus 25th percentile",
      "Median absolute deviation scaled by 1.4826",
      "Values below Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR",
      "Mean of standardized cubed deviations",
      "Mean of standardized fourth-power deviations minus 3",
      "p-value > alpha means no evidence against normality"
    ),
    stringsAsFactors = FALSE
  )

  c(
    "---",
    paste0("title: \"", .escape_yaml(title), "\""),
    paste0("format: ", output_format),
    "execute:",
    "  echo: false",
    "---",
    "",
    paste0("Generated: ", format(profile$generated_at)),
    "",
    "## Dataset Overview",
    "",
    .markdown_table(profile$dataset),
    "",
    "## Variable Diagnostics",
    "",
    .markdown_table(profile$summary),
    "",
    "## Analyst Warnings",
    "",
    if (nrow(profile$warnings) == 0) "No warnings were detected by the first-pass diagnostic rules." else .markdown_table(profile$warnings),
    "",
    "## Formula Definitions",
    "",
    .markdown_table(formulas),
    "",
    "## Interpretation Notes",
    "",
    "DataSum is a diagnostic companion, not a substitute for study design, domain expertise, or model-specific assumptions. Treat warnings as prompts for inspection before modeling, publication, or teaching use."
  )
}

.markdown_table <- function(data) {
  if (!is.data.frame(data) || ncol(data) == 0) {
    return("No data available.")
  }
  display <- data
  display[] <- lapply(display, function(column) {
    value <- as.character(column)
    value[is.na(value)] <- "NA"
    gsub("\\|", "\\\\|", value)
  })
  header <- paste("|", paste(names(display), collapse = " | "), "|")
  separator <- paste("|", paste(rep("---", ncol(display)), collapse = " | "), "|")
  if (nrow(display) == 0) {
    return(paste(c(header, separator), collapse = "\n"))
  }
  rows <- apply(display, 1, function(row) paste("|", paste(row, collapse = " | "), "|"))
  paste(c(header, separator, rows), collapse = "\n")
}

.escape_yaml <- function(x) {
  gsub("\"", "'", as.character(x), fixed = TRUE)
}
