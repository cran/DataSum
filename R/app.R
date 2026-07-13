#' Launch the DataSum Shiny App
#'
#' Opens an interactive Shiny interface for uploading a CSV file, inspecting
#' dataset diagnostics, visualizing a selected variable, and downloading a
#' reproducible DataSum report source file.
#'
#' @param data Optional data frame used as the starting dataset. If omitted, the
#'   app starts with `datasets::iris` until a CSV is uploaded.
#'
#' @return A Shiny application object.
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   app <- run_datasum_app(iris)
#' }
#' @export
run_datasum_app <- function(data = NULL) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("`run_datasum_app()` requires the optional `shiny` package.", call. = FALSE)
  }
  if (!is.null(data) && !is.data.frame(data)) {
    stop("`data` must be NULL or a data frame.", call. = FALSE)
  }

  starter_data <- if (is.null(data)) datasets::iris else data

  ui <- shiny::fluidPage(
    shiny::titlePanel("DataSum Research Diagnostics"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::fileInput("file", "Upload CSV", accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")),
        shiny::uiOutput("variable_picker"),
        shiny::downloadButton("download_report", "Download Quarto report")
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("Dataset", shiny::verbatimTextOutput("dataset_profile")),
          shiny::tabPanel("Summary", shiny::tableOutput("summary_table")),
          shiny::tabPanel("Warnings", shiny::tableOutput("warning_table")),
          shiny::tabPanel("Plot", shiny::plotOutput("variable_plot"))
        )
      )
    )
  )

  server <- function(input, output, session) {
    dataset <- shiny::reactive({
      if (is.null(input$file)) {
        return(starter_data)
      }
      utils::read.csv(input$file$datapath, stringsAsFactors = FALSE, check.names = FALSE)
    })

    output$variable_picker <- shiny::renderUI({
      current <- dataset()
      shiny::selectInput("variable", "Variable", choices = names(current), selected = names(current)[1])
    })

    profile <- shiny::reactive(profile_data(dataset(), digits = 3))

    output$dataset_profile <- shiny::renderPrint({
      print(profile()$dataset, row.names = FALSE)
    })

    output$summary_table <- shiny::renderTable({
      profile()$summary
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$warning_table <- shiny::renderTable({
      warnings <- profile()$warnings
      if (nrow(warnings) == 0) {
        data.frame(message = "No warnings were detected.", stringsAsFactors = FALSE)
      } else {
        warnings
      }
    }, striped = TRUE, bordered = TRUE, spacing = "s")

    output$variable_plot <- shiny::renderPlot({
      shiny::req(input$variable)
      current <- dataset()[[input$variable]]
      if (is.numeric(current)) {
        graphics::hist(current, main = input$variable, xlab = input$variable, col = "#4C78A8", border = "white")
      } else {
        tab <- sort(table(current, useNA = "ifany"), decreasing = TRUE)
        graphics::barplot(tab, main = input$variable, las = 2, col = "#72B7B2")
      }
    })

    output$download_report <- shiny::downloadHandler(
      filename = function() "datasum-report.qmd",
      content = function(file) {
        report <- datasum_report(dataset(), path = file, format = "qmd", render = FALSE)
        if (!identical(normalizePath(report, mustWork = FALSE), normalizePath(file, mustWork = FALSE))) {
          file.copy(report, file, overwrite = TRUE)
        }
      }
    )
  }

  shiny::shinyApp(ui, server)
}
