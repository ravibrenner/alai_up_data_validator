library(shiny)
library(bslib)
library(shinyjs)
library(readxl)
library(data.validator)
library(assertr)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(DT)

options(shiny.maxRequestSize = 30 * 1024^2)

source("R/data_validator.R")

ui <- page_sidebar(
  title = "Data Validation App",
  useShinyjs(),

  sidebar = sidebar(
    fileInput("file1", "Choose a File (.xlsx)", accept = c(".xlsx", ".xls")),
    selectInput("select_sheet", "Select a sheet", choices = NULL),
    textInput("site_name_input", "Site name", value = NULL),
    disabled(actionButton(
      "go_button", "Process data", icon = icon("play")
    )),
    uiOutput("report_download_ui")
  ),

  navset_tab(
    nav_panel("Home",
              p("Complete instructions will go here."),
              p("1. Upload an Excel file containing the data you want to validate."),
              p("2. Select the sheet within the Excel file that contains the data."),
              p("3. Enter the name of the site (this will be used in the report filename)."),
              p("4. Click the 'Process data' button to run the validation checks."),
              p("When processing is complete, a download button for the report will appear in the sidebar."),
              p("The tabs here provide a summary of the errors found, as well as detailed information about each error, and a summary of missing demographic data (both found in the excel sheet too).")),
    nav_panel("Error Summary", DT::dataTableOutput("error_summary_table")),
    nav_panel("Error Details", DT::dataTableOutput("error_full_table")),
    nav_panel("Missing Demographics", DT::dataTableOutput("missing_summary"))
  )
)

server <- function(input, output, session) {
  observeEvent(input$file1, {
    file_path <- input$file1$datapath

    # Get all sheet names from the uploaded Excel file
    sheet_names <- excel_sheets(file_path)

    # Dynamically update the 'select_sheet' input with the found names
    updateSelectInput(
      session,
      inputId = "select_sheet",
      choices = sheet_names,
      selected = NULL # Automatically select the first sheet
    )
  })

  observe({
    file_ready <- !is.null(input$file1)
    name_ready <- !is.null(input$site_name_input) && input$site_name_input != ""
    if (file_ready && name_ready) {
      enable("go_button")
    } else {
      disable("go_button")
    }
  })

  # loading in the master dataset and processing it
  report <- eventReactive(input$go_button, {
    req(input$file1, input$select_sheet)

    report_data <- withProgress(message = 'Processing data', value = 0, {
      n_steps <- 17 # Number of progress updates in data_validation_report

      progress_updater <- function(detail = NULL) {
        incProgress(1/n_steps, detail = detail)
      }

      result <- data_validator(
        input$file1$datapath,
        sheet = input$select_sheet,
        progress_updater = progress_updater)

      result
    })
    req(report_data)
    return(report_data)
  })

  output$error_summary_table <- DT::renderDataTable({
    req(report())

    report()$final_report |>
      tibble::as_tibble() |>
      summarize(.by = short_message,
                error_count = n()) |>
      arrange(desc(error_count)) |>
      DT::datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = FALSE,
          ordering = TRUE
        ),
        rownames = FALSE,
        colnames = c("Description", "Error Count")
      )
  })



  output$error_full_table <- DT::renderDataTable({
    req(report())
    report()$final_report |>
      select(-short_message) |>
      DT::datatable(
        filter = "top",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 25, 50),
          autoWidth = TRUE,
          scrollX = TRUE,
          searching = FALSE,
          ordering = TRUE
        ),
        rownames = FALSE
      )
  })

  output$missing_summary <- DT::renderDataTable({
    req(report())

    report()$missing_demographics |>
      DT::datatable(
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          searching = FALSE,
          ordering = FALSE
        ),
        rownames = FALSE,
        colnames = c("Column", "Missing Count", "Total Rows")
      )
  })

  download_data <- reactive({
    req(report())

    # Get the report data
    current_report <- report()
    # Remove the 'short_message' column from the final_report dataframe within the current_report list
    current_report$final_report <- current_report$final_report |>
      select(-short_message)
    # Return the modified report list
    return(current_report)
  })

  output$report_download <- downloadHandler(
    filename = function() {
      paste0(input$site_name_input, "_data_validation_report_",Sys.Date(),".xlsx")
    },
    content = function(file) {
      write_xlsx(download_data(), file)
    }
  )
  output$report_download_ui <- renderUI({
    req(report())
    downloadButton("report_download", "Download Results")
  })
}

shinyApp(ui = ui, server = server)
