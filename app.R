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
  "When processing is complete, a download button for the report will appear in the sidebar."
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

  output$report_download <- downloadHandler(
    filename = function() {
      paste0(input$site_name_input, "_data_validation_report_",Sys.Date(),".xlsx")
    },
    content = function(file) {
      write_xlsx(report(), file)
    }
  )
  output$report_download_ui <- renderUI({
    req(report())
    downloadButton("report_download", "Download Results")
  })
}

shinyApp(ui = ui, server = server)
