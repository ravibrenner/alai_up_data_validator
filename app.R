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

options(shiny.maxRequestSize = 30 * 1024^2)

source("R/data_validator.R")

ui <- page_sidebar(
  title = "Data Validation App",

  sidebar = sidebar(
    fileInput("file1", "Choose a File (.xlsx)", accept = c(".xlsx", ".xls")),
    selectInput("select_sheet", "Select a sheet", choices = NULL),
    textInput("site_name_input", "Site name", value = NULL),
    disabled(actionButton(
      "go_button", "Process data", icon = icon("play")
    )),

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

    result <- tryCatch({
      data <- data_validation_report(
        input$file1$datapath,
        sheet = input$select_sheet)

      data

    }, error = function(e) {
      showNotification(
        paste(
          "Error processing data. Please check your data carefully.\n
              The R error message was:",
          conditionMessage(e)
        ),
        type = "error",
        duration = 10,
        id = "processing_error" # Prevents duplicate notifications
      )

      return(NULL)
    })

    req(result)
    return(result)

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
