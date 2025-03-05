library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(lubridate)

ui <- dashboardPage(
  
  dashboardHeader(title = "Feature Engineering App (No Recipes)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("database")),
      menuItem("Scaling", tabName = "scaling", icon = icon("arrows-alt-v")),
      menuItem("Encoding", tabName = "encoding", icon = icon("font")),
      menuItem("Polynomial", tabName = "polynomial", icon = icon("superscript")),
      menuItem("Datetime", tabName = "datetime", icon = icon("calendar")),
      menuItem("Save", tabName = "save", icon = icon("save"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_input",
              fluidRow(
                box(width = 4, title = "Choose Data Source", status = "primary",
                    radioButtons("dataChoice", "Data Source:",
                                 choices = c("Built-in", "Upload"),
                                 selected = "Built-in"),
                    conditionalPanel(
                      condition = "input.dataChoice == 'Built-in'",
                      selectInput("builtinDataset", "Select a dataset:",
                                  choices = c("iris", "mtcars", "airquality", "ToothGrowth"))
                    ),
                    conditionalPanel(
                      condition = "input.dataChoice == 'Upload'",
                      fileInput("fileUpload", "Upload CSV File:",
                                accept = c(".csv", "text/csv"))
                    ),
                    actionButton("loadData", "Load Data")
                ),
                box(width = 8, title = "Data Preview", status = "primary",
                    DTOutput("dataTable")
                )
              )
      ),
      tabItem(tabName = "scaling",
              fluidRow(
                box(width = 4, title = "Scaling Options", status = "warning",
                    uiOutput("scalingUI"),
                    actionButton("applyScaling", "Apply Scaling")
                ),
                box(width = 8, title = "Data Preview After Scaling", status = "warning",
                    DTOutput("scaledTable")
                )
              )
      ),
      tabItem(tabName = "encoding",
              fluidRow(
                box(width = 4, title = "Encoding Options", status = "info",
                    uiOutput("encodingUI"),
                    actionButton("applyEncoding", "Apply Encoding")
                ),
                box(width = 8, title = "Data Preview After Encoding", status = "info",
                    DTOutput("encodedTable")
                )
              )
      ),
      tabItem(tabName = "polynomial",
              fluidRow(
                box(width = 4, title = "Polynomial Features", status = "success",
                    uiOutput("polyUI"),
                    actionButton("applyPoly", "Apply Polynomial")
                ),
                box(width = 8, title = "Data Preview After Polynomial", status = "success",
                    DTOutput("polyTable")
                )
              )
      ),
      tabItem(tabName = "datetime",
              fluidRow(
                box(width = 4, title = "Datetime Feature Engineering", status = "primary",
                    uiOutput("datetimeUI"),
                    actionButton("applyDatetime", "Apply Datetime Extraction")
                ),
                box(width = 8, title = "Data Preview After Datetime", status = "primary",
                    DTOutput("datetimeTable")
                )
              )
      ),
      tabItem(tabName = "save",
              fluidRow(
                box(width = 4, title = "Save Data", status = "primary",
                    p("Download the final transformed data as a CSV"),
                    downloadButton("downloadData", "Download Transformed Data")
                )
                )
              )
      )
    )
  )


server <- function(input, output, session) {
  
  rv <- reactiveValues(
    originalData = NULL,
    transformedData = NULL
  )
  
  observeEvent(input$loadData, {
    if (input$dataChoice == "Built-in") {
      datasetName <- input$builtinDataset
      if (datasetName == "iris") {
        rv$originalData <- iris
      } else if (datasetName == "mtcars") {
        rv$originalData <- mtcars
      } else if (datasetName == "airquality") {
        rv$originalData <- airquality
      } else if (datasetName == "ToothGrowth") {
        rv$originalData <- ToothGrowth
      }
    } else {
      req(input$fileUpload)
      rv$originalData <- read.csv(input$fileUpload$datapath, stringsAsFactors = FALSE)
    }
    rv$transformedData <- rv$originalData
  })
  
  output$dataTable <- renderDT({
    req(rv$originalData)
    datatable(rv$originalData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$scalingUI <- renderUI({
    req(rv$transformedData)
    numCols <- names(rv$transformedData)[sapply(rv$transformedData, is.numeric)]
    tagList(
      checkboxGroupInput("scaleCols", "Select numeric columns to scale:", choices = numCols),
      selectInput("scaleMethod", "Scaling Method:", choices = c("Min-Max", "Z-score"))
    )
  })
  
  observeEvent(input$applyScaling, {
    req(input$scaleCols)
    if (input$scaleMethod == "Min-Max") {
      range_spec <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      rv$transformedData <- rv$transformedData %>% mutate(across(all_of(input$scaleCols), range_spec))
    } else {
      zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
      rv$transformedData <- rv$transformedData %>% mutate(across(all_of(input$scaleCols), zscore))
    }
  })
  
  output$scaledTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$encodingUI <- renderUI({
    req(rv$transformedData)
    factorCols <- names(rv$transformedData)[sapply(rv$transformedData, function(x) is.character(x) || is.factor(x))]
    tagList(
      selectInput("encodeCol", "Select a categorical column to encode:", choices = factorCols),
      selectInput("encodeMethod", "Encoding Method:", choices = c("One-Hot", "Label"))
    )
  })
  
  observeEvent(input$applyEncoding, {
    req(input$encodeCol)
    colName <- input$encodeCol
    if (input$encodeMethod == "One-Hot") {
      temp <- data.frame(var = as.factor(rv$transformedData[[colName]]))
      dummies <- model.matrix(~ var - 1, data = temp)
      colnames(dummies) <- gsub("^var", paste0(colName,'_'), colnames(dummies))
      rv$transformedData[[colName]] <- NULL
      rv$transformedData <- cbind(rv$transformedData, dummies)
    } else {
      rv$transformedData[[colName]] <- as.integer(as.factor(rv$transformedData[[colName]]))
    }
  })
  
  output$encodedTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$polyUI <- renderUI({
    req(rv$transformedData)
    numCols <- names(rv$transformedData)[sapply(rv$transformedData, is.numeric)]
    tagList(
      selectInput("polyCol", "Column for polynomial:", choices = numCols),
      numericInput("polyDegree", "Degree (2 = square, 3 = cube, etc.)", value = 2, min = 2, max = 10)
    )
  })
  
  observeEvent(input$applyPoly, {
    req(input$polyCol, input$polyDegree)
    colName <- input$polyCol
    newColName <- paste0(colName, "^", input$polyDegree)
    rv$transformedData[[newColName]] <- rv$transformedData[[colName]] ^ input$polyDegree
  })
  
  output$polyTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() paste0("transformed_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(rv$transformedData, file, row.names = FALSE)
  )
}

shinyApp(ui, server)
