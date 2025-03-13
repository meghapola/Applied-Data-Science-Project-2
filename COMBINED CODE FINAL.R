library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(janitor)
library(DT)
library(readxl)
library(stringr)
library(lubridate)
library(jsonlite)
library(arrow)
library(caret)
library(data.table)

ui <- dashboardPage(
  dashboardHeader(title = "Project2 APP"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input",   tabName = "data_input",   icon = icon("database")),
      menuItem("Data Cleaning",tabName = "cleaning",     icon = icon("broom")),
      menuItem("Scaling",      tabName = "scaling",      icon = icon("arrows-alt-v")),
      menuItem("Encoding",     tabName = "encoding",     icon = icon("font")),
      menuItem("Polynomial",   tabName = "polynomial",   icon = icon("superscript")),
      menuItem("Interaction",  tabName = "interaction",  icon = icon("times")),
      menuItem("Datetime",     tabName = "datetime",     icon = icon("calendar")),
      menuItem("EDA",          tabName = "eda",          icon = icon("chart-bar")), 
      menuItem("Save",         tabName = "save",         icon = icon("save"))        
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "data_input",
              fluidRow(
                box(width = 4, title = "Load Data", status = "primary",
                    fileInput("file", "Upload File", accept = c(".csv", ".xlsx", ".json", ".rds")),
                    selectInput("sample_data", "Or Choose a Sample Dataset:", choices = c("None", "mtcars", "iris")),
                    helpText("After uploading or choosing a dataset, the table on the right will be updated.")
                ),
                box(width = 8, title = "Raw Data Preview", status = "primary",
                    DTOutput("raw_data"))
              )
      ),
      tabItem(tabName = "cleaning",
              fluidRow(
                box(width = 4, title = "Data Cleaning Options", status = "warning",
                    checkboxGroupInput("clean_options", "Select Cleaning Steps:",
                                       choices = list(
                                         "Standardize Column Names" = "standardize",
                                         "Remove Duplicates" = "remove_dupes",
                                         "Handle Missing Values (Mean)" = "impute_mean",
                                         "Handle Missing Values (Median)" = "impute_median",
                                         "Trim Whitespace" = "trim_whitespace",
                                         "Convert Text to Lowercase" = "lowercase_text",
                                         "Convert Dates to Standard Format" = "convert_dates",
                                         "Remove Special Characters" = "remove_special_chars",
                                         "Remove Outliers (Z-score > 3)" = "remove_outliers",
                                         "Replace Outliers w/ Mean" = "replace_outliers",
                                         "Drop Columns with High Missing" = "drop_high_na",
                                         "Fix Inconsistent Categorical" = "fix_categorical"
                                       )
                    ),
                    actionButton("apply_cleaning", "Apply Cleaning")
                ),
                box(width = 8, title = "Cleaned Data Preview", status = "warning",
                    DTOutput("cleaned_data"))
              )
      ),
      tabItem(tabName = "scaling",
              fluidRow(
                box(width = 4, title = "Scaling Options", status = "warning",
                    uiOutput("scalingUI"),
                    actionButton("applyScaling", "Apply Scaling")
                ),
                box(width = 8, title = "Data Preview After Scaling", status = "warning",
                    DTOutput("scaledTable"))
              )
      ),
      tabItem(tabName = "encoding",
              fluidRow(
                box(width = 4, title = "Encoding Options", status = "info",
                    uiOutput("encodingUI"),
                    actionButton("applyEncoding", "Apply Encoding")
                ),
                box(width = 8, title = "Data Preview After Encoding", status = "info",
                    DTOutput("encodedTable"))
              )
      ),
      tabItem(tabName = "polynomial",
              fluidRow(
                box(width = 4, title = "Polynomial Features", status = "success",
                    uiOutput("polyUI"),
                    actionButton("applyPoly", "Apply Polynomial")
                ),
                box(width = 8, title = "Data Preview After Polynomial", status = "success",
                    DTOutput("polyTable"))
              )
      ),
      tabItem(tabName = "interaction",
              fluidRow(
                box(width = 4, title = "Interaction Features", status = "primary",
                    uiOutput("interactionUI"),
                    actionButton("applyInteraction", "Apply Interaction")
                ),
                box(width = 8, title = "Data Preview After Interaction", status = "primary",
                    DTOutput("interactionTable"))
              )
      ),
      tabItem(tabName = "datetime",
              fluidRow(
                box(width = 4, title = "Datetime Feature Engineering", status = "primary",
                    uiOutput("datetimeUI"),
                    actionButton("applyDatetime", "Apply Datetime Extraction")
                ),
                box(width = 8, title = "Data Preview After Datetime", status = "primary",
                    DTOutput("datetimeTable"))
              )
      ),
      tabItem(tabName = "eda",
              fluidRow(
                box(width = 4, title = "EDA Options", status = "primary",
                    selectInput("eda_x_var", "Select X-axis Variable:", choices = NULL),
                    selectInput("eda_y_var", "Select Y-axis Variable (Optional):", choices = NULL),
                    selectInput("eda_plot_type", "Select Chart Type:",
                                choices = c("Time Series Plot", "Scatter Plot", "Histogram", "Box Plot",
                                            "Density Plot", "Pie Chart", "Line Chart", "Correlation Heatmap")),
                    textInput("eda_plot_title", "Enter Chart Title", "Data Visualization"),
                    textInput("eda_x_label", "X-axis Label", "X Variable"),
                    textInput("eda_y_label", "Y-axis Label", "Y Variable"),
                    textInput("eda_x_interval", "X-axis Interval (Optional)", ""),
                    selectInput("eda_date_format", "Date Format (For Time Series)", 
                                choices = c("%Y-%m-%d", "%Y-%m", "%m/%d/%Y")),
                    actionButton("generate_plot", "Generate Plot")
                ),
                box(width = 8, title = "EDA Plot Output", status = "primary",
                    plotOutput("plot_output", height = "500px"))
              ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Data Preview", tableOutput("data_preview")),
                  tabPanel("Data Inspection", verbatimTextOutput("data_check")),
                  tabPanel("Categorical Variable Analysis",
                           selectInput("cat_var", "Select Categorical Variable", choices = NULL),
                           tableOutput("cat_summary")),
                  tabPanel("Numerical Variable Analysis", tableOutput("num_summary")),
                )
              ),
      ),
    
    tabItem(tabName = "save",
            fluidRow(
              box(width = 4, title = "Save Data", status = "primary",
                  p("Download the final transformed data as a CSV"),
                  downloadButton("downloadData", "Download Transformed Data")
              )
            )
    )
  ))
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    originalData = NULL,
    transformedData = NULL
  )
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    df <- switch(ext,
                 "csv" = read.csv(input$file$datapath, stringsAsFactors = FALSE),
                 "xlsx" = readxl::read_excel(input$file$datapath),
                 "json" = jsonlite::fromJSON(input$file$datapath, flatten = TRUE) %>% as.data.frame(),
                 "rds" = readRDS(input$file$datapath),
                 {
                   showNotification("Unsupported file format!", type = "error")
                   return(NULL)
                 }
    )
    rv$originalData <- df
    rv$transformedData <- df
  })
  observeEvent(input$sample_data, {
    if (input$sample_data == "None") return(NULL)
    df <- switch(input$sample_data,
                 "mtcars" = mtcars,
                 "iris" = iris,
                 NULL)
    if (!is.null(df)) {
      rv$originalData <- df
      rv$transformedData <- df
    }
  })
  output$raw_data <- renderDT({
    req(rv$originalData)
    datatable(rv$originalData, options = list(pageLength = 5, scrollX = TRUE))
  })
  observeEvent(input$apply_cleaning, {
    req(rv$transformedData)
    df <- rv$transformedData
    if ("standardize" %in% input$clean_options) {
      df <- df %>% clean_names()
    }
    if ("remove_dupes" %in% input$clean_options) {
      df <- df %>% distinct()
    }
    if ("impute_mean" %in% input$clean_options) {
      df <- df %>% mutate(across(where(is.numeric), ~replace_na(.x, mean(.x, na.rm = TRUE))))
    }
    if ("impute_median" %in% input$clean_options) {
      df <- df %>% mutate(across(where(is.numeric), ~replace_na(.x, median(.x, na.rm = TRUE))))
    }
    if ("trim_whitespace" %in% input$clean_options) {
      df <- df %>% mutate(across(where(is.character), str_trim))
    }
    if ("lowercase_text" %in% input$clean_options) {
      df <- df %>% mutate(across(where(is.character), tolower))
    }
    if ("convert_dates" %in% input$clean_options) {
      date_like_cols <- names(df)[sapply(df, function(x) {
        is.character(x) && any(grepl("\\d{4}", x))
      })]
      for (col in date_like_cols) {
        tmp_date <- suppressWarnings(lubridate::ymd(df[[col]]))
        df[[col]] <- ifelse(is.na(tmp_date), df[[col]], as.character(tmp_date))
      }
    }
    if ("remove_special_chars" %in% input$clean_options) {
      df <- df %>% mutate(across(where(is.character), ~gsub("[^A-Za-z0-9 ]", "", .x)))
    }
    if ("remove_outliers" %in% input$clean_options) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      for (col in num_cols) {
        z_scores <- scale(df[[col]], center = TRUE, scale = TRUE)
        df <- df[abs(z_scores) <= 3, , drop = FALSE]
      }
    }
    if ("replace_outliers" %in% input$clean_options) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      for (col in num_cols) {
        z_scores <- scale(df[[col]], center = TRUE, scale = TRUE)
        outlier_idx <- which(abs(z_scores) > 3)
        if (length(outlier_idx) > 0) {
          df[[col]][outlier_idx] <- mean(df[[col]], na.rm = TRUE)
        }
      }
    }
    if ("drop_high_na" %in% input$clean_options) {
      na_prop <- colMeans(is.na(df))
      drop_cols <- names(na_prop[na_prop > 0.5])
      if (length(drop_cols) > 0) {
        df <- df %>% select(-all_of(drop_cols))
      }
    }
    if ("fix_categorical" %in% input$clean_options) {
      cat_cols <- sapply(df, is.character)
      for (col in names(df)[cat_cols]) {
        df[[col]] <- gsub("^yes$|^y$|^ye$|^yeah$", "yes", df[[col]])
        df[[col]] <- gsub("^no$|^n$|^nope$", "no", df[[col]])
      }
    }
    rv$transformedData <- df
  })
  output$cleaned_data <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
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
    df <- rv$transformedData
    if (input$scaleMethod == "Min-Max") {
      range_spec <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      df <- df %>% mutate(across(all_of(input$scaleCols), range_spec))
    } else {
      zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
      df <- df %>% mutate(across(all_of(input$scaleCols), zscore))
    }
    rv$transformedData <- df
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
    df <- rv$transformedData
    colName <- input$encodeCol
    if (input$encodeMethod == "One-Hot") {
      temp <- data.frame(var = as.factor(df[[colName]]))
      dummies <- model.matrix(~ var - 1, data = temp)
      colnames(dummies) <- gsub("^var", paste0(colName,"_"), colnames(dummies))
      df[[colName]] <- NULL
      df <- cbind(df, dummies)
    } else {
      df[[colName]] <- as.integer(as.factor(df[[colName]]))
    }
    rv$transformedData <- df
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
    df <- rv$transformedData
    colName <- input$polyCol
    newColName <- paste0(colName, "^", input$polyDegree)
    df[[newColName]] <- df[[colName]] ^ input$polyDegree
    rv$transformedData <- df
  })
  output$polyTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$interactionUI <- renderUI({
    req(rv$transformedData)
    numCols <- names(rv$transformedData)[sapply(rv$transformedData, is.numeric)]
    tagList(
      selectInput("intCol1", "Column 1:", numCols),
      selectInput("intCol2", "Column 2:", numCols)
    )
  })
  observeEvent(input$applyInteraction, {
    df <- rv$transformedData
    newCol <- paste0(input$intCol1, "_x_", input$intCol2)
    df[[newCol]] <- df[[input$intCol1]] * df[[input$intCol2]]
    rv$transformedData <- df
  })
  output$interactionTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$datetimeUI <- renderUI({
    req(rv$transformedData)
    possible_dates <- names(rv$transformedData)[sapply(rv$transformedData, function(x) {
      inherits(x, "Date") || is.character(x)
    })]
    selectInput("dateColumn", "Select a date column:", choices = possible_dates)
  })
  observeEvent(input$applyDatetime, {
    req(input$dateColumn)
    df <- rv$transformedData
    col <- input$dateColumn
    if (!inherits(df[[col]], "Date")) {
      tmp_date <- suppressWarnings(lubridate::ymd(df[[col]]))
      if (sum(!is.na(tmp_date)) > 0) {
        df[[col]] <- tmp_date
      }
    }
    if (inherits(df[[col]], "Date")) {
      df[[paste0(col, "_year")]] <- lubridate::year(df[[col]])
      df[[paste0(col, "_month")]] <- lubridate::month(df[[col]])
      df[[paste0(col, "_day")]] <- lubridate::day(df[[col]])
    }
    rv$transformedData <- df
  })
  observe({
    req(rv$transformedData)
    updateSelectInput(session, "eda_x_var", choices = names(rv$transformedData))
    updateSelectInput(session, "eda_y_var", choices = c("None", names(rv$transformedData)))
    updateSelectInput(session, "cat_var", choices = names(rv$transformedData)[sapply(rv$transformedData, is.character)])
  })
  
  output$data_preview <- renderTable({
    req(rv$transformedData)
    head(rv$transformedData, 5)
  })
  
  output$data_check <- renderPrint({
    req(rv$transformedData)
    str(rv$transformedData)
  })
  
  output$cat_summary <- renderTable({
    req(input$cat_var)
    df <- rv$transformedData
    summary_table <- as.data.frame(table(df[[input$cat_var]]))
    colnames(summary_table) <- c("Category", "Frequency")
    summary_table <- summary_table[order(-summary_table$Frequency), ]
    return(summary_table)
  })
  
  output$num_summary <- renderTable({
    df <- rv$transformedData
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) == 0) return(data.frame(Variable = NA, Mean = NA, Median = NA, Variance = NA, SD = NA))
    
    summary_df <- data.frame(
      Variable = num_vars,
      Mean = sapply(df[num_vars], mean, na.rm = TRUE),
      Median = sapply(df[num_vars], median, na.rm = TRUE),
      Variance = sapply(df[num_vars], var, na.rm = TRUE),
      SD = sapply(df[num_vars], sd, na.rm = TRUE)
    )
    return(summary_df)
  })
  
  #EDA
  output$plot_output <- renderPlot({
    req(input$eda_x_var, input$eda_plot_type, rv$transformedData)
    df <- rv$transformedData
    
    plot_title <- input$eda_plot_title
    x_label <- input$eda_x_label
    y_label <- input$eda_y_label
    x_var <- input$eda_x_var
    y_var <- input$eda_y_var
    plot_type <- input$eda_plot_type
    
    if (plot_type == "Time Series Plot") {
      req(input$eda_y_var != "None")
      if (!inherits(df[[x_var]], "Date") & !inherits(df[[x_var]], "POSIXt")) {
        df[[x_var]] <- as.Date(df[[x_var]], tryFormats = c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%Y-%m"))
      }
      ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_line(color = "blue", size = 1) +
        geom_point(color = "red", size = 2) +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label) + ylab(y_label)
      
    } else if (plot_type == "Scatter Plot") {
      req(input$eda_y_var != "None")
      ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_point(color = "blue", size = 2, alpha = 0.6) +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label) + ylab(y_label)
      
    } else if (plot_type == "Histogram") {
      ggplot(df, aes_string(x = x_var)) +
        geom_histogram(fill = "blue", color = "black", bins = 30, alpha = 0.7) +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label)
      
    } else if (plot_type == "Box Plot") {
      req(input$eda_y_var != "None")
      ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label) + ylab(y_label)
      
    } else if (plot_type == "Density Plot") {
      ggplot(df, aes_string(x = x_var)) +
        geom_density(fill = "blue", alpha = 0.5) +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label)
      
    } else if (plot_type == "Pie Chart") {
      req(input$eda_x_var)
      pie_data <- df %>% count(.data[[x_var]])
      ggplot(pie_data, aes(x = "", y = n, fill = .data[[x_var]])) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta = "y") +
        theme_void() +
        ggtitle(plot_title)
      
    } else if (plot_type == "Line Chart") {
      req(input$eda_y_var != "None")
      ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_line(color = "blue", size = 1) +
        theme_minimal() +
        ggtitle(plot_title) + xlab(x_label) + ylab(y_label)
      
    } else if (plot_type == "Correlation Heatmap") {
      num_vars <- df %>% select(where(is.numeric))
      if (ncol(num_vars) < 2) return(NULL)
      corr_matrix <- cor(num_vars, use = "complete.obs")
      corrplot::corrplot(corr_matrix, method = "color", addCoef.col = "black", tl.col = "black")
    }
  })

  output$datetimeTable <- renderDT({
    req(rv$transformedData)
    datatable(rv$transformedData, options = list(pageLength = 5, scrollX = TRUE))
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("transformed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(rv$transformedData, file, row.names = FALSE)
    }
  )
}


shinyApp(ui, server)
