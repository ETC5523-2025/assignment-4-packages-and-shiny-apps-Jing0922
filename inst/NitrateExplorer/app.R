#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(shinythemes)

ui <- fluidPage(

  theme = shinytheme("flatly"),

  tags$head(
    tags$style(HTML("

      /* Main title style */
      .navbar-brand {
        font-weight: bold;
        font-size: 24px;
      }

      /* Sidebar Style */
      .well {
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }

      /* Panel title style */
      .panel-heading {
        background-color: #2c3e50 !important;
        color: white !important;
        font-weight: bold;
        border-radius: 6px 6px 0 0;
      }

      /* 按钮和输入控件样式 */
      .form-control, .selectize-input {
        border-radius: 4px;
        border: 1px solid #bdc3c7;
      }

      /* 表格样式 */
      .table {
        border-radius: 6px;
        overflow: hidden;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }

      /* 统计摘要样式 */
      .shiny-text-output {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 6px;
        border-left: 4px solid #3498db;
        font-family: 'Courier New', monospace;
      }

      /* 主图区域样式 */
      .shiny-plot-output {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border: 1px solid #dee2e6;
      }
    "))
  ),


  titlePanel(
    div(
      style = "background: linear-gradient(to top, #0c3483 0%, #a2b6df 100%, #6b8cce 100%, #a2b6df 100%);

               color: white;
               padding: 20px;
               border-radius: 8px;
               margin-bottom: 20px;
               text-align: center;",
      h1("📊 Nitrate Data Explorer", style = "margin: 0; font-weight: bold;"),
      p("Interactive visualization and analysis of nitrate concentration data",
        style = "margin: 5px 0 0 0; opacity: 0.9;")
    )
  ),


  sidebarLayout(
    sidebarPanel(

      width = 3,
      style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px;",

      div(
        style = "margin-bottom: 20px;",
        tags$label("Select Site(s):",
                   style = "font-weight: bold; color: #2c3e50;",
                   icon("location-dot", class = "fa-regular")),
      selectInput("siteID", "Select Site(s):",
                  choices = NULL,
                  multiple = TRUE)

      ),


      # Date range selection
      div(
        style = "margin-bottom: 20px;",
        tags$label("Select Date Range:",
                   style = "font-weight: bold; color: #2c3e50;",
                   icon("calendar", class = "fa-regular")),
        dateRangeInput(
          "dateRange",
          "Select Date Range:",
          start = NULL,
          end = NULL,
          separator = " to ")

      ),


      # Statistical granularity selection
      radioButtons(
        "timeAggregation",
        "Time Aggregation:",
        choices = c("Daily" = "daily",
                  "Monthly" = "monthly",
                  "Yearly" = "yearly"),
        selected = "daily"
      ),

      # Plot type
      radioButtons(
        "plotType",
        "Plot Type:",
        choices = c("Time Series" = "timeseries",
                  "Boxplot Comparison" = "boxplot"),
        selected = "timeseries"
      )


  ),


    mainPanel(
      plotOutput("mainPlot", height = "500px"),
      tableOutput("statsTable"),
      verbatimTextOutput("dataSummary")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    NitrateExplorer::nitrate_clean
    })

  # Update selector options
  observe({
    nitrate_data <- data()
    site_choices <- sort(unique(nitrate_data$siteID))
    choices_list <- as.list(site_choices)
    names(choices_list) <- site_choices
    updateSelectInput(
      session,
      "siteID",
      choices = choices_list,
      selected = site_choices[1]
    )
  })

  observe({
    nitrate_data <- data()
    updateDateRangeInput(
      session,
      "dateRange",
      start = min(nitrate_data$startDate, na.rm = TRUE),
      end = max(nitrate_data$startDate, na.rm = TRUE)
    )
  })


  # Filter data
  filtered_data <- reactive({
    req(input$siteID, input$dateRange)

    nitrate_data <- data()


    nitrate_data %>%
      filter(
        siteID %in% input$siteID,
        startDate >= input$dateRange[1],
        startDate <= input$dateRange[2])

  })

  # Calculate statistics
  computed_stats <- reactive({
    req(filtered_data())

    agg_data <- filtered_data()

    switch(input$timeAggregation,
           "daily" = NitrateExplorer::calculate_daily_stats(agg_data),
           "monthly" = NitrateExplorer::calculate_monthly_stats(agg_data),
           "yearly" = NitrateExplorer::calculate_yearly_stats(agg_data))
  })

  # main plot
  output$mainPlot <- renderPlot({
    req(computed_stats(), input$plotType)

    stats_data <- computed_stats()
    sites <- input$siteID
    aggregation <- input$timeAggregation

    if (input$plotType == "timeseries") {
      NitrateExplorer::plot_time_series(stats_data, sites = sites, aggregation = aggregation)
    } else {
      NitrateExplorer::boxplot_comparison(stats_data, sites)
    }
  })


  # Generate statistical tables
  output$statsTable <- renderTable({
    req(computed_stats())
    head(computed_stats(),10)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # Data summary
  output$dataSummary <- renderPrint({
    req(filtered_data())

    data <- filtered_data()
    cat("Dataset Summary:\n")
    cat("===============\n")
    cat("Selected sites:", paste(input$siteID, collapse = ", "), "\n")
    cat("Date range:", as.character(input$dateRange[1]), "to",
        as.character(input$dateRange[2]), "\n")
    cat("Number of observations:", nrow(data), "\n")
    cat("Number of sites:", length(unique(data$siteID)), "\n")
    cat("\nSummary statistics for nitrate concentrations:\n")

    # 使用正确的列名
    if ("surfWaterNitrateMean" %in% names(data)) {
      summary(data$surfWaterNitrateMean)
    } else {
      cat("Nitrate data not available in filtered dataset\n")
    }
  })
}

shinyApp(ui = ui, server = server)
