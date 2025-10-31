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
#library(shinydashboard)
library(shinythemes)
library(plotly)

ui <- fluidPage(

  theme = shinythemes::shinytheme("flatly"),

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

      /* Button and input control styles */
      .form-control, .selectize-input {
        border-radius: 4px;
        border: 1px solid #bdc3c7;
      }

      /* Table style */
      .table {
        border-radius: 6px;
        overflow: hidden;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }

      /* Statistics summary style */
      .shiny-text-output {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 6px;
        border-left: 4px solid #3498db;
        font-family: 'Courier New', monospace;
      }

      /* Main image area style */
      .shiny-plot-output {
        background-color: white;
        padding: 15px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border: 1px solid #dee2e6;
      }

      /* Download area styles */
      .download-section {
        background: #f8f9fa;
        padding: 15px;
        border-radius: 8px;
        border: 1px solid #dee2e6;
        margin-top: 10px;
      }

      /* Summary styles */
      .summary-output {
        background-color: #f8f9fa;
        padding: 15px;
        border-radius: 6px;
        border-left: 4px solid #3498db;
        font-family: 'Courier New', monospace;
        font-size: 12px;
        margin-top: 10px;
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
      selectInput("siteID", NULL,
                  choices = NULL,
                  multiple = TRUE)

      ),


      # Date range selection with icon
      div(
        style = "margin-bottom: 20px;",
        tags$label("Select Date Range:",
                   style = "font-weight: bold; color: #2c3e50;",
                   icon("calendar", class = "fa-regular")),
        dateRangeInput(
          "dateRange",
          NULL,
          start = NULL,
          end = NULL,
          separator = " to ")

      ),



      # Time Series: Time aggregation with icon
      conditionalPanel(
        condition = "input.main_tabs == '📊 Interactive Plots' && input.plot_subtabs == 'Time Series'",
        div(
          style = "margin-bottom: 20px;",
          tags$label("Time Series Settings:",
                     style = "font-weight: bold; color: #2c3e50;",
                     icon("chart-line", class = "fa-regular")),
          radioButtons(
            "timeAggregation",
            "Time Aggregation:",
            choices = c("Daily" = "daily",
                        "Monthly" = "monthly",
                        "Yearly" = "yearly"),
            selected = "daily",
            inline = FALSE
          )
        )
      ),

      # Data Tables
      conditionalPanel(
        condition = "input.main_tabs == '📋 Data Tables'",
        div(
          style = "margin-bottom: 20px;",
          tags$label("Data Aggregation Level:",
                     style = "font-weight: bold; color: #2c3e50;",
                     icon("calendar-alt", class = "fa-regular")),
          radioButtons(
            "tableAggregation",
            "Aggregation Level:",
            choices = c("Daily" = "daily",
                        "Monthly" = "monthly",
                        "Yearly" = "yearly"),
            selected = "daily",
            inline = FALSE
           )
         )
        ),




      # Distribution plot
      conditionalPanel(
        condition = "input.main_tabs == '📊 Interactive Plots' && input.plot_subtabs == 'Distribution'",
        div(
          style = "margin-bottom: 20px;",
          tags$label("Distribution Settings:",
                     style = "font-weight: bold; color: #2c3e50;",
                     icon("sliders", class = "fa-regular")),
          sliderInput("binSize", "Number of Bins:",
                      min = 5, max = 50, value = 20, step = 5),
          checkboxInput("showDensity", "Show Density Curve", value = FALSE),
          checkboxInput("facetSites", "Facet by Site", value = FALSE)
        )
      ),


      # Download settings
      div(
        style = "margin-bottom: 20px;",
        tags$label("Download Settings:",
                   style = "font-weight: bold; color: #2c3e50;",
                   icon("download", class = "fa-regular")),

        # Plot download settings
        div(
          style = "background: #e8f4fd; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
          tags$small("Plot Settings:", style = "font-weight: bold; color: #2c3e50;"),
          numericInput("plotWidth", "Plot Width (inches):", value = 10, min = 5, max = 20, step = 0.5),
          numericInput("plotHeight", "Plot Height (inches):", value = 6, min = 4, max = 15, step = 0.5),
          selectInput("plotFormat", "Plot Format:",
                      choices = c("PNG" = "png", "PDF" = "pdf", "JPEG" = "jpeg"),
                      selected = "png")
        )
      ),


      # Info box
      div(
        style = "background: #e8f4fd;
                border-left: 4px solid #3498db;
                padding: 10px;
                border-radius: 4px;
                margin-top: 20px;",
        p(icon("info-circle"),
          "Use the controls above to filter and visualize nitrate data.",
          style = "margin: 0; font-size: 12px; color: #2c3e50;")
      )
    ),



    mainPanel(
      width = 9,


      tabsetPanel(
        id = "main_tabs",

        # Interactive Plots
        tabPanel(
          "📊 Interactive Plots",
          br(),
          fluidRow(
            column(
              8,
              div(
                style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                # Chart Type Subtab
                tabsetPanel(
                  id = "plot_subtabs",

                  tabPanel("Time Series",
                           plotOutput("mainPlot", height = "500px"),
                           div(
                             style = "margin-top: 20px;",
                             h4("Time Series Summary"),
                             verbatimTextOutput("timeSeriesSummary")
                           )),

                  tabPanel("Boxplot Comparison",
                           plotOutput("boxplot", height = "500px"),
                           div(
                             style = "margin-top: 20px;",
                             h4("Boxplot Statistics"),
                             tableOutput("boxplotStats")
                           )),

                  tabPanel("Distribution",
                           plotlyOutput("histPlot", height = "500px"),
                           div(
                             style = "margin-top: 20px;",
                             h4("Distribution Statistics"),
                             verbatimTextOutput("distSummary")
                           ))
                )
              )
            ),

            column(
              4,
              div(
                style = "background: #f8f9fa; padding: 15px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);",
                h4("📈 Plot Guidance"),

                tags$strong("Quick Interpretation Tips:"), br(),
                tags$ul(
                  tags$li(tags$strong("Time Series:"), "Look for seasonal patterns and long-term trends"),
                  tags$li(tags$strong("Boxplot Comparison:"), "Compare variability and central tendencies between sites"),
                  tags$li(tags$strong("Distribution:"), "Identify data skewness and potential outliers")
                ),
                br(),

                tags$strong("Navigation:"), br(),
                tags$ul(
                  tags$li("Use site selection to compare specific locations"),
                  tags$li("Adjust date range to focus on periods of interest"),
                  tags$li("Switch between aggregation levels for different insights")
                ),
                br(),

                downloadButton("downloadPlot", "Download Current Plot",
                               class = "btn-primary btn-sm"),
                br(), br(),

                tags$small("💡 For detailed field descriptions, visit the 'Data Dictionary' tab.")
              )
            )
          )
        ),

        # Data tables
        tabPanel(
          "📋 Data Tables",
          br(),
          div(
            style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            # Table information title
            uiOutput("tableInfo"),
            h4("Statistical Summary Table"),
            tableOutput("statsTable"),

            # Modified Data Description
            div(
              style = "margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 4px;",
              uiOutput("tableDescription")
            ),
            br(),
            downloadButton("downloadTable", "Download Table as CSV",
                           class = "btn-primary")
          )
        ),


        # Raw Data & Download
        tabPanel(
          "📂 Raw Data & Download",
          br(),
          div(
            style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            h4("Raw Data Preview & Download"),

            # Data source description
            div(
              style = "background: #e8f4fd; padding: 10px; border-radius: 6px; margin-bottom: 15px;",
              tags$strong("📋 Data Source Information:"),
              p("Source: NEON (National Ecological Observatory Network)"),
              p("Product: Nitrate in Surface Water (DP1.20033.001)"),
              p("Release: RELEASE-2025"),
              p("Processing: Filtered for quality flags (finalQF = 0)"),
              p("Please cite NEON appropriately when using these data.")
            ),

            # Preview settings
            fluidRow(
              column(6,
                     sliderInput("previewRows", "Rows to preview:",
                                 min = 5, max = 100, value = 20, step = 5)
              ),
              column(6,
                     style = "margin-top: 25px;",
                     downloadButton("downloadRawData", "Download Raw Data (CSV)",
                                    class = "btn-primary")
              )
            ),

            # Data Preview
            div(
              style = "margin-top: 15px;",
              tags$strong("Data Preview:"),
              tableOutput("rawPreview")
            ),

            # Download options
            div(
              style = "background: #f8f9fa; padding: 10px; border-radius: 4px; margin-top: 15px;",
              tags$small("Download Options:"),
              checkboxInput("download_head_only", "Download only previewed rows", value = FALSE),
              conditionalPanel(
                condition = "input.download_head_only == true",
                numericInput("download_head_n", "Number of rows to download:",
                             value = 100, min = 10, max = 1000, step = 10)
              )
            ),

            # Instructions for use
            div(
              style = "margin-top: 15px; padding: 10px; background: #fff3cd; border-radius: 4px;",
              tags$small(icon("lightbulb"),
                         "Tip: Use site and date filters to subset data before downloading. The full dataset contains",
                         nrow(nitrate_clean), "observations.")
            )
          )
        ),


        tabPanel(
          "📖 Data Dictionary",
          br(),
          div(
            style = "background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            h4("📚 Data Dictionary & Field Descriptions"),


            div(
              style = "background: #e8f4fd; padding: 15px; border-radius: 6px; margin-bottom: 20px;",
              tags$h5("📋 Dataset Information"),
              tags$p(icon("database"), tags$strong("Source:"), "NEON (National Ecological Observatory Network)"),
              tags$p(icon("filter"), tags$strong("Processing:"), "Quality-filtered (finalQF = 0)"),
              tags$p(icon("calendar"), tags$strong("Time Range:"), "2018-2023"),
              tags$p(icon("map-marker"), tags$strong("Sites:"), "ARIK, COMO, KING, LEWI, MAYF")
            ),


            tags$h5("📊 Field Descriptions"),
            tags$p("Complete description of all variables in the dataset:"),

            tableOutput("dataDictionary"),

            # Instructions for use
            div(
              style = "background: #fff3cd; padding: 15px; border-radius: 6px; margin-top: 20px;",
              tags$h5("💡 How to Use This Information"),
              tags$ul(
                tags$li(tags$strong("Time Series Analysis:"), "Use siteID and startDate to track temporal patterns"),
                tags$li(tags$strong("Quality Assessment:"), "finalQF = 0 indicates data passed all quality checks"),
                tags$li(tags$strong("Statistical Analysis:"), "Use n for weighted analyses based on sample size"),
                tags$li(tags$strong("Data Interpretation:"), "surfWaterNitrateMean represents 15-minute average concentrations")
              )
            )
          )
        ),


        # Raw Data Summary
        tabPanel(
          "📈 Raw Data Summary",
          br(),
          div(
            style = "background: white; padding: 15px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
            h4("Raw Dataset Overview & Statistics"),
            verbatimTextOutput("dataSummary")
          )
        )
      )
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


    nitrate_data |>
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



  # Add data aggregation calculations specifically for tables
  table_computed_stats <- reactive({
    req(filtered_data(), input$tableAggregation)

    agg_data <- filtered_data()

    switch(input$tableAggregation,
           "daily" = NitrateExplorer::calculate_daily_stats(agg_data),
           "monthly" = NitrateExplorer::calculate_monthly_stats(agg_data),
           "yearly" = NitrateExplorer::calculate_yearly_stats(agg_data))
  })


  table_stats <- reactive({
    req(table_computed_stats())
    stats_data <- table_computed_stats()

    # Handle different date columns according to different aggregation levels
    if (input$tableAggregation == "daily" && "date" %in% names(stats_data)) {
      stats_data <- stats_data |>
        mutate(date = as.Date(date))
    }

    return(stats_data)
  })




  plot_obj <- reactive({
    req(computed_stats())
    stats_data <- computed_stats()
    sites <- input$siteID
    aggregation <- input$timeAggregation

    NitrateExplorer::plot_time_series(stats_data, sites = sites, aggregation = aggregation)
  })

  # Add boxplot output
  output$boxplot <- renderPlot({
    req(filtered_data())
    NitrateExplorer::boxplot_comparison(filtered_data(), input$siteID)
  })



  # Distribution plotly
  output$histPlot <- renderPlotly({
    req(filtered_data())
    NitrateExplorer::plot_distribution(
      filtered_data(),
      sites = input$siteID,
      bin_size = input$binSize,
      show_density = input$showDensity,
      facet_sites = input$facetSites
    )
  })



  # main plot
  output$mainPlot <- renderPlot({
    plot_obj()
  })

  # Download plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("nitrate_plot_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".", input$plotFormat, sep = "")
    },

    content = function(file) {
      # Decide which image to download based on the currently active tab
      current_tab <- input$plot_subtabs
      plot_to_save <- switch(current_tab,
                             "Time Series" = plot_obj(),
                             "Boxplot Comparison" = NitrateExplorer::boxplot_comparison(filtered_data(), input$siteID),
                             "Distribution" = {
                               p <- ggplot2::ggplot(filtered_data(), ggplot2::aes(x = surfWaterNitrateMean)) +
                                 ggplot2::geom_histogram(bins = input$binSize, fill = "#F1605DFF", alpha = 0.7) +
                                 ggplot2::labs(title = "Distribution of Nitrate Concentrations",
                                               x = "Nitrate Concentration (mg/L)",
                                               y = "Frequency") +
                                 ggplot2::theme_minimal()
                               if (isTRUE(input$showDensity)) {
                                 p <- p + ggplot2::geom_density(ggplot2::aes(y = after_stat(count)), size = 1)
                               }
                               p
                             })

      # Set plot dimensions (units only apply to raster formats; pdf ignores units)
      width <- input$plotWidth
      height <- input$plotHeight

      # Save depending on format
      if (input$plotFormat == "png") {
        png(file, width = width, height = height, units = "in", res = 300)
        print(plot_to_save)
        dev.off()
      } else if (input$plotFormat == "pdf") {
        pdf(file, width = width, height = height)
        print(plot_to_save)
        dev.off()
      } else if (input$plotFormat == "jpeg") {
        jpeg(file, width = width, height = height, units = "in", res = 300)
        print(plot_to_save)
        dev.off()
      }
    }
  )

  # Download table
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("nitrate_data_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv", sep = "")
    },
    content = function(file) {
      data_to_export <- table_stats()
      write.csv(data_to_export, file, row.names = FALSE)
    }
  )

  # Generate statistical tables
  output$statsTable <- renderTable({
    req(table_stats())
    head(table_stats(), 15)
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c', width = '100%')

  # Add table title and summary information
  output$tableInfo <- renderUI({
    req(table_stats())

    data <- table_stats()
    aggregation <- input$tableAggregation

    div(
      style = "background: #e8f4fd; padding: 10px; border-radius: 6px; margin-bottom: 15px;",
      h4(paste(toupper(aggregation), "Aggregated Data Table")),
      p(strong("Aggregation Level:"), toupper(aggregation)),
      p(strong("Number of Records:"), nrow(data)),
      p(strong("Date Range:"),
        paste(as.character(input$dateRange[1]), "to", as.character(input$dateRange[2]))),
      p(strong("Selected Sites:"), paste(input$siteID, collapse = ", "))
    )
  })



  output$tableDescription <- renderUI({
    req(input$tableAggregation)

    tags$small(
      icon("info-circle"),
      "This table shows the ",
      tags$strong(tolower(input$tableAggregation)),
      " aggregated nitrate data. Use the aggregation level selector in the sidebar to change the time period."
    )
  })




  # Time series summary
  output$timeSeriesSummary <- renderPrint({
    req(computed_stats())
    agg_data <- computed_stats()

    cat("📈 AGGREGATED TIME SERIES SUMMARY\n")
    cat("=================================\n")
    cat("Aggregation level:", input$timeAggregation, "\n")
    cat("Selected sites:", paste(input$siteID, collapse = ", "), "\n")
    cat("Date range:", as.character(input$dateRange[1]), "to",
        as.character(input$dateRange[2]), "\n")
    cat("Number of time points:", nrow(agg_data), "\n\n")

    if ("surfWaterNitrateMean" %in% names(agg_data)) {
      cat("Nitrate Concentration Statistics (aggregated):\n")
      cat("---------------------------------------------\n")
      print(summary(agg_data$surfWaterNitrateMean))
      cat("\nStandard deviation:", round(sd(agg_data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
      cat("Number of NA values:", sum(is.na(agg_data$surfWaterNitrateMean)), "\n")
    }
  })

  # Boxplot summary
  output$boxplotStats <- renderTable({
    req(filtered_data())
    data <- filtered_data()

    # Five-number summary
    stats <- data |>
      group_by(siteID) |>
      summarise(
        n = n(),
        Min = min(surfWaterNitrateMean, na.rm = TRUE),
        Q1 = quantile(surfWaterNitrateMean, 0.25, na.rm = TRUE),
        Median = median(surfWaterNitrateMean, na.rm = TRUE),
        Q3 = quantile(surfWaterNitrateMean, 0.25, na.rm = TRUE),
        Max = max(surfWaterNitrateMean, na.rm = TRUE),
        Mean = mean(surfWaterNitrateMean, na.rm = TRUE),
        SD = sd(surfWaterNitrateMean, na.rm = TRUE),
        .groups = 'drop'
      )

    stats
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c', width = '100%')

  # Distribution summary
  output$distSummary <- renderPrint({
    req(filtered_data())
    data <- filtered_data()

    cat("📊 DISTRIBUTION SUMMARY\n")
    cat("======================\n")
    cat("Selected sites:", paste(input$siteID, collapse = ", "), "\n")
    cat("Number of observations:", nrow(data), "\n")
    cat("Number of bins:", input$binSize, "\n\n")

    if ("surfWaterNitrateMean" %in% names(data)) {
      cat("Nitrate Concentration Distribution:\n")
      cat("----------------------------------\n")
      print(summary(data$surfWaterNitrateMean))
      cat("\nStandard deviation:", round(sd(data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
      cat("Skewness:", round(moments::skewness(data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
      cat("Kurtosis:", round(moments::kurtosis(data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
    }
  })


  # Raw data
  output$rawPreview <- renderTable({
    req(filtered_data())
    dat <- filtered_data()
    n_show <- min(nrow(dat), input$previewRows)

    if (n_show == 0) {
      return(data.frame(Message = "No data available for selected filters"))
    }

    # Only display the first few columns of key information
    preview_data <- dat |>
      select(siteID, startDate, surfWaterNitrateMean, finalQF) |>
      head(n_show)

    preview_data
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'c')

  # Raw data download
  output$downloadRawData <- downloadHandler(
    filename = function() {
      paste("neon_nitrate_raw_",
            format(Sys.time(), "%Y%m%d_%H%M%S"),
            ".csv", sep = "")
    },
    content = function(file) {
      req(filtered_data())
      dat <- filtered_data()

      if (isTRUE(input$download_head_only)) {
        n <- min(input$download_head_n, nrow(dat))
        dat_to_write <- head(dat, n)
      } else {
        dat_to_write <- dat
      }

      # Add data source information as comments
      writeLines(paste("# NEON Data Download - Nitrate in Surface Water (DP1.20033.001)",
                       "# Release: RELEASE-2025",
                       "# Generated:", Sys.time(),
                       "# Filter Criteria:",
                       paste("# - Sites:", paste(input$siteID, collapse = ", ")),
                       paste("# - Date Range:", input$dateRange[1], "to", input$dateRange[2]),
                       "# Please cite NEON appropriately",
                       "", sep = "\n"), file)

      write.table(dat_to_write, file,
                  sep = ",", row.names = FALSE,
                  append = TRUE, quote = TRUE)
    }
  )



  # Data dictionary
  output$dataDictionary <- renderTable({
    data.frame(
      `Field Name` = c("siteID", "surfWaterNitrateMean", "startDate",
                       "startTime", "endDate", "endTime",
                       "finalQF"),
      `Description` = c(
        "Sampling location identifier",
        "Average nitrate concentration in surface water (mg/L)",
        "Start date when sample was collected",
        "Start time when sample was collected",
        "End date when sample was collected",
        "End time when sample was collected",
        "Quality flag (0 = pass, 1 = fail)"
      ),
      `Data Type` = c("Character", "Numeric", "date", "Character",
                      "date", "Character", "Integer"),
      `Example` = c("'ARIK', 'LEWI'", "207.1" , "2023-01-15", "00:15:00",
                    "2018-01-01", "00:30:00", "0")
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, align = 'l', width = '100%')



  # Data summary
  output$dataSummary <- renderPrint({
    req(filtered_data())
    raw_data <- filtered_data()

    cat("📊 RAW DATASET SUMMARY\n")
    cat("=====================\n")
    cat("Selected sites:", paste(input$siteID, collapse = ", "), "\n")
    cat("Date range:", as.character(input$dateRange[1]), "to",
        as.character(input$dateRange[2]), "\n")
    cat("Number of observations:", nrow(raw_data), "\n")
    cat("Number of unique sites:", length(unique(raw_data$siteID)), "\n")
    cat("\nNitrate Concentration Statistics (raw measurements):\n")
    cat("----------------------------------------------------\n")

    if ("surfWaterNitrateMean" %in% names(raw_data)) {
      print(summary(raw_data$surfWaterNitrateMean))
      cat("\nStandard deviation:", round(sd(raw_data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
      cat("Number of NA values:", sum(is.na(raw_data$surfWaterNitrateMean)), "\n")
      cat("Skewness:", round(moments::skewness(raw_data$surfWaterNitrateMean, na.rm = TRUE), 3), "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
