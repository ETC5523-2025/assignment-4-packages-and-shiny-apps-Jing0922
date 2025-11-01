#' Create Time Series Plot of Nitrate Concentrations
#'
#' Generates a time series plot showing nitrate concentration trends over time.
#'
#' @param nitrate_data A dataframe containing nitrate concentration data
#' @param sites Character vector of site IDs to include. If NULL, includes all sites.
#' @param aggregation Level of temporal aggregation: "daily", "monthly" or "yearly"
#' @param start_date Start date for the plot (Date object or string in "YYYY-MM-DD" format)
#' @param end_date End date for the plot (Date object or string in "YYYY-MM-DD" format)
#'
#'
#' @returns A ggplot object showing time series of nitrate concentrations
#' @export
#'
#' @examples
#' # Daily aggregated time series for specific sites
#' selected_sites <- c("ARIK", "COMO")
#' plot_time_series(nitrate_clean, sites = selected_sites)
#'
#' # Daily aggregated time series with date filtering
#' plot_time_series(nitrate_clean,
#'                  aggregation =  "daily",
#'                  start_date = "2023-01-01",
#'                  end_date = "2023-12-31")
plot_time_series <- function(nitrate_data, sites = NULL, aggregation = "daily",
                             start_date = NULL, end_date = NULL) {


  if ("surfWaterNitrateMean" %in% names(nitrate_data)) {

    required_cols <- c("siteID", "startDate", "surfWaterNitrateMean")
    missing_cols <- setdiff(required_cols, names(nitrate_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }


    plot_data <- nitrate_data
    time_var <- "startDate"
    y_var <- "surfWaterNitrateMean"
    title <- "Nitrate Concentrations"

  } else {
    # Aggregate data format - dynamically detected based on column name
    if (aggregation == "daily" && all(c("date", "daily_mean") %in% names(nitrate_data))) {
      plot_data <- nitrate_data
      time_var <- "date"
      y_var <- "daily_mean"
      sd_var <- "daily_sd"
      title <- "Daily Mean Nitrate Concentrations"

    } else if (aggregation == "monthly" && all(c("year_month", "monthly_mean") %in% names(nitrate_data))) {
      plot_data <- nitrate_data |>
        dplyr::mutate(date = as.Date(paste0(year_month, "-01")))
      time_var <- "date"
      y_var <- "monthly_mean"
      sd_var <- "monthly_sd"
      title <- "Monthly Mean Nitrate Concentrations"

    } else if (aggregation == "yearly" && all(c("year", "yearly_mean") %in% names(nitrate_data))) {
      plot_data <- nitrate_data |>
        dplyr::mutate(date = as.Date(paste0(year, "-01-01")))
      time_var <- "date"
      y_var <- "yearly_mean"
      sd_var <- "yearly_sd"
      title <- "Yearly Mean Nitrate Concentrations"

    } else {
      stop("Data format not recognized for aggregation level: ", aggregation)
    }
  }

  if (!aggregation %in% c("daily", "monthly", "yearly")) {
    stop("aggregation must be one of: 'daily', 'monthly', 'yearly'")
  }

  # Filter by sites if specified
  if (!is.null(sites)) {
    plot_data <- plot_data |>
      dplyr::filter(siteID %in% sites)
  }

  # Filter by date range if specified
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    plot_data <- plot_data |>
      dplyr::filter(.data[[time_var]] >= start_date)
  }

  if (!is.null(end_date)) {
    end_date <- as.Date(end_date)
    plot_data <- plot_data |>
      dplyr::filter(.data[[time_var]] <= end_date)
  }

  # Create time series plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data[[time_var]], y = .data[[y_var]], color = siteID)) +
    ggplot2::geom_line(alpha = 0.8) +
    ggplot2::geom_point(size = 0.5, alpha = 0.5) +
    ggplot2::labs(
      title = title,
      subtitle = paste("Aggregation:", tools::toTitleCase(aggregation)),
      x = "Date",
      y = "Nitrate Concentration (mg/L)",
      color = "Monitoring Site"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16,
        margin = ggplot2::margin(b = 10)
      ),
      plot.subtitle = ggplot2::element_text(
        color = "gray40",
        size = 12,
        margin = ggplot2::margin(b = 15)
      ),
      axis.title = ggplot2::element_text(face = "bold", size = 12),
      axis.text = ggplot2::element_text(size = 10),
      legend.title = ggplot2::element_text(face = "bold", size = 11),
      legend.text = ggplot2::element_text(size = 10),
      legend.position = "bottom",
      legend.box.background = ggplot2::element_rect(color = "gray80", fill = "white"),
      legend.box.margin = ggplot2::margin(t = 5, r = 5, b = 5, l = 5),
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "gray95", linewidth = 0.2),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(15, 15, 15, 15)
    ) +
    ggplot2::scale_color_viridis_d(
      name = "Monitoring Site",
      option = "plasma",
      end = 0.85,
      guide = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y",
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = 0.05),
      labels = scales::label_number(accuracy = 0.1)
    )



  return(p)
}
