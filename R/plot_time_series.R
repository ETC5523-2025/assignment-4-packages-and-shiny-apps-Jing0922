#' Create Time Series Plot of Nitrate Concentrations
#'
#' Generates a time series plot showing nitrate concentration trends over time.
#'
#' @param nitrate_data A dataframe containing nitrate concentration data
#' @param sites Character vector of site IDs to include. If NULL, includes all sites.
#' @param aggregation Level of temporal aggregation: "daily", "monthly" or "yearly"
#' @param start_date Start date for the plot (Date object or string in "YYYY-MM-DD" format)
#' @param end_date End date for the plot (Date object or string in "YYYY-MM-DD" format)
#' @param show_ribbon Whether to show variability ribbon (default: FALSE)
#'
#'
#' @returns A ggplot object showing time series of nitrate concentrations
#' @export
#'
#' @examples
#' # Daily aggregated time series
#' plot_time_series(nitrate_clean, aggregation = "daily")
plot_time_series <- function(nitrate_data, sites = NULL, aggregation = "daily",
                             start_date = NULL, end_date = NULL, show_ribbon = FALSE) {

  # 动态检测数据格式
  if ("surfWaterNitrateMean" %in% names(nitrate_data)) {
    # 原始数据格式
    required_cols <- c("siteID", "startDate", "surfWaterNitrateMean")
    missing_cols <- setdiff(required_cols, names(nitrate_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    # 使用原始数据
    plot_data <- nitrate_data
    time_var <- "startDate"
    y_var <- "surfWaterNitrateMean"
    title <- "Nitrate Concentrations"

  } else {
    # 聚合数据格式 - 根据列名动态检测
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
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::geom_point(size = 0.5, alpha = 0.5) +
    ggplot2::labs(
      title = title,
      x = "Date",
      y = "Nitrate Concentration (mg/L)",
      color = "Site"
    ) +
    ggplot2::theme_minimal()

  # Add ribbon for variability if requested and available
  if (show_ribbon && exists("sd_var") && sd_var %in% names(plot_data)) {
    p <- p +
      ggplot2::geom_ribbon(ggplot2::aes(
        ymin = .data[[y_var]] - .data[[sd_var]],
        ymax = .data[[y_var]] + .data[[sd_var]],
        fill = siteID
      ), alpha = 0.2, color = NA) +
      ggplot2::labs(fill = "Site")
  }

  return(p)
}
