#' Create Boxplot Comparing Nitrate Concentrations Across Sites
#'
#' Generates a boxplot to compare the distribution of nitrate concentrations
#' across different NEON sites. Useful for identifying sites with consistently high or low levels.
#'
#' @param nitrate_data A dataframe containing nitrate concentration data
#' @param sites Character vector of site IDs to include. If NULL, includes all sites.
#' @param log_transform Logical indicating whether to log-transform nitrate concentrations.
#'   Default is FALSE. Useful when data are highly skewed.
#' @param title Plot title. If NULL, a default title is used.
#'
#' @returns A ggplot object showing boxplots of nitrate concentrations by site
#' @export
#'
#' @examples
#'
#' boxplot_comparison(nitrate_clean)
#'
#' # With specific sites and log transformation
#' boxplot_comparison(nitrate_clean,
#'                      sites = c("LEWI", "OTHER"),
#'                      log_transform = TRUE)
boxplot_comparison <- function(nitrate_data, sites = NULL, log_transform = FALSE, title = NULL) {

  # 动态检测数据格式
  if ("surfWaterNitrateMean" %in% names(nitrate_data)) {
    # 原始数据格式
    required_cols <- c("siteID", "surfWaterNitrateMean")
    missing_cols <- setdiff(required_cols, names(nitrate_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    y_var <- "surfWaterNitrateMean"
    data_label <- "Nitrate Concentration"

  } else if ("daily_mean" %in% names(nitrate_data)) {
    # 日聚合数据
    y_var <- "daily_mean"
    data_label <- "Daily Mean Nitrate Concentration"

  } else if ("monthly_mean" %in% names(nitrate_data)) {
    # 月聚合数据
    y_var <- "monthly_mean"
    data_label <- "Monthly Mean Nitrate Concentration"

  } else if ("yearly_mean" %in% names(nitrate_data)) {
    # 年聚合数据
    y_var <- "yearly_mean"
    data_label <- "Yearly Mean Nitrate Concentration"

  } else {
    # 尝试找到包含"mean"的列
    mean_cols <- grep("mean", names(nitrate_data), value = TRUE, ignore.case = TRUE)
    if (length(mean_cols) > 0) {
      y_var <- mean_cols[1]
      data_label <- "Nitrate Concentration"
    } else {
      stop("Cannot identify nitrate concentration column. Available columns: ",
           paste(names(nitrate_data), collapse = ", "))
    }
  }

  # Filter by sites if specified
  if (!is.null(sites)) {
    nitrate_data <- nitrate_data |>
      dplyr::filter(siteID %in% sites)
  }

  # Prepare data
  nitrate_boxplot_data <- nitrate_data |>
    dplyr::filter(!is.na(.data[[y_var]]))

  # Apply log transformation if requested
  if (log_transform) {
    nitrate_boxplot_data <- nitrate_boxplot_data |>
      dplyr::mutate(!!y_var := log(.data[[y_var]] + 1))
    y_label <- paste("Log", data_label, "(mg/L)")
  } else {
    y_label <- paste(data_label, "(mg/L)")
  }

  # Create default title if not provided
  if (is.null(title)) {
    title <- paste("Distribution of", data_label, "by Site")
    if (log_transform) {
      title <- paste(title, "(Log Transformed)")
    }
  }

  # Create boxplot
  nitrate_boxplot <- ggplot2::ggplot(nitrate_boxplot_data,
                                     ggplot2::aes(
                                       x = siteID,
                                       y = .data[[y_var]],
                                       fill = siteID)) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
    ggplot2::labs(
      title = title,
      x = "Site",
      y = y_label,
      fill = "Site"
    ) +
    ggplot2::facet_wrap(~siteID, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none"
    )

  return(nitrate_boxplot)
}
