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
#' # Basic boxplot
#' boxplot_comparison(nitrate_clean)
#'
#' # With specific sites and log transformation
#' boxplot_comparison(nitrate_clean,
#'                      sites = c("LEWI"),
#'                      log_transform = TRUE)

boxplot_comparison <- function(nitrate_data,
                               sites = NULL,
                               log_transform = FALSE,
                               title = NULL) {


  if ("surfWaterNitrateMean" %in% names(nitrate_data)) {

    required_cols <- c("siteID", "surfWaterNitrateMean")
    missing_cols <- setdiff(required_cols, names(nitrate_data))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }

    y_var <- "surfWaterNitrateMean"
    data_label <- "Nitrate Concentration"

  } else {
    stop("Required column 'surfWaterNitrateMean' not found. Available columns: ",
         paste(names(nitrate_data), collapse = ", "))
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

    ggplot2::geom_boxplot(
      alpha = 0.8,
      outlier.alpha = 0.7,
      outlier.size = 1.5,
      outlier.shape = 21,
      outlier.fill = "white",
      outlier.color = "red",
      width = 0.5,
      size = 0.6
    ) +

    ggplot2::labs(
      title = title,
      x = "Monitoring Site",
      y = y_label
    ) +
    ggplot2::facet_wrap(~siteID, scales = "free") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        size = 16,
        margin = ggplot2::margin(b = 8),
        hjust = 0.5
      ),

      axis.title = ggplot2::element_text(face = "bold", size = 12),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
      axis.text = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(
        hjust = 1,
        vjust = 1,
        face = "bold"
      ),
      legend.position = "none",
      panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_line(color = "gray95", linewidth = 0.2),
      panel.grid.minor.y = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(20, 20, 20, 20)
    ) +
    ggplot2::scale_fill_viridis_d(
      option = "plasma",
      end = 0.85,
      name = "Monitoring Site"
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = 0.05),
      labels = scales::label_number(accuracy = 0.1)
    )

  return(nitrate_boxplot)
}
