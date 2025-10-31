#' Plot distribution of nitrate concentrations
#'
#' @param data Filtered nitrate data
#' @param sites Selected site IDs
#' @param bin_size Number of bins for histogram
#' @param show_density Whether to show density curve
#' @param facet_sites Whether to facet by site
#'
#' @returns plotly object showing distribution
#' @export
#'
#' @examples
#' # Basic Usage - All Sites
#' plot_distribution(nitrate_clean, bin_size = 10)
#'
#' # Single site analysis
#' plot_distribution(nitrate_clean, sites = "ARIK", bin_size = 20)
#'
#' # Compare multiple sites
#' plot_distribution(
#'   nitrate_clean,
#'   sites = c("ARIK", "KING", "COMO"),
#'   bin_size = 25,
#'   show_density = TRUE
#' )
#'
#' # Multiple sites are displayed side-by-side.
#' plot_distribution(
#'   nitrate_clean,
#'   sites = c("ARIK", "KING", "COMO", "WLOU", "SYCA"),
#'   bin_size = 15,
#'   facet_sites = TRUE,
#'   show_density = TRUE
#' )
plot_distribution <- function(data, sites = NULL, bin_size = 20,
                              show_density = FALSE, facet_sites = FALSE) {

  if (!is.null(sites)) {
    data <- data[data$siteID %in% sites, ]
  }

  # Remove NA values
  data <- data[!is.na(data$surfWaterNitrateMean), ]

  if (length(unique(data$siteID)) == 1 || !facet_sites) {

    if (length(unique(data$siteID)) == 1) {
      # Single site
      site_name <- unique(data$siteID)
      p <- ggplot2::ggplot(data, ggplot2::aes(x = surfWaterNitrateMean)) +
        ggplot2::geom_histogram(
          bins = bin_size,
          fill = "#F1605DFF",
          alpha = 0.7,
          color = "white"
        ) +
        ggplot2::labs(
          title = paste("Distribution of Nitrate Concentrations -", site_name),
          x = "Nitrate Concentration (mg/L)",
          y = "Frequency"
        ) +
        ggplot2::theme_minimal()

    } else {
      # Multiple sites but not faceted display
      p <- ggplot2::ggplot(data, ggplot2::aes(x = surfWaterNitrateMean, fill = siteID)) +
        ggplot2::geom_histogram(
          bins = bin_size,
          alpha = 0.7,
          color = "white",
          position = "stack"
        ) +
        ggplot2::labs(
          title = "Distribution of Nitrate Concentrations",
          x = "Nitrate Concentration (mg/L)",
          y = "Frequency",
          fill = "Site"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_viridis_d(
          option = "plasma",
          end = 0.85
        ) +
        ggplot2::theme(legend.position = "bottom")
    }


    # Add density curve
    if (show_density) {
      p <- p +
        ggplot2::geom_density(
          ggplot2::aes(y = ..count..),
          color = "darkred",
          linewidth = 1,
          alpha = 0.7,
          fill = NA
        )
    }

  } else {
    # Faceted display
    p <- ggplot2::ggplot(data, ggplot2::aes(x = surfWaterNitrateMean, fill = siteID)) +
      ggplot2::geom_histogram(
        bins = bin_size,
        alpha = 0.7,
        color = "white"
      ) +
      ggplot2::facet_wrap(~siteID, scales = "free") +
      ggplot2::labs(
        title = "Distribution of Nitrate Concentrations by Site",
        x = "Nitrate Concentration (mg/L)",
        y = "Frequency"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_viridis_d(
        option = "plasma",
        end = 0.85
      ) +
      ggplot2::theme(legend.position = "none")

    # Add density curve
    if (show_density) {
      p <- p +
        ggplot2::geom_density(
          ggplot2::aes(y = ..count..),
          color = "darkred",
          linewidth = 0.4,
          alpha = 0.5,
          fill = NA
        )
    }
  }

  # plotly
  plotly::ggplotly(p) |>
    plotly::layout(
      hoverlabel = list(bgcolor = "white"),
      font = list(family = "Arial")
    )

}
