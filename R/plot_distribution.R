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
#' plot_distribution(nitrate_clean)
plot_distribution <- function(data, sites = NULL, bin_size = 20,
                              show_density = FALSE, facet_sites = FALSE) {

  if (!is.null(sites)) {
    data <- data[data$siteID %in% sites, ]
  }

  # Remove NA values
  data <- data[!is.na(data$surfWaterNitrateMean), ]

  # 如果是单个站点或不分面，使用plotly基础绘图
  if (length(unique(data$siteID)) == 1 || !facet_sites) {

    if (length(unique(data$siteID)) == 1) {
      # 单个站点
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
      # 多个站点但不分面
      p <- ggplot2::ggplot(data, ggplot2::aes(x = surfWaterNitrateMean, fill = siteID)) +
        ggplot2::geom_histogram(
          bins = bin_size,
          alpha = 0.7,
          color = "white",
          position = "identity"
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


    # 添加密度曲线
    if (show_density) {
      p <- p +
        ggplot2::geom_density(
          ggplot2::aes(y = after_stat(count)),
          color = "darkred",
          linewidth = 1,
          alpha = 0.7,
          fill = NA
        )
    }

  } else {
    # 分面显示
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

    # 添加密度曲线
    if (show_density) {
      p <- p +
        ggplot2::geom_density(
          ggplot2::aes(y = after_stat(count)),
          color = "darkred",
          linewidth = 0.4,
          alpha = 0.5,
          fill = NA
        )
    }
  }

  # 转换为plotly
  plotly::ggplotly(p) %>%
    plotly::layout(
      hoverlabel = list(bgcolor = "white"),
      font = list(family = "Arial")
    )

}
