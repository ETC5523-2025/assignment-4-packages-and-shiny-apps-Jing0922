#' Calculate Yearly Summary Statistics for Nitrate Data
#'
#' calculates yearly summary statistics for nitrate concentration data.
#'
#' @param nitrate_data A dataframe containing nitrate concentration data
#' @param sites Character vector of site IDs to include. If NULL, includes all sites.
#'
#' @returns A tibble with yearly summary statistics
#' @export
#'
#' @examples
#' yearly_stats <- calculate_yearly_stats(nitrate_clean)
#' head(yearly_stats)
#'
#' # Calculate daily statistics for specific sites:
#' selected_sites <- c("MAYF")
#' yearly_stats_selected <- calculate_daily_stats(nitrate_clean, sites = selected_sites)
#' head(yearly_stats_selected)
calculate_yearly_stats <- function(nitrate_data, sites = NULL) {
  # Input validation
  required_cols <- c("siteID", "startDate", "surfWaterNitrateMean")
  missing_cols <- setdiff(required_cols, names(nitrate_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Filter by sites if specified
  if (!is.null(sites)) {
    nitrate_data <- nitrate_data |>
      dplyr::filter(siteID %in% sites)
  }

  # Calculate yearly statistics
  yearly_summary <- nitrate_data |>
    dplyr::mutate(
      year = format(startDate, "%Y")
    ) |>
    dplyr::group_by(siteID, year) |>
    dplyr::summarise(
      yearly_mean = mean(surfWaterNitrateMean, na.rm = TRUE),
      yearly_sd = sd(surfWaterNitrateMean, na.rm = TRUE),
      yearly_max = max(surfWaterNitrateMean, na.rm = TRUE),
      yearly_min = min(surfWaterNitrateMean, na.rm = TRUE),
      n_year = dplyr::n_distinct(startDate),
      .groups = "drop"
    )

  return(yearly_summary)
}
