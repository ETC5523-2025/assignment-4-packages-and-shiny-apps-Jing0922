#' Calculate Daily Summary Statistics for Nitrate Data
#'
#' Calculates daily summary statistics (mean, standard deviation, maximum, minimum)
#' for nitrate concentration data from NEON surface water measurements.
#'
#'
#' @returns A tibble with daily summary statistics for each site and date.
#' @export
#'
#' @examples
#' # Calculate daily statistics
#' daily_stats <- calculate_daily_stats(nitrate_clean)
#' head(daily_stats)
#'
#' # Calculate daily statistics for specific sites
#' selected_sites <- c("ARIK", "MAYF")
#' daily_stats_selected <- calculate_daily_stats(nitrate_clean, sites = selected_sites)
#' head(daily_stats_selected)

calculate_daily_stats <- function(nitrate_data, sites = NULL){

  # Input validation
  if (!is.data.frame(nitrate_data)) {
    stop("nitrate_data must be a dataframe")
  }

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

  nitrate_data |>
    dplyr::group_by(siteID, startDate) |>
    dplyr::summarise(
      daily_mean = mean(surfWaterNitrateMean, na.rm = TRUE),
      daily_sd = sd(surfWaterNitrateMean, na.rm = TRUE),
      daily_max = max(surfWaterNitrateMean, na.rm = TRUE),
      daily_min = min(surfWaterNitrateMean, na.rm = TRUE),
      n_observations = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::rename(date = startDate)


}




