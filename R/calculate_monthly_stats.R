#' Calculate Monthly Summary Statistics for Nitrate Data
#'
#' calculates monthly summary statistics for nitrate concentration data.
#'
#'
#' @returns A tibble with monthly summary statistics
#' @export
#'
#' @examples
#' monthly_stats <- calculate_monthly_stats(nitrate_clean)
calculate_monthly_stats <- function(nitrate_data, sites = NULL) {
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

  # Calculate monthly statistics
  monthly_summary <- nitrate_data |>
    dplyr::mutate(
      year_month = format(startDate, "%Y-%m")
    ) |>
    dplyr::group_by(siteID, year_month) |>
    dplyr::summarise(
      monthly_mean = mean(surfWaterNitrateMean, na.rm = TRUE),
      monthly_sd = sd(surfWaterNitrateMean, na.rm = TRUE),
      monthly_max = max(surfWaterNitrateMean, na.rm = TRUE),
      monthly_min = min(surfWaterNitrateMean, na.rm = TRUE),
      n_days = dplyr::n_distinct(startDate),
      .groups = "drop"
    )

  return(monthly_summary)
}
