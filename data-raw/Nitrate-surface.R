## code to prepare `Nitrate-surface` dataset goes here

library(neonUtilities)
library(neonOS)
library(tidyverse)
library(usethis)


sites <- c("ARIK","COMO","KING","LEWI","MAYF")

nitrate_list <- neonUtilities::loadByProduct(
  dpID = "DP1.20033.001",
  site = sites,
  startdate="2018-01",
  enddate="2023-12",
  release = "RELEASE-2025",
  check.size = FALSE
)

# nitrate_list is a named list
names(nitrate_list)

list2env(nitrate_list, .GlobalEnv)


nitrate_raw <- nitrate_list$NSW_15_minute

nitrate_clean <- nitrate_raw |>

  select(
    siteID,
    horizontalPosition,
    verticalPosition,
    startDateTime,
    endDateTime,
    surfWaterNitrateMean,
    surfWaterNitrateMinimum,
    surfWaterNitrateMaximum,
    surfWaterNitrateStdErMean,
    finalQF
  ) |>

  # Filter valid data
  filter(
    !is.na(surfWaterNitrateMean),
    finalQF == 0      # Quality flag indicating whether a data product has passed or failed an overall assessment of its quality, detailed in NEON.DOC.001113 (1=fail, 0=pass)
  ) |>

  # Convert time column format
  mutate(
    startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S"),
    endDateTime = as.POSIXct(endDateTime, format = "%Y-%m-%d %H:%M:%S"),
    startDate = as.Date(startDateTime),
    startTime = format(startDateTime, "%H:%M:%S"),
    endDate = as.Date(endDateTime),
    endTime = format(endDateTime, "%H:%M:%S")
  )





usethis::use_data(nitrate_clean, overwrite = TRUE)
