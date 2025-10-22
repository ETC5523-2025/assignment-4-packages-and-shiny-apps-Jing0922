## code to prepare `Nitrate-surface` dataset goes here

library(neonUtilities)
library(neonOS)


stackByTable("data-raw/NEON_nitrate-surfacewater.zip")


usethis::use_data(Nitrate_clean, overwrite = TRUE)
