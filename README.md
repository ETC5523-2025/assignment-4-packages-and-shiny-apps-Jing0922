
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NitrateExplorer

<!-- badges: start -->

<!-- badges: end -->

## Overview

The `NEONNitrateExplorer` package provides tools to explore and analyze
nitrate concentration data in surface water from the National Ecological
Observatory Network (NEON). It includes a user-friendly Shiny
application for interactive data visualization and exploration.

## Installation

You can install the development version of NitrateExplorer from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-Jing0922")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(NitrateExplorer)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Data Source

The package uses data from the **[NEON Surface Water Nitrate
(DP1.20033.001)](https://data.neonscience.org/data-products/DP1.20033.001/RELEASE-2025)**
product. The data includes:

- **Sites**: ARIK (Arikaree River), COMO (Como Creek), KING (Kings
  Creek), LEWI (Lewis Run), MAYF (Mayberry Creek)
- **Time Period**: January 2018 - December 2023
- **Variables**:
  - `surfWaterNitrateMean`: Mean nitrate concentration (mg/L)
  - `surfWaterNitrateMinimum`: Minimum nitrate concentration
  - `surfWaterNitrateMaximum`: Maximum nitrate concentration  
  - `surfWaterNitrateStdErMean`: Standard error of the mean
  - `finalQF`: Quality flag (0 = pass, 1 = fail)

## Data Cleaning

The raw data has been processed to: - Remove records with missing
nitrate concentration values - Filter for quality-approved data only
(`finalQF == 0`) - Standardize datetime formats - Select relevant
variables for analysis
