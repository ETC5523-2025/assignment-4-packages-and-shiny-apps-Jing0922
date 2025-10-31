README: NitrateExplorer
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

# NitrateExplorer

<!-- badges: start -->

<!-- badges: end -->

NitrateExplorer is an R package. It helps you explore NEON surface-water
nitrate data. The package has a Shiny app and simple functions for
summary and plotting. The data used in the app is included in the
package.

## Installation

You can install the package from GitHub. Run this in R:

``` r
# install remotes if you do not have it
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")


# install the package from GitHub (replace with your repo)
remotes::install_github("ETC5523-2025/assignment-4-packages-and-shiny-apps-Jing0922")
```

You can install the development version of NitrateExplorer from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ETC5523-2025/assignment-4-packages-and-shiny-apps-Jing0922")
```

## Run the Shiny app

After install, run:

``` r
library(NitrateExplorer)
# run_nitrate_app()
```

This will open the interactive app. The app lets you choose sites,
dates, and plot types. It also allows download of filtered data and
plots.

## Main functions

The package includes these main functions:

- run_app(): launch the Shiny app.

- calculate_daily_stats(data) — return daily summaries.

- calculate_monthly_stats(data) — return monthly summaries.

- calculate_yearly_stats(data) — return yearly summaries.

- plot_time_series(agg_data, sites, aggregation) — draw time series plot
  from aggregated data.

- boxplot_comparison(data, sites) — draw boxplots to compare sites.

- plot_distribution(data, sites, bin_size, show_density, facet_sites) —
  draw distribution plot.

All functions are documented. See the man/ folder for details and
examples.

## Data

The package includes the `nitrate_clean` dataset containing
quality-controlled nitrate measurements from NEON sites. It is saved
with `usethis::use_data()` in `data/`. When using this data, please cite
NEON appropriately according to their data use policies.

Key fields:

- siteID: ARIK, COMO, KING, LEWI, MAYF

- startDate: date of sample.

- surfWaterNitrateMean: nitrate concentration (mg/L).

- finalQF: quality flag (0 = pass).

- n: number of measurements.

The cleaning script is in data-raw/Nitrate-surface.R.

## Vignette and help

A vignette explains the main workflows. To view it, run:

``` r
vignette("nitrate_explorer_guide")
#> Warning: vignette 'nitrate_explorer_guide' not found
```

If you need more help, read the function help pages:

``` r
?calculate_monthly_stats
#> starting httpd help server ... done
?plot_time_series
```

## Citation and source

Data source: NEON (National Ecological Observatory Network). Nitrate in
surface water (DP1.20033.001), RELEASE-2025.
<https://doi.org/10.48443/wwa3-p420>. Dataset accessed from
<https://data.neonscience.org/data-products/DP1.20033.001/RELEASE-2025>
on October 27, 2025.

**Please cite NEON when you use the data.**

## Contributing

Contributions are welcome! Please feel free to submit pull requests or
open issues for bugs and feature requests.

## License

MIT License (see file LICENSE).

## Contact

For questions or support, please open an issue on GitHub or contact
jwan0560@student.monash.edu.
