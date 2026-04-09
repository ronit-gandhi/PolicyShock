# PolicyShock: 00_install.R
# Run this once to install all required packages.

pkgs <- c(
  # Data retrieval
  "tidyquant",    # sector ETF prices from Yahoo Finance
  "fredr",        # FRED API for Federal Funds Rate
  # Data wrangling
  "tidyverse",    # dplyr, ggplot2, tidyr, purrr, etc.
  "lubridate",    # date handling
  "zoo",          # rolling windows, irregular time series
  "xts",          # extensible time series
  # Econometrics
  "sandwich",     # heteroskedasticity/autocorrelation-robust SEs
  "lmtest",       # coeftest with robust SEs
  "strucchange",  # structural break tests (Chow, CUSUM)
  "broom",        # tidy model output
  "estimatr",     # lm_robust for easy HC/cluster SEs
  # Visualization
  "patchwork",    # combine ggplot panels
  "scales",       # axis formatting
  "ggrepel",      # non-overlapping labels
  "kableExtra",   # publication tables
  # Reporting
  "quarto",       # Quarto report rendering
  "knitr"
)

new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) {
  install.packages(new_pkgs, repos = "https://cran.rstudio.com/")
} else {
  message("All packages already installed.")
}

message("Done. You can now source('run_all.R')")
