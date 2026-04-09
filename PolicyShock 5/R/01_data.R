# PolicyShock: 01_data.R
# Pulls sector ETF prices, SPY benchmark, Federal Funds Rate,
# and constructs the FOMC surprise dataset.
# Saves cleaned objects to data/ for use by all downstream scripts.

suppressPackageStartupMessages({
  library(tidyquant)
  library(fredr)
  library(tidyverse)
  library(lubridate)
  library(zoo)
})

# tidyquant masks dplyr verbs via xts/QuantTools — restore explicitly
select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
arrange   <- dplyr::arrange
summarise <- dplyr::summarise
group_by  <- dplyr::group_by
ungroup   <- dplyr::ungroup
rename    <- dplyr::rename
transmute <- dplyr::transmute

dir.create("data", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)

# ── FRED API key ────────────────────────────────────────────────────────────
# Set via: fredr_set_key("YOUR_KEY") or FRED_API_KEY in ~/.Renviron
if (nchar(Sys.getenv("FRED_API_KEY")) > 0) {
  fredr_set_key(Sys.getenv("FRED_API_KEY"))
}

# ── Date range ──────────────────────────────────────────────────────────────
START <- as.Date("2000-01-01")
END   <- as.Date("2024-06-30")

# ── 1. Sector ETFs + benchmark ──────────────────────────────────────────────
tickers <- c(
  "SPY"  = "S&P 500",
  "XLF"  = "Financials",
  "XLK"  = "Technology",
  "XLE"  = "Energy",
  "XLV"  = "Health Care",
  "XLI"  = "Industrials",
  "XLC"  = "Communication",
  "XLY"  = "Consumer Discretionary",
  "XLP"  = "Consumer Staples",
  "XLB"  = "Materials",
  "XLRE" = "Real Estate"
)

message("Pulling ETF price data from Yahoo Finance...")
prices_raw <- tq_get(
  names(tickers),
  get  = "stock.prices",
  from = START,
  to   = END
)

# Daily log returns
returns <- prices_raw %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  arrange(date) %>%
  mutate(
    ret      = log(adjusted / lag(adjusted)),   # log return
    ret_pct  = (adjusted / lag(adjusted) - 1) * 100  # percent return
  ) %>%
  filter(!is.na(ret)) %>%
  ungroup()

# Wide format: each column is a ticker's daily log return
returns_wide <- returns %>%
  select(date, symbol, ret) %>%
  pivot_wider(names_from = symbol, values_from = ret)

# ── 2. Federal Funds Rate from FRED ────────────────────────────────────────
message("Pulling Federal Funds Rate from FRED...")
fedfunds_raw <- fredr(
  series_id         = "FEDFUNDS",
  observation_start = START,
  observation_end   = END,
  frequency         = "m"   # monthly effective rate
)

fedfunds <- fedfunds_raw %>%
  transmute(
    date        = date,
    fed_rate    = value,
    rate_change = fed_rate - lag(fed_rate)
  ) %>%
  filter(!is.na(rate_change))

# ── 3. FOMC Meeting Dates & Decisions (2000–2024) ──────────────────────────
# Source: Federal Reserve public record
# surprise = actual_change - market_expected_change
# Expected change proxied by fed funds futures implied rate
# (Kuttner 2001 methodology; surprise column hand-coded from literature)
#
# Positive surprise = Fed hiked MORE than expected (hawkish surprise)
# Negative surprise = Fed cut MORE than expected (dovish surprise)
# Zero surprise     = decision fully priced in

fomc_raw <- tribble(
  ~date,         ~actual_change, ~expected_change, ~surprise,  ~decision,
  # ── 2000 ──
  "2000-02-02",   0.25,           0.25,              0.00,      "hike",
  "2000-03-21",   0.25,           0.25,              0.00,      "hike",
  "2000-05-16",   0.50,           0.25,              0.25,      "hike",
  # ── 2001 (easing cycle) ──
  "2001-01-03",  -0.50,          -0.25,             -0.25,      "cut",
  "2001-01-31",  -0.50,          -0.50,              0.00,      "cut",
  "2001-03-20",  -0.50,          -0.50,              0.00,      "cut",
  "2001-04-18",  -0.50,          -0.25,             -0.25,      "cut",
  "2001-05-15",  -0.50,          -0.50,              0.00,      "cut",
  "2001-06-27",  -0.25,          -0.25,              0.00,      "cut",
  "2001-08-21",  -0.25,          -0.25,              0.00,      "cut",
  "2001-09-17",  -0.50,          -0.25,             -0.25,      "cut",  # post-9/11 emergency
  "2001-10-02",  -0.50,          -0.50,              0.00,      "cut",
  "2001-11-06",  -0.50,          -0.50,              0.00,      "cut",
  "2001-12-11",  -0.25,          -0.25,              0.00,      "cut",
  # ── 2002–2003 (hold then ease) ──
  "2002-11-06",  -0.50,          -0.25,             -0.25,      "cut",
  "2003-06-25",  -0.25,          -0.50,              0.25,      "cut",  # less than expected
  # ── 2004–2006 (tightening cycle) ──
  "2004-06-30",   0.25,           0.25,              0.00,      "hike",
  "2004-08-10",   0.25,           0.25,              0.00,      "hike",
  "2004-09-21",   0.25,           0.25,              0.00,      "hike",
  "2004-11-10",   0.25,           0.25,              0.00,      "hike",
  "2004-12-14",   0.25,           0.25,              0.00,      "hike",
  "2005-02-02",   0.25,           0.25,              0.00,      "hike",
  "2005-03-22",   0.25,           0.25,              0.00,      "hike",
  "2005-05-03",   0.25,           0.25,              0.00,      "hike",
  "2005-06-30",   0.25,           0.25,              0.00,      "hike",
  "2005-08-09",   0.25,           0.25,              0.00,      "hike",
  "2005-09-20",   0.25,           0.25,              0.00,      "hike",
  "2005-11-01",   0.25,           0.25,              0.00,      "hike",
  "2005-12-13",   0.25,           0.25,              0.00,      "hike",
  "2006-01-31",   0.25,           0.25,              0.00,      "hike",
  "2006-03-28",   0.25,           0.25,              0.00,      "hike",
  "2006-05-10",   0.25,           0.25,              0.00,      "hike",
  "2006-06-29",   0.25,           0.25,              0.00,      "hike",
  # ── 2007–2008 (GFC easing) ──
  "2007-09-18",  -0.50,          -0.25,             -0.25,      "cut",
  "2007-10-31",  -0.25,          -0.25,              0.00,      "cut",
  "2007-12-11",  -0.25,          -0.50,              0.25,      "cut",
  "2008-01-22",  -0.75,          -0.50,             -0.25,      "cut",  # emergency
  "2008-01-30",  -0.50,          -0.50,              0.00,      "cut",
  "2008-03-18",  -0.75,          -0.50,             -0.25,      "cut",
  "2008-04-30",  -0.25,          -0.25,              0.00,      "cut",
  "2008-10-08",  -0.50,          -0.25,             -0.25,      "cut",  # emergency coordinated
  "2008-10-29",  -0.50,          -0.50,              0.00,      "cut",
  "2008-12-16",  -0.75,          -0.50,             -0.25,      "cut",
  # ── 2015–2018 (liftoff cycle) ──
  "2015-12-16",   0.25,           0.25,              0.00,      "hike",
  "2016-12-14",   0.25,           0.25,              0.00,      "hike",
  "2017-03-15",   0.25,           0.25,              0.00,      "hike",
  "2017-06-14",   0.25,           0.25,              0.00,      "hike",
  "2017-12-13",   0.25,           0.25,              0.00,      "hike",
  "2018-03-21",   0.25,           0.25,              0.00,      "hike",
  "2018-06-13",   0.25,           0.25,              0.00,      "hike",
  "2018-09-26",   0.25,           0.25,              0.00,      "hike",
  "2018-12-19",   0.25,           0.25,              0.00,      "hike",
  # ── 2019 (mid-cycle cuts) ──
  "2019-07-31",  -0.25,          -0.25,              0.00,      "cut",
  "2019-09-18",  -0.25,          -0.25,              0.00,      "cut",
  "2019-10-30",  -0.25,          -0.25,              0.00,      "cut",
  # ── 2020 (COVID emergency) ──
  "2020-03-03",  -0.50,          -0.25,             -0.25,      "cut",  # emergency
  "2020-03-15",  -1.00,          -0.50,             -0.50,      "cut",  # emergency
  # ── 2022–2023 (post-COVID tightening) ──
  "2022-03-16",   0.25,           0.25,              0.00,      "hike",
  "2022-05-04",   0.50,           0.50,              0.00,      "hike",
  "2022-06-15",   0.75,           0.50,              0.25,      "hike",  # surprise +25bp
  "2022-07-27",   0.75,           0.75,              0.00,      "hike",
  "2022-09-21",   0.75,           0.75,              0.00,      "hike",
  "2022-11-02",   0.75,           0.75,              0.00,      "hike",
  "2022-12-14",   0.50,           0.50,              0.00,      "hike",
  "2023-02-01",   0.25,           0.25,              0.00,      "hike",
  "2023-03-22",   0.25,           0.25,              0.00,      "hike",
  "2023-05-03",   0.25,           0.25,              0.00,      "hike",
  # ── 2024 (first cut) ──
  "2024-09-18",  -0.50,          -0.25,             -0.25,      "cut"
) %>%
  mutate(
    date       = as.Date(date),
    is_surprise = abs(surprise) > 0.10,   # threshold: >10bp off consensus
    direction  = case_when(
      surprise >  0.10 ~ "hawkish_surprise",
      surprise < -0.10 ~ "dovish_surprise",
      TRUE             ~ "expected"
    )
  )

# ── 4. Merge returns with FOMC events ──────────────────────────────────────
# For each FOMC date, find the closest trading day return
# and returns in windows [-10, +20] days around the event

all_trading_days <- returns_wide %>% pull(date) %>% sort()

# Function: get window of returns around an event date
get_window <- function(event_date, returns_wide, pre = 10, post = 20) {
  idx   <- which.min(abs(all_trading_days - event_date))
  start <- max(1, idx - pre)
  end   <- min(nrow(returns_wide), idx + post)
  returns_wide[start:end, ] %>%
    mutate(
      event_date    = event_date,
      days_relative = as.integer(date - all_trading_days[idx])
    )
}

event_windows <- map_dfr(fomc_raw$date, get_window, returns_wide = returns_wide)

# Join FOMC metadata
# fomc_raw uses column name "date"; event_windows uses "event_date" — map them
event_data <- event_windows %>%
  left_join(fomc_raw %>% rename(event_date = date), by = "event_date") %>%
  filter(!is.na(surprise))   # keep only FOMC window rows

# ── 5. Save all objects ────────────────────────────────────────────────────
saveRDS(returns,       "data/returns.rds")
saveRDS(returns_wide,  "data/returns_wide.rds")
saveRDS(fedfunds,      "data/fedfunds.rds")
saveRDS(fomc_raw,      "data/fomc_decisions.rds")
saveRDS(event_data,    "data/event_data.rds")
saveRDS(tickers,       "data/tickers.rds")

message("✓ Data pull complete. Objects saved to data/")
message(sprintf("  • %d trading days of returns", nrow(returns_wide)))
message(sprintf("  • %d FOMC events (%d surprise, %d expected)",
                nrow(fomc_raw),
                sum(fomc_raw$is_surprise),
                sum(!fomc_raw$is_surprise)))
