# PolicyShock: 04_its.R
# Interrupted Time Series (ITS) Analysis
#
# For each MAJOR shock event (|surprise| >= 0.25), estimate whether the
# event caused a structural break in:
#   (a) the level of sector returns
#   (b) the volatility (rolling SD) of sector returns
#
# Method: Segmented regression with Newey-West HAC standard errors
#   Y(t) = alpha + beta1*t + beta2*D(t) + beta3*(t - T0)*D(t) + epsilon
#   where D(t) = 1 if t >= shock date
#   beta2 = immediate level change
#   beta3 = change in slope (trend)
#
# Additional: Chow test (strucchange) for formal structural break detection

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(sandwich)
  library(lmtest)
  library(strucchange)
  library(broom)
  library(patchwork)
  library(scales)
})

select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
arrange   <- dplyr::arrange
summarise <- dplyr::summarise
group_by  <- dplyr::group_by
ungroup   <- dplyr::ungroup
rename    <- dplyr::rename

returns_wide <- readRDS("data/returns_wide.rds")
fomc_raw     <- readRDS("data/fomc_decisions.rds")
tickers      <- readRDS("data/tickers.rds")

sector_tickers <- setdiff(names(tickers), "SPY")

# ── Identify major shock events ────────────────────────────────────────────
major_shocks <- fomc_raw %>%
  filter(abs(surprise) >= 0.25) %>%
  arrange(date)

message(sprintf("Major shocks (|surprise| >= 25bp): %d events", nrow(major_shocks)))
print(major_shocks %>% select(date, actual_change, surprise, direction))

# ── Rolling 21-day volatility ──────────────────────────────────────────────
returns_vol <- returns_wide %>%
  arrange(date) %>%
  mutate(across(
    all_of(sector_tickers),
    ~ rollapply(.x, width = 21, FUN = sd, fill = NA, align = "right") * 100,
    .names = "{.col}_vol"
  ))

# ── ITS regression function ────────────────────────────────────────────────
# Pre-compute sorted trading days once (used by all fit_its calls)
all_its_dates <- returns_vol %>% pull(date) %>% sort()

fit_its <- function(shock_date, ticker, data, outcome = "return",
                    window_days = 120) {

  t0_idx <- which.min(abs(all_its_dates - shock_date))

  start_idx <- max(1, t0_idx - window_days)
  end_idx   <- min(nrow(data), t0_idx + window_days)

  if (outcome == "return") {
    y_col <- ticker
  } else {
    y_col <- paste0(ticker, "_vol")
  }

  df <- data[start_idx:end_idx, ] %>%
    select(date, y = all_of(y_col)) %>%
    filter(!is.na(y)) %>%
    mutate(
      t   = as.integer(date - shock_date),   # time relative to shock
      D   = as.integer(date >= shock_date),  # post-shock indicator
      tD  = t * D                             # interaction: slope change
    )

  if (nrow(df) < 30) return(NULL)

  # Need variation in D (pre and post observations) for the model to work
  if (length(unique(df$D)) < 2) return(NULL)

  # OLS with Newey-West HAC SEs (lag = 5 trading days)
  tryCatch({
    fit   <- lm(y ~ t + D + tD, data = df)
    nw_se <- NeweyWest(fit, lag = 5, prewhite = FALSE)
    ct    <- coeftest(fit, vcov = nw_se)

    # Check that the coefficients we need actually exist in the output
    coef_names <- rownames(ct)
    if (!("D" %in% coef_names) || !("tD" %in% coef_names)) return(NULL)

    tibble(
      shock_date  = shock_date,
      ticker      = ticker,
      outcome     = outcome,
      # Level shift
      level_shift = ct["D", "Estimate"],
      ls_se       = ct["D", "Std. Error"],
      ls_p        = ct["D", "Pr(>|t|)"],
      # Slope change
      slope_chg   = ct["tD", "Estimate"],
      sc_se       = ct["tD", "Std. Error"],
      sc_p        = ct["tD", "Pr(>|t|)"],
      n_obs       = nrow(df)
    )
  }, error = function(e) {
    message(sprintf("  [ITS skip] %s / %s / %s: %s",
                    shock_date, ticker, outcome, conditionMessage(e)))
    NULL
  })
}

# ── Run ITS for all major shocks × sectors × outcomes ─────────────────────
message("Running ITS regressions...")

its_results <- map_dfr(seq_along(major_shocks$date), function(i) {
  sd <- major_shocks$date[i]
  message(sprintf("  ITS shock %d/%d: %s", i, nrow(major_shocks), sd))
  map_dfr(sector_tickers, function(tk) {
    bind_rows(
      fit_its(sd, tk, returns_vol, "return"),
      fit_its(sd, tk, returns_vol, "volatility")
    )
  })
})

its_results <- its_results %>%
  left_join(fomc_raw %>% select(shock_date = date, surprise, direction),
            by = "shock_date") %>%
  mutate(
    sector = tickers[ticker],
    ls_sig = case_when(ls_p < 0.01 ~ "***",
                       ls_p < 0.05 ~ "**",
                       ls_p < 0.10 ~ "*",
                       TRUE        ~ ""),
    sc_sig = case_when(sc_p < 0.01 ~ "***",
                       sc_p < 0.05 ~ "**",
                       sc_p < 0.10 ~ "*",
                       TRUE        ~ "")
  )

# ── Chow structural break tests ────────────────────────────────────────────
# For each sector × shock, run an F-test for structural break in a local window
chow_window <- 250  # trading days on each side of shock

chow_results <- map_dfr(sector_tickers, function(tk) {
  full_data <- returns_wide %>%
    arrange(date) %>%
    filter(!is.na(.data[[tk]]))

  map_dfr(major_shocks$date, function(sd) {
    idx <- which.min(abs(full_data$date - sd))
    n   <- nrow(full_data)
    if (idx < 10 || idx > n - 10) return(NULL)

    # Scope to local window for speed
    win_start <- max(1, idx - chow_window)
    win_end   <- min(n, idx + chow_window)
    local_ts  <- full_data[[tk]][win_start:win_end]

    if (length(local_ts) < 30) return(NULL)

    tryCatch({
      fs <- Fstats(local_ts ~ 1, from = 0.15)
      tibble(
        ticker     = tk,
        shock_date = sd,
        chow_p     = sctest(fs)$p.value
      )
    }, error = function(e) NULL)
  })
})

# ── Summary: proportion of sectors with significant ITS effects ─────────────
its_summary <- its_results %>%
  group_by(shock_date, direction, surprise, outcome) %>%
  summarise(
    n_sectors_ls = sum(ls_p < 0.05, na.rm = TRUE),
    n_sectors_sc = sum(sc_p < 0.05, na.rm = TRUE),
    mean_ls      = mean(level_shift, na.rm = TRUE) * ifelse(outcome[1] == "return", 100, 1),
    .groups      = "drop"
  )

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(its_results,   "data/its_results.rds")
saveRDS(its_summary,   "data/its_summary.rds")
saveRDS(chow_results,  "data/chow_results.rds")
saveRDS(returns_vol,   "data/returns_vol.rds")

message("✓ ITS analysis complete.")
message(sprintf("  • %d ITS models estimated", nrow(its_results)))
message(sprintf("  • Shocks with significant level shifts (>=3 sectors): %d",
                sum(its_summary$n_sectors_ls >= 3 & its_summary$outcome == "return")))
