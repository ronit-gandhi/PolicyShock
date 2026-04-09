# PolicyShock: 02_event_study.R
# Computes abnormal returns (ARs) and cumulative abnormal returns (CARs)
# around FOMC meeting dates using the market model.
#
# Methodology (standard in finance/economics consulting):
#   1. Estimate "normal" returns via OLS market model over a pre-event
#      estimation window [-120, -11] trading days
#   2. AR(t) = actual return - predicted return from market model
#   3. CAR[t1, t2] = sum of ARs over the event window
#   4. Test significance via standardized CAR t-tests (Boehmer et al. 1991)

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(broom)
  library(sandwich)
  library(lmtest)
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

returns_wide  <- readRDS("data/returns_wide.rds")
fomc_raw      <- readRDS("data/fomc_decisions.rds")
tickers       <- readRDS("data/tickers.rds")

sector_tickers <- setdiff(names(tickers), "SPY")
all_trading_days <- returns_wide %>% pull(date) %>% sort()

# ── Market model estimation ─────────────────────────────────────────────────
# For each event × sector, estimate alpha + beta over estimation window,
# then compute ARs in event window [-5, +10]

ESTIM_PRE  <- 120   # estimation window start (days before event)
ESTIM_END  <- 11    # estimation window end   (days before event)
EVENT_PRE  <- 5     # event window: days before
EVENT_POST <- 10    # event window: days after

compute_market_model <- function(event_date, ticker, returns_wide) {

  idx <- which.min(abs(all_trading_days - event_date))

  # Indices
  est_start <- idx - ESTIM_PRE
  est_end   <- idx - ESTIM_END
  evt_start <- idx - EVENT_PRE
  evt_end   <- idx + EVENT_POST

  if (est_start < 1 || evt_end > nrow(returns_wide)) return(NULL)

  estim_data <- returns_wide[est_start:est_end, ] %>%
    select(date, ret_sector = all_of(ticker), ret_mkt = SPY) %>%
    filter(!is.na(ret_sector), !is.na(ret_mkt))

  event_data <- returns_wide[evt_start:evt_end, ] %>%
    select(date, ret_sector = all_of(ticker), ret_mkt = SPY) %>%
    filter(!is.na(ret_sector), !is.na(ret_mkt))

  if (nrow(estim_data) < 60 || nrow(event_data) < 1) return(NULL)

  # OLS market model
  fit <- lm(ret_sector ~ ret_mkt, data = estim_data)
  alpha <- coef(fit)[1]
  beta  <- coef(fit)[2]
  sigma <- summary(fit)$sigma   # residual SD for standardization

  # Abnormal returns
  event_data %>%
    mutate(
      predicted = alpha + beta * ret_mkt,
      AR        = ret_sector - predicted,
      AR_std    = AR / sigma,            # standardized AR
      event_date = event_date,
      ticker     = ticker,
      days_rel   = as.integer(date - all_trading_days[idx])
    )
}

message("Computing abnormal returns for all FOMC events × sectors...")

ar_results <- map_dfr(
  fomc_raw$date,
  function(ev_date) {
    map_dfr(sector_tickers, function(tk) {
      compute_market_model(ev_date, tk, returns_wide)
    })
  }
)

# Join FOMC metadata
ar_results <- ar_results %>%
  left_join(fomc_raw %>% rename(event_date = date), by = "event_date")

# ── Cumulative Abnormal Returns (CARs) ─────────────────────────────────────
# Windows: [0,1] (announcement day + 1), [0,5], [-1,+1], [-1,+5]
car_windows <- list(
  "CAR[0,1]"  = c(0, 1),
  "CAR[0,5]"  = c(0, 5),
  "CAR[-1,1]" = c(-1, 1),
  "CAR[-1,5]" = c(-1, 5)
)

cars <- map_dfr(names(car_windows), function(win_name) {
  w <- car_windows[[win_name]]
  ar_results %>%
    filter(days_rel >= w[1], days_rel <= w[2]) %>%
    group_by(event_date, ticker, surprise, direction, decision,
             actual_change, is_surprise) %>%
    summarise(
      CAR     = sum(AR, na.rm = TRUE),
      CAR_std = sum(AR_std, na.rm = TRUE) / sqrt(n()),  # Boehmer standardized
      n_days  = n(),
      .groups = "drop"
    ) %>%
    mutate(window = win_name)
})

# ── Average CARs by sector and event type ──────────────────────────────────
avg_cars <- cars %>%
  group_by(ticker, direction, window) %>%
  summarise(
    mean_CAR  = mean(CAR, na.rm = TRUE) * 100,   # convert to percent
    se_CAR    = sd(CAR, na.rm = TRUE) / sqrt(n()) * 100,
    n_events  = n(),
    t_stat    = mean(CAR_std, na.rm = TRUE),
    p_value   = if (n() > 1) {
      2 * pt(-abs(mean(CAR_std, na.rm = TRUE)), df = n() - 1)
    } else {
      NA_real_
    },
    .groups   = "drop"
  ) %>%
  mutate(
    sig_label = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    sector = tickers[ticker]
  )

# ── AR time-path: average AR by day-relative for each direction ─────────────
ar_path <- ar_results %>%
  group_by(ticker, days_rel, direction) %>%
  summarise(
    mean_AR = mean(AR, na.rm = TRUE) * 100,
    se_AR   = sd(AR, na.rm = TRUE) / sqrt(n()) * 100,
    .groups = "drop"
  ) %>%
  mutate(sector = tickers[ticker])

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(ar_results, "data/ar_results.rds")
saveRDS(cars,       "data/cars.rds")
saveRDS(avg_cars,   "data/avg_cars.rds")
saveRDS(ar_path,    "data/ar_path.rds")

message("✓ Event study complete.")
message(sprintf("  • %d AR observations across %d events × %d sectors",
                nrow(ar_results), nrow(fomc_raw), length(sector_tickers)))
