# PolicyShock: 03_did.R
# Difference-in-Differences: Do SURPRISE rate decisions cause larger
# sector return responses than EXPECTED decisions?
#
# Design:
#   Treatment = surprise FOMC decision (|surprise| > 10bp)
#   Control   = expected FOMC decision (|surprise| <= 10bp)
#   Outcome   = CAR[-1, +5] for each sector
#   Covariates: direction (hike/cut), VIX proxy (abs market return day-of),
#               year FE, sector FE
#
# Identification: Conditional on the direction of the decision, the
# "surprise" component is plausibly exogenous to sector-specific factors
# (Kuttner 2001; Bernanke & Kuttner 2005).

suppressPackageStartupMessages({
  library(tidyverse)
  library(estimatr)
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
rename    <- dplyr::rename

cars       <- readRDS("data/cars.rds")
fomc_raw   <- readRDS("data/fomc_decisions.rds")
tickers    <- readRDS("data/tickers.rds")

# ── Build DiD dataset ──────────────────────────────────────────────────────
# Use CAR[-1,+5] as outcome (captures announcement + drift)
did_data <- cars %>%
  filter(window == "CAR[-1,5]") %>%
  left_join(
    fomc_raw %>% select(event_date = date, surprise, direction,
                        actual_change, is_surprise),
    by = c("event_date", "surprise", "direction",
           "actual_change", "is_surprise")
  ) %>%
  mutate(
    # Treatment: surprise decision
    treat      = as.integer(is_surprise),
    # Direction dummies
    is_hike    = as.integer(actual_change > 0),
    is_cut     = as.integer(actual_change < 0),
    # Surprise magnitude (continuous version)
    surp_mag   = abs(surprise),
    # Year and sector fixed effects
    year       = as.factor(year(event_date)),
    sector_fe  = as.factor(ticker),
    # Outcome in percent
    car_pct    = CAR * 100
  )

# ── Model 1: Simple DiD ────────────────────────────────────────────────────
# CAR_i,e = alpha + beta*Treat_e + gamma*Direction_e + delta_s + epsilon
m1 <- lm_robust(
  car_pct ~ treat + is_hike + sector_fe,
  data    = did_data,
  se_type = "HC2"
)

# ── Model 2: DiD with sector × direction interaction ──────────────────────
# Tests whether surprise effect varies by sector
m2 <- lm_robust(
  car_pct ~ treat * sector_fe + is_hike + year,
  data    = did_data,
  se_type = "HC2"
)

# ── Model 3: Continuous treatment (surprise magnitude) ────────────────────
# Uses |surprise| as continuous treatment variable
m3 <- lm_robust(
  car_pct ~ surp_mag * sector_fe + is_hike + year,
  data    = did_data,
  se_type = "HC2"
)

# ── Model 4: Separate hike vs cut surprises ────────────────────────────────
m4 <- lm_robust(
  car_pct ~ treat * is_hike * sector_fe,
  data    = did_data,
  se_type = "HC2"
)

# ── Extract sector-level surprise effects ─────────────────────────────────
# Marginal effect of surprise by sector from Model 2
sector_surprise_effects <- tidy(m2) %>%
  filter(grepl("^treat:sector_fe", term)) %>%
  mutate(
    ticker    = gsub("treat:sector_fe", "", term),
    effect    = estimate,
    ci_lo     = conf.low,
    ci_hi     = conf.high,
    p_val     = p.value,
    sig       = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE           ~ ""
    ),
    sector    = tickers[ticker]
  ) %>%
  # Add baseline (reference sector = XLB)
  bind_rows(
    tidy(m2) %>%
      filter(term == "treat") %>%
      mutate(
        ticker  = "XLB",
        effect  = estimate,
        ci_lo   = conf.low,
        ci_hi   = conf.high,
        p_val   = p.value,
        sig     = ifelse(p.value < 0.05, "*", ""),
        sector  = tickers["XLB"]
      )
  ) %>%
  filter(!is.na(sector)) %>%
  select(ticker, sector, effect, ci_lo, ci_hi, p_val, sig) %>%
  arrange(desc(effect))

# ── Parallel trends check ──────────────────────────────────────────────────
# Test that surprise and expected events have similar pre-event trends
# by checking ARs in [-10, -2] window
ar_results <- readRDS("data/ar_results.rds")

pre_trends <- ar_results %>%
  filter(days_rel %in% -10:-2) %>%
  group_by(ticker, is_surprise, days_rel) %>%
  summarise(
    mean_AR = mean(AR, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    group  = ifelse(is_surprise, "Surprise", "Expected"),
    sector = tickers[ticker]
  )

# Formal test: regress pre-event AR on treat × time interaction
# (should be insignificant under parallel trends)
pre_trend_test <- ar_results %>%
  filter(days_rel %in% -10:-2) %>%
  mutate(
    treat    = as.integer(is_surprise),
    car_pct  = AR * 100,
    sector_fe = as.factor(ticker)
  ) %>%
  lm_robust(
    car_pct ~ treat * days_rel + sector_fe,
    data    = .,
    se_type = "HC2"
  )

pre_trend_result <- tidy(pre_trend_test) %>%
  filter(term == "treat:days_rel") %>%
  transmute(
    term      = "Treat × Pre-trend",
    estimate  = round(estimate, 5),
    std.error = round(std.error, 5),
    p.value   = round(p.value, 3),
    verdict   = ifelse(p.value > 0.10,
                       "✓ Parallel trends plausible",
                       "⚠ Potential pre-trend violation")
  )

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(did_data,               "data/did_data.rds")
saveRDS(list(m1=m1,m2=m2,m3=m3,m4=m4), "data/did_models.rds")
saveRDS(sector_surprise_effects, "data/sector_surprise_effects.rds")
saveRDS(pre_trends,             "data/pre_trends.rds")
saveRDS(pre_trend_result,       "data/pre_trend_test.rds")

message("✓ DiD analysis complete.")
message(sprintf("  • Baseline surprise effect (Model 1): %.3f%% CAR (p=%.3f)",
                tidy(m1) %>% filter(term=="treat") %>% pull(estimate),
                tidy(m1) %>% filter(term=="treat") %>% pull(p.value)))
message(sprintf("  • Parallel trends test: %s",
                pre_trend_result$verdict))
