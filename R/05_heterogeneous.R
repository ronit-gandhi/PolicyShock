# PolicyShock: 05_heterogeneous.R
# Heterogeneous Treatment Effects (HTE) Analysis
#
# Questions:
#   1. Do sectors differ in their sensitivity to rate HIKES vs CUTS?
#   2. Is the effect of surprise larger during recessions vs expansions?
#   3. Does sector sensitivity change over time (pre/post GFC, pre/post COVID)?
#
# Methods:
#   - Fully interacted OLS with robust SEs (sector × direction × era)
#   - NBER recession indicator as moderator
#   - Effect size heatmap for executive-level communication

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(estimatr)
  library(broom)
  library(patchwork)
  library(scales)
  library(ggrepel)
})

select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
arrange   <- dplyr::arrange
summarise <- dplyr::summarise
group_by  <- dplyr::group_by
ungroup   <- dplyr::ungroup
rename    <- dplyr::rename

cars     <- readRDS("data/cars.rds")
fomc_raw <- readRDS("data/fomc_decisions.rds")
tickers  <- readRDS("data/tickers.rds")

sector_tickers <- setdiff(names(tickers), "SPY")

# ── NBER recession dates (hardcoded from NBER public record) ───────────────
recessions <- tribble(
  ~start,        ~end,
  "2001-03-01",  "2001-11-30",   # dot-com
  "2007-12-01",  "2009-06-30",   # GFC
  "2020-02-01",  "2020-04-30"    # COVID
) %>%
  mutate(across(everything(), as.Date))

is_recession <- function(date) {
  any(map2_lgl(recessions$start, recessions$end,
               ~ date >= .x & date <= .y))
}

# ── Build HTE dataset ──────────────────────────────────────────────────────
hte_data <- cars %>%
  filter(window == "CAR[-1,5]") %>%
  left_join(fomc_raw %>% select(event_date = date, surprise, direction,
                                actual_change, is_surprise),
            by = c("event_date", "surprise", "direction",
                   "actual_change", "is_surprise")) %>%
  mutate(
    car_pct    = CAR * 100,
    treat      = as.integer(is_surprise),
    surp_mag   = abs(surprise),
    is_hike    = as.integer(actual_change > 0),
    is_cut     = as.integer(actual_change < 0),
    in_recession = map_lgl(event_date, is_recession),
    era        = case_when(
      event_date < as.Date("2007-12-01") ~ "Pre-GFC",
      event_date < as.Date("2015-12-01") ~ "Post-GFC / ZLB",
      event_date < as.Date("2020-02-01") ~ "Normalization",
      TRUE                               ~ "COVID / Post-COVID"
    ),
    era        = factor(era, levels = c("Pre-GFC", "Post-GFC / ZLB",
                                        "Normalization", "COVID / Post-COVID")),
    sector     = tickers[ticker],
    sector_fe  = as.factor(ticker),
    year       = as.factor(year(event_date))
  )

# ── Model A: Hike vs Cut asymmetry by sector ──────────────────────────────
hte_hike_cut <- map_dfr(sector_tickers, function(tk) {
  df <- hte_data %>% filter(ticker == tk)
  if (nrow(df) < 5) return(NULL)

  fit <- lm_robust(
    car_pct ~ is_hike,
    data    = df,
    se_type = "HC2"
  )
  tidy(fit) %>%
    filter(term == "is_hike") %>%
    mutate(
      ticker    = tk,
      sector    = tickers[tk],
      model     = "Hike vs Cut",
      direction_effect = "hike_premium"
    )
})

# ── Model B: Surprise × Direction × Sector ────────────────────────────────
hte_surprise_dir <- map_dfr(sector_tickers, function(tk) {
  df <- hte_data %>% filter(ticker == tk)
  if (nrow(df) < 5) return(NULL)

  fit <- lm_robust(
    car_pct ~ treat * is_hike,
    data    = df,
    se_type = "HC2"
  )
  tidy(fit) %>%
    mutate(ticker = tk, sector = tickers[tk])
})

# ── Model C: Recession moderation ─────────────────────────────────────────
hte_recession <- lm_robust(
  car_pct ~ treat * in_recession * sector_fe + is_hike,
  data    = hte_data,
  se_type = "HC2"
)

recession_effects <- tidy(hte_recession) %>%
  filter(grepl("treat:in_recessionTRUE:sector_fe", term)) %>%
  mutate(
    ticker = gsub("treat:in_recessionTRUE:sector_fe", "", term),
    sector = tickers[ticker],
    effect = estimate,
    ci_lo  = conf.low,
    ci_hi  = conf.high,
    sig    = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  ) %>%
  filter(!is.na(sector))

# ── Model D: Era-specific effects ─────────────────────────────────────────
era_effects <- hte_data %>%
  group_by(ticker, era) %>%
  filter(n() >= 3) %>%
  group_modify(~ {
    fit <- lm(car_pct ~ treat + is_hike, data = .x)
    tidy(fit) %>% filter(term == "treat")
  }) %>%
  ungroup() %>%
  mutate(
    sector = tickers[ticker],
    sig    = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  )

# ── Effect size heatmap matrix ─────────────────────────────────────────────
# Mean CAR by sector × direction for "surprise" events only
heatmap_data <- hte_data %>%
  filter(is_surprise) %>%
  group_by(ticker, is_hike) %>%
  summarise(
    mean_car   = mean(car_pct, na.rm = TRUE),
    se_car     = sd(car_pct, na.rm = TRUE) / sqrt(n()),
    n          = n(),
    .groups    = "drop"
  ) %>%
  mutate(
    sector    = tickers[ticker],
    direction = ifelse(is_hike == 1, "Surprise Hike", "Surprise Cut")
  )

# ── Sector sensitivity ranking ─────────────────────────────────────────────
sensitivity_rank <- hte_data %>%
  filter(is_surprise) %>%
  group_by(ticker) %>%
  summarise(
    mean_abs_car = mean(abs(car_pct), na.rm = TRUE),
    mean_car     = mean(car_pct, na.rm = TRUE),
    sd_car       = sd(car_pct, na.rm = TRUE),
    hike_car     = mean(car_pct[is_hike == 1], na.rm = TRUE),
    cut_car      = mean(car_pct[is_hike == 0], na.rm = TRUE),
    asymmetry    = abs(hike_car - cut_car),
    .groups      = "drop"
  ) %>%
  mutate(sector = tickers[ticker]) %>%
  arrange(desc(mean_abs_car))

# ── Save ────────────────────────────────────────────────────────────────────
saveRDS(hte_data,          "data/hte_data.rds")
saveRDS(hte_hike_cut,      "data/hte_hike_cut.rds")
saveRDS(hte_surprise_dir,  "data/hte_surprise_dir.rds")
saveRDS(recession_effects, "data/recession_effects.rds")
saveRDS(era_effects,       "data/era_effects.rds")
saveRDS(heatmap_data,      "data/heatmap_data.rds")
saveRDS(sensitivity_rank,  "data/sensitivity_rank.rds")

message("✓ Heterogeneous treatment effects complete.")
message("\nSector sensitivity ranking (surprise events):")
print(sensitivity_rank %>% select(sector, mean_abs_car, hike_car, cut_car, asymmetry))
