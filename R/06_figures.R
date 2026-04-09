# PolicyShock: 06_figures.R
# Generates all publication-quality figures for the report and GitHub.
# All figures saved to output/ as high-res PNG and PDF.

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(scales)
  library(ggrepel)
  library(lubridate)
})

select    <- dplyr::select
filter    <- dplyr::filter
mutate    <- dplyr::mutate
arrange   <- dplyr::arrange
summarise <- dplyr::summarise
group_by  <- dplyr::group_by
ungroup   <- dplyr::ungroup
rename    <- dplyr::rename

dir.create("output", showWarnings = FALSE)

tickers          <- readRDS("data/tickers.rds")
avg_cars         <- readRDS("data/avg_cars.rds")
ar_path          <- readRDS("data/ar_path.rds")
sector_surprise  <- readRDS("data/sector_surprise_effects.rds")
pre_trends       <- readRDS("data/pre_trends.rds")
its_results      <- readRDS("data/its_results.rds")
heatmap_data     <- readRDS("data/heatmap_data.rds")
sensitivity_rank <- readRDS("data/sensitivity_rank.rds")
era_effects      <- readRDS("data/era_effects.rds")
fomc_raw         <- readRDS("data/fomc_decisions.rds")
returns_wide     <- readRDS("data/returns_wide.rds")

# ── Theme ──────────────────────────────────────────────────────────────────
theme_policy <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2,
                                      color = "#1a285a"),
      plot.subtitle    = element_text(size = base_size - 1, color = "#555555"),
      plot.caption     = element_text(size = base_size - 2, color = "#888888",
                                      hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#eeeeee"),
      axis.title       = element_text(size = base_size - 1, color = "#333333"),
      legend.position  = "bottom",
      strip.text       = element_text(face = "bold", color = "#1a285a"),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

NAVY   <- "#1a285a"
RED    <- "#c0392b"
GREEN  <- "#1a7a4a"
ORANGE <- "#d35400"
GRAY   <- "#95a5a6"

save_fig <- function(p, name, w = 10, h = 6.5) {
  ggsave(file.path("output", paste0(name, ".png")), p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path("output", paste0(name, ".pdf")), p,
         width = w, height = h, bg = "white")
  message(sprintf("  Saved: output/%s", name))
}

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 1: Cumulative Abnormal Returns by Sector × Event Type
# ═══════════════════════════════════════════════════════════════════════════
fig1_data <- avg_cars %>%
  filter(window == "CAR[-1,5]") %>%
  mutate(
    sector = factor(sector, levels = sensitivity_rank$sector),
    direction_lab = case_when(
      direction == "hawkish_surprise" ~ "Hawkish Surprise",
      direction == "dovish_surprise"  ~ "Dovish Surprise",
      direction == "expected"         ~ "Expected Decision"
    ),
    direction_lab = factor(direction_lab,
                           levels = c("Hawkish Surprise",
                                      "Dovish Surprise",
                                      "Expected Decision"))
  )

fig1 <- fig1_data %>%
  ggplot(aes(x = sector, y = mean_CAR,
             ymin = mean_CAR - 1.96 * se_CAR,
             ymax = mean_CAR + 1.96 * se_CAR,
             color = direction_lab, shape = direction_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = GRAY, linewidth = 0.5) +
  geom_errorbar(width = 0.3, linewidth = 0.6, alpha = 0.7,
                position = position_dodge(0.6)) +
  geom_point(size = 3, position = position_dodge(0.6)) +
  geom_text(aes(label = sig_label, y = mean_CAR + 1.96 * se_CAR + 0.15),
            size = 3, position = position_dodge(0.6), show.legend = FALSE) +
  scale_color_manual(
    values = c("Hawkish Surprise" = RED,
               "Dovish Surprise"  = GREEN,
               "Expected Decision"= GRAY)
  ) +
  scale_shape_manual(
    values = c("Hawkish Surprise" = 16,
               "Dovish Surprise"  = 17,
               "Expected Decision"= 1)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "Figure 1: Sector Abnormal Returns Around FOMC Decisions",
    subtitle = "CAR[-1, +5] trading days | Market model adjusted | 95% confidence intervals\n* p<0.10, ** p<0.05, *** p<0.01 (Boehmer standardized test)",
    x        = NULL,
    y        = "Cumulative Abnormal Return (%)",
    color    = NULL, shape = NULL,
    caption  = "Source: Yahoo Finance (tidyquant), FOMC public record. Estimation window: [-120, -11] trading days."
  ) +
  theme_policy() +
  theme(legend.position = "top")

save_fig(fig1, "fig1_car_by_sector", w = 10, h = 7)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 2: AR Time Path ([-5, +10] days) for key sectors
# ═══════════════════════════════════════════════════════════════════════════
key_sectors <- c("Financials", "Technology", "Real Estate",
                 "Energy", "Consumer Staples", "Health Care")

fig2_data <- ar_path %>%
  filter(sector %in% key_sectors,
         direction %in% c("hawkish_surprise", "dovish_surprise"),
         days_rel %in% -5:10) %>%
  mutate(
    direction_lab = ifelse(direction == "hawkish_surprise",
                           "Hawkish Surprise", "Dovish Surprise")
  )

fig2 <- fig2_data %>%
  ggplot(aes(x = days_rel, y = mean_AR,
             ymin = mean_AR - 1.96 * se_AR,
             ymax = mean_AR + 1.96 * se_AR,
             color = direction_lab, fill = direction_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = GRAY) +
  geom_vline(xintercept = 0, linetype = "dotted", color = NAVY, alpha = 0.5) +
  geom_ribbon(alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ sector, ncol = 3, scales = "free_y") +
  scale_color_manual(values = c("Hawkish Surprise" = RED,
                                "Dovish Surprise"  = GREEN)) +
  scale_fill_manual(values  = c("Hawkish Surprise" = RED,
                                "Dovish Surprise"  = GREEN)) +
  scale_x_continuous(breaks = c(-5, 0, 5, 10),
                     labels = c("-5", "0\n(FOMC)", "+5", "+10")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 2: Abnormal Return Dynamics Around Surprise FOMC Decisions",
    subtitle = "Mean daily abnormal return (%) | ±95% CI | Day 0 = FOMC announcement",
    x        = "Trading Days Relative to FOMC",
    y        = "Mean Abnormal Return (%)",
    color    = NULL, fill = NULL,
    caption  = "Source: Yahoo Finance, FOMC public record."
  ) +
  theme_policy() +
  theme(legend.position = "top")

save_fig(fig2, "fig2_ar_timepath", w = 12, h = 8)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 3: DiD — Surprise vs Expected, Sector Comparison
# ═══════════════════════════════════════════════════════════════════════════
fig3_data <- sector_surprise %>%
  mutate(
    sector = factor(sector, levels = sector[order(effect)]),
    color  = ifelse(effect > 0, GREEN, RED)
  )

fig3 <- fig3_data %>%
  ggplot(aes(x = sector, y = effect,
             ymin = ci_lo, ymax = ci_hi)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = GRAY) +
  geom_errorbar(aes(color = effect > 0), width = 0.3, linewidth = 0.7) +
  geom_point(aes(color = effect > 0), size = 3.5) +
  geom_text(aes(label = paste0(sig, "\n", round(effect, 2), "%"),
                y = ifelse(effect > 0, ci_hi + 0.1, ci_lo - 0.1),
                hjust = ifelse(effect > 0, 0, 1)),
            size = 2.8, color = "#333333") +
  scale_color_manual(values = c("TRUE" = GREEN, "FALSE" = RED),
                     labels = c("TRUE" = "Positive", "FALSE" = "Negative"),
                     guide  = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_flip() +
  labs(
    title    = "Figure 3: Differential Impact of Surprise vs. Expected FOMC Decisions",
    subtitle = "Marginal effect of surprise (vs. expected decision) on CAR[-1,+5]\nDiD estimate with HC2 robust standard errors | * p<0.10, ** p<0.05, *** p<0.01",
    x        = NULL,
    y        = "Additional CAR from Surprise (%)",
    caption  = "Model: CAR ~ Surprise × Sector + Direction FE + Year FE. Reference sector: Materials (XLB)."
  ) +
  theme_policy()

save_fig(fig3, "fig3_did_surprise_effect", w = 9, h = 7)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 4: Heatmap — Sector × Direction effect sizes
# ═══════════════════════════════════════════════════════════════════════════
fig4 <- heatmap_data %>%
  mutate(sector = factor(sector, levels = sensitivity_rank$sector)) %>%
  ggplot(aes(x = direction, y = sector, fill = mean_car)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f%%\n(n=%d)", mean_car, n)),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = RED,
    mid      = "white",
    high     = GREEN,
    midpoint = 0,
    labels   = function(x) paste0(x, "%"),
    name     = "Mean CAR"
  ) +
  labs(
    title    = "Figure 4: Mean CAR by Sector and Surprise Direction",
    subtitle = "Surprise FOMC events only (|surprise| > 10bp) | CAR[-1, +5] trading days",
    x        = NULL,
    y        = NULL,
    caption  = "Sectors ordered by overall surprise sensitivity (top = most sensitive)."
  ) +
  theme_policy() +
  theme(
    axis.text.x    = element_text(face = "bold", size = 11),
    legend.position = "right",
    panel.grid      = element_blank()
  )

save_fig(fig4, "fig4_heatmap", w = 8, h = 6.5)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 5: Era-specific effects (Pre-GFC / ZLB / Normalization / Post-COVID)
# ═══════════════════════════════════════════════════════════════════════════
fig5_data <- era_effects %>%
  filter(!is.na(estimate)) %>%
  mutate(
    sector = factor(sector, levels = sensitivity_rank$sector),
    sig_pt = case_when(p.value < 0.05 ~ "●", TRUE ~ "○")
  )

fig5 <- fig5_data %>%
  ggplot(aes(x = era, y = estimate, color = estimate > 0,
             group = sector)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = GRAY) +
  geom_line(color = GRAY, alpha = 0.4) +
  geom_point(aes(shape = p.value < 0.05), size = 3) +
  facet_wrap(~ sector, ncol = 5) +
  scale_color_manual(values = c("TRUE" = GREEN, "FALSE" = RED),
                     guide  = "none") +
  scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "p ≥ 0.05"),
                     name   = NULL) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_discrete(labels = c("Pre-GFC"         = "Pre-\nGFC",
                               "Post-GFC / ZLB"  = "ZLB",
                               "Normalization"   = "Normal.",
                               "COVID / Post-COVID" = "COVID+")) +
  labs(
    title    = "Figure 5: Surprise Effect on Sector Returns Across Monetary Regimes",
    subtitle = "OLS estimate of CAR[-1,+5] from surprise decisions by era | Filled = p < 0.05",
    x        = NULL,
    y        = "Estimated Effect (%)",
    caption  = "Eras: Pre-GFC (<Dec 2007), ZLB (Dec 2007–Dec 2015), Normalization (Dec 2015–Feb 2020), COVID+ (Feb 2020–)."
  ) +
  theme_policy() +
  theme(
    axis.text.x     = element_text(size = 7),
    strip.text      = element_text(size = 8),
    legend.position = "top"
  )

save_fig(fig5, "fig5_era_effects", w = 13, h = 7)

# ═══════════════════════════════════════════════════════════════════════════
# FIGURE 6: Parallel Trends Validation
# ═══════════════════════════════════════════════════════════════════════════
fig6_data <- pre_trends %>%
  filter(sector %in% key_sectors)

fig6 <- fig6_data %>%
  ggplot(aes(x = days_rel, y = mean_AR,
             color = group, linetype = group)) +
  geom_hline(yintercept = 0, color = GRAY, linetype = "dashed") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  facet_wrap(~ sector, ncol = 3) +
  scale_color_manual(values = c("Surprise" = RED, "Expected" = NAVY)) +
  scale_linetype_manual(values = c("Surprise" = "solid", "Expected" = "dashed")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Figure 6: Parallel Pre-Trends Validation",
    subtitle = "Mean daily AR in [-10, -2] window before FOMC | Surprise vs. Expected events\nSimilar pre-event trends support DiD identification assumption",
    x        = "Trading Days Before FOMC",
    y        = "Mean Abnormal Return (%)",
    color    = NULL, linetype = NULL,
    caption  = "If the parallel trends assumption holds, surprise and expected groups should show similar pre-event return patterns."
  ) +
  theme_policy() +
  theme(legend.position = "top")

save_fig(fig6, "fig6_parallel_trends", w = 12, h = 7)

message("\n✓ All 6 figures saved to output/")
