# PolicyShock: run_all.R
# Single script to reproduce the entire analysis from scratch.
# Run from the project root directory: source("run_all.R")
#
# Prerequisites:
#   1. source("R/00_install.R")  to install packages
#   2. fredr::fredr_set_key("YOUR_FRED_API_KEY") or set FRED_API_KEY in ~/.Renviron

cat("╔══════════════════════════════════════════════════════════╗\n")
cat("║           PolicyShock: Full Analysis Pipeline            ║\n")
cat("║  Causal Impact of Fed Rate Decisions on Sector Returns   ║\n")
cat("╚══════════════════════════════════════════════════════════╝\n\n")

start_time <- Sys.time()

steps <- list(
  list(script = "R/01_data.R",          label = "Step 1/6: Pulling & cleaning data"),
  list(script = "R/02_event_study.R",   label = "Step 2/6: Event study (market model ARs)"),
  list(script = "R/03_did.R",           label = "Step 3/6: Difference-in-differences"),
  list(script = "R/04_its.R",           label = "Step 4/6: Interrupted time series"),
  list(script = "R/05_heterogeneous.R", label = "Step 5/6: Heterogeneous treatment effects"),
  list(script = "R/06_figures.R",       label = "Step 6/6: Generating all figures")
)

for (s in steps) {
  cat(sprintf("\n─── %s ───\n", s$label))
  source(s$script)
}

elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 1)
cat(sprintf("\n✓ Pipeline complete in %.1f minutes.\n", elapsed))
cat("  • Analysis objects: data/\n")
cat("  • Figures:          output/\n")
cat("\nTo render the full report:\n")
cat("  quarto::quarto_render('docs/PolicyShock_Report.qmd')\n")
