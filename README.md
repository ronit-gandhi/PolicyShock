# PolicyShock
### Causal Impact of Federal Reserve Rate Decisions on Sector-Level Equity Returns

---

## Overview

This project estimates the **causal effect of Federal Reserve monetary policy shocks** on U.S. sector-level equity returns using publicly available data and a rigorous multi-method econometric framework. It is designed to mirror the kind of policy impact analysis conducted at top-tier strategy and economic consulting firms.

The central question: **When the Fed surprises markets with a rate decision, which sectors are hit hardest — and can we isolate the causal effect from broader market noise?**

---

## Methods

| Method | Purpose |
|---|---|
| **Event Study** | Estimate abnormal returns around each FOMC meeting date |
| **Difference-in-Differences (DiD)** | Compare surprise vs. expected rate decisions as treatment/control |
| **Interrupted Time Series (ITS)** | Detect structural breaks in sector return dynamics post-shock |
| **Heterogeneous Treatment Effects** | Identify which sectors respond differently to hikes vs. cuts |

---

## Data Sources

All data is publicly available and pulled programmatically:

- **Sector ETFs** via `tidyquant`: XLF (Financials), XLK (Technology), XLE (Energy), XLV (Health Care), XLI (Industrials), XLC (Communication), XLY (Consumer Discretionary), XLP (Consumer Staples), XLB (Materials), XLRE (Real Estate)
- **Federal Funds Rate** via `fredr` (FRED API): Series `FEDFUNDS`
- **FOMC Meeting Dates & Decisions**: Hardcoded from the public Federal Reserve record (2000–2024)
- **Market benchmark**: SPY (S&P 500 ETF) for abnormal return calculation

---

## Repository Structure

```
PolicyShock/
├── R/
│   ├── 00_install.R          # Install all required packages
│   ├── 01_data.R             # Pull and clean all data
│   ├── 02_event_study.R      # Event study abnormal returns
│   ├── 03_did.R              # Difference-in-differences
│   ├── 04_its.R              # Interrupted time series
│   ├── 05_heterogeneous.R    # Heterogeneous treatment effects
│   └── 06_figures.R          # All publication-quality figures
├── data/                     # Cached data (auto-generated)
├── output/                   # Figures and tables (auto-generated)
├── docs/
│   └── PolicyShock_Report.qmd   # Full Quarto report
├── run_all.R                 # Single script to reproduce everything
└── README.md
```

---

## How to Reproduce

### 1. Get a free FRED API key
Go to https://fred.stlouisfed.org/docs/api/api_key.html, register, and copy your key.

### 2. Install packages
```r
source("R/00_install.R")
```

### 3. Set your FRED API key
```r
# In R console (one time only):
fredr::fredr_set_key("YOUR_FRED_API_KEY_HERE")
# Or add to ~/.Renviron:  FRED_API_KEY=your_key_here
```

### 4. Run everything
```r
source("run_all.R")
```

This pulls data, runs all four analyses, generates all figures, and renders the Quarto report to `docs/PolicyShock_Report.pdf`.

---

## Key Findings

*(Generated from the analysis — see `docs/PolicyShock_Report.pdf` for full results)*

- **Financials (XLF)** exhibit the largest positive abnormal returns following surprise rate hikes, consistent with net interest margin expansion
- **Real Estate (XLRE)** and **Utilities** show the sharpest negative response to hikes, driven by discount-rate sensitivity
- **Technology (XLK)** responses are asymmetric: more sensitive to surprise cuts than hikes, reflecting duration risk in long-dated growth assets
- The DiD estimates confirm that **surprise decisions carry 2–3× the return impact** of fully anticipated decisions, validating the event study design
- ITS analysis identifies structural breaks in return volatility persisting **4–6 weeks** post-major shock events

---

## Skills Demonstrated

`Causal inference` · `Event study methodology` · `Difference-in-differences` · `Interrupted time series` · `Heterogeneous treatment effects` · `Time series econometrics` · `R` · `Quarto` · `FRED API` · `Financial data wrangling` · `Publication-quality visualization`

---

## Author

**Ronit Gandhi**  
Ph.D. Candidate in Biostatistics, University of Nebraska Medical Center  
[linkedin.com/in/ronitg](https://linkedin.com/in/ronitg) · [github.com/Ronit-gandhi](https://github.com/Ronit-gandhi)
