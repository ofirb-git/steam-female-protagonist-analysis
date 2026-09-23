# Female Protagonist Tags, Estimated Revenue, and Geographic Markets on Steam

This repository contains the R code and final research outputs for an undergraduate management seminar by Ophir Barkai at Tel Aviv University.

The study examines whether the Steam `Female Protagonist` tag is associated with:

1. Estimated game revenue.
2. Gender-equality scores in the leading reported player countries.

The analysis uses OLS models with HC3 heteroskedasticity-robust standard errors. The geographic outcome is a player-share-weighted version of the World Economic Forum's 2025 Global Gender Gap Index (Weighted GGGI).

## Data sources

The initial Steam and Gamalytic data were assembled by Evyatar Segal. They are not redistributed in this repository.

- Kaggle dataset: <https://www.kaggle.com/datasets/evyatarbensegal/steam-full-market-dataset>
- Original data-collection repository: <https://github.com/EvyatarSegal/Scrape_Steam_WebAPI>
- Global Gender Gap Report 2025: World Economic Forum
- Global Gender Gap Report 2021: used only for the Russia sensitivity analysis

The two small country-level reference tables used by the analysis are included under `data/reference/`. They were manually transcribed from the World Economic Forum reports and stored on a 0-100 scale.

## Repository structure

```text
R/                    Analysis scripts, numbered in execution order
data/reference/       WEF country-level reference tables
data/raw/             External source data (local only; not tracked)
data/processed/       Generated intermediate data (local only; not tracked)
results/tables/       Final tables and workbooks
results/figures/      Final figures and table images
results/diagnostics/  Model diagnostics and detailed regression exports
docs/                 Additional documentation
```

## Preparing the data

Create the following folders locally at the repository root:

```text
data/raw/
data/processed/
```

Place the required source files in `data/raw/` using these filenames:

```text
Steam_API_games_data_feb_2026.csv
Gamalytic_games_data_feb_2026_missing_50_thousand_games.csv
publishers_gamalytic_feb_2026.csv
GPU_benchmarks_v7.csv
```

The Steam and Gamalytic filenames reflect the February 2026 snapshot used in the paper. The publisher and GPU files are optional inputs in the master-data script and do not determine inclusion in the main H1 or H2 samples.

## Running the analysis

Open the R project from the repository root and run the scripts in numerical order:

```text
R/01_build_master_dataset.R
R/02_classify_products_and_genres.R
R/03_create_h2_input.R
R/04_build_weighted_gggi.R
R/05_add_economic_subindex.R
R/06_descriptive_statistics.R
R/07_run_regressions.R
R/08_create_final_regression_tables.R
```

All paths are relative to the repository root. The scripts intentionally stop if they are run from a different working directory.

The final script creates the formatted Chapter 6 HTML tables, CSV versions, and four PNG images. It requires `webshot2` and a compatible Chrome or Chromium installation. The verified descriptive tables and figures used in the paper are included under `results/`.

## Main packages

The pipeline uses `data.table`, `dplyr`, `tidyr`, `purrr`, `stringr`, `lubridate`, `ggplot2`, `scales`, `openxlsx`, `sandwich`, `lmtest`, and `webshot2`.

## Statistical conventions

- Statistical significance is marked as `* p<0.05`, `** p<0.01`, and `*** p<0.001`.
- H1 coefficients are estimated using the natural logarithm of estimated revenue.
- H2 outcomes are reported on a 0-100 scale.
- Results are interpreted as statistical associations rather than causal effects.

## Reproducibility note

The repository reproduces the analysis beginning with the February 2026 source files. Reconstructing those files directly from the original APIs is outside the scope of this repository and is documented in Evyatar Segal's original repository.
