# Data instructions

Only the small WEF reference tables are tracked in this repository.

## `reference/`

- `GGGI_2025_country_lookup.csv` contains the 2025 Global Gender Gap Index scores used to construct Weighted GGGI.
- `WEF_2025_Economic_Participation_lookup.csv` contains the 2025 Economic Participation and Opportunity subindex scores.

Both tables use a 0-100 scale. Missing values for Russia, Taiwan, and Hong Kong reflect the absence of official 2025 WEF scores used in the main analysis.

## `raw/`

Create this folder locally and place the external source files described in the main README inside it. Its contents are excluded from Git.

## `processed/`

This folder is created locally by the analysis pipeline and contains derived datasets. Its contents are excluded from Git because they can be regenerated from the source data and scripts.
