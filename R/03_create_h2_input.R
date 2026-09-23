# ==============================================================================
# Create the compact H2 input, including language indicator variables
# ==============================================================================
# Drop-in replacement for create_H2_compact_input.R.
# The key correction is that language columns are selected by both:
#   1. the word "language" anywhere in the column name; and
#   2. the prefix "lang_" used by the individual language indicators.
#
# The script reads the corrected master and writes only a derived compact file.
# It does not change the master or any existing GGGI score.
# ==============================================================================

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data")) {
  stop("Run this script from the repository root (the folder containing R/ and data/).")
}

if (!requireNamespace("data.table", quietly = TRUE)) {
  stop("Install the data.table package before running this script.")
}
library(data.table)

input_file <- "data/processed/Clean_games_data_feb_2026_v2.csv"
output_dir <- "data/processed"
output_file <- file.path(output_dir, "H2_input_compact.csv")
columns_file <- file.path(output_dir, "H2_detected_columns.txt")

if (!file.exists(input_file)) stop("Master file not found: ", input_file)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 1. Detect the required columns without loading the full master
# ------------------------------------------------------------------------------

header <- fread(input_file, nrows = 0L)
all_names <- names(header)

essential_candidates <- c(
  "appid", "name", "title", "is_non_game_product", "has_tag_data",
  "tag_female_protagonist", "has_country_data", "is_free",
  "is_early_access", "release_year", "price_current"
)
essential_cols <- intersect(essential_candidates, all_names)

required_essential <- c(
  "appid", "is_non_game_product", "has_tag_data",
  "tag_female_protagonist", "has_country_data", "is_free",
  "is_early_access", "release_year"
)
missing_essential <- setdiff(required_essential, essential_cols)
if (length(missing_essential)) {
  stop("Missing essential columns: ", paste(missing_essential, collapse = ", "))
}

country_cols <- grep(
  "^(pct_)|country|countries|players_by_country|reported_country",
  all_names,
  value = TRUE,
  ignore.case = TRUE
)
genre_cols <- grep("genre", all_names, value = TRUE, ignore.case = TRUE)

# Corrected language selection: the former script searched only for
# "language" and therefore omitted columns such as lang_French.
language_cols <- unique(c(
  grep("language", all_names, value = TRUE, ignore.case = TRUE),
  grep("^lang_", all_names, value = TRUE, ignore.case = TRUE)
))
language_flags <- grep("^lang_", all_names, value = TRUE, ignore.case = TRUE)

if (!length(language_flags)) {
  stop("No lang_* indicator columns were detected in the corrected master.")
}
if (!setequal(language_flags, intersect(language_cols, language_flags))) {
  stop("Not all lang_* indicators were selected.")
}

selected_cols <- unique(c(
  essential_cols, country_cols, genre_cols, language_cols
))

cat("\nSelected columns:", length(selected_cols), "\n")
cat("Language indicators detected:", length(language_flags), "\n")

# ------------------------------------------------------------------------------
# 2. Read only the selected columns and construct the H2 candidate sample
# ------------------------------------------------------------------------------

h2_raw <- fread(
  input_file,
  select = selected_cols,
  colClasses = list(character = "appid"),
  na.strings = c("", "NA"),
  showProgress = TRUE
)

if (anyNA(h2_raw$appid) || anyDuplicated(h2_raw$appid)) {
  stop("appid must be nonmissing and unique in the master.")
}

to_binary <- function(x, label) {
  s <- tolower(trimws(as.character(x)))
  missing <- is.na(x) | s %in% c("", "na", "nan")
  valid <- missing | s %in% c("0", "1", "true", "false", "t", "f", "yes", "no")
  if (any(!valid)) stop("Unexpected binary coding in ", label)
  ans <- rep(NA_integer_, length(x))
  ans[!missing & s %in% c("1", "true", "t", "yes")] <- 1L
  ans[!missing & s %in% c("0", "false", "f", "no")] <- 0L
  ans
}

filter_cols <- c("is_non_game_product", "has_tag_data", "has_country_data")
for (nm in filter_cols) {
  set(h2_raw, j = nm, value = to_binary(h2_raw[[nm]], nm))
}

h2_candidate <- h2_raw[
  is_non_game_product == 0L &
    has_tag_data == 1L &
    has_country_data == 1L
]

if (nrow(h2_candidate) != 33766L) {
  stop(
    "Expected 33,766 H2 candidate games; found ", nrow(h2_candidate),
    ". Stop and inspect the upstream master before rebuilding GGGI."
  )
}

female_tag <- to_binary(
  h2_candidate$tag_female_protagonist,
  "tag_female_protagonist"
)
if (anyNA(female_tag)) {
  stop("Female Protagonist tag is missing in the H2 candidate sample.")
}

pct_cols <- grep("^pct_", names(h2_candidate), value = TRUE, ignore.case = TRUE)
if (!length(pct_cols)) stop("No pct_* country-share columns were detected.")

# ------------------------------------------------------------------------------
# 3. Save the drop-in compact input and a transparent column manifest
# ------------------------------------------------------------------------------

fwrite(h2_candidate, output_file, na = "")

writeLines(
  c(
    "H2 COMPACT INPUT - DETECTED COLUMNS",
    "===================================",
    paste("Candidate observations:", nrow(h2_candidate)),
    paste("Female Protagonist:", sum(female_tag == 1L)),
    paste("No Female Protagonist tag:", sum(female_tag == 0L)),
    paste("Language indicators:", length(language_flags)),
    "",
    "ALL SELECTED COLUMNS",
    "--------------------",
    selected_cols,
    "",
    "LANGUAGE COLUMNS",
    "----------------",
    language_cols,
    "",
    "LANGUAGE INDICATORS (lang_*)",
    "----------------------------",
    language_flags,
    "",
    "COUNTRY-SHARE COLUMNS (pct_*)",
    "-----------------------------",
    pct_cols
  ),
  columns_file,
  useBytes = TRUE
)

cat("\n========================================\n")
cat("H2 compact input created successfully\n")
cat("========================================\n")
cat("Candidate H2 observations:", nrow(h2_candidate), "\n")
cat("Female Protagonist:", sum(female_tag == 1L), "\n")
cat("No Female Protagonist tag:", sum(female_tag == 0L), "\n")
cat("lang_* indicators included:", length(language_flags), "\n")
cat("Compact file:", normalizePath(output_file, winslash = "/"), "\n")
cat("Column manifest:", normalizePath(columns_file, winslash = "/"), "\n")
cat("File size (MiB):", round(file.info(output_file)$size / 1024^2, 2), "\n")
