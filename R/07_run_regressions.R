# ==============================================================================
# Chapter 6 - regression models for H1 and H2
# ==============================================================================
# This script:
# 1. Audits all estimation samples before fitting any model.
# 2. Estimates the pre-specified H1 and H2 models by OLS with HC3 standard errors.
# 3. Adds language flags to the existing H2 samples by appid without rebuilding GGGI.
# 4. Groups release years through 2008 into one category. The early H2 years
#    contain very few observations (including one in 2003), so this preserves
#    all observations and keeps HC3 leverage well below 1.
# 5. Selects language controls before inspecting regression results, using only
#    their prevalence in the cleaned H2 main sample (20%-80%).
# 6. Creates a separate Russia sensitivity sample using Russia's last official
#    WEF GGGI score (2021: 0.708, or 70.8 on the 0-100 scale; Table 1.1,
#    report page 10). Taiwan and Hong Kong remain
#    uncovered. This does not alter the official-2025 main or 80% samples.
#
# Required packages: data.table, sandwich, lmtest
# Required existing files:
#   data/processed/Clean_games_data_feb_2026_v2.csv
#   data/processed/H2_main_GGGI_2025_with_Economic.csv
#   data/processed/H2_robustness_80pct_GGGI_2025_with_Economic.csv
#   data/reference/GGGI_2025_country_lookup.csv
#
# Important: source files are read only. Every run replaces the contents of the
# fixed output folder results/diagnostics/chapter6.
# ==============================================================================

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data") || !dir.exists("results")) {
  stop("Run this script from the repository root (the folder containing R/, data/ and results/).")
}

required_packages <- c("data.table", "sandwich", "lmtest")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop(
    "Install the required packages first: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(data.table)

# ------------------------------------------------------------------------------
# 1. Paths and output directory
# ------------------------------------------------------------------------------

locate_one <- function(candidates, label) {
  existing <- candidates[file.exists(candidates)]
  existing <- unique(normalizePath(existing, winslash = "/", mustWork = TRUE))
  if (length(existing) != 1L) {
    stop(
      "Expected exactly one ", label, ". Found ", length(existing),
      ". Checked: ", paste(candidates, collapse = " | ")
    )
  }
  existing[[1L]]
}

master_file <- locate_one(
  "data/processed/Clean_games_data_feb_2026_v2.csv",
  "corrected master file"
)

h2_main_file <- locate_one(
  "data/processed/H2_main_GGGI_2025_with_Economic.csv",
  "H2 main file with Economic Participation"
)

h2_80_file <- locate_one(
  "data/processed/H2_robustness_80pct_GGGI_2025_with_Economic.csv",
  "H2 80% file with Economic Participation"
)

gggi_lookup_file <- locate_one(
  "data/reference/GGGI_2025_country_lookup.csv",
  "GGGI 2025 country lookup"
)

output_parent <- "results/diagnostics"
dir.create(output_parent, recursive = TRUE, showWarnings = FALSE)
output_dir <- file.path(output_parent, "chapter6")

if (file.exists(output_dir) && !dir.exists(output_dir)) {
  stop("The intended output path exists but is not a directory: ", output_dir)
}
if (dir.exists(output_dir)) {
  unlink(output_dir, recursive = TRUE, force = TRUE)
}
if (dir.exists(output_dir) || !dir.create(output_dir, recursive = TRUE)) {
  stop("Could not reset the output directory: ", output_dir)
}

audit_log <- character()
note <- function(...) {
  line <- paste0(...)
  audit_log <<- c(audit_log, line)
  cat(line, "\n", sep = "")
}
save_audit <- function() {
  writeLines(
    enc2utf8(audit_log),
    file.path(output_dir, "Chapter_6_audit_log.txt"),
    useBytes = TRUE
  )
}

note("Chapter 6 regression audit - ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
note("Source files are read only; GGGI values in existing H2 files are preserved.")
note("Master: ", master_file)
note("H2 main: ", h2_main_file)
note("H2 80%: ", h2_80_file)

# ------------------------------------------------------------------------------
# 2. Read only the required master columns
# ------------------------------------------------------------------------------

master_header <- fread(master_file, nrows = 0L)
all_master_names <- names(master_header)

core_genres <- paste0(
  "genre_",
  c("Action", "Adventure", "Simulation", "Sports", "Racing", "RPG", "Strategy", "Other")
)
language_flags <- grep("^lang_", all_master_names, value = TRUE)
country_share_cols <- grep("^pct_", all_master_names, value = TRUE)

if (!length(language_flags)) stop("No lang_* columns found in the corrected master.")
if (!length(country_share_cols)) stop("No pct_* country-share columns found.")

master_required <- unique(c(
  "appid", "title", "is_non_game_product", "has_tag_data",
  "tag_female_protagonist", "revenue", "has_positive_revenue",
  "is_free", "is_early_access", "release_year", core_genres,
  "price_current", "language_count", "has_language_list",
  "has_country_data", language_flags, country_share_cols
))

missing_master_required <- setdiff(master_required, all_master_names)
if (length(missing_master_required)) {
  note("Missing master columns: ", paste(missing_master_required, collapse = ", "))
  save_audit()
  stop("The corrected master is missing required columns.")
}

master <- fread(
  master_file,
  select = master_required,
  colClasses = list(character = "appid"),
  na.strings = c("", "NA"),
  showProgress = TRUE
)

if (anyDuplicated(master$appid) || anyNA(master$appid)) {
  stop("Master appid must be nonmissing and unique.")
}

# Defensive type conversion. Unexpected values stop the run.
as_binary <- function(x, label) {
  s <- tolower(trimws(as.character(x)))
  missing <- is.na(x) | s %in% c("", "na", "nan")
  valid <- missing | s %in% c("0", "1", "true", "false", "t", "f")
  if (any(!valid)) stop("Unexpected binary coding in ", label)
  ans <- rep(NA_integer_, length(x))
  ans[!missing & s %in% c("1", "true", "t")] <- 1L
  ans[!missing & s %in% c("0", "false", "f")] <- 0L
  ans
}

binary_cols <- unique(c(
  "is_non_game_product", "has_tag_data", "tag_female_protagonist",
  "has_positive_revenue", "is_free", "is_early_access", "has_language_list",
  "has_country_data", core_genres, language_flags
))
for (nm in intersect(binary_cols, names(master))) {
  set(master, j = nm, value = as_binary(master[[nm]], nm))
}

numeric_cols <- unique(c(
  "revenue", "price_current", "release_year", "language_count", country_share_cols
))
for (nm in intersect(numeric_cols, names(master))) {
  old <- master[[nm]]
  value <- suppressWarnings(as.numeric(old))
  malformed <- !is.na(old) & is.na(value)
  if (any(malformed)) stop("Unexpected nonnumeric values in ", nm)
  set(master, j = nm, value = value)
}

if (any(master[, vapply(.SD, function(x) any(is.infinite(x)), logical(1)),
               .SDcols = numeric_cols])) {
  stop("Infinite numeric values found in the master.")
}

# Sparse early release years are pooled into a single fixed-effect category.
# In the H2 main sample, each individual year through 2008 contains fewer than
# 100 observations, whereas every year from 2009 onward contains at least 100.
make_release_period <- function(x) {
  ifelse(x <= 2008, "2008_or_earlier", as.character(as.integer(x)))
}
master[, release_period := factor(
  make_release_period(release_year),
  levels = c(
    "2008_or_earlier",
    as.character(sort(unique(release_year[!is.na(release_year) & release_year > 2008])))
  )
)]

# ------------------------------------------------------------------------------
# 3. H1 samples
# ------------------------------------------------------------------------------

h1_base <- master[
  is_non_game_product == 0L &
    has_tag_data == 1L &
    !is.na(revenue) &
    revenue > 0
]
h1_base[, log_revenue := log(revenue)]

if (nrow(h1_base) != 79891L) {
  stop("Expected 79,891 H1 observations before controls; found ", nrow(h1_base))
}
if (h1_base[, sum(tag_female_protagonist == 1L)] != 8458L) {
  stop("Unexpected Female Protagonist count in H1.")
}

h1_core_vars <- c(
  "tag_female_protagonist", "is_free", "is_early_access",
  core_genres, "release_year"
)
h1_core <- h1_base[complete.cases(h1_base[, ..h1_core_vars])]
h1_price <- h1_base[
  complete.cases(h1_base[, c(h1_core_vars, "price_current"), with = FALSE])
]
h1_paid_vars <- c(
  "tag_female_protagonist", "is_early_access", core_genres, "release_year"
)
h1_paid <- h1_base[
  is_free == 0L & complete.cases(h1_base[, ..h1_paid_vars])
]

note("H1 before controls: ", nrow(h1_base))
note("H1 core complete cases: ", nrow(h1_core))
note("H1 core plus price complete cases: ", nrow(h1_price))
note("H1 paid-only complete cases: ", nrow(h1_paid))

# ------------------------------------------------------------------------------
# 4. H2 samples and language join
# ------------------------------------------------------------------------------

read_h2 <- function(path, expected_n, label) {
  x <- fread(path, colClasses = list(character = "appid"), na.strings = c("", "NA"))
  if (nrow(x) != expected_n) {
    stop(label, ": expected ", expected_n, " rows; found ", nrow(x))
  }
  if (anyDuplicated(x$appid) || anyNA(x$appid)) {
    stop(label, ": appid must be nonmissing and unique.")
  }
  required <- c(
    "appid", "weighted_gggi_2025", "weighted_economic_participation_2025",
    "tag_female_protagonist", "is_free", "is_early_access",
    core_genres, "release_year", "language_count"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop(label, ": missing columns: ", paste(missing, collapse = ", "))

  # fread may infer TRUE/FALSE for H2 binary columns. Convert them explicitly
  # to 0/1 so coefficient names and exported tables remain consistent.
  h2_binary_cols <- c(
    "tag_female_protagonist", "is_free", "is_early_access", core_genres
  )
  for (nm in h2_binary_cols) {
    set(x, j = nm, value = as_binary(x[[nm]], paste0(label, ":", nm)))
  }

  # The current H2 pipeline may already preserve the lang_* columns created in
  # the master. Remove any existing copies before joining the canonical values
  # from the master; otherwise merge() adds .x/.y suffixes and the expected
  # lang_* names disappear.
  columns_rejoined_from_master <- intersect(
    c("release_period", language_flags),
    names(x)
  )
  if (length(columns_rejoined_from_master)) {
    x[, (columns_rejoined_from_master) := NULL]
  }

  language_data <- master[, c("appid", "release_period", language_flags), with = FALSE]
  x <- merge(x, language_data, by = "appid", all.x = TRUE, all.y = FALSE, sort = FALSE)
  if (nrow(x) != expected_n || x[, anyNA(appid)]) stop(label, ": invalid language join.")
  if (x[, any(!complete.cases(.SD)), .SDcols = language_flags]) {
    stop(label, ": language flags are missing after the appid join.")
  }
  x
}

h2_main_raw <- read_h2(h2_main_file, 23053L, "H2 main")
h2_80_raw <- read_h2(h2_80_file, 26088L, "H2 80%")

# Compare overlapping covariates with the master before modeling.
compare_with_master <- function(h2, label) {
  vars <- c(
    "appid", "tag_female_protagonist", "is_free", "is_early_access",
    core_genres, "release_year", "language_count"
  )
  m <- master[, ..vars]
  z <- merge(h2[, ..vars], m, by = "appid", suffixes = c("_h2", "_master"), sort = FALSE)
  for (nm in setdiff(vars, "appid")) {
    a <- z[[paste0(nm, "_h2")]]
    b <- z[[paste0(nm, "_master")]]
    disagree <- !(a == b | (is.na(a) & is.na(b)))
    disagree[is.na(disagree)] <- TRUE
    if (any(disagree)) stop(label, ": disagreement with master in ", nm)
  }
}
compare_with_master(h2_main_raw, "H2 main")
compare_with_master(h2_80_raw, "H2 80%")

h2_core_vars <- c(
  "weighted_gggi_2025", "weighted_economic_participation_2025",
  "tag_female_protagonist", "is_free", "is_early_access",
  core_genres, "release_year", "language_count"
)

clean_h2 <- function(x, label) {
  before <- nrow(x)
  missing_core <- !complete.cases(x[, ..h2_core_vars])
  excluded <- data.table(
    sample = label,
    appid = x$appid[missing_core],
    reason = rep("Missing core regression variable", sum(missing_core))
  )
  clean <- x[!missing_core]
  note(label, ": raw=", before, "; analytic=", nrow(clean),
       "; missing core=", sum(missing_core),
       "; early years pooled=", sum(clean$release_year <= 2008))
  list(data = clean, excluded = excluded)
}

h2_main_cleaned <- clean_h2(h2_main_raw, "H2 main")
h2_80_cleaned <- clean_h2(h2_80_raw, "H2 80%")
h2_main <- h2_main_cleaned$data
h2_80 <- h2_80_cleaned$data

if (nrow(h2_main) != 23039L) stop("Expected 23,039 H2 main analytic rows.")
if (nrow(h2_80) != 26071L) stop("Expected 26,071 H2 80% analytic rows.")

h2_exclusions <- rbindlist(
  list(h2_main_cleaned$excluded, h2_80_cleaned$excluded),
  use.names = TRUE
)
fwrite(h2_exclusions, file.path(output_dir, "H2_excluded_observations.csv"), na = "")

# ------------------------------------------------------------------------------
# 5. Pre-specified language-control rule
# ------------------------------------------------------------------------------
# The rule uses only predictor prevalence, before any regression is fitted:
# retain a language flag when 20%-80% of the H2 main analytic sample has value 1.
# This removes constant/nearly universal English and low-prevalence flags while
# avoiding outcome-based variable selection.

language_prevalence <- data.table(
  variable = language_flags,
  prevalence = vapply(language_flags, function(v) mean(h2_main[[v]] == 1L), numeric(1))
)
language_prevalence[, selected := prevalence >= 0.20 & prevalence <= 0.80]
selected_language_flags <- language_prevalence[selected == TRUE, variable]

if (!length(selected_language_flags)) {
  stop("The pre-specified language rule selected no variables.")
}
if ("lang_English" %in% selected_language_flags) {
  stop("English is constant in H2 and must not be selected.")
}

fwrite(
  language_prevalence,
  file.path(output_dir, "H2_language_control_selection.csv"),
  na = ""
)
note("Selected language controls (20%-80% prevalence): ",
     paste(selected_language_flags, collapse = ", "))

# ------------------------------------------------------------------------------
# 6. Formula construction
# ------------------------------------------------------------------------------

rhs_core <- c(
  "tag_female_protagonist", "is_free", "is_early_access",
  core_genres, "factor(release_period)"
)
rhs_paid <- c(
  "tag_female_protagonist", "is_early_access",
  core_genres, "factor(release_period)"
)
rhs_language <- c(rhs_core, "language_count", selected_language_flags)

make_formula <- function(outcome, rhs) {
  as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

# ------------------------------------------------------------------------------
# 7. Safe OLS + HC3 helpers
# ------------------------------------------------------------------------------

fit_hc3 <- function(model_id, formula, data, family, description) {
  model <- lm(formula, data = data, na.action = na.fail, model = TRUE, x = TRUE)
  if (anyNA(coef(model))) {
    bad <- names(coef(model))[is.na(coef(model))]
    stop(model_id, ": aliased coefficients: ", paste(bad, collapse = ", "))
  }
  leverage <- hatvalues(model)
  if (any(!is.finite(leverage)) || any(leverage >= 1 - 1e-10)) {
    bad_rows <- which(!is.finite(leverage) | leverage >= 1 - 1e-10)
    # na.action = na.fail guarantees that lm used every row in its original
    # order, so these positions map directly back to appid.
    bad_ids <- data$appid[bad_rows]
    stop(
      model_id, ": HC3 is undefined because leverage is 1/nonfinite. appid: ",
      paste(bad_ids, collapse = ", ")
    )
  }
  vc <- sandwich::vcovHC(model, type = "HC3")
  if (any(!is.finite(vc))) stop(model_id, ": nonfinite HC3 covariance matrix.")
  test <- lmtest::coeftest(model, vcov. = vc)
  critical <- qnorm(0.975)
  coefficients <- data.table(
    model_id = model_id,
    family = family,
    description = description,
    term = rownames(test),
    estimate = unname(test[, 1]),
    std_error_hc3 = unname(test[, 2]),
    statistic = unname(test[, 3]),
    p_value = unname(test[, 4])
  )
  coefficients[, `:=`(
    conf_low_95 = estimate - critical * std_error_hc3,
    conf_high_95 = estimate + critical * std_error_hc3
  )]
  fit <- summary(model)
  diagnostics <- data.table(
    model_id = model_id,
    family = family,
    description = description,
    outcome = all.vars(formula)[1L],
    n = nobs(model),
    r_squared = unname(fit$r.squared),
    adjusted_r_squared = unname(fit$adj.r.squared),
    residual_df = df.residual(model),
    max_leverage = max(leverage),
    aic = AIC(model),
    bic = BIC(model),
    formula = paste(deparse(formula), collapse = " ")
  )
  list(model = model, coefficients = coefficients, diagnostics = diagnostics)
}

# ------------------------------------------------------------------------------
# 8. Fit H1 models
# ------------------------------------------------------------------------------

results <- list()

results$H1_M1 <- fit_hc3(
  "H1_M1", log_revenue ~ tag_female_protagonist, h1_base,
  "H1", "Female Protagonist only"
)
results$H1_M2 <- fit_hc3(
  "H1_M2",
  log_revenue ~ tag_female_protagonist + is_free + is_early_access,
  h1_base,
  "H1", "Add Free to Play and Early Access"
)
results$H1_M3 <- fit_hc3(
  "H1_M3", make_formula("log_revenue", rhs_core), h1_core,
  "H1", "Main H1: game characteristics, genre flags and release-period effects"
)
results$H1_M4 <- fit_hc3(
  "H1_M4", make_formula("log_revenue", c(rhs_core, "price_current")), h1_price,
  "H1", "Price sensitivity: main H1 plus current price"
)
results$H1_M5 <- fit_hc3(
  "H1_M5", make_formula("log_revenue", rhs_paid), h1_paid,
  "H1", "Paid-only robustness; is_free omitted because it is constant"
)

# ------------------------------------------------------------------------------
# 9. Fit H2 models
# ------------------------------------------------------------------------------
# Models M1-M4 use the same H2-main analytic sample so coefficient changes are
# not caused by changing observations. The 80% model uses its own audited sample.

results$H2_M1 <- fit_hc3(
  "H2_M1", weighted_gggi_2025 ~ tag_female_protagonist, h2_main,
  "H2", "Weighted GGGI: Female Protagonist only; common H2-main sample"
)
results$H2_M2 <- fit_hc3(
  "H2_M2",
  weighted_gggi_2025 ~ tag_female_protagonist + is_free + is_early_access,
  h2_main,
  "H2", "Weighted GGGI: add Free to Play and Early Access"
)
results$H2_M3 <- fit_hc3(
  "H2_M3", make_formula("weighted_gggi_2025", rhs_core), h2_main,
  "H2", "Main H2: game characteristics, genre flags and release-period effects"
)
results$H2_M4 <- fit_hc3(
  "H2_M4", make_formula("weighted_gggi_2025", rhs_language), h2_main,
  "H2", "Language extension selected by pre-specified prevalence rule"
)
results$H2_M5 <- fit_hc3(
  "H2_M5", make_formula("weighted_gggi_2025", rhs_core), h2_80,
  "H2", "Coverage robustness: at least 80% official GGGI coverage"
)
results$H2_M6 <- fit_hc3(
  "H2_M6", make_formula("weighted_economic_participation_2025", rhs_core), h2_main,
  "H2 complementary", "Economic Participation and Opportunity: main specification"
)
results$H2_M7 <- fit_hc3(
  "H2_M7", make_formula("weighted_economic_participation_2025", rhs_language), h2_main,
  "H2 complementary", "Economic Participation and Opportunity: language extension"
)
results$H2_M8 <- fit_hc3(
  "H2_M8", make_formula("weighted_economic_participation_2025", rhs_core), h2_80,
  "H2 complementary", "Economic Participation and Opportunity: 80% coverage robustness"
)

# ------------------------------------------------------------------------------
# 10. Separate Russia sensitivity analysis
# ------------------------------------------------------------------------------
# Russia's last WEF profile before its absence from GGGI 2025 is the 2021
# profile. Overall score: 0.708, converted here to 70.8.
# Exact location in the supplied PDF: Table 1.1, report/PDF page 10,
# row 81 (Russian Federation).
# Official report: https://www.weforum.org/publications/global-gender-gap-report-2021/
# This is a cross-year imputation and therefore remains a sensitivity analysis.

gggi_lookup <- fread(gggi_lookup_file, na.strings = c("", "NA"))
required_lookup <- c("code", "gggi_2025")
if (length(setdiff(required_lookup, names(gggi_lookup)))) {
  stop("GGGI lookup must contain code and gggi_2025.")
}
gggi_lookup[, score_russia_sensitivity := as.numeric(gggi_2025)]
gggi_lookup[code == "RU", score_russia_sensitivity := 70.8]
gggi_lookup[code %in% c("TW", "HK"), score_russia_sensitivity := NA_real_]

pct_codes <- sub("^pct_", "", country_share_cols)
if (!setequal(pct_codes, gggi_lookup$code)) {
  stop("Country-share columns do not match the GGGI lookup codes.")
}

russia_candidate <- master[
  is_non_game_product == 0L &
    has_tag_data == 1L &
    has_country_data == 1L
]
if (nrow(russia_candidate) != 33766L) {
  stop("Expected 33,766 H2 candidates for Russia sensitivity.")
}

country_long <- melt(
  russia_candidate,
  id.vars = setdiff(names(russia_candidate), country_share_cols),
  measure.vars = country_share_cols,
  variable.name = "pct_variable",
  value.name = "player_share",
  variable.factor = FALSE
)
country_long[, code := sub("^pct_", "", pct_variable)]
country_long <- country_long[!is.na(player_share) & player_share > 0]
country_long <- merge(
  country_long,
  gggi_lookup[, .(code, score_russia_sensitivity)],
  by = "code",
  all.x = TRUE,
  sort = FALSE
)

russia_index <- country_long[, .(
  reported_positive_share = sum(player_share),
  covered_share = sum(player_share[!is.na(score_russia_sensitivity)]),
  any_uncovered = any(is.na(score_russia_sensitivity)),
  weighted_gggi_russia_sensitivity = if (
    any(is.na(score_russia_sensitivity)) || sum(player_share) <= 0
  ) {
    NA_real_
  } else {
    weighted.mean(score_russia_sensitivity, w = player_share)
  }
), by = appid]

russia_sample <- merge(
  russia_candidate,
  russia_index,
  by = "appid",
  all.x = TRUE,
  sort = FALSE
)
russia_sample <- russia_sample[
  !is.na(weighted_gggi_russia_sensitivity) &
    complete.cases(russia_sample[, c(
      "tag_female_protagonist", "is_free", "is_early_access",
      core_genres, "release_year"
    ), with = FALSE])
]

# This is a newly constructed sensitivity sample, so its size is audited rather
# than hard-coded. Its exact N may differ slightly across equivalent corrected
# master exports. Validate the substantive inclusion rules instead.
if (nrow(russia_sample) <= nrow(h2_main) || nrow(russia_sample) > nrow(russia_candidate)) {
  stop("Russia sensitivity sample size is outside the valid bounds.")
}
if (russia_sample[, anyNA(weighted_gggi_russia_sensitivity)]) {
  stop("Russia sensitivity outcome contains missing values.")
}
if (russia_sample[, any(!complete.cases(.SD)), .SDcols = c(
  "tag_female_protagonist", "is_free", "is_early_access",
  core_genres, "release_period"
)]) {
  stop("Russia sensitivity sample contains missing regression controls.")
}
if (russia_sample[, any(pct_TW > 0, na.rm = TRUE) | any(pct_HK > 0, na.rm = TRUE)]) {
  stop("Russia sensitivity sample incorrectly includes Taiwan or Hong Kong exposure.")
}

results$H2_M9 <- fit_hc3(
  "H2_M9",
  make_formula("weighted_gggi_russia_sensitivity", rhs_core),
  russia_sample,
  "H2 sensitivity",
  "Russia sensitivity: 2021 WEF score 70.8; Taiwan and Hong Kong remain uncovered"
)
note("Russia-sensitivity analytic sample: ", nrow(russia_sample))
note("Russia source: WEF GGGR 2021, Table 1.1, report/PDF page 10, score 0.708.")

russia_method <- data.table(
  item = c(
    "Russia score", "Score year", "Score scale", "Taiwan", "Hong Kong",
    "Official source", "Exact source location", "Interpretation"
  ),
  value = c(
    "70.8", "2021", "0-100", "Remains uncovered", "Remains uncovered",
    "World Economic Forum, Global Gender Gap Report 2021",
    "Table 1.1, report/PDF page 10, Russian Federation row (rank 81)",
    "Separate cross-year sensitivity analysis; not part of the 2025 main model"
  )
)
fwrite(russia_method, file.path(output_dir, "H2_Russia_sensitivity_method.csv"), na = "")

# ------------------------------------------------------------------------------
# 11. Export complete results and focused Chapter 6 summaries
# ------------------------------------------------------------------------------

all_coefficients <- rbindlist(
  lapply(results, function(x) x$coefficients),
  use.names = TRUE,
  fill = TRUE
)
all_diagnostics <- rbindlist(
  lapply(results, function(x) x$diagnostics),
  use.names = TRUE,
  fill = TRUE
)

fwrite(
  all_coefficients,
  file.path(output_dir, "Chapter_6_all_coefficients_HC3.csv"),
  na = ""
)
fwrite(
  all_diagnostics,
  file.path(output_dir, "Chapter_6_model_diagnostics.csv"),
  na = ""
)

female_results <- all_coefficients[term == "tag_female_protagonist"]
female_results[, interpretation_scale := fifelse(
  family == "H1",
  "Percent difference in estimated revenue",
  "Points on the outcome's 0-100 scale"
)]
female_results[, interpreted_effect := fifelse(
  family == "H1",
  100 * (exp(estimate) - 1),
  estimate
)]
female_results[, interpreted_ci_low_95 := fifelse(
  family == "H1",
  100 * (exp(conf_low_95) - 1),
  conf_low_95
)]
female_results[, interpreted_ci_high_95 := fifelse(
  family == "H1",
  100 * (exp(conf_high_95) - 1),
  conf_high_95
)]
female_results <- merge(
  female_results,
  all_diagnostics[, .(
    model_id, n, r_squared, adjusted_r_squared, max_leverage, formula
  )],
  by = "model_id",
  all.x = TRUE,
  sort = FALSE
)

fwrite(
  female_results,
  file.path(output_dir, "Chapter_6_Female_Protagonist_results.csv"),
  na = ""
)

sample_audit <- data.table(
  sample = c(
    "H1 before controls", "H1 core", "H1 core plus price", "H1 paid only",
    "H2 main raw", "H2 main analytic", "H2 80% raw", "H2 80% analytic",
    "H2 Russia sensitivity analytic"
  ),
  n = c(
    nrow(h1_base), nrow(h1_core), nrow(h1_price), nrow(h1_paid),
    nrow(h2_main_raw), nrow(h2_main), nrow(h2_80_raw), nrow(h2_80),
    nrow(russia_sample)
  )
)
fwrite(sample_audit, file.path(output_dir, "Chapter_6_sample_audit.csv"), na = "")

# Wide coefficient table for quick inspection. The complete long table remains
# the authoritative export because it retains exact standard errors and CIs.
display_terms <- unique(c(
  "tag_female_protagonist", "is_free", "is_early_access",
  core_genres, "price_current", "language_count", selected_language_flags
))
focused <- all_coefficients[term %in% display_terms]
focused[, estimate_se := sprintf("%.4f (%.4f)", estimate, std_error_hc3)]
wide_table <- dcast(
  focused,
  term ~ model_id,
  value.var = "estimate_se",
  fill = ""
)
fwrite(wide_table, file.path(output_dir, "Chapter_6_focused_regression_table.csv"), na = "")

# ------------------------------------------------------------------------------
# 12. Diagnostic econometric tables (HC3)
# ------------------------------------------------------------------------------
# These tables deliberately suppress the individual genre, language and
# release-period coefficients. Their inclusion is reported in control rows.
# The complete coefficient file remains the authoritative detailed output.

significance_stars <- function(p) {
  ifelse(
    p < 0.001, "***",
    ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
  )
}

format_econometric_cell <- function(mid, trm) {
  row <- all_coefficients[model_id == mid & term == trm]
  if (!nrow(row)) return("")
  if (nrow(row) != 1L) stop("Duplicate coefficient in table: ", mid, " / ", trm)
  paste0(
    sprintf("%.3f", row$estimate),
    significance_stars(row$p_value),
    "\n(", sprintf("%.3f", row$std_error_hc3), ")"
  )
}

make_econometric_table <- function(model_ids, term_labels, control_rows) {
  out <- data.frame(
    Statistic = unname(term_labels),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  terms <- names(term_labels)
  for (mid in model_ids) {
    out[[mid]] <- vapply(
      terms,
      function(trm) format_econometric_cell(mid, trm),
      character(1)
    )
  }

  for (label in names(control_rows)) {
    values <- control_rows[[label]]
    if (length(values) != length(model_ids)) {
      stop("Invalid control-row length for ", label)
    }
    new_row <- as.data.frame(
      as.list(c(Statistic = label, setNames(values, model_ids))),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    out <- rbind(out, new_row)
  }

  diag_rows <- all_diagnostics[match(model_ids, model_id)]
  if (anyNA(diag_rows$model_id)) stop("Missing model diagnostics for table.")

  n_row <- as.data.frame(
    as.list(c(
      Statistic = "Observations",
      setNames(format(diag_rows$n, scientific = FALSE, trim = TRUE), model_ids)
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  r2_row <- as.data.frame(
    as.list(c(
      Statistic = "Adjusted R-squared",
      setNames(sprintf("%.3f", diag_rows$adjusted_r_squared), model_ids)
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  rbind(out, n_row, r2_row)
}

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

econometric_table_html <- function(table_data, caption, table_number) {
  header <- paste0(
    "<tr><th>Variable</th>",
    paste0(
      "<th>(", seq_len(ncol(table_data) - 1L), ")<br><span class='model-id'>",
      html_escape(names(table_data)[-1L]), "</span></th>",
      collapse = ""
    ),
    "</tr>"
  )

  body_rows <- vapply(seq_len(nrow(table_data)), function(i) {
    label <- html_escape(table_data$Statistic[[i]])
    cells <- vapply(table_data[i, -1L, drop = FALSE], function(value) {
      value <- as.character(value)
      if (grepl("\n", value, fixed = TRUE)) {
        parts <- strsplit(value, "\n", fixed = TRUE)[[1L]]
        paste0(
          "<td class='numeric'>", html_escape(parts[[1L]]),
          "<br><span class='se'>", html_escape(parts[[2L]]), "</span></td>"
        )
      } else {
        paste0("<td class='numeric'>", html_escape(value), "</td>")
      }
    }, character(1))
    paste0("<tr><td class='label'>", label, "</td>", paste0(cells, collapse = ""), "</tr>")
  }, character(1))

  paste0(
    "<section><h2>Table ", table_number, ". ", html_escape(caption), "</h2>",
    "<table>", header, paste0(body_rows, collapse = "\n"), "</table></section>"
  )
}

h1_table <- make_econometric_table(
  model_ids = paste0("H1_M", 1:5),
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access",
    price_current = "Current price"
  ),
  control_rows = list(
    "Genre controls" = c("No", "No", "Yes", "Yes", "Yes"),
    "Release-period fixed effects" = c("No", "No", "Yes", "Yes", "Yes"),
    "Paid-only sample" = c("No", "No", "No", "No", "Yes")
  )
)

h2_gggi_table <- make_econometric_table(
  model_ids = c(paste0("H2_M", 1:5), "H2_M9"),
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access",
    language_count = "Number of supported languages"
  ),
  control_rows = list(
    "Genre controls" = c("No", "No", "Yes", "Yes", "Yes", "Yes"),
    "Release-period fixed effects" = c("No", "No", "Yes", "Yes", "Yes", "Yes"),
    "Language indicators" = c("No", "No", "No", "Yes", "No", "No"),
    "Country coverage" = c("100%", "100%", "100%", "100%", ">=80%", "Russia sensitivity")
  )
)

h2_economic_table <- make_econometric_table(
  model_ids = paste0("H2_M", 6:8),
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access",
    language_count = "Number of supported languages"
  ),
  control_rows = list(
    "Genre controls" = c("Yes", "Yes", "Yes"),
    "Release-period fixed effects" = c("Yes", "Yes", "Yes"),
    "Language indicators" = c("No", "Yes", "No"),
    "Country coverage" = c("100%", "100%", ">=80%")
  )
)

fwrite(h1_table, file.path(output_dir, "Diagnostic_H1_models.csv"), na = "")
fwrite(h2_gggi_table, file.path(output_dir, "Diagnostic_H2_GGGI_models.csv"), na = "")
fwrite(
  h2_economic_table,
  file.path(output_dir, "Diagnostic_H2_Economic_models.csv"),
  na = ""
)

html_document <- paste0(
  "<!doctype html><html><head><meta charset='utf-8'>",
  "<title>Chapter 6 Econometric Tables</title>",
  "<style>",
  "body{font-family:Arial,sans-serif;color:#111;max-width:1250px;margin:32px auto;}",
  "h1{font-size:22px;margin-bottom:28px;}h2{font-size:17px;margin:26px 0 8px;}",
  "section{page-break-after:always;}table{border-collapse:collapse;width:100%;font-size:13px;}",
  "th{border-top:2px solid #111;border-bottom:1px solid #111;padding:7px;text-align:center;}",
  "td{padding:6px 8px;border-bottom:1px solid #ddd;}td.label{text-align:left;}",
  "td.numeric{text-align:center;white-space:nowrap;}.se{color:#444;}.model-id{font-weight:normal;font-size:11px;}",
  ".notes{font-size:12px;line-height:1.45;margin-top:18px;border-top:2px solid #111;padding-top:8px;}",
  "</style></head><body><h1>Chapter 6 - Econometric Results</h1>",
  econometric_table_html(h1_table, "H1: Estimated revenue", "6.1"),
  econometric_table_html(h2_gggi_table, "H2: Weighted Global Gender Gap Index", "6.2"),
  econometric_table_html(
    h2_economic_table,
    "H2 complementary analysis: Economic Participation and Opportunity",
    "6.3"
  ),
  "<div class='notes'><strong>Notes:</strong> OLS estimates. HC3 robust standard errors ",
  "in parentheses. * p&lt;0.05, ** p&lt;0.01, *** p&lt;0.001. Release years through 2008 ",
  "are pooled into one fixed-effect category. Individual genre, language and release-period ",
  "coefficients are available in Chapter_6_all_coefficients_HC3.csv. H1 coefficients are ",
  "estimated on log revenue; percentage interpretations should use 100(exp(beta)-1). ",
  "H2 outcomes are measured on a 0-100 scale.</div></body></html>"
)
writeLines(
  html_document,
  file.path(output_dir, "Chapter_6_diagnostic_tables.html"),
  useBytes = TRUE
)

note("Diagnostic econometric tables created in HTML and CSV formats.")

manifest <- data.table(
  file = c(
    "Chapter_6_Female_Protagonist_results.csv",
    "Chapter_6_all_coefficients_HC3.csv",
    "Chapter_6_model_diagnostics.csv",
    "Chapter_6_focused_regression_table.csv",
    "Chapter_6_sample_audit.csv",
    "H2_language_control_selection.csv",
    "H2_excluded_observations.csv",
    "H2_Russia_sensitivity_method.csv",
    "Chapter_6_diagnostic_tables.html",
    "Diagnostic_H1_models.csv",
    "Diagnostic_H2_GGGI_models.csv",
    "Diagnostic_H2_Economic_models.csv",
    "Chapter_6_audit_log.txt"
  ),
  purpose = c(
    "Main coefficients and interpretations for writing Chapter 6",
    "All coefficients, HC3 standard errors, p-values and 95% confidence intervals",
    "N, R-squared, adjusted R-squared, leverage and model formulas",
    "Compact coefficient table for inspection",
    "Sample sizes used by each analysis family",
    "Pre-outcome language-control selection rule and prevalence",
    "H2 rows excluded for missing core regression variables",
    "Documented assumptions for the separate Russia sensitivity",
    "Diagnostic HTML tables for model verification",
    "Diagnostic H1 model table in CSV format",
    "Diagnostic H2 Weighted GGGI model table in CSV format",
    "Diagnostic H2 Economic Participation model table in CSV format",
    "Run checks, paths and key decisions"
  )
)
fwrite(manifest, file.path(output_dir, "Chapter_6_output_manifest.csv"), na = "")

# ------------------------------------------------------------------------------
# 13. Final checks and console summary
# ------------------------------------------------------------------------------

expected_models <- c(
  paste0("H1_M", 1:5),
  paste0("H2_M", 1:9)
)
if (!setequal(all_diagnostics$model_id, expected_models)) {
  stop("Not all planned models were exported.")
}
if (female_results[, any(!is.finite(estimate) | !is.finite(std_error_hc3))]) {
  stop("Nonfinite Female Protagonist result.")
}
if (all_diagnostics[, any(max_leverage >= 1 - 1e-10)]) {
  stop("At least one exported model has leverage too close to 1 for HC3.")
}

note("Models completed: ", paste(all_diagnostics$model_id, collapse = ", "))
note("All standard errors: HC3.")
note("All existing H2 GGGI values preserved; language flags joined only by appid.")
note("Output directory: ", normalizePath(output_dir, winslash = "/"))
save_audit()

cat("\n========================================\n")
cat("FEMALE PROTAGONIST RESULTS\n")
cat("========================================\n")
print(
  female_results[, .(
    model_id, description, estimate, std_error_hc3, p_value,
    conf_low_95, conf_high_95, interpreted_effect,
    interpreted_ci_low_95, interpreted_ci_high_95, n,
    r_squared, adjusted_r_squared
  )]
)
cat("\nFiles saved in:\n", normalizePath(output_dir, winslash = "/"), "\n", sep = "")
