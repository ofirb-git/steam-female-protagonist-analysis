# H2 - חיבור נתוני המדינות ל-GGGI 2025 ובניית מדגמי הניתוח
# מקור ציוני GGGI: World Economic Forum, Global Gender Gap Report 2025
# הציונים נשמרים כאן בסולם 0-100 לצורך הצגה ופרשנות.
# רוסיה (RU), טאיוואן (TW) והונג קונג (HK) אינן מקבלות ציון רשמי ב-GGGI 2025
# ולכן אינן נכללות בחישוב המרכזי.
#
# שים לב:
# - המדגם המרכזי דורש כיסוי רשמי מלא לכל המדינות המדווחות בעלות אחוז חיובי.
# - בדיקת החוסן של 80% כוללת משחקים שבהם המדינות המכוסות מהוות לפחות 80%
#   מסך אחוזי השחקנים במדינות המדווחות, ומנרמלת מחדש את המשקלים.
# - בדיקת הרגישות הנפרדת לרוסיה אינה מבוצעת בסקריפט זה.

library(data.table)

if (!dir.exists("R") || !dir.exists("data")) {
  stop("Run this script from the repository root (the folder containing R/ and data/).")
}

input_file <- "data/processed/H2_input_compact.csv"
lookup_file <- "data/reference/GGGI_2025_country_lookup.csv"
output_dir <- "data/processed"

if (!file.exists(lookup_file)) {
  stop("Missing reference table: ", lookup_file)
}

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------------------
# 1. טעינת מדגם המועמדים ל-H2
# ------------------------------------------------------------------------------

dt <- fread(input_file)

cat("\n========================================\n")
cat("H2 GGGI 2025 BUILD\n")
cat("========================================\n")
cat("Candidate H2 observations:", nrow(dt), "\n")

if (nrow(dt) != 33766) {
  warning(
    paste(
      "Expected 33,766 candidate H2 observations, but obtained",
      nrow(dt)
    )
  )
}

# ------------------------------------------------------------------------------
# 2. טבלת התאמה בין קודי המדינות ב-Gamalytic לבין GGGI 2025
# ------------------------------------------------------------------------------

gggi_lookup <- fread(lookup_file, na.strings = c("", "NA"))
required_lookup_columns <- c("code", "country", "gggi_2025", "official_2025")
if (length(setdiff(required_lookup_columns, names(gggi_lookup)))) {
  stop("The GGGI reference table is missing required columns.")
}
if (anyDuplicated(gggi_lookup$code)) {
  stop("The GGGI reference table contains duplicate country codes.")
}

# ------------------------------------------------------------------------------
# 3. בדיקת התאמה בין עמודות pct_ לבין טבלת המדינות
# ------------------------------------------------------------------------------

pct_cols <- grep(
  "^pct_",
  names(dt),
  value = TRUE
)

data_codes <- sub("^pct_", "", pct_cols)

missing_from_lookup <- setdiff(
  data_codes,
  gggi_lookup$code
)

extra_in_lookup <- setdiff(
  gggi_lookup$code,
  data_codes
)

if (length(missing_from_lookup) > 0) {
  stop(
    paste(
      "Country codes found in the data but missing from lookup:",
      paste(missing_from_lookup, collapse = ", ")
    )
  )
}

if (length(extra_in_lookup) > 0) {
  warning(
    paste(
      "Country codes in lookup but not found in the data:",
      paste(extra_in_lookup, collapse = ", ")
    )
  )
}

# המרה בטוחה למספרים
for (col in pct_cols) {
  set(
    dt,
    j = col,
    value = as.numeric(dt[[col]])
  )
  set(
    dt,
    i = which(is.na(dt[[col]])),
    j = col,
    value = 0
  )
}

# ------------------------------------------------------------------------------
# 4. מעבר לפורמט ארוך לצורך חישוב ואימות
# ------------------------------------------------------------------------------

id_cols <- setdiff(
  names(dt),
  pct_cols
)

country_long <- melt(
  dt,
  id.vars = id_cols,
  measure.vars = pct_cols,
  variable.name = "country_variable",
  value.name = "player_pct"
)

country_long[
  ,
  code := sub("^pct_", "", country_variable)
]

country_long <- merge(
  country_long,
  gggi_lookup,
  by = "code",
  all.x = TRUE,
  sort = FALSE
)

# נשמור רק מדינות עם אחוז שחקנים חיובי
country_positive <- country_long[
  player_pct > 0
]

# ------------------------------------------------------------------------------
# 5. Audit של המדינות בפועל
# ------------------------------------------------------------------------------

country_audit <- country_positive[
  ,
  .(
    games_with_positive_share = uniqueN(appid),
    total_reported_player_pct = sum(player_pct, na.rm = TRUE),
    mean_player_pct_when_reported = mean(player_pct, na.rm = TRUE)
  ),
  by = .(
    code,
    country,
    gggi_2025,
    official_2025
  )
][
  order(-games_with_positive_share)
]

cat("\n========================================\n")
cat("COUNTRIES WITHOUT OFFICIAL GGGI 2025\n")
cat("========================================\n")

print(
  country_audit[
    official_2025 == FALSE
  ]
)

# ------------------------------------------------------------------------------
# 6. חישוב Weighted GGGI לכל משחק
# ------------------------------------------------------------------------------

game_gggi <- country_positive[
  ,
  .(
    reported_country_sum_recalc = sum(
      player_pct,
      na.rm = TRUE
    ),
    covered_country_sum = sum(
      player_pct[official_2025 == TRUE],
      na.rm = TRUE
    ),
    uncovered_country_sum = sum(
      player_pct[official_2025 == FALSE],
      na.rm = TRUE
    ),
    n_positive_reported_countries = .N,
    n_covered_countries = sum(
      official_2025 == TRUE
    ),
    n_uncovered_countries = sum(
      official_2025 == FALSE
    ),
    weighted_gggi_2025 = {
      w <- player_pct[
        official_2025 == TRUE
      ]
      s <- gggi_2025[
        official_2025 == TRUE
      ]

      if (length(w) == 0 || sum(w) <= 0) {
        NA_real_
      } else {
        sum(w * s) / sum(w)
      }
    }
  ),
  by = appid
]

game_gggi[
  ,
  official_coverage_share :=
    fifelse(
      reported_country_sum_recalc > 0,
      covered_country_sum /
        reported_country_sum_recalc,
      NA_real_
    )
]

# חיבור חזרה לנתוני המשחקים
dt_h2 <- merge(
  dt,
  game_gggi,
  by = "appid",
  all.x = TRUE,
  sort = FALSE
)

# ------------------------------------------------------------------------------
# 7. מדגם H2 המרכזי - 100% כיסוי רשמי
# ------------------------------------------------------------------------------

h2_main <- dt_h2[
  n_uncovered_countries == 0 &
    covered_country_sum > 0 &
    !is.na(weighted_gggi_2025)
]

cat("\n========================================\n")
cat("H2 MAIN SAMPLE - FULL OFFICIAL COVERAGE\n")
cat("========================================\n")

cat(
  "N H2 main:",
  nrow(h2_main),
  "\n"
)

cat(
  "Female Protagonist:",
  sum(
    h2_main$tag_female_protagonist == 1,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "No Female Protagonist tag:",
  sum(
    h2_main$tag_female_protagonist == 0,
    na.rm = TRUE
  ),
  "\n"
)

if (nrow(h2_main) != 23053) {
  warning(
    paste(
      "Expected 23,053 observations in the full-coverage H2 sample, but obtained",
      nrow(h2_main)
    )
  )
}

# ------------------------------------------------------------------------------
# 8. בדיקת חוסן - לפחות 80% כיסוי רשמי
# ------------------------------------------------------------------------------

h2_80 <- dt_h2[
  official_coverage_share >= 0.80 &
    covered_country_sum > 0 &
    !is.na(weighted_gggi_2025)
]

cat("\n========================================\n")
cat("H2 ROBUSTNESS SAMPLE - >= 80% COVERAGE\n")
cat("========================================\n")

cat(
  "N H2 80%:",
  nrow(h2_80),
  "\n"
)

cat(
  "Female Protagonist:",
  sum(
    h2_80$tag_female_protagonist == 1,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "No Female Protagonist tag:",
  sum(
    h2_80$tag_female_protagonist == 0,
    na.rm = TRUE
  ),
  "\n"
)

if (nrow(h2_80) != 26088) {
  warning(
    paste(
      "Expected 26,088 observations in the >=80% coverage sample, but obtained",
      nrow(h2_80)
    )
  )
}

# ------------------------------------------------------------------------------
# 9. סטטיסטיקה תיאורית של Weighted GGGI
# ------------------------------------------------------------------------------

weighted_desc <- function(x, sample_name) {

  data.table(
    sample = sample_name,
    N = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    q1 = as.numeric(
      quantile(
        x,
        probs = 0.25,
        na.rm = TRUE
      )
    ),
    median = median(
      x,
      na.rm = TRUE
    ),
    q3 = as.numeric(
      quantile(
        x,
        probs = 0.75,
        na.rm = TRUE
      )
    ),
    max = max(
      x,
      na.rm = TRUE
    )
  )
}

summary_main_all <- weighted_desc(
  h2_main$weighted_gggi_2025,
  "H2 main - all"
)

summary_main_female <- weighted_desc(
  h2_main[
    tag_female_protagonist == 1
  ]$weighted_gggi_2025,
  "H2 main - Female Protagonist"
)

summary_main_no_female <- weighted_desc(
  h2_main[
    tag_female_protagonist == 0
  ]$weighted_gggi_2025,
  "H2 main - No Female Protagonist tag"
)

summary_80_all <- weighted_desc(
  h2_80$weighted_gggi_2025,
  "H2 robustness 80% - all"
)

summary_80_female <- weighted_desc(
  h2_80[
    tag_female_protagonist == 1
  ]$weighted_gggi_2025,
  "H2 robustness 80% - Female Protagonist"
)

summary_80_no_female <- weighted_desc(
  h2_80[
    tag_female_protagonist == 0
  ]$weighted_gggi_2025,
  "H2 robustness 80% - No Female Protagonist tag"
)

gggi_descriptives <- rbindlist(
  list(
    summary_main_all,
    summary_main_female,
    summary_main_no_female,
    summary_80_all,
    summary_80_female,
    summary_80_no_female
  )
)

cat("\n========================================\n")
cat("WEIGHTED GGGI DESCRIPTIVES\n")
cat("========================================\n")

print(gggi_descriptives)

# ------------------------------------------------------------------------------
# 10. סיכום אובדן תצפיות בגלל מדינות ללא ציון רשמי
# ------------------------------------------------------------------------------

missing_country_game_counts <- country_positive[
  official_2025 == FALSE,
  .(
    games = uniqueN(appid)
  ),
  by = .(
    code,
    country
  )
][
  order(-games)
]

games_with_any_uncovered <- dt_h2[
  n_uncovered_countries > 0,
  uniqueN(appid)
]

coverage_summary <- data.table(
  stage = c(
    "H2 candidate sample",
    "H2 main - full official coverage",
    "H2 robustness - at least 80% coverage"
  ),
  N = c(
    nrow(dt_h2),
    nrow(h2_main),
    nrow(h2_80)
  )
)

cat("\nGames with at least one uncovered country:",
    games_with_any_uncovered, "\n")

# ------------------------------------------------------------------------------
# 11. שמירת הפלטים
# ------------------------------------------------------------------------------

fwrite(
  country_audit,
  file.path(
    output_dir,
    "H2_GGGI_country_audit.csv"
  )
)

fwrite(
  coverage_summary,
  file.path(
    output_dir,
    "H2_GGGI_sample_sizes.csv"
  )
)

fwrite(
  missing_country_game_counts,
  file.path(
    output_dir,
    "H2_GGGI_missing_country_counts.csv"
  )
)

fwrite(
  gggi_descriptives,
  file.path(
    output_dir,
    "H2_GGGI_descriptives.csv"
  )
)

fwrite(
  h2_main,
  file.path(
    output_dir,
    "H2_main_GGGI_2025.csv"
  )
)

fwrite(
  h2_80,
  file.path(
    output_dir,
    "H2_robustness_80pct_GGGI_2025.csv"
  )
)

cat("\n========================================\n")
cat("FILES CREATED\n")
cat("========================================\n")

cat(
  file.path(
    output_dir,
    "H2_GGGI_country_audit.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "H2_GGGI_sample_sizes.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "H2_GGGI_missing_country_counts.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "H2_GGGI_descriptives.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "H2_main_GGGI_2025.csv"
  ),
  "\n"
)

cat(
  file.path(
    output_dir,
    "H2_robustness_80pct_GGGI_2025.csv"
  ),
  "\n"
)

cat("\nH2 GGGI build completed successfully.\n")
