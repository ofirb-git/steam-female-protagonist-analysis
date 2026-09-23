# Descriptive-analysis update for H2:
# 1) KEEP the existing Weighted GGGI exactly as already calculated.
# 2) Add only the weighted Economic Participation and Opportunity subindex.
# 3) Create the Weighted GGGI histogram from the existing H2 main sample.
#
# Inputs already created by build_H2_GGGI_2025.R:
#   data/processed/H2_main_GGGI_2025.csv
#   data/processed/H2_robustness_80pct_GGGI_2025.csv
#
# Nothing in this script recalculates or overwrites weighted_gggi_2025.

library(data.table)


rm(list = ls())

if (!dir.exists("R") || !dir.exists("data") || !dir.exists("results")) {
  stop("Run this script from the repository root (the folder containing R/, data/ and results/).")
}

output_dir <- "data/processed"
lookup_file <- "data/reference/WEF_2025_Economic_Participation_lookup.csv"
figure_dir <- "results/figures"
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(lookup_file)) {
  stop("Missing reference table: ", lookup_file)
}

main_file <- file.path(
  output_dir,
  "H2_main_GGGI_2025.csv"
)

robust_file <- file.path(
  output_dir,
  "H2_robustness_80pct_GGGI_2025.csv"
)

if (!file.exists(main_file)) {
  stop(
    paste(
      "Missing file:",
      main_file,
      "\nRun the already-validated build_H2_GGGI_2025.R first."
    )
  )
}

if (!file.exists(robust_file)) {
  stop(
    paste(
      "Missing file:",
      robust_file,
      "\nRun the already-validated build_H2_GGGI_2025.R first."
    )
  )
}

# ------------------------------------------------------------------------------
# 1. Load the ALREADY-BUILT H2 samples
# ------------------------------------------------------------------------------

h2_main <- fread(main_file)
h2_80 <- fread(robust_file)

required_existing <- c(
  "appid",
  "tag_female_protagonist",
  "weighted_gggi_2025"
)

for (nm in required_existing) {
  if (!nm %in% names(h2_main)) {
    stop(
      paste(
        "Required column missing from H2 main file:",
        nm
      )
    )
  }

  if (!nm %in% names(h2_80)) {
    stop(
      paste(
        "Required column missing from H2 80% file:",
        nm
      )
    )
  }
}

cat("\n============================================================\n")
cat("EXISTING H2 SAMPLES LOADED - WEIGHTED GGGI IS NOT REBUILT\n")
cat("============================================================\n")
cat("H2 main N:", nrow(h2_main), "\n")
cat("H2 80% N:", nrow(h2_80), "\n")

if (nrow(h2_main) != 23053) {
  warning(
    paste(
      "Expected H2 main N = 23,053 but found",
      nrow(h2_main)
    )
  )
}

if (nrow(h2_80) != 26088) {
  warning(
    paste(
      "Expected H2 80% N = 26,088 but found",
      nrow(h2_80)
    )
  )
}

# Preserve a simple audit of the EXISTING Weighted GGGI.
# This is descriptive only - no recalculation.
gggi_existing_audit <- data.table(
  sample = c(
    "H2 main",
    "H2 robustness 80%"
  ),
  N = c(
    sum(!is.na(h2_main$weighted_gggi_2025)),
    sum(!is.na(h2_80$weighted_gggi_2025))
  ),
  mean = c(
    mean(
      h2_main$weighted_gggi_2025,
      na.rm = TRUE
    ),
    mean(
      h2_80$weighted_gggi_2025,
      na.rm = TRUE
    )
  ),
  sd = c(
    sd(
      h2_main$weighted_gggi_2025,
      na.rm = TRUE
    ),
    sd(
      h2_80$weighted_gggi_2025,
      na.rm = TRUE
    )
  ),
  median = c(
    median(
      h2_main$weighted_gggi_2025,
      na.rm = TRUE
    ),
    median(
      h2_80$weighted_gggi_2025,
      na.rm = TRUE
    )
  )
)

cat("\nExisting Weighted GGGI audit:\n")
print(gggi_existing_audit)

# ------------------------------------------------------------------------------
# 2. Official WEF 2025 Economic Participation and Opportunity lookup
# ------------------------------------------------------------------------------

economic_lookup <- fread(lookup_file, na.strings = c("", "NA"))
required_lookup_columns <- c(
  "code", "country", "economic_participation_2025", "official_economic_2025"
)
if (length(setdiff(required_lookup_columns, names(economic_lookup)))) {
  stop("The Economic Participation reference table is missing required columns.")
}
if (anyDuplicated(economic_lookup$code)) {
  stop("The Economic Participation reference table contains duplicate country codes.")
}

# ------------------------------------------------------------------------------
# 3. Function to add Weighted Economic Participation to an EXISTING H2 sample
# ------------------------------------------------------------------------------

add_weighted_economic <- function(data, sample_label) {

  pct_cols <- grep(
    "^pct_",
    names(data),
    value = TRUE
  )

  if (length(pct_cols) == 0) {
    stop(
      paste(
        "No pct_ country columns found in",
        sample_label
      )
    )
  }

  data_codes <- sub(
    "^pct_",
    "",
    pct_cols
  )

  missing_codes <- setdiff(
    data_codes,
    economic_lookup$code
  )

  if (length(missing_codes) > 0) {
    stop(
      paste(
        "Country codes in data but missing from economic lookup:",
        paste(
          missing_codes,
          collapse = ", "
        )
      )
    )
  }

  country_long <- melt(
    data[
      ,
      c(
        "appid",
        pct_cols
      ),
      with = FALSE
    ],
    id.vars = "appid",
    measure.vars = pct_cols,
    variable.name = "country_variable",
    value.name = "player_pct"
  )

  country_long[
    ,
    player_pct :=
      as.numeric(player_pct)
  ]

  country_long[
    is.na(player_pct),
    player_pct := 0
  ]

  country_long[
    ,
    code :=
      sub(
        "^pct_",
        "",
        country_variable
      )
  ]

  country_long <- merge(
    country_long,
    economic_lookup,
    by = "code",
    all.x = TRUE,
    sort = FALSE
  )

  country_positive <- country_long[
    player_pct > 0
  ]

  game_economic <- country_positive[
    ,
    .(
      reported_country_sum_economic =
        sum(
          player_pct,
          na.rm = TRUE
        ),

      economic_covered_country_sum =
        sum(
          player_pct[
            official_economic_2025 == TRUE
          ],
          na.rm = TRUE
        ),

      weighted_economic_participation_2025 = {
        use <-
          official_economic_2025 == TRUE

        w <- player_pct[use]
        s <-
          economic_participation_2025[use]

        if (
          length(w) == 0 ||
          sum(w, na.rm = TRUE) <= 0
        ) {
          NA_real_
        } else {
          sum(
            w * s,
            na.rm = TRUE
          ) /
            sum(
              w,
              na.rm = TRUE
            )
        }
      }
    ),
    by = appid
  ]

  game_economic[
    ,
    economic_coverage_share :=
      fifelse(
        reported_country_sum_economic > 0,
        economic_covered_country_sum /
          reported_country_sum_economic,
        NA_real_
      )
  ]

  out <- merge(
    data,
    game_economic,
    by = "appid",
    all.x = TRUE,
    sort = FALSE
  )

  out
}

# ------------------------------------------------------------------------------
# 4. Add ONLY the new subindex variable
# ------------------------------------------------------------------------------

h2_main_economic <- add_weighted_economic(
  h2_main,
  "H2 main"
)

h2_80_economic <- add_weighted_economic(
  h2_80,
  "H2 robustness 80%"
)

# Main sample should have full official coverage.
main_bad_coverage <- h2_main_economic[
  is.na(economic_coverage_share) |
    abs(
      economic_coverage_share - 1
    ) > 1e-10
]

if (nrow(main_bad_coverage) > 0) {
  warning(
    paste(
      nrow(main_bad_coverage),
      "H2 main observations do not have full Economic Participation coverage."
    )
  )
}

# 80% sample should retain at least 80% official coverage.
robust_bad_coverage <- h2_80_economic[
  is.na(economic_coverage_share) |
    economic_coverage_share < 0.80
]

if (nrow(robust_bad_coverage) > 0) {
  warning(
    paste(
      nrow(robust_bad_coverage),
      "H2 80% observations fall below 80% Economic Participation coverage."
    )
  )
}

# ------------------------------------------------------------------------------
# 5. Descriptive statistics for the NEW subindex
# ------------------------------------------------------------------------------

desc_row <- function(
    data,
    sample_name) {

  x <-
    data$weighted_economic_participation_2025

  x <- x[
    !is.na(x)
  ]

  data.table(
    sample = sample_name,
    N = length(x),
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    q1 = as.numeric(
      quantile(
        x,
        0.25
      )
    ),
    median = median(x),
    q3 = as.numeric(
      quantile(
        x,
        0.75
      )
    ),
    max = max(x)
  )
}

economic_descriptives <- rbindlist(
  list(
    desc_row(
      h2_main_economic,
      "H2 main - all"
    ),

    desc_row(
      h2_main_economic[
        tag_female_protagonist == 1
      ],
      "H2 main - Female Protagonist"
    ),

    desc_row(
      h2_main_economic[
        tag_female_protagonist == 0
      ],
      "H2 main - No Female Protagonist tag"
    ),

    desc_row(
      h2_80_economic,
      "H2 robustness 80% - all"
    ),

    desc_row(
      h2_80_economic[
        tag_female_protagonist == 1
      ],
      "H2 robustness 80% - Female Protagonist"
    ),

    desc_row(
      h2_80_economic[
        tag_female_protagonist == 0
      ],
      "H2 robustness 80% - No Female Protagonist tag"
    )
  )
)

cat("\n============================================================\n")
cat("WEIGHTED ECONOMIC PARTICIPATION - DESCRIPTIVES\n")
cat("============================================================\n")
print(economic_descriptives)

# Raw descriptive gap for the main H2 sample
economic_raw_gap <- data.table(
  female_mean =
    mean(
      h2_main_economic[
        tag_female_protagonist == 1,
        weighted_economic_participation_2025
      ],
      na.rm = TRUE
    ),

  no_female_mean =
    mean(
      h2_main_economic[
        tag_female_protagonist == 0,
        weighted_economic_participation_2025
      ],
      na.rm = TRUE
    )
)

economic_raw_gap[
  ,
  female_minus_no_female :=
    female_mean -
    no_female_mean
]

cat("\nRaw descriptive gap - main H2 sample:\n")
print(economic_raw_gap)

# ------------------------------------------------------------------------------
# 6. Histogram of EXISTING Weighted GGGI in H2 main
#    Improved Chapter 5 version:
#    - uses the existing weighted_gggi_2025 values
#    - narrower bins (0.5 points) to show the distribution more clearly
#    - cleaner academic layout
#    - mean and median lines retained
# ------------------------------------------------------------------------------

gggi_x <-
  h2_main$weighted_gggi_2025

gggi_x <-
  gggi_x[
    !is.na(gggi_x)
  ]

gggi_mean <-
  mean(gggi_x)

gggi_median <-
  median(gggi_x)

# Half-point bins show the concentration of observations more clearly
# without changing the underlying data.
hist_bin_width <- 0.5

hist_breaks <- seq(
  floor(
    min(gggi_x) /
      hist_bin_width
  ) *
    hist_bin_width,
  ceiling(
    max(gggi_x) /
      hist_bin_width
  ) *
    hist_bin_width +
    hist_bin_width,
  by = hist_bin_width
)

hist_object <- hist(
  gggi_x,
  breaks = hist_breaks,
  plot = FALSE,
  right = FALSE
)

histogram_bins <- data.table(
  bin_left =
    hist_object$breaks[
      -length(
        hist_object$breaks
      )
    ],
  bin_right =
    hist_object$breaks[-1],
  bin_mid =
    hist_object$mids,
  count =
    hist_object$counts,
  percent =
    100 *
      hist_object$counts /
      sum(
        hist_object$counts
      )
)

png(
  filename = file.path(
    figure_dir,
    "Figure_H2_weighted_GGGI_histogram.png"
  ),
  width = 2400,
  height = 1500,
  res = 240
)

# Save existing graphics settings and restore them after the plot.
old_par <- par(
  no.readonly = TRUE
)

par(
  mar = c(
    5.2,
    5.4,
    4.4,
    1.5
  ),
  mgp = c(
    3.2,
    0.9,
    0
  ),
  las = 1,
  cex.axis = 1.05,
  cex.lab = 1.15,
  cex.main = 1.25
)

hist(
  gggi_x,
  breaks = hist_breaks,
  main =
    "Distribution of Weighted GGGI - H2 Main Sample",
  xlab =
    "Weighted GGGI (0-100)",
  ylab =
    "Number of Games",
  border = NA,
  xaxt = "n",
  xlim = c(
    63,
    88.5
  )
)

axis(
  side = 1,
  at = seq(
    64,
    88,
    by = 2
  )
)

abline(
  v = gggi_mean,
  lwd = 2.2,
  lty = 2
)

abline(
  v = gggi_median,
  lwd = 2.2,
  lty = 3
)

legend(
  "topright",
  legend = c(
    paste0(
      "Mean = ",
      round(
        gggi_mean,
        1
      )
    ),
    paste0(
      "Median = ",
      round(
        gggi_median,
        1
      )
    )
  ),
  lty = c(
    2,
    3
  ),
  lwd = 2.2,
  bty = "n",
  cex = 1.05
)

par(old_par)

dev.off()

# ------------------------------------------------------------------------------
# 7. Export NEW outputs only - originals are not overwritten
# ------------------------------------------------------------------------------

fwrite(
  economic_descriptives,
  file.path(
    output_dir,
    "H2_Economic_Participation_descriptives.csv"
  )
)

fwrite(
  economic_raw_gap,
  file.path(
    output_dir,
    "H2_Economic_Participation_raw_gap.csv"
  )
)

fwrite(
  histogram_bins,
  file.path(
    output_dir,
    "H2_weighted_GGGI_histogram_bins.csv"
  )
)

# These extended samples will be useful later for the complementary H2 regression.
# The original H2 files remain untouched.
fwrite(
  h2_main_economic,
  file.path(
    output_dir,
    "H2_main_GGGI_2025_with_Economic.csv"
  )
)

fwrite(
  h2_80_economic,
  file.path(
    output_dir,
    "H2_robustness_80pct_GGGI_2025_with_Economic.csv"
  )
)

# ------------------------------------------------------------------------------
# 8. Compact console output to paste back into ChatGPT
# ------------------------------------------------------------------------------

cat("\n\n============================================================\n")
cat("COPY THIS SUMMARY BACK TO CHATGPT\n")
cat("============================================================\n")

cat("\nEXISTING WEIGHTED GGGI - H2 MAIN\n")
cat(
  "N =",
  length(gggi_x),
  "\n"
)
cat(
  "Mean =",
  mean(gggi_x),
  "\n"
)
cat(
  "SD =",
  sd(gggi_x),
  "\n"
)
cat(
  "Min =",
  min(gggi_x),
  "\n"
)
cat(
  "Q1 =",
  as.numeric(
    quantile(
      gggi_x,
      0.25
    )
  ),
  "\n"
)
cat(
  "Median =",
  median(gggi_x),
  "\n"
)
cat(
  "Q3 =",
  as.numeric(
    quantile(
      gggi_x,
      0.75
    )
  ),
  "\n"
)
cat(
  "Max =",
  max(gggi_x),
  "\n"
)

cat("\nNEW WEIGHTED ECONOMIC PARTICIPATION\n")
print(economic_descriptives)

cat("\nRAW ECONOMIC GAP - MAIN H2\n")
print(economic_raw_gap)

cat("\nSAMPLE CHECK\n")
cat(
  "H2 main N =",
  nrow(h2_main_economic),
  "\n"
)
cat(
  "H2 main Female =",
  sum(
    h2_main_economic$tag_female_protagonist == 1,
    na.rm = TRUE
  ),
  "\n"
)
cat(
  "H2 main No Female tag =",
  sum(
    h2_main_economic$tag_female_protagonist == 0,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "H2 80% N =",
  nrow(h2_80_economic),
  "\n"
)
cat(
  "H2 80% Female =",
  sum(
    h2_80_economic$tag_female_protagonist == 1,
    na.rm = TRUE
  ),
  "\n"
)
cat(
  "H2 80% No Female tag =",
  sum(
    h2_80_economic$tag_female_protagonist == 0,
    na.rm = TRUE
  ),
  "\n"
)

cat("\nFILES CREATED\n")
cat(
  "data/processed/H2_Economic_Participation_descriptives.csv\n"
)
cat(
  "data/processed/H2_Economic_Participation_raw_gap.csv\n"
)
cat(
  "data/processed/H2_weighted_GGGI_histogram_bins.csv\n"
)
cat(
  "results/figures/Figure_H2_weighted_GGGI_histogram.png\n"
)
cat(
  "data/processed/H2_main_GGGI_2025_with_Economic.csv\n"
)
cat(
  "data/processed/H2_robustness_80pct_GGGI_2025_with_Economic.csv\n"
)

cat(
  "\nDone. Existing Weighted GGGI values were read, not recalculated.\n"
)
