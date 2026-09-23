# ==============================================================================
# סטטיסטיקה תיאורית למשחקי Steam - מגדר, פוליטיקה, ניהול ומדדי שוק - הגדרות ליבה בלבד
# ==============================================================================

# ==============================================================================
# גרסה ללא משתני CCU
# משתני ccu, peak_ccu ו-avg_concurrent_players אינם נכללים בטבלאות או בגרפים,
# משום שאינם נדרשים להשערות המחקר וכיסוים סלקטיבי ולא אחיד.
# הגדרות Management מבוססות אך ורק על התאמה מדויקת לתגיות Steam.
# ==============================================================================

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data")) {
  stop("Run this script from the repository root (the folder containing R/ and data/).")
}


# ==============================================================================
# 1. טעינת חבילות והגדרת נתיבי קלט ופלט
# ==============================================================================

required_packages <- c(
  "data.table",
  "dplyr",
  "tidyr",
  "purrr",
  "tibble",
  "stringr",
  "ggplot2",
  "scales",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Install the required packages first: ", paste(missing_packages, collapse = ", "))
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)

input_candidates <- "data/processed/Clean_games_data_feb_2026_v2.csv"

input_file <- input_candidates[file.exists(input_candidates)][1]

if (is.na(input_file)) {
  stop(
    paste0(
      "Input file was not found. Checked: ",
      paste(input_candidates, collapse = ", ")
    )
  )
}

output_dir <- "data/processed"
chart_dir <- file.path(output_dir, "charts")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!dir.exists(chart_dir)) {
  dir.create(chart_dir, recursive = TRUE)
}

output_excel <- file.path(
  output_dir,
  "descriptive_statistics.xlsx"
)

# ==============================================================================
# 2. טעינת מסד הנתונים ובדיקת משתנים הכרחיים
# ==============================================================================

games_v2 <- data.table::fread(input_file) %>%
  tibble::as_tibble()

required_columns <- c(
  "appid",
  "is_non_game_product",
  "tags_combined",
  "has_tag_data",
  "tag_female_protagonist",
  "tag_char_customization",
  "revenue",
  "copies_sold",
  "owners",
  "owners_midpoint",
  "positive_reviews",
  "negative_reviews",
  "has_country_data",
  "is_free",
  "genre_primary_clean",
  "release_year"
)

missing_columns <- setdiff(required_columns, names(games_v2))

if (length(missing_columns) > 0) {
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}

if (anyDuplicated(games_v2$appid) > 0) {
  stop("Duplicate appids were found in the input dataset.")
}

# ==============================================================================
# 3. פונקציות עזר בטוחות לחישוב שיעורים וסטטיסטיקה
# ==============================================================================

safe_percent_true <- function(x) {
  nonmissing <- !is.na(x)

  if (sum(nonmissing) == 0) {
    return(NA_real_)
  }

  round(
    100 * mean(x[nonmissing] %in% c(1, TRUE, "1", "TRUE", "true")),
    2
  )
}

continuous_stats <- function(data, variables, sample_name) {
  purrr::map_dfr(
    variables,
    function(variable_name) {
      if (!variable_name %in% names(data)) {
        return(
          tibble::tibble(
            sample = sample_name,
            variable = variable_name,
            n = NA_integer_,
            missing = NA_integer_,
            pct_missing = NA_real_,
            mean = NA_real_,
            sd = NA_real_,
            p25 = NA_real_,
            median = NA_real_,
            p75 = NA_real_,
            min = NA_real_,
            max = NA_real_
          )
        )
      }

      x <- suppressWarnings(as.numeric(data[[variable_name]]))
      valid_x <- x[!is.na(x) & is.finite(x)]
      n_total <- nrow(data)
      n_valid <- length(valid_x)

      if (n_valid == 0) {
        return(
          tibble::tibble(
            sample = sample_name,
            variable = variable_name,
            n = 0L,
            missing = n_total,
            pct_missing = ifelse(n_total > 0, 100, NA_real_),
            mean = NA_real_,
            sd = NA_real_,
            p25 = NA_real_,
            median = NA_real_,
            p75 = NA_real_,
            min = NA_real_,
            max = NA_real_
          )
        )
      }

      tibble::tibble(
        sample = sample_name,
        variable = variable_name,
        n = n_valid,
        missing = n_total - n_valid,
        pct_missing = round(100 * (n_total - n_valid) / n_total, 2),
        mean = mean(valid_x),
        sd = stats::sd(valid_x),
        p25 = as.numeric(stats::quantile(valid_x, 0.25)),
        median = stats::median(valid_x),
        p75 = as.numeric(stats::quantile(valid_x, 0.75)),
        min = min(valid_x),
        max = max(valid_x)
      )
    }
  )
}

binary_stats <- function(data, variables, sample_name) {
  purrr::map_dfr(
    variables,
    function(variable_name) {
      if (!variable_name %in% names(data)) {
        return(
          tibble::tibble(
            sample = sample_name,
            variable = variable_name,
            n_nonmissing = NA_integer_,
            n_equal_1 = NA_integer_,
            percent_equal_1 = NA_real_,
            missing = NA_integer_,
            pct_missing = NA_real_
          )
        )
      }

      x <- data[[variable_name]]
      nonmissing <- !is.na(x)
      x_as_text <- tolower(as.character(x))
      equal_one <- x_as_text %in% c("1", "true", "yes", "verified")
      n_nonmissing <- sum(nonmissing)
      n_equal_1 <- sum(equal_one & nonmissing)

      tibble::tibble(
        sample = sample_name,
        variable = variable_name,
        n_nonmissing = n_nonmissing,
        n_equal_1 = n_equal_1,
        percent_equal_1 = ifelse(
          n_nonmissing > 0,
          round(100 * n_equal_1 / n_nonmissing, 2),
          NA_real_
        ),
        missing = sum(!nonmissing),
        pct_missing = ifelse(
          length(x) > 0,
          round(100 * sum(!nonmissing) / length(x), 2),
          NA_real_
        )
      )
    }
  )
}

categorical_stats <- function(
    data,
    variable_name,
    sample_name,
    missing_label = "Missing") {
  if (!variable_name %in% names(data)) {
    return(
      tibble::tibble(
        sample = sample_name,
        variable = variable_name,
        value = NA_character_,
        n = NA_integer_,
        percent = NA_real_
      )
    )
  }

  data %>%
    dplyr::transmute(
      value = dplyr::case_when(
        is.na(.data[[variable_name]]) ~ missing_label,
        as.character(.data[[variable_name]]) == "" ~ missing_label,
        TRUE ~ as.character(.data[[variable_name]])
      )
    ) %>%
    dplyr::count(value, sort = TRUE) %>%
    dplyr::mutate(
      sample = sample_name,
      variable = variable_name,
      percent = round(100 * n / sum(n), 2),
      .before = 1
    )
}

# ==============================================================================
# 4. הכנת משתנים מסחריים, ביקורות ופעילות שחקנים
# ==============================================================================

games_stats <- games_v2 %>%
  dplyr::mutate(
    is_game = !dplyr::coalesce(as.logical(is_non_game_product), FALSE),

    revenue_positive = dplyr::if_else(
      !is.na(revenue) & as.numeric(revenue) > 0,
      as.numeric(revenue),
      NA_real_
    ),

    log_revenue_positive = dplyr::if_else(
      !is.na(revenue_positive),
      log(revenue_positive),
      NA_real_
    ),

    copies_sold_positive = dplyr::if_else(
      !is.na(copies_sold) & as.numeric(copies_sold) > 0,
      as.numeric(copies_sold),
      NA_real_
    ),

    owners_positive = dplyr::if_else(
      !is.na(owners) & as.numeric(owners) > 0,
      as.numeric(owners),
      NA_real_
    ),

    owners_midpoint_positive = dplyr::if_else(
      !is.na(owners_midpoint) & as.numeric(owners_midpoint) > 0,
      as.numeric(owners_midpoint),
      NA_real_
    ),

    avg_playtime_valid = dplyr::if_else(
      !is.na(avg_playtime) & as.numeric(avg_playtime) >= 0,
      as.numeric(avg_playtime),
      NA_real_
    ),

    total_reviews = dplyr::case_when(
      is.na(positive_reviews) & is.na(negative_reviews) ~ NA_real_,
      TRUE ~
        dplyr::coalesce(as.numeric(positive_reviews), 0) +
        dplyr::coalesce(as.numeric(negative_reviews), 0)
    ),

    positive_review_share = dplyr::case_when(
      total_reviews > 0 ~
        dplyr::coalesce(as.numeric(positive_reviews), 0) /
        total_reviews,
      TRUE ~ NA_real_
    )
  )

# ==============================================================================
# 5. פירוק תגיות Steam ויצירת תגיות מדויקות לפוליטיקה ולניהול
# ==============================================================================

management_core_tags <- c(
  "Management",
  "Time Management",
  "Resource Management",
  "Inventory Management"
)

politics_core_tags <- c(
  "Political",
  "Politics",
  "Political Sim"
)

exact_tag_map <- list(
  tag_exact_political = "Political",
  tag_exact_politics = "Politics",
  tag_exact_political_sim = "Political Sim",
  tag_exact_management = "Management",
  tag_exact_time_management = "Time Management",
  tag_exact_resource_management = "Resource Management",
  tag_exact_inventory_management = "Inventory Management"
)

tag_lists <- stringr::str_split(
  dplyr::coalesce(as.character(games_stats$tags_combined), ""),
  "\\s*[,;|]\\s*"
) %>%
  purrr::map(
    ~unique(stringr::str_trim(.x[.x != ""]))
  )

exact_flags <- purrr::imap_dfc(
  exact_tag_map,
  function(target_tags, variable_name) {
    has_tag_information <- dplyr::coalesce(
      as.logical(games_stats$has_tag_data),
      FALSE
    )

    values <- purrr::map_lgl(
      tag_lists,
      ~any(.x %in% target_tags)
    )

    tibble::tibble(
      !!variable_name := dplyr::if_else(
        has_tag_information,
        as.integer(values),
        NA_integer_
      )
    )
  }
)

games_stats <- dplyr::bind_cols(
  games_stats,
  exact_flags
) %>%
  dplyr::mutate(
    politics_core = dplyr::if_else(
      dplyr::coalesce(as.logical(has_tag_data), FALSE),
      as.integer(
        dplyr::coalesce(tag_exact_political, 0L) == 1L |
          dplyr::coalesce(tag_exact_politics, 0L) == 1L |
          dplyr::coalesce(tag_exact_political_sim, 0L) == 1L
      ),
      NA_integer_
    ),

    management_core = dplyr::if_else(
      dplyr::coalesce(as.logical(has_tag_data), FALSE),
      as.integer(
        rowSums(
          dplyr::across(
            dplyr::all_of(c(
              "tag_exact_management",
              "tag_exact_time_management",
              "tag_exact_resource_management",
              "tag_exact_inventory_management"
            ))
          ),
          na.rm = TRUE
        ) > 0
      ),
      NA_integer_
    ),

    politics_adjacent_war = dplyr::if_else(
      dplyr::coalesce(as.logical(has_tag_data), FALSE),
      as.integer(dplyr::coalesce(tag_war, 0) == 1),
      NA_integer_
    ),

    politics_adjacent_historical = dplyr::if_else(
      dplyr::coalesce(as.logical(has_tag_data), FALSE),
      as.integer(dplyr::coalesce(tag_historical, 0) == 1),
      NA_integer_
    ),

    sample_h1_positive_revenue =
      is_game &
      !is.na(revenue_positive) &
      dplyr::coalesce(as.logical(has_tag_data), FALSE),

    sample_h1_paid_positive_revenue =
      is_game &
      !is.na(revenue_positive) &
      dplyr::coalesce(as.logical(has_tag_data), FALSE) &
      !dplyr::coalesce(as.logical(is_free), FALSE),

    sample_h2_country_data =
      is_game & dplyr::coalesce(as.logical(has_country_data), FALSE)
  )

# ==============================================================================
# 6. הגדרת המשתנים שיופיעו בטבלאות התיאוריות
# ==============================================================================

continuous_variables <- c(
  "revenue_positive",
  "log_revenue_positive",
  "owners_positive",
  "owners_midpoint_positive",
  "price_initial",
  "price_current",
  "total_reviews",
  "positive_review_share",
  "review_score",
  "avg_playtime_valid",
  "followers",
  "copies_sold_last_7d",
  "age_years",
  "language_count",
  "achievement_count",
  "dlc_count",
  "genre_count",
  "tag_count",
  "feature_count",
  "score_gpu",
  "n_reported_countries",
  "reported_country_sum",
  "pub_numberOfGames"
)

binary_variables <- c(
  "tag_female_protagonist",
  "tag_char_customization",
  "is_free",
  "is_early_access",
  "is_casual",
  "is_MMO",
  "is_education",
  "feat_SinglePlayer",
  "feat_Multiplayer",
  "feat_Local_Multiplayer",
  "feat_InAppPurchases",
  "feat_DLC",
  "feat_Achievements",
  "feat_Cloud",
  "feat_Leaderboards",
  "feat_VR",
  "controller_support",
  "steam_deck",
  "is_multi_developer",
  "is_multi_publisher",
  "is_self_published",
  "publisher_matched",
  "has_country_data",
  "has_positive_revenue",
  "politics_core",
  "management_core",
  "politics_adjacent_war",
  "politics_adjacent_historical"
)

# ==============================================================================
# 7. הגדרת כל המדגמים לאחר יצירת כל המשתנים
# ==============================================================================

samples <- list(
  All_games = games_stats %>%
    dplyr::filter(is_game),

  Female_protagonist = games_stats %>%
    dplyr::filter(
      is_game,
      tag_female_protagonist == 1
    ),

  No_female_tag = games_stats %>%
    dplyr::filter(
      is_game,
      tag_female_protagonist == 0
    ),

  H1_positive_revenue = games_stats %>%
    dplyr::filter(sample_h1_positive_revenue),

  H1_paid_positive_revenue = games_stats %>%
    dplyr::filter(sample_h1_paid_positive_revenue),

  H2_country_data = games_stats %>%
    dplyr::filter(sample_h2_country_data),

  Politics_core = games_stats %>%
    dplyr::filter(
      is_game,
      politics_core == 1
    ),

  Management_core = games_stats %>%
    dplyr::filter(
      is_game,
      management_core == 1
    )
)

# ==============================================================================
# 8. יצירת טבלת סיכום של המדגמים
# ==============================================================================

all_games_n <- nrow(samples$All_games)

sample_overview <- purrr::imap_dfr(
  samples,
  function(data, sample_name) {
    tibble::tibble(
      sample = sample_name,
      observations = nrow(data),
      percent_of_all_games = round(
        100 * nrow(data) / all_games_n,
        2
      ),
      unique_appids = dplyr::n_distinct(data$appid),
      female_protagonist_games = sum(
        data$tag_female_protagonist == 1,
        na.rm = TRUE
      ),
      female_protagonist_percent = safe_percent_true(
        data$tag_female_protagonist
      ),
      positive_revenue_games = sum(
        !is.na(data$revenue_positive)
      ),
      positive_revenue_percent = round(
        100 * mean(!is.na(data$revenue_positive)),
        2
      ),
      country_data_games = sum(
        dplyr::coalesce(as.logical(data$has_country_data), FALSE)
      ),
      country_data_percent = round(
        100 * mean(
          dplyr::coalesce(as.logical(data$has_country_data), FALSE)
        ),
        2
      )
    )
  }
)

# ==============================================================================
# 9. יצירת טבלאות כלליות של משתנים רציפים, בינאריים וקטגוריאליים
# ==============================================================================

continuous_all_samples <- purrr::imap_dfr(
  samples,
  function(data, sample_name) {
    continuous_stats(
      data,
      continuous_variables,
      sample_name
    )
  }
)

binary_all_samples <- purrr::imap_dfr(
  samples,
  function(data, sample_name) {
    binary_stats(
      data,
      binary_variables,
      sample_name
    )
  }
)

genre_all_games <- categorical_stats(
  samples$All_games,
  "genre_primary_clean",
  "All games"
)

genre_female <- categorical_stats(
  samples$Female_protagonist,
  "genre_primary_clean",
  "Female protagonist"
)

genre_no_female <- categorical_stats(
  samples$No_female_tag,
  "genre_primary_clean",
  "No female tag"
)

genre_politics <- categorical_stats(
  samples$Politics_core,
  "genre_primary_clean",
  "Politics - core"
)

genre_management <- categorical_stats(
  samples$Management_core,
  "genre_primary_clean",
  "Management - core"
)

protagonist_groups <- categorical_stats(
  samples$All_games,
  "protagonist_group",
  "All games"
)

publisher_classes <- samples$All_games %>%
  dplyr::filter(
    dplyr::coalesce(as.logical(publisher_matched), FALSE)
  ) %>%
  categorical_stats(
    "pub_class",
    "Matched publishers"
  )

# ==============================================================================
# 10. יצירת טבלאות ייעודיות לפוליטיקה ולניהול
# ==============================================================================

politics_tag_variables <- c(
  "tag_exact_political",
  "tag_exact_politics",
  "tag_exact_political_sim"
)

management_tag_variables <- c(
  "tag_exact_management",
  "tag_exact_time_management",
  "tag_exact_resource_management",
  "tag_exact_inventory_management"
)

politics_tag_counts <- binary_stats(
  samples$All_games,
  politics_tag_variables,
  "All games"
)

management_tag_counts <- binary_stats(
  samples$All_games,
  management_tag_variables,
  "All games"
)

politics_core_overlap <- samples$Politics_core %>%
  dplyr::count(
    tag_exact_political,
    tag_exact_politics,
    tag_exact_political_sim,
    sort = TRUE
  ) %>%
  dplyr::mutate(
    percent = round(100 * n / sum(n), 2)
  ) %>%
  tibble::as_tibble()

management_core_overlap <- samples$Management_core %>%
  dplyr::count(
    tag_exact_management,
    tag_exact_time_management,
    tag_exact_resource_management,
    tag_exact_inventory_management,
    sort = TRUE
  ) %>%
  dplyr::mutate(
    percent = round(100 * n / sum(n), 2)
  ) %>%
  tibble::as_tibble()

management_definitions <- tibble::tribble(
  ~definition, ~included_exact_tags, ~role_in_analysis,
  "Management - core",
  paste(management_core_tags, collapse = ", "),
  "Definition used in the descriptive analysis and regression controls"
)

politics_adjacent_summary <- binary_stats(
  samples$All_games,
  c(
    "tag_war",
    "tag_historical",
    "politics_adjacent_war",
    "politics_adjacent_historical"
  ),
  "All games"
)

# ==============================================================================
# 11. השוואת הייצוג הנשי בין כלל המשחקים, פוליטיקה וניהול
# ==============================================================================

summarise_representation <- function(data, group_name) {
  data_with_tag_information <- data %>%
    dplyr::filter(!is.na(tag_female_protagonist))

  tibble::tibble(
    group = group_name,
    observations = nrow(data),
    games_with_tag_information = nrow(data_with_tag_information),
    female_protagonist_games = sum(
      data_with_tag_information$tag_female_protagonist == 1,
      na.rm = TRUE
    ),
    female_protagonist_percent = safe_percent_true(
      data_with_tag_information$tag_female_protagonist
    ),
    character_customization_percent = safe_percent_true(
      data_with_tag_information$tag_char_customization
    ),
    positive_revenue_percent = round(
      100 * mean(!is.na(data_with_tag_information$revenue_positive)),
      2
    ),
    country_data_percent = round(
      100 * mean(
        dplyr::coalesce(
          as.logical(data_with_tag_information$has_country_data),
          FALSE
        )
      ),
      2
    )
  )
}

representation_summary <- dplyr::bind_rows(
  summarise_representation(samples$All_games, "All games"),
  summarise_representation(samples$Politics_core, "Politics - core"),
  summarise_representation(samples$Management_core, "Management - core")
)

# ==============================================================================
# 12. סטטיסטיקה תיאורית של רכישות, בעלות ומדדי שוק
# ==============================================================================

market_activity_variables <- c(
  "revenue_positive",
  "owners_positive",
  "owners_midpoint_positive",
  "avg_playtime_valid",
  "followers",
  "total_reviews"
)

market_variable_labels <- c(
  revenue_positive = "Estimated revenue - positive values",
  owners_positive = "Estimated owners - positive values",
  owners_midpoint_positive = "SteamSpy owners midpoint - positive values",
  avg_playtime_valid = "Average playtime",
  followers = "Followers",
  total_reviews = "Total user reviews"
)

market_activity_groups <- samples[c(
  "All_games",
  "Female_protagonist",
  "No_female_tag",
  "Politics_core",
  "Management_core"
)]

market_activity_summary <- purrr::imap_dfr(
  market_activity_groups,
  function(data, group_name) {
    continuous_stats(
      data,
      market_activity_variables,
      group_name
    ) %>%
      dplyr::rename(
        group = sample
      ) %>%
      dplyr::mutate(
        description = unname(market_variable_labels[variable]),
        .after = variable
      )
  }
)

# ==============================================================================
# 13. יצירת גרפים ויזואליים של הסטטיסטיקה התיאורית
# ==============================================================================

representation_chart_file <- file.path(
  chart_dir,
  "female_representation_by_group.png"
)

genre_chart_file <- file.path(
  chart_dir,
  "female_genre_comparison.png"
)

plot_representation <- ggplot2::ggplot(
  representation_summary,
  ggplot2::aes(
    x = reorder(group, female_protagonist_percent),
    y = female_protagonist_percent
  )
) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::geom_text(
    ggplot2::aes(label = paste0(female_protagonist_percent, "%")),
    hjust = -0.1,
    size = 3.5
  ) +
  ggplot2::expand_limits(
    y = max(representation_summary$female_protagonist_percent, na.rm = TRUE) * 1.15
  ) +
  ggplot2::labs(
    title = "Share of games tagged Female Protagonist",
    x = NULL,
    y = "Percent"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  representation_chart_file,
  plot_representation,
  width = 9,
  height = 5.5,
  dpi = 300,
  bg = "white"
)

# ------------------------------------------------------------------------------
# 13.1 התפלגות ז'אנרים לפי תגית Female Protagonist
# ------------------------------------------------------------------------------

top_genres <- genre_all_games %>%
  dplyr::filter(value != "Missing") %>%
  dplyr::slice_max(n, n = 10) %>%
  dplyr::pull(value)

genre_comparison_data <- dplyr::bind_rows(
  genre_female,
  genre_no_female
) %>%
  dplyr::filter(value %in% top_genres)

plot_genres <- ggplot2::ggplot(
  genre_comparison_data,
  ggplot2::aes(
    x = reorder(value, percent),
    y = percent,
    fill = sample
  )
) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    title = "Genre distribution by Female Protagonist tag",
    x = NULL,
    y = "Percent within group",
    fill = NULL
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  genre_chart_file,
  plot_genres,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)

# ==============================================================================
# 14. שמירת כל הטבלאות והגרפים לקובץ Excel
# ==============================================================================

excel_tables <- list(
  "Sample overview" = sample_overview,
  "Continuous stats" = continuous_all_samples,
  "Binary stats" = binary_all_samples,
  "Representation" = representation_summary,
  "Market activity" = market_activity_summary,
  "Genres all" = genre_all_games,
  "Genres female" = genre_female,
  "Genres no female" = genre_no_female,
  "Genres politics" = genre_politics,
  "Genres management" = genre_management,
  "Protagonist groups" = protagonist_groups,
  "Publisher classes" = publisher_classes,
  "Politics tags" = politics_tag_counts,
  "Politics overlap" = politics_core_overlap,
  "Politics adjacent" = politics_adjacent_summary,
  "Management definitions" = management_definitions,
  "Management tags" = management_tag_counts,
  "Management overlap" = management_core_overlap
)

wb <- openxlsx::createWorkbook()

purrr::walk2(
  names(excel_tables),
  excel_tables,
  function(sheet_name, table_data) {
    openxlsx::addWorksheet(wb, sheet_name)

    openxlsx::writeData(
      wb,
      sheet_name,
      as.data.frame(table_data),
      withFilter = TRUE
    )

    openxlsx::freezePane(
      wb,
      sheet_name,
      firstRow = TRUE
    )

    if (ncol(table_data) > 0) {
      openxlsx::setColWidths(
        wb,
        sheet_name,
        cols = seq_len(ncol(table_data)),
        widths = "auto"
      )
    }
  }
)

openxlsx::addWorksheet(wb, "Chart representation")
openxlsx::insertImage(
  wb,
  "Chart representation",
  representation_chart_file,
  startRow = 1,
  startCol = 1,
  width = 10,
  height = 6
)

openxlsx::addWorksheet(wb, "Chart genres")
openxlsx::insertImage(
  wb,
  "Chart genres",
  genre_chart_file,
  startRow = 1,
  startCol = 1,
  width = 11,
  height = 7.5
)

openxlsx::saveWorkbook(
  wb,
  output_excel,
  overwrite = TRUE
)

# ==============================================================================
# 15. הצגת טבלאות מפתח ואישור שמירת הפלט
# ==============================================================================

print(sample_overview, n = Inf)
print(representation_summary, n = Inf)

market_activity_summary %>%
  dplyr::filter(
    variable %in% c(
          "owners_positive"
    )
  ) %>%
  print(n = Inf)

print(management_definitions, n = Inf)
print(management_tag_counts, n = Inf)

cat(
  "\nDescriptive statistics and charts saved to:\n",
  output_excel,
  "\n"
)


# גודל מדגם H1 ומאפייניו
samples$H1_positive_revenue %>%
  dplyr::summarise(
    N_H1 = dplyr::n(),
    Female = sum(tag_female_protagonist == 1, na.rm = TRUE),
    No_Female = sum(tag_female_protagonist == 0, na.rm = TRUE)
  )

