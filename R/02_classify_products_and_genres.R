
# 02 - Classify products and create the cleaned primary-genre variable

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data")) {
  stop("Run this script from the repository root (the folder containing R/ and data/).")
}


library(data.table)
library(dplyr)
library(tidyr)
library(tibble)

games_v2 <- fread(
  "data/processed/Clean_games_data_feb_2026_v2.csv"
)

core_genres <- c(
  "Action",
  "Adventure",
  "Simulation",
  "Sports",
  "Racing",
  "RPG",
  "Strategy"
)

preserved_primary_categories <- c(
  "Casual",
  "Massively Multiplayer",
  "Education"
)

non_game_categories <- c(
  "Design & Illustration",
  "Animation & Modeling",
  "Web Publishing",
  "Audio Production",
  "Utilities",
  "Game Development",
  "Video Production",
  "Software Training",
  "Photo Editing"
)

genre_dummy_cols <- c(
  "genre_Action",
  "genre_Adventure",
  "genre_Simulation",
  "genre_Sports",
  "genre_Racing",
  "genre_RPG",
  "genre_Strategy"
)

# אם כבר שינית קודם את השם, לא משנים אותו שוב
if ("genre_primary" %in% names(games_v2) &&
    !"genre_primary_raw" %in% names(games_v2)) {

  games_v2 <- games_v2 %>%
    rename(genre_primary_raw = genre_primary)
}

games_v2 <- games_v2 %>%
  # מסירים גרסאות קודמות כדי למנוע התנגשות
  select(
    -any_of(c(
      "genre_primary_clean",
      "genre_primary_source",
      "n_core_genres",
      "is_non_game_product"
    ))
  ) %>%
  mutate(
    # הגנה במקרה שחלק ממשתני הז'אנר מכילים NA
    across(
      all_of(genre_dummy_cols),
      ~replace_na(as.numeric(.x), 0)
    ),

    n_core_genres = rowSums(
      pick(all_of(genre_dummy_cols)),
      na.rm = TRUE
    ),

    is_non_game_product =
      genre_primary_raw %in% non_game_categories,

    # קטגוריה שנגזרת ממשתני הז'אנר רק כאשר
    # אין קטגוריה מקורית תקפה
    genre_from_indicators = case_when(
      n_core_genres == 1 & genre_Action == 1 ~
        "Action",

      n_core_genres == 1 & genre_Adventure == 1 ~
        "Adventure",

      n_core_genres == 1 & genre_Simulation == 1 ~
        "Simulation",

      n_core_genres == 1 & genre_Sports == 1 ~
        "Sports",

      n_core_genres == 1 & genre_Racing == 1 ~
        "Racing",

      n_core_genres == 1 & genre_RPG == 1 ~
        "RPG",

      n_core_genres == 1 & genre_Strategy == 1 ~
        "Strategy",

      n_core_genres > 1 ~
        "Multiple genres",

      TRUE ~
        NA_character_
    ),

    genre_primary_clean = case_when(
      # קודם כול מוצרים שאינם משחקים
      is_non_game_product ~
        "Non-game software/content",

      # שומרים ז'אנר ראשי תקף שהגיע מהמקור
      genre_primary_raw %in% core_genres ~
        genre_primary_raw,

      # שומרים קטגוריות ראשיות מקוריות שימושיות
      genre_primary_raw == "Casual" ~
        "Casual",

      genre_primary_raw == "Massively Multiplayer" ~
        "Massively Multiplayer",

      genre_primary_raw == "Education" ~
        "Education",

      # רק כאן משתמשים בז'אנרים הבינאריים
      !is.na(genre_from_indicators) ~
        genre_from_indicators,

      # מידע נוסף שנבנה בניקוי הז'אנרים
      is_casual == TRUE ~
        "Casual",

      is_MMO == TRUE ~
        "Massively Multiplayer",

      is_education == TRUE ~
        "Education",

      # חוסר מוחלט במידע
      is.na(genre_primary_raw) |
        genre_primary_raw == "" ~
        "Missing",

      # Indie, Free To Play, Early Access ותיאורי תוכן
      # שלא ניתן היה לשייך לז'אנר
      TRUE ~
        "Other"
    ),

    genre_primary_source = case_when(
      is_non_game_product ~
        "Non-game classification",

      genre_primary_raw %in%
        c(core_genres, preserved_primary_categories) ~
        "Original Steam primary category",

      !is.na(genre_from_indicators) ~
        "Derived from genre indicators",

      genre_primary_clean == "Missing" ~
        "Missing",

      TRUE ~
        "Other or unclassified"
    )
  ) %>%
  select(-genre_from_indicators)


genre_primary_clean_table <- games_v2 %>%
  count(genre_primary_clean, sort = TRUE) %>%
  mutate(
    percent = round(100 * n / sum(n), 2)
  ) %>%
  as_tibble()

print(genre_primary_clean_table, n = Inf)

games_v2 %>%
  filter(
    genre_primary_raw %in% c(
      "Casual",
      "Massively Multiplayer",
      "Education",
      non_game_categories
    )
  ) %>%
  count(
    genre_primary_raw,
    genre_primary_clean,
    sort = TRUE
  ) %>%
  as_tibble() %>%
  print(n = Inf)

data.table::fwrite(
  games_v2,
  "data/processed/Clean_games_data_feb_2026_v2.csv"
)
