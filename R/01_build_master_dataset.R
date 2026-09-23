# ==============================================================================
# 01 - Build corrected master Steam/Gamalytic dataset
# ==============================================================================
# Purpose:
#   Rebuild a single analysis-ready file from the raw Steam API, Gamalytic,
#   and publisher files, while correcting the main issues in the previous
#   pipeline:
#   1. Deduplicate by appid rather than title.
#   2. Do not drop games because optional variables are missing.
#   3. Preserve missingness instead of automatically converting it to zero.
#   4. Build tag variables from exact cleaned tag names, not substring matching.
#   5. Distinguish games with no tag/country/feature data from true zeros.
#   6. Keep all reported country codes, not only a predefined subset.
#   7. Correct developer/publisher counts (no hidden cap at 3).
#   8. Do not classify unmatched publishers as Hobbyist or impute publisher data.
#   9. Keep observed and fallback prices separate and transparent.
#  10. Create useful derived variables for descriptive statistics.
#
# Default output:
#   data/processed/Clean_games_data_feb_2026_v2.csv
#
# Important:
#   The script intentionally does NOT impute missing revenue. Revenue completion
#   should remain a separate robustness exercise.
# ==============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(lubridate)
})

options(scipen = 999)

# ==============================================================================
# 0. CONFIGURATION
# ==============================================================================

if (!dir.exists("R") || !dir.exists("data")) {
  stop("Run this script from the repository root (the folder containing R/ and data/).")
}

PATH_STEAM_RAW <- "data/raw/Steam_API_games_data_feb_2026.csv"
PATH_GAMALYTIC_RAW <- "data/raw/Gamalytic_games_data_feb_2026_missing_50_thousand_games.csv"
PATH_PUBLISHERS_RAW <- "data/raw/publishers_gamalytic_feb_2026.csv"
PATH_GPU_BENCHMARKS <- "data/raw/GPU_benchmarks_v7.csv"

OUTPUT_FILE <- "data/processed/Clean_games_data_feb_2026_v2.csv"

# The previous scripts used 2026-02-01. Keep it explicit and easy to change.
SNAPSHOT_DATE <- as.Date("2026-02-01")
MIN_RELEASE_YEAR <- 2003L

# Keep the original long text fields in the master file. They are useful if the
# research question or tag definitions change later.
KEEP_RAW_TEXT_COLUMNS <- TRUE

# Hardware construction is optional because it requires an external benchmark
# file and fuzzy text matching. Missing hardware data will NEVER remove a game.
RUN_HARDWARE_BLOCK <- TRUE

required_files <- c(PATH_STEAM_RAW, PATH_GAMALYTIC_RAW)
missing_required <- required_files[!file.exists(required_files)]
if (length(missing_required) > 0) {
  stop(
    "Missing required input file(s):\n",
    paste0(" - ", missing_required, collapse = "\n")
  )
}

dir.create(dirname(OUTPUT_FILE), recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# 1. HELPER FUNCTIONS
# ==============================================================================

get_col <- function(df, column, default = NA) {
  if (column %in% names(df)) {
    df[[column]]
  } else {
    rep(default, nrow(df))
  }
}

parse_date_flexible <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "Unknown", "NA", "N/A")] <- NA_character_
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      x,
      orders = c("ymd", "a b d Y", "b d Y", "d b Y", "mdy", "dmy"),
      quiet = TRUE
    )
  )
  as.Date(parsed)
}

clean_company_names <- function(x) {
  x <- as.character(x)
  x[x == ""] <- NA_character_
  x %>%
    str_replace_all("Co\\.,\\s*Ltd", "Co. Ltd") %>%
    str_replace_all(",\\s+(Inc\\.|Ltd\\.|LLC|L\\.L\\.C\\.|S\\.L\\.|S\\.A\\.|Corp\\.|AG)", " \\1") %>%
    str_replace_all("\\(([^)]*),([^)]*)\\)", "(\\1 &\\2)") %>%
    str_squish()
}

normalize_company_key <- function(x) {
  x %>%
    clean_company_names() %>%
    str_to_lower() %>%
    str_replace_all("&", " and ") %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish() %>%
    na_if("")
}

entity_count <- function(x) {
  x <- clean_company_names(x)
  ifelse(is.na(x), NA_integer_, str_count(x, ",") + 1L)
}

primary_entity <- function(x) {
  x <- clean_company_names(x)
  str_trim(str_extract(x, "^[^,]+")) %>% na_if("")
}

combine_any_true <- function(...) {
  vectors <- lapply(list(...), as.logical)
  m <- do.call(cbind, vectors)
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  n_known <- rowSums(!is.na(m))
  n_true <- rowSums(m == TRUE, na.rm = TRUE)
  out <- ifelse(n_known == 0, NA, n_true > 0)
  as.logical(out)
}

snake_case <- function(x) {
  x %>%
    str_replace_all("([a-z0-9])([A-Z])", "\\1_\\2") %>%
    str_replace_all("[^A-Za-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_remove("^_") %>%
    str_remove("_$") %>%
    str_to_lower()
}

clean_list_text <- function(x) {
  x <- as.character(x)
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  x %>%
    str_remove_all("[{}]") %>%
    str_squish()
}

build_exact_flags <- function(ids, item_long, group_map, has_data_col) {
  flag_names <- names(group_map)

  map_table <- tibble(
    variable = rep(flag_names, lengths(group_map)),
    item = unname(unlist(group_map, use.names = FALSE))
  ) %>%
    distinct(variable, item)

  flags_wide <- item_long %>%
    inner_join(
      map_table,
      by = "item",
      relationship = "many-to-many"
    ) %>%
    distinct(appid, variable) %>%
    mutate(value = 1L) %>%
    pivot_wider(
      id_cols = appid,
      names_from = variable,
      values_from = value,
      values_fill = 0L
    )

  out <- ids %>%
    left_join(flags_wide, by = "appid")

  for (nm in flag_names) {
    if (!nm %in% names(out)) {
      out[[nm]] <- NA_integer_
    }

    out[[nm]] <- case_when(
      out[[has_data_col]] == TRUE ~ replace_na(as.integer(out[[nm]]), 0L),
      TRUE ~ NA_integer_
    )
  }

  out
}

# ==============================================================================
# 2. LOAD AND DEDUPLICATE RAW SOURCES
# ==============================================================================

steam_raw <- fread(PATH_STEAM_RAW, na.strings = c("", "NA", "N/A"))
gamalytic_raw <- fread(PATH_GAMALYTIC_RAW, na.strings = c("", "NA", "N/A"))

# Steam: one row per appid. If duplicates exist, retain the first complete row.
steam_raw <- steam_raw %>%
  filter(!is.na(appid)) %>%
  distinct(appid, .keep_all = TRUE)

# Gamalytic: deduplicate by app_id, never by title. When scraped_at exists,
# prefer the latest scrape for each app.
if ("scraped_at" %in% names(gamalytic_raw)) {
  gamalytic_raw <- gamalytic_raw %>%
    mutate(.scraped_ts = suppressWarnings(ymd_hms(scraped_at, quiet = TRUE))) %>%
    arrange(app_id, desc(.scraped_ts)) %>%
    distinct(app_id, .keep_all = TRUE) %>%
    select(-.scraped_ts)
} else {
  gamalytic_raw <- gamalytic_raw %>%
    distinct(app_id, .keep_all = TRUE)
}

gamalytic_raw <- gamalytic_raw %>% filter(!is.na(app_id))

# ==============================================================================
# 3. CREATE SOURCE-SPECIFIC BASE TABLES
# ==============================================================================

steam_base <- tibble(
  appid = as.integer(get_col(steam_raw, "appid")),
  title_steam = as.character(get_col(steam_raw, "name")),
  genre_primary = as.character(get_col(steam_raw, "genre_primary")),
  release_date_steam = parse_date_flexible(get_col(steam_raw, "release_date")),
  price_initial_observed = suppressWarnings(as.numeric(get_col(steam_raw, "price_initial"))),
  price_current_steam = suppressWarnings(as.numeric(get_col(steam_raw, "price_final"))),
  discount_percent = suppressWarnings(as.numeric(get_col(steam_raw, "discount_percent"))),
  is_free_steam = as.logical(get_col(steam_raw, "is_free")),
  owners_min = suppressWarnings(as.numeric(get_col(steam_raw, "owners_min"))),
  owners_max = suppressWarnings(as.numeric(get_col(steam_raw, "owners_max"))),
  owners_midpoint = suppressWarnings(as.numeric(get_col(steam_raw, "owners_midpoint"))),
  positive_reviews = suppressWarnings(as.numeric(get_col(steam_raw, "positive_reviews"))),
  negative_reviews = suppressWarnings(as.numeric(get_col(steam_raw, "negative_reviews"))),
  ccu = suppressWarnings(as.numeric(get_col(steam_raw, "ccu"))),
  peak_ccu = suppressWarnings(as.numeric(get_col(steam_raw, "peak_ccu"))),
  languages_count_steam = suppressWarnings(as.numeric(get_col(steam_raw, "languages_count"))),
  achievement_count_raw = suppressWarnings(as.numeric(get_col(steam_raw, "achievement_count"))),
  dlc_count = suppressWarnings(as.numeric(get_col(steam_raw, "dlc_count"))),
  is_early_access_steam = as.logical(get_col(steam_raw, "is_early_access")),
  steam_deck = as.logical(get_col(steam_raw, "steam_deck")),
  controller_support = as.logical(get_col(steam_raw, "controller_support")),
  developer_steam_raw = as.character(get_col(steam_raw, "developer")),
  publisher_steam_raw = as.character(get_col(steam_raw, "publisher")),
  tags_steam_raw = as.character(get_col(steam_raw, "tags")),
  pc_req_min_steam = as.character(get_col(steam_raw, "pc_req_min"))
)

gamalytic_base <- tibble(
  appid = as.integer(get_col(gamalytic_raw, "app_id")),
  title_gamalytic = as.character(get_col(gamalytic_raw, "title")),
  release_date_gamalytic = parse_date_flexible(get_col(gamalytic_raw, "release_date")),
  developer_gamalytic_raw = as.character(get_col(gamalytic_raw, "developers")),
  publisher_gamalytic_raw = as.character(get_col(gamalytic_raw, "publishers")),
  genres_gamalytic_raw = as.character(get_col(gamalytic_raw, "genres")),
  languages_gamalytic_raw = as.character(get_col(gamalytic_raw, "languages")),
  tags_gamalytic_raw = as.character(get_col(gamalytic_raw, "tags")),
  features_gamalytic_raw = as.character(get_col(gamalytic_raw, "features")),
  players_by_country_raw = as.character(get_col(gamalytic_raw, "players_by_country")),
  price_current_gamalytic = suppressWarnings(as.numeric(get_col(gamalytic_raw, "price"))),
  copies_sold = suppressWarnings(as.numeric(get_col(gamalytic_raw, "copies_sold"))),
  revenue = suppressWarnings(as.numeric(get_col(gamalytic_raw, "revenue"))),
  owners = suppressWarnings(as.numeric(get_col(gamalytic_raw, "owners"))),
  review_score = suppressWarnings(as.numeric(get_col(gamalytic_raw, "review_score"))),
  avg_playtime = suppressWarnings(as.numeric(get_col(gamalytic_raw, "avg_playtime"))),
  avg_concurrent_players = suppressWarnings(as.numeric(get_col(gamalytic_raw, "avg_concurrent_players"))),
  followers = suppressWarnings(as.numeric(get_col(gamalytic_raw, "followers"))),
  copies_sold_last_7d = suppressWarnings(as.numeric(get_col(gamalytic_raw, "copies_sold_last_7d")))
)

# Keep the intersection used by the research, but do not drop rows based on any
# optional variable. Every retained game exists in both source files.
master <- inner_join(steam_base, gamalytic_base, by = "appid")

# ==============================================================================
# 4. CORE IDENTIFIERS, RELEASE DATE, DEVELOPERS AND PUBLISHERS
# ==============================================================================

master <- master %>%
  mutate(
    title = coalesce(na_if(str_squish(title_steam), ""), na_if(str_squish(title_gamalytic), "")),
    release_date = coalesce(release_date_steam, release_date_gamalytic),
    release_date_source = case_when(
      !is.na(release_date_steam) ~ "Steam API",
      !is.na(release_date_gamalytic) ~ "Gamalytic",
      TRUE ~ NA_character_
    ),
    release_year = year(release_date),
    release_month = month(release_date),
    release_day = day(release_date),
    age_years = as.numeric(difftime(SNAPSHOT_DATE, release_date, units = "days")) / 365.25,

    developer_raw = coalesce(
      na_if(clean_company_names(developer_steam_raw), ""),
      na_if(clean_company_names(developer_gamalytic_raw), "")
    ),
    publisher_raw = coalesce(
      na_if(clean_company_names(publisher_steam_raw), ""),
      na_if(clean_company_names(publisher_gamalytic_raw), "")
    ),

    developer_count = entity_count(developer_raw),
    developer_count_3plus = if_else(is.na(developer_count), NA_integer_, pmin(developer_count, 3L)),
    is_multi_developer = if_else(is.na(developer_count), NA, developer_count > 1L),
    primary_developer = primary_entity(developer_raw),

    publisher_count = entity_count(publisher_raw),
    publisher_count_3plus = if_else(is.na(publisher_count), NA_integer_, pmin(publisher_count, 3L)),
    is_multi_publisher = if_else(is.na(publisher_count), NA, publisher_count > 1L),
    primary_publisher = primary_entity(publisher_raw),

    is_self_published = case_when(
      str_to_lower(primary_publisher) == "self published" ~ TRUE,
      !is.na(primary_developer) & !is.na(primary_publisher) ~
        normalize_company_key(primary_developer) == normalize_company_key(primary_publisher),
      TRUE ~ NA
    )
  ) %>%
  filter(
    !is.na(release_date),
    release_year >= MIN_RELEASE_YEAR,
    release_date < SNAPSHOT_DATE
  )

# ==============================================================================
# 5. EXACT TAG CATALOG AND TAG VARIABLES
# ==============================================================================

# Build one clean tag catalog from both sources. The suffix "More" is an
# extraction artifact and is removed before exact matching.
tag_source <- bind_rows(
  master %>% transmute(appid, source = "Steam API", raw = tags_steam_raw),
  master %>% transmute(appid, source = "Gamalytic", raw = tags_gamalytic_raw)
) %>%
  mutate(
    raw = clean_list_text(raw),
    source_has_tag_text = !is.na(raw) & raw != ""
  )

tag_availability <- tag_source %>%
  group_by(appid) %>%
  summarise(has_tag_data = any(source_has_tag_text), .groups = "drop")

tag_long <- tag_source %>%
  filter(source_has_tag_text) %>%
  separate_rows(raw, sep = ",\\s*") %>%
  mutate(
    item = str_trim(raw),
    item = str_remove(item, "More$"),
    item = str_squish(item)
  ) %>%
  filter(!is.na(item), item != "") %>%
  distinct(appid, item)

tags_combined <- tag_long %>%
  arrange(appid, item) %>%
  group_by(appid) %>%
  summarise(
    tag_count = n_distinct(item),
    tags_combined = paste(item, collapse = ", "),
    .groups = "drop"
  )

# Exact tag lists. A game may belong to several groups simultaneously.
sports_keywords <- c(
  "Sports", "Football", "Football (Soccer)", "Football (American)", "Soccer",
  "Baseball", "Basketball", "Hockey", "Tennis", "Golf", "Bowling",
  "Volleyball", "Rugby", "Cricket", "Boxing", "Wrestling", "Skateboarding",
  "Skating", "Snowboarding", "Skiing", "Cycling", "BMX", "Motocross", "ATV",
  "Pool", "Snooker", "Archery", "Motorsports", "Racing", "Bikes",
  "Motorbike", "Table Tennis", "Fishing"
)
job_sim_keywords <- c(
  "Farming", "Medical Sim", "Political Sim", "Hacking", "Typing",
  "Job Simulator", "Software Training", "Video Production", "Audio Production",
  "Photo Editing", "Game Development", "Programming", "Police", "Train",
  "Agriculture", "Management", "Shop Keeper"
)
historical_keywords <- c(
  "World War I", "World War II", "Cold War", "Vietnam", "Rome", "Vikings",
  "Western", "Medieval", "Historical", "Dinosaurs", "Prehistoric", "Ancient",
  "Alternate History"
)
local_coop_keywords <- c(
  "Asynchronous Multiplayer", "Split Screen", "Local Co-Op", "Local Multiplayer",
  "4 Player Local", "Co-op Campaign", "Shared/Split Screen"
)
horror_keywords <- c(
  "Horror", "Survival Horror", "Psychological Horror", "Zombies", "Gore",
  "Violent", "Blood", "Jump Scare", "Lovecraftian", "Demons", "Vampire",
  "Thriller", "Dark", "Scary"
)
scifi_keywords <- c(
  "Sci-fi", "Space", "Cyberpunk", "Aliens", "Robots", "Futuristic", "Mechs",
  "Transhumanism", "Mars", "Spaceships", "Space Sim", "Steampunk",
  "Post-apocalyptic", "Artificial Intelligence", "Dystopian"
)
shooter_keywords <- c(
  "Shooter", "FPS", "Third-Person Shooter", "Top-Down Shooter",
  "Twin Stick Shooter", "Arena Shooter", "Looter Shooter", "Hero Shooter",
  "Bullet Hell", "Shoot 'Em Up", "Sniper", "Boomer Shooter", "Gun Customization",
  "Extraction Shooter", "On-Rails Shooter"
)
narrative_keywords <- c(
  "Story Rich", "Visual Novel", "Choices Matter", "Multiple Endings", "Narrative",
  "Interactive Fiction", "Choose Your Own Adventure", "Lore-Rich", "Drama",
  "Emotional", "Romance", "Walking Simulator", "Dating Sim", "Based On A Novel",
  "Well-Written", "Immersive", "Dynamic Narration", "Nonlinear", "Text-Based",
  "Narration", "Conversation"
)
retro_keywords <- c(
  "Pixel Graphics", "Retro", "Old School", "1980s", "1990's", "Arcade",
  "Classic", "8-bit Music", "2D Platformer"
)
roguelike_keywords <- c(
  "Roguelike", "Roguelite", "Rogue-like", "Rogue-lite", "Action Roguelike",
  "Traditional Roguelike", "Roguelike Deckbuilder", "Perma Death",
  "Dungeon Crawler", "Procedural Generation"
)
nsfw_keywords <- c("Sexual Content", "Nudity", "Hentai", "NSFW", "Mature")
fantasy_keywords <- c(
  "Fantasy", "Magic", "Dragons", "Elf", "Dwarf", "Mythology", "Dark Fantasy",
  "Dungeons & Dragons", "Swordplay"
)
platformer_keywords <- c(
  "Platformer", "3D Platformer", "Precision Platformer", "Puzzle Platformer",
  "Puzzle-Platformer", "Side Scroller", "Metroidvania", "Runner", "Parkour",
  "Roguevania"
)
puzzle_keywords <- c(
  "Puzzle", "Logic", "Hidden Object", "Match 3", "Tile-Matching", "Sokoban",
  "Trivia", "Brain Teaser", "Point & Click", "Word Game", "Puzzle-Platformer",
  "Puzzle Platformer", "Escape Room"
)
turnbased_keywords <- c(
  "Turn-Based", "Turn-Based Strategy", "Turn-Based Combat", "Turn-Based Tactics",
  "Card Game", "Card Battler", "Deckbuilding", "Board Game", "Tabletop",
  "Solitaire", "Grid-Based Movement"
)
survival_keywords <- c(
  "Survival", "Crafting", "Building", "Base Building", "Base-Building",
  "Open World Survival Craft", "Resource Management", "Colony Sim", "City Builder"
)
humor_keywords <- c("Funny", "Comedy", "Dark Humor", "Satire", "Parody", "Memes", "Dark Comedy")
relaxing_keywords <- c(
  "Relaxing", "Cozy", "Wholesome", "Family Friendly", "Nature", "Cute",
  "Colorful", "Casual", "Farming Sim", "Cooking"
)
visuals_keywords <- c(
  "Atmospheric", "Stylized", "Anime", "Cartoony", "Minimalist", "Hand-drawn",
  "Isometric", "Voxel", "2.5D", "Cinematic", "Beautiful", "Abstract", "Cartoon",
  "Animation & Modeling", "Design & Illustration"
)
vehicle_keywords <- c(
  "Driving", "Flight", "Naval", "Trains", "Tanks", "Sailing", "Automobile Sim",
  "Offroad", "Vehicular Combat", "Combat Racing", "Jet", "Motorbike", "Bikes"
)
fighting_keywords <- c(
  "Fighting", "Hack and Slash", "Beat 'em up", "Spectacle fighter", "Musou",
  "Martial Arts", "2D Fighter", "3D Fighter"
)
music_keywords <- c(
  "Soundtrack", "Great Soundtrack", "Rhythm", "Music", "Electronic Music",
  "Instrumental Music", "Rock Music"
)
strat_hardcore_keywords <- c(
  "RTS", "4X", "Grand Strategy", "Tower Defense", "Wargame", "Diplomacy",
  "Real Time Tactics", "Hex Grid", "Action RTS"
)
openworld_keywords <- c("Open World", "Exploration", "Sandbox")
hardcore_keywords <- c(
  "Difficult", "Souls-like", "Physics", "Fast-Paced", "Score Attack",
  "Replay Value", "Unforgiving", "Competitive", "eSports", "e-sports"
)
management_keywords <- c(
  "Economy", "Time Management", "Life Sim", "Idler", "Clicker",
  "Inventory Management", "Automation", "Utilities", "Software", "Capitalism",
  "Management"
)
investigation_keywords <- c(
  "Mystery", "Investigation", "Detective", "Crime", "Conspiracy", "Noir",
  "Mystery Dungeon"
)
war_keywords <- c("War", "Military", "Modern", "Combat", "Destruction")
weird_keywords <- c("Psychedelic", "Surreal", "Experimental")
deep_content_keywords <- c("Psychological", "Conversation", "Philosophical", "Philisophical")
multiplayer_group_keywords <- c(
  "Multiplayer", "Co-op", "PvP", "PvE", "Online Co-Op", "Massively Multiplayer",
  "MMORPG", "Team-Based"
)

tag_groups <- list(
  tag_sports = sports_keywords,
  tag_job_sim = job_sim_keywords,
  tag_historical = historical_keywords,
  tag_local_coop = local_coop_keywords,
  tag_horror = horror_keywords,
  tag_scifi = scifi_keywords,
  tag_shooter = shooter_keywords,
  tag_narrative = narrative_keywords,
  tag_retro = retro_keywords,
  tag_roguelike = roguelike_keywords,
  tag_nsfw = nsfw_keywords,
  tag_fantasy = fantasy_keywords,
  tag_platformer = platformer_keywords,
  tag_puzzle = puzzle_keywords,
  tag_turnbased = turnbased_keywords,
  tag_survival = survival_keywords,
  tag_humor = humor_keywords,
  tag_relaxing = relaxing_keywords,
  tag_visuals = visuals_keywords,
  tag_vehicle = vehicle_keywords,
  tag_fighting = fighting_keywords,
  tag_music = music_keywords,
  tag_strat_hardcore = strat_hardcore_keywords,
  tag_openworld = openworld_keywords,
  tag_hardcore = hardcore_keywords,
  tag_management = management_keywords,
  tag_investigation = investigation_keywords,
  tag_war = war_keywords,
  tag_weird = weird_keywords,
  tag_deep_content = deep_content_keywords,
  tag_multiplayer_group = multiplayer_group_keywords,

  # Core exact tags
  tag_female_protagonist = "Female Protagonist",
  tag_char_customization = "Character Customization",
  tag_villain_protagonist = "Villain Protagonist",
  tag_silent_protagonist = "Silent Protagonist",
  tag_crpg = "CRPG",
  tag_jrpg = "JRPG",
  tag_tactical_rpg = "Tactical RPG",
  tag_party_based_rpg = "Party-Based RPG",
  tag_immersive_sim = "Immersive Sim",
  tag_tactical = "Tactical",
  tag_stealth = "Stealth",
  tag_base_building = c("Base Building", "Base-Building"),
  tag_god_game = "God Game",
  tag_2d = "2D",
  tag_3d = "3D",
  tag_first_person = "First-Person",
  tag_third_person = "Third Person",
  tag_vr = c("VR", "VR Only"),
  tag_gambling = "Gambling",
  tag_singleplayer = "Singleplayer",
  tag_lgbtq = "LGBTQ+",
  tag_tutorial = "Tutorial",
  tag_rpgmaker = "RPGMaker",
  tag_short = "Short",
  tag_top_down = "Top-Down",
  tag_controller = "Controller",
  tag_linear = "Linear",
  tag_mouse_only = "Mouse only",
  tag_3d_vision = "3D Vision",
  tag_6dof = "6DOF",

  # Additional exact tags that may be useful for descriptive or future work
  tag_robots = "Robots",
  tag_cats = "Cats",
  tag_dog = "Dog",
  tag_creature_collector = "Creature Collector",
  tag_animal_content = c("Cats", "Dog", "Birds", "Fox", "Horses"),
  tag_political = "Political",
  tag_politics = "Politics",
  tag_political_sim = "Political Sim",
  tag_diplomacy = "Diplomacy",
  tag_job_simulator = "Job Simulator",
  tag_medical_sim = "Medical Sim",
  tag_free_to_play = "Free to Play",
  tag_early_access = "Early Access"
)

tag_flags <- master %>%
  select(appid) %>%
  left_join(tag_availability, by = "appid") %>%
  mutate(has_tag_data = replace_na(has_tag_data, FALSE)) %>%
  build_exact_flags(
    item_long = tag_long,
    group_map = tag_groups,
    has_data_col = "has_tag_data"
  ) %>%
  left_join(tags_combined, by = "appid") %>%
  mutate(
    protagonist_group = case_when(
      has_tag_data != TRUE ~ "Missing tag data",
      tag_female_protagonist == 1L & tag_char_customization == 0L ~
        "Female protagonist without customization",
      tag_female_protagonist == 1L & tag_char_customization == 1L ~
        "Female protagonist with customization",
      tag_female_protagonist == 0L & tag_char_customization == 1L ~
        "Customization without female protagonist tag",
      TRUE ~ "Neither tag"
    )
  )

master <- master %>% left_join(tag_flags, by = "appid")

# ==============================================================================
# 6. GENRES
# ==============================================================================

genre_source <- bind_rows(
  master %>% transmute(appid, source = "Gamalytic", raw = genres_gamalytic_raw),
  master %>% transmute(appid, source = "Steam primary genre", raw = genre_primary)
) %>%
  mutate(raw = clean_list_text(raw), source_has_genre = !is.na(raw) & raw != "")

genre_availability <- genre_source %>%
  group_by(appid) %>%
  summarise(has_genre_data = any(source_has_genre), .groups = "drop")

genre_long <- genre_source %>%
  filter(source_has_genre) %>%
  separate_rows(raw, sep = ",\\s*") %>%
  mutate(item = str_squish(str_trim(raw))) %>%
  filter(!is.na(item), item != "") %>%
  distinct(appid, item)

genre_targets <- c("Action", "Adventure", "Simulation", "Sports", "Racing", "RPG", "Strategy")

genre_flags <- genre_long %>%
  filter(item %in% genre_targets) %>%
  mutate(value = 1L, variable = paste0("genre_", item)) %>%
  distinct(appid, variable, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = appid,
    names_from = variable,
    values_from = value,
    values_fill = 0L
  )

genre_summary <- master %>%
  select(appid) %>%
  left_join(genre_availability, by = "appid") %>%
  mutate(has_genre_data = replace_na(has_genre_data, FALSE)) %>%
  left_join(genre_flags, by = "appid")

for (nm in paste0("genre_", genre_targets)) {
  if (!nm %in% names(genre_summary)) genre_summary[[nm]] <- NA_integer_
  genre_summary[[nm]] <- case_when(
    genre_summary$has_genre_data ~ replace_na(as.integer(genre_summary[[nm]]), 0L),
    TRUE ~ NA_integer_
  )
}

genre_summary <- genre_summary %>%
  mutate(
    genre_target_count = rowSums(across(all_of(paste0("genre_", genre_targets))), na.rm = TRUE),
    genre_Other = case_when(
      !has_genre_data ~ NA_integer_,
      genre_target_count == 0 ~ 1L,
      TRUE ~ 0L
    ),
    genre_count = case_when(
      !has_genre_data ~ NA_integer_,
      genre_target_count == 0 ~ 1L,
      TRUE ~ as.integer(genre_target_count)
    )
  ) %>%
  select(-genre_target_count)

genre_special_flags <- genre_long %>%
  group_by(appid) %>%
  summarise(
    is_early_access_genre = any(item == "Early Access"),
    is_free_to_play_genre = any(item %in% c("Free to Play", "Free To Play")),
    is_casual = any(item == "Casual"),
    is_education = any(item == "Education"),
    is_MMO = any(item == "Massively Multiplayer"),
    genres_combined = paste(sort(unique(item)), collapse = ", "),
    .groups = "drop"
  )

master <- master %>%
  left_join(genre_summary, by = "appid") %>%
  left_join(genre_special_flags, by = "appid") %>%
  mutate(
    across(
      c(is_early_access_genre, is_free_to_play_genre, is_casual, is_education, is_MMO),
      ~case_when(
        has_genre_data == TRUE ~ replace_na(as.logical(.x), FALSE),
        TRUE ~ NA
      )
    )
  )

# ==============================================================================
# 7. LANGUAGES
# ==============================================================================

language_source <- master %>%
  transmute(appid, raw = clean_list_text(languages_gamalytic_raw)) %>%
  mutate(has_language_list = !is.na(raw) & raw != "")

language_long <- language_source %>%
  filter(has_language_list) %>%
  separate_rows(raw, sep = ",\\s*") %>%
  mutate(
    item = str_remove_all(raw, "<[^>]+>"),
    item = str_remove_all(item, "\\*"),
    item = str_squish(str_trim(item)),
    item = case_when(
      str_detect(item, regex("Portuguese", ignore_case = TRUE)) ~ "Portuguese",
      str_detect(item, regex("Spanish", ignore_case = TRUE)) ~ "Spanish",
      TRUE ~ item
    )
  ) %>%
  filter(!is.na(item), item != "") %>%
  distinct(appid, item)

target_languages <- c(
  "English", "Dutch", "French", "German", "Spanish", "Italian", "Russian",
  "Portuguese", "Japanese", "Korean", "Simplified Chinese",
  "Traditional Chinese", "Polish", "Turkish"
)

language_counts <- language_long %>% count(appid, name = "language_count_parsed")

language_flags <- language_long %>%
  filter(item %in% target_languages) %>%
  mutate(value = 1L, variable = paste0("lang_", str_replace_all(item, " ", "_"))) %>%
  distinct(appid, variable, .keep_all = TRUE) %>%
  pivot_wider(
    id_cols = appid,
    names_from = variable,
    values_from = value,
    values_fill = 0L
  )

language_summary <- language_source %>%
  select(appid, has_language_list) %>%
  left_join(language_counts, by = "appid") %>%
  left_join(language_flags, by = "appid")

language_flag_names <- paste0("lang_", str_replace_all(target_languages, " ", "_"))
for (nm in language_flag_names) {
  if (!nm %in% names(language_summary)) language_summary[[nm]] <- NA_integer_
  language_summary[[nm]] <- case_when(
    language_summary$has_language_list ~ replace_na(as.integer(language_summary[[nm]]), 0L),
    TRUE ~ NA_integer_
  )
}

language_summary <- language_summary %>%
  mutate(
    language_count = coalesce(language_count_parsed, as.integer(master$languages_count_steam[match(appid, master$appid)])),
    language_count_source = case_when(
      !is.na(language_count_parsed) ~ "Parsed Gamalytic language list",
      !is.na(language_count) ~ "Steam languages_count",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-language_count_parsed)

master <- master %>% left_join(language_summary, by = "appid")

# ==============================================================================
# 8. FEATURES
# ==============================================================================

feature_source <- master %>%
  transmute(appid, raw = clean_list_text(features_gamalytic_raw)) %>%
  mutate(has_feature_data = !is.na(raw) & raw != "")

feature_long <- feature_source %>%
  filter(has_feature_data) %>%
  separate_rows(raw, sep = ",\\s*") %>%
  mutate(item = str_squish(str_trim(raw))) %>%
  filter(!is.na(item), item != "") %>%
  distinct(appid, item)

feature_summary <- feature_long %>%
  arrange(appid, item) %>%
  group_by(appid) %>%
  summarise(
    feature_count = n_distinct(item),
    features_combined = paste(item, collapse = ", "),
    .groups = "drop"
  )

feature_groups <- list(
  feat_Multiplayer = c("Online PvP", "Online Co-op", "Cross-Platform Multiplayer", "LAN PvP", "LAN Co-op", "MMO"),
  feat_Local_Multiplayer = c("Shared/Split Screen", "Remote Play Together", "Shared/Split Screen PvP", "Shared/Split Screen Co-op"),
  feat_VR = c("VR Only", "VR Supported", "Tracked Controller Support"),
  feat_Workshop_Editor = c("Steam Workshop", "Includes level editor"),
  feat_Achievements = "Steam Achievements",
  feat_Cloud = "Steam Cloud",
  feat_InAppPurchases = "In-App Purchases",
  feat_Stats = "Stats",
  feat_SinglePlayer = "Single-player",
  feat_HDR = "HDR available",
  feat_DLC = "Downloadable Content",
  feat_Leaderboards = "Steam Leaderboards"
)

feature_flags <- feature_source %>%
  select(appid, has_feature_data) %>%
  build_exact_flags(
    item_long = feature_long,
    group_map = feature_groups,
    has_data_col = "has_feature_data"
  )

master <- master %>%
  left_join(feature_flags, by = "appid") %>%
  left_join(feature_summary, by = "appid")

# ==============================================================================
# 9. COUNTRY DATA - RETAIN ALL OBSERVED CODES
# ==============================================================================

country_source <- master %>%
  transmute(appid, raw = as.character(players_by_country_raw)) %>%
  mutate(
    raw = na_if(str_squish(raw), ""),
    json_clean = str_replace_all(raw, '""', '"'),
    matches = str_match_all(
      json_clean,
      '"code"\\s*:\\s*"([^"]+)"[^}]*"percentage"\\s*:\\s*([0-9.]+)'
    )
  )

country_long <- country_source %>%
  transmute(
    appid,
    parsed = map(matches, function(m) {
      if (is.null(m) || nrow(m) == 0) {
        return(tibble(code = character(), percentage = numeric()))
      }
      tibble(
        code = as.character(m[, 2]),
        percentage = suppressWarnings(as.numeric(m[, 3]))
      )
    })
  ) %>%
  unnest(parsed) %>%
  filter(!is.na(code), code != "", code != "Others", !is.na(percentage), percentage > 0) %>%
  group_by(appid, code) %>%
  summarise(percentage = max(percentage), .groups = "drop")

country_summary <- country_long %>%
  group_by(appid) %>%
  summarise(
    has_country_data = TRUE,
    n_reported_countries = n_distinct(code),
    reported_country_sum = sum(percentage, na.rm = TRUE),
    .groups = "drop"
  )

country_wide <- country_long %>%
  mutate(variable = paste0("pct_", code)) %>%
  select(appid, variable, percentage) %>%
  pivot_wider(
    id_cols = appid,
    names_from = variable,
    values_from = percentage
  )

country_data <- master %>%
  select(appid) %>%
  left_join(country_summary, by = "appid") %>%
  mutate(
    has_country_data = replace_na(has_country_data, FALSE),
    n_reported_countries = if_else(has_country_data, n_reported_countries, NA_integer_),
    reported_country_sum = if_else(has_country_data, reported_country_sum, NA_real_)
  ) %>%
  left_join(country_wide, by = "appid")

pct_columns <- names(country_data)[startsWith(names(country_data), "pct_")]
for (nm in pct_columns) {
  country_data[[nm]] <- case_when(
    country_data$has_country_data ~ replace_na(as.numeric(country_data[[nm]]), 0),
    TRUE ~ NA_real_
  )
}

master <- master %>% left_join(country_data, by = "appid")

# ==============================================================================
# 10. PRICES, FREE-TO-PLAY, REVIEWS AND DERIVED DESCRIPTIVE VARIABLES
# ==============================================================================

master <- master %>%
  mutate(
    price_current = coalesce(price_current_steam, price_current_gamalytic),

    # A transparent cleaned price. Current price is used only as a flagged
    # fallback when the original price is unavailable for a paid game.
    is_free = combine_any_true(
      is_free_steam,
      is_free_to_play_genre,
      tag_free_to_play == 1L,
      if_else(!is.na(price_initial_observed), price_initial_observed == 0, NA)
    ),
    is_early_access = combine_any_true(
      is_early_access_steam,
      is_early_access_genre,
      tag_early_access == 1L
    ),

    price_initial_fallback_current =
      is.na(price_initial_observed) & is_free != TRUE & !is.na(price_current),

    price_initial = case_when(
      !is.na(price_initial_observed) ~ price_initial_observed,
      is_free == TRUE ~ 0,
      price_initial_fallback_current ~ price_current,
      TRUE ~ NA_real_
    ),
    price_initial_source = case_when(
      !is.na(price_initial_observed) ~ "Observed Steam original price",
      is_free == TRUE ~ "Set to zero for free game",
      price_initial_fallback_current ~ "Current price fallback",
      TRUE ~ NA_character_
    ),
    is_paid = case_when(
      is_free == TRUE ~ FALSE,
      !is.na(price_initial) & price_initial > 0 ~ TRUE,
      TRUE ~ NA
    ),

    # Preserve observed zeros and missing values. No revenue imputation here.
    revenue_original = revenue,
    has_positive_revenue = !is.na(revenue_original) & revenue_original > 0,
    revenue_missing_or_zero = is.na(revenue_original) | revenue_original <= 0,
    log_revenue = if_else(has_positive_revenue, log(revenue_original), NA_real_),
    log1p_revenue = if_else(!is.na(revenue_original) & revenue_original >= 0, log1p(revenue_original), NA_real_),

    total_reviews = case_when(
      !is.na(positive_reviews) & !is.na(negative_reviews) ~ positive_reviews + negative_reviews,
      TRUE ~ NA_real_
    ),
    positive_review_share = case_when(
      !is.na(total_reviews) & total_reviews > 0 ~ positive_reviews / total_reviews,
      TRUE ~ NA_real_
    ),
    review_score_raw = review_score,
    review_score = case_when(
      review_score_raw == 0 & (is.na(total_reviews) | total_reviews == 0) ~ NA_real_,
      TRUE ~ review_score_raw
    ),

    # Do not automatically convert missing achievement counts to zero unless
    # the feature data explicitly indicates that achievements are absent.
    achievement_count = case_when(
      !is.na(achievement_count_raw) ~ achievement_count_raw,
      has_feature_data == TRUE & feat_Achievements == 0L ~ 0,
      TRUE ~ NA_real_
    ),

    log1p_copies_sold = if_else(!is.na(copies_sold) & copies_sold >= 0, log1p(copies_sold), NA_real_),
    log1p_owners = if_else(!is.na(owners) & owners >= 0, log1p(owners), NA_real_),
    log1p_followers = if_else(!is.na(followers) & followers >= 0, log1p(followers), NA_real_),
    log1p_peak_ccu = if_else(!is.na(peak_ccu) & peak_ccu >= 0, log1p(peak_ccu), NA_real_)
  )

# ==============================================================================
# 11. OPTIONAL HARDWARE VARIABLES
# ==============================================================================

master <- master %>%
  mutate(
    score_gpu = NA_real_,
    gpu_raw = NA_real_,
    gpu_tech_spread = NA_real_,
    gpu_match_found = NA,
    gpu_integrated_proxy = NA
  )

if (RUN_HARDWARE_BLOCK && file.exists(PATH_GPU_BENCHMARKS)) {
  if (!requireNamespace("stringdist", quietly = TRUE)) {
    warning("Package 'stringdist' is not installed. Hardware variables were left missing.")
  } else {
    clean_hardware_text <- function(text) {
      text %>%
        str_to_lower() %>%
        str_replace_all("<[^>]+>", " ") %>%
        str_replace_all("nvidia|geforce", "nv") %>%
        str_replace_all("radeon", "amd") %>%
        str_replace("(\\s+or\\s+|\\s*[/|]\\s*).*$", "") %>%
        str_remove_all("\\b(corporation|ati|desktop|graphics|video card|cpu|gpu|accelerator)\\b") %>%
        str_remove_all("\\b\\d+\\s*(gb|mb|g)\\b") %>%
        str_remove_all("\\b(dedicated|vram|memory|compliant|shader|model|directx|dx\\d*|opengl|version)\\b") %>%
        str_remove_all("\\b(the|an|a|gen|tm|series|compatible|requires|minimum|recommended)\\b") %>%
        str_replace_all("[^a-z0-9\\s]", " ") %>%
        str_squish() %>%
        str_replace_all("\\b(\\w+)(\\s+\\1)+\\b", "\\1")
    }

    extract_gpu_section <- function(text) {
      clean_text <- str_replace_all(text, "<[^>]+>", " ")
      pattern <- "(?si)(?:Graphics|Video|Video Card|GPU)\\s*[:\\-]?\\s*(.*?)(?=(?:DirectX|Storage|Sound|Hard Drive|Network|OS|Processor|Memory|Other|$))"
      main_section <- str_extract(clean_text, pattern)
      extra <- str_extract(clean_text, "(?si)Additional.*")
      ifelse(
        (!is.na(main_section) & nchar(main_section) < 50) | is.na(main_section),
        paste(ifelse(is.na(main_section), "", main_section), ifelse(is.na(extra), "", extra)),
        main_section
      )
    }

    get_best_match_score <- function(inputs, candidates, scores) {
      unique_inputs <- unique(inputs)
      unique_inputs <- unique_inputs[
        !is.na(unique_inputs) & unique_inputs != "" & nchar(unique_inputs) > 2
      ]
      match_idx <- stringdist::amatch(unique_inputs, candidates, method = "lv", maxDist = 4)
      lookup <- tibble(original = unique_inputs, found_score = scores[match_idx])
      tibble(original = inputs) %>%
        left_join(lookup, by = "original") %>%
        pull(found_score)
    }

    gpu_db <- fread(PATH_GPU_BENCHMARKS) %>%
      transmute(
        match_key = clean_hardware_text(gpuName),
        match_score = as.numeric(G3Dmark)
      ) %>%
      filter(!is.na(match_key), match_key != "", !is.na(match_score), match_score > 0)

    missing_gpus <- tribble(
      ~gpuName, ~G3Dmark,
      "GeForce RTX 4090", 39000,
      "GeForce RTX 4080", 35000,
      "GeForce RTX 4070 Ti", 31000,
      "GeForce RTX 4070", 27000,
      "Radeon RX 7900 XTX", 31000,
      "Intel Arc A770", 13000
    ) %>%
      transmute(match_key = clean_hardware_text(gpuName), match_score = G3Dmark)

    gpu_db <- bind_rows(gpu_db, missing_gpus) %>% distinct(match_key, .keep_all = TRUE)

    known_brands <- "NVIDIA|AMD|RADEON|GEFORCE|GTX|RTX|INTEL|ARC|ATI|QUADRO"

    hardware_temp <- master %>%
      transmute(
        appid,
        release_year,
        pc_req_min_steam,
        raw_gpu_text = extract_gpu_section(pc_req_min_steam),
        clean_gpu_req = clean_hardware_text(raw_gpu_text),
        has_gpu_brand = str_detect(pc_req_min_steam, regex(known_brands, ignore_case = TRUE)),
        gpu_is_integrated = str_detect(
          pc_req_min_steam,
          regex("Intel|Integrated|Shared|UHD|Iris|HD Graphics", ignore_case = TRUE)
        )
      )

    hardware_temp$gpu_score_fuzzy <- ifelse(
      hardware_temp$has_gpu_brand & nchar(hardware_temp$clean_gpu_req) > 2,
      get_best_match_score(hardware_temp$clean_gpu_req, gpu_db$match_key, gpu_db$match_score),
      NA_real_
    )

    yearly_low <- hardware_temp %>%
      filter(!is.na(gpu_score_fuzzy), gpu_score_fuzzy > 0, !is.na(release_year)) %>%
      group_by(release_year) %>%
      summarise(low_end_gpu = quantile(gpu_score_fuzzy, 0.10, na.rm = TRUE), .groups = "drop")

    hardware_raw <- hardware_temp %>%
      left_join(yearly_low, by = "release_year") %>%
      mutate(
        gpu_raw_new = case_when(
          !is.na(gpu_score_fuzzy) ~ gpu_score_fuzzy,
          gpu_is_integrated & !is.na(low_end_gpu) ~ low_end_gpu,
          TRUE ~ NA_real_
        ),
        gpu_match_found_new = !is.na(gpu_score_fuzzy),
        gpu_integrated_proxy_new = is.na(gpu_score_fuzzy) & gpu_is_integrated & !is.na(low_end_gpu)
      )

    yearly_stats <- hardware_raw %>%
      filter(!is.na(gpu_raw_new), gpu_raw_new > 0, !is.na(release_year)) %>%
      group_by(release_year) %>%
      summarise(
        p01_log = quantile(log(gpu_raw_new), 0.01, na.rm = TRUE),
        p99_log = quantile(log(gpu_raw_new), 0.99, na.rm = TRUE),
        gpu_tech_spread_new = p99_log - p01_log,
        .groups = "drop"
      )

    hardware_final <- hardware_raw %>%
      left_join(yearly_stats, by = "release_year") %>%
      mutate(
        score_gpu_new = case_when(
          !is.na(gpu_raw_new) & !is.na(gpu_tech_spread_new) & gpu_tech_spread_new > 0 ~
            pmax(0, pmin(1, (log(gpu_raw_new) - p01_log) / gpu_tech_spread_new)) * 100,
          TRUE ~ NA_real_
        ),
        score_gpu_new = round(score_gpu_new, 1)
      ) %>%
      select(
        appid,
        score_gpu_new,
        gpu_raw_new,
        gpu_tech_spread_new,
        gpu_match_found_new,
        gpu_integrated_proxy_new
      )

    master <- master %>%
      select(-score_gpu, -gpu_raw, -gpu_tech_spread, -gpu_match_found, -gpu_integrated_proxy) %>%
      left_join(hardware_final, by = "appid") %>%
      rename(
        score_gpu = score_gpu_new,
        gpu_raw = gpu_raw_new,
        gpu_tech_spread = gpu_tech_spread_new,
        gpu_match_found = gpu_match_found_new,
        gpu_integrated_proxy = gpu_integrated_proxy_new
      )
  }
}

# ==============================================================================
# 12. PUBLISHER DATA - NO AUTOMATIC IMPUTATION FOR UNMATCHED PUBLISHERS
# ==============================================================================

if (file.exists(PATH_PUBLISHERS_RAW)) {
  publisher_raw <- fread(PATH_PUBLISHERS_RAW, na.strings = c("", "NA", "N/A"))
  names(publisher_raw) <- snake_case(names(publisher_raw))

  # Remove columns that are clearly non-game categories or duplicate artifacts.
  publisher_raw <- publisher_raw %>%
    select(-any_of(c(
      "movie", "photo_editing", "web_publishing", "game_development",
      "video_production", "animation_modeling", "accounting", "free_to_play_1",
      "utilities", "software_training", "audio_production", "genres"
    )))

  if (!"name" %in% names(publisher_raw)) {
    warning("Publisher file has no 'name' column. Publisher variables were not merged.")
    master <- master %>% mutate(publisher_matched = NA)
  } else {
    publisher_clean <- publisher_raw %>%
      mutate(
        publisher_name_source = name,
        publisher_join_key = normalize_company_key(name)
      ) %>%
      filter(!is.na(publisher_join_key)) %>%
      distinct(publisher_join_key, .keep_all = TRUE)

    # Rename core metadata first, only when the source column exists.
    rename_if_present <- function(df, old, new) {
      if (old %in% names(df)) names(df)[names(df) == old] <- new
      df
    }
    publisher_clean <- publisher_clean %>%
      rename_if_present("total_revenue", "pub_total_revenue") %>%
      rename_if_present("average_revenue", "pub_average_revenue") %>%
      rename_if_present("median_revenue", "pub_median_revenue") %>%
      rename_if_present("class", "pub_class_source") %>%
      rename_if_present("in_house", "pub_in_house_raw") %>%
      rename_if_present("number_of_games", "pub_number_of_games")

    required_pub_core <- c(
      "pub_total_revenue", "pub_average_revenue", "pub_median_revenue",
      "pub_class_source", "pub_in_house_raw", "pub_number_of_games"
    )
    for (nm in required_pub_core) {
      if (!nm %in% names(publisher_clean)) publisher_clean[[nm]] <- NA
    }

    protected_cols <- c(
      "name", "publisher_name_source", "publisher_join_key",
      "pub_total_revenue", "pub_average_revenue", "pub_median_revenue",
      "pub_class_source", "pub_in_house_raw", "pub_number_of_games"
    )

    publisher_profile_cols <- setdiff(names(publisher_clean), protected_cols)
    if (length(publisher_profile_cols) > 0) {
      publisher_clean <- publisher_clean %>%
        rename_with(~paste0("pub_genre_", .x), all_of(publisher_profile_cols))
    }

    # Manual class corrections from the previous script are retained, but are
    # explicitly flagged and never overwrite the source silently.
    manual_class_overrides <- tribble(
      ~publisher_join_key, ~pub_class_override_value,
      normalize_company_key("Jackbox Games"), "Indie",
      normalize_company_key("Koei Tecmo"), "AAA",
      normalize_company_key("Blizzard Entertainment"), "AAA",
      normalize_company_key("FromSoftware"), "AAA",
      normalize_company_key("Amazon Games"), "AA",
      normalize_company_key("Capcom"), "AAA",
      normalize_company_key("NetEase Games"), "AAA",
      normalize_company_key("PopCap Games"), "AA"
    )

    publisher_clean <- publisher_clean %>%
      left_join(manual_class_overrides, by = "publisher_join_key") %>%
      mutate(
        pub_class_manual_override = !is.na(pub_class_override_value),
        pub_class = coalesce(pub_class_override_value, pub_class_source)
      ) %>%
      select(-pub_class_override_value)

    master <- master %>%
      mutate(publisher_join_key = normalize_company_key(primary_publisher)) %>%
      left_join(publisher_clean, by = "publisher_join_key") %>%
      mutate(
        publisher_matched = !is.na(publisher_name_source),
        pub_class_aaa = if_else(publisher_matched, as.integer(pub_class == "AAA"), NA_integer_),
        pub_class_aa = if_else(publisher_matched, as.integer(pub_class == "AA"), NA_integer_),
        pub_class_indie = if_else(publisher_matched, as.integer(pub_class == "Indie"), NA_integer_),
        pub_class_hobbyist = if_else(publisher_matched, as.integer(pub_class == "Hobbyist"), NA_integer_)
      ) %>%
      select(-name)
  }
} else {
  warning("Publisher file not found. Publisher-level variables were omitted.")
  master <- master %>% mutate(publisher_matched = NA)
}

# ==============================================================================
# 13. FINAL CLEANUP AND COLUMN ORDER
# ==============================================================================

# Remove raw fields whose meaning was not sufficiently documented for active use.
# They remain available in the original raw source files if needed later.
master <- master %>%
  select(
    -title_steam,
    -title_gamalytic,
    -release_date_steam,
    -release_date_gamalytic,
    -achievement_count_raw
  )

if (!KEEP_RAW_TEXT_COLUMNS) {
  master <- master %>%
    select(-any_of(c(
      "developer_steam_raw", "developer_gamalytic_raw",
      "publisher_steam_raw", "publisher_gamalytic_raw",
      "tags_steam_raw", "tags_gamalytic_raw",
      "genres_gamalytic_raw", "languages_gamalytic_raw",
      "features_gamalytic_raw", "players_by_country_raw",
      "pc_req_min_steam", "developer_raw", "publisher_raw"
    )))
}

# Put the most useful variables first while retaining all generated tag,
# language, feature, country and publisher-profile columns.
master <- master %>%
  select(
    appid, title,
    release_date, release_year, release_month, release_day, age_years, release_date_source,
    genre_primary, genres_combined, has_genre_data,
    is_early_access, is_early_access_steam, is_early_access_genre, is_casual, is_education, is_MMO,
    genre_count, starts_with("genre_"),

    is_free, is_paid,
    price_initial, price_initial_observed, price_initial_fallback_current,
    price_initial_source, price_current, price_current_steam,
    price_current_gamalytic, discount_percent,

    revenue, revenue_original, has_positive_revenue, revenue_missing_or_zero,
    log_revenue, log1p_revenue,
    copies_sold, log1p_copies_sold, copies_sold_last_7d,
    owners, log1p_owners, owners_min, owners_max, owners_midpoint,

    positive_reviews, negative_reviews, total_reviews, positive_review_share,
    review_score, review_score_raw, ccu, peak_ccu, log1p_peak_ccu,
    avg_concurrent_players, avg_playtime, followers, log1p_followers,

    language_count, language_count_source, languages_count_steam,
    has_language_list, starts_with("lang_"),

    achievement_count, dlc_count, steam_deck, controller_support,
    score_gpu, gpu_raw, gpu_tech_spread, gpu_match_found, gpu_integrated_proxy,

    has_tag_data, tag_count, protagonist_group, tags_combined, starts_with("tag_"),
    has_feature_data, feature_count, features_combined, starts_with("feat_"),

    has_country_data, n_reported_countries, reported_country_sum, starts_with("pct_"),

    developer_count, developer_count_3plus, is_multi_developer, primary_developer,
    publisher_count, publisher_count_3plus, is_multi_publisher, primary_publisher,
    is_self_published, publisher_matched, starts_with("pub_"),

    everything()
  )

# Guarantee one row per appid.
if (anyDuplicated(master$appid) > 0) {
  stop("The final dataset contains duplicate appids. Review joins before saving.")
}

# ==============================================================================
# 14. VALIDATION REPORT
# ==============================================================================

cat("\n================ CLEANING VALIDATION ================\n")
cat("Steam raw unique appids:          ", nrow(steam_raw), "\n")
cat("Gamalytic raw unique appids:      ", nrow(gamalytic_raw), "\n")
cat("Final merged released games:      ", nrow(master), "\n")
cat("Unique final appids:              ", n_distinct(master$appid), "\n")
cat("Games with tag data:              ", sum(master$has_tag_data == TRUE, na.rm = TRUE), "\n")
cat("Female Protagonist games:         ", sum(master$tag_female_protagonist == 1, na.rm = TRUE), "\n")
cat("Character Customization games:    ", sum(master$tag_char_customization == 1, na.rm = TRUE), "\n")
cat("Games with country data:          ", sum(master$has_country_data == TRUE, na.rm = TRUE), "\n")
cat("Games with positive revenue:      ", sum(master$has_positive_revenue, na.rm = TRUE), "\n")
cat("Games using current-price fallback:", sum(master$price_initial_fallback_current, na.rm = TRUE), "\n")
if ("publisher_matched" %in% names(master)) {
  cat("Games matched to publisher data:  ", sum(master$publisher_matched == TRUE, na.rm = TRUE), "\n")
}
cat("Country columns created:          ", length(names(master)[startsWith(names(master), "pct_")]), "\n")
cat("=====================================================\n\n")

# A small missingness report for important variables.
important_vars <- intersect(
  c(
    "tag_female_protagonist", "tag_char_customization", "revenue", "price_initial",
    "positive_reviews", "negative_reviews", "peak_ccu", "language_count",
    "has_country_data", "primary_publisher", "score_gpu"
  ),
  names(master)
)

missingness_report <- tibble(
  variable = important_vars,
  n_missing = map_int(important_vars, ~sum(is.na(master[[.x]]))),
  pct_missing = round(100 * n_missing / nrow(master), 2)
)
print(missingness_report, n = Inf)

# ==============================================================================
# 15. SAVE
# ==============================================================================

fwrite(master, OUTPUT_FILE, na = "")
cat("\nSaved corrected master dataset to:\n", OUTPUT_FILE, "\n")
