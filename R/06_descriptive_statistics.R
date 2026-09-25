# ==============================================================================
# Chapter 4 - descriptive statistics, final tables and figures
# ==============================================================================
# Recreates Tables 1-4 and Figures 1-2 used in the paper. Exploratory analyses
# unrelated to the present study are intentionally excluded.
# ==============================================================================

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data") || !dir.exists("results")) {
  stop("Run this script from the repository root (the folder containing R/, data/ and results/).")
}

required_packages <- c("data.table", "ggplot2", "scales", "openxlsx", "webshot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the required packages first: ", paste(missing_packages, collapse = ", "))
}
library(data.table)

# ------------------------------------------------------------------------------
# 1. Inputs and output folders
# ------------------------------------------------------------------------------

master_file <- "data/processed/Clean_games_data_feb_2026_v2.csv"
h2_main_file <- "data/processed/H2_main_GGGI_2025_with_Economic.csv"
h2_80_file <- "data/processed/H2_robustness_80pct_GGGI_2025_with_Economic.csv"
required_files <- c(master_file, h2_main_file, h2_80_file)
if (any(!file.exists(required_files))) {
  stop("Missing required input files: ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

table_dir <- "results/tables"
figure_dir <- "results/figures"
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

master <- fread(master_file, na.strings = c("", "NA"), showProgress = TRUE)
h2_main <- fread(h2_main_file, na.strings = c("", "NA"))
h2_80 <- fread(h2_80_file, na.strings = c("", "NA"))

as_binary <- function(x, label) {
  s <- tolower(trimws(as.character(x)))
  missing <- is.na(x) | s %in% c("", "na", "nan")
  valid <- missing | s %in% c("0", "1", "true", "false", "t", "f")
  if (any(!valid)) stop("Unexpected binary coding in ", label)
  out <- rep(NA_integer_, length(x))
  out[!missing & s %in% c("1", "true", "t")] <- 1L
  out[!missing & s %in% c("0", "false", "f")] <- 0L
  out
}

binary_master <- c(
  "is_non_game_product", "has_tag_data", "tag_female_protagonist",
  "has_country_data", "is_free", "is_early_access"
)
master_required <- c(
  "appid", binary_master, "revenue", "price_current", "genre_primary_clean"
)
missing_master_columns <- setdiff(master_required, names(master))
if (length(missing_master_columns)) {
  stop("Master file is missing: ", paste(missing_master_columns, collapse = ", "))
}
for (nm in binary_master) set(master, j = nm, value = as_binary(master[[nm]], nm))

h2_required <- c(
  "appid", "tag_female_protagonist", "weighted_gggi_2025",
  "weighted_economic_participation_2025"
)
if (!all(h2_required %in% names(h2_main)) || !all(h2_required %in% names(h2_80))) {
  stop("An H2 input file is missing required descriptive-statistics columns.")
}
h2_main[, tag_female_protagonist := as_binary(
  tag_female_protagonist, "H2 main: tag_female_protagonist"
)]
h2_80[, tag_female_protagonist := as_binary(
  tag_female_protagonist, "H2 80%: tag_female_protagonist"
)]

if (anyDuplicated(master$appid) || anyDuplicated(h2_main$appid) || anyDuplicated(h2_80$appid)) {
  stop("Duplicate appids were found in an input file.")
}

# ------------------------------------------------------------------------------
# 2. Samples and size audit
# ------------------------------------------------------------------------------

all_games <- master[is_non_game_product == 0L]
games_with_tags <- all_games[has_tag_data == 1L]
h1 <- games_with_tags[!is.na(revenue) & revenue > 0]
h1[, log_revenue := log(revenue)]
h2_candidates <- games_with_tags[has_country_data == 1L]

expected_sizes <- c(
  all_games = 139059L, games_with_tags = 138961L, h1 = 79891L,
  h2_candidates = 33766L, h2_main = 23053L, h2_80 = 26088L
)
actual_sizes <- c(
  all_games = nrow(all_games), games_with_tags = nrow(games_with_tags),
  h1 = nrow(h1), h2_candidates = nrow(h2_candidates),
  h2_main = nrow(h2_main), h2_80 = nrow(h2_80)
)
if (!identical(unname(actual_sizes), unname(expected_sizes))) {
  stop(
    "Unexpected descriptive sample sizes. Expected: ",
    paste(names(expected_sizes), expected_sizes, sep = "=", collapse = ", "),
    "; found: ",
    paste(names(actual_sizes), actual_sizes, sep = "=", collapse = ", ")
  )
}

h1_female <- sum(h1$tag_female_protagonist == 1L)
h1_no_female <- sum(h1$tag_female_protagonist == 0L)
if (h1_female != 8458L || h1_no_female != 71433L) {
  stop("Unexpected Female Protagonist composition in H1.")
}

# ------------------------------------------------------------------------------
# 3. Tables 1 and 2
# ------------------------------------------------------------------------------

table1 <- data.table(
  Sample = c(
    "Full dataset", "Games with tag data", "H1 sample",
    "Female Protagonist in H1", "No Female Protagonist tag in H1",
    "H2 candidate sample", "H2 main sample", "H2 robustness sample"
  ),
  Value = c(
    "139,059", "138,961", "79,891", "8,458 (10.59%)",
    "71,433 (89.41%)", "33,766", "23,053", "26,088"
  ),
  `Short Explanation` = c(
    "Total number of games in the merged dataset",
    "Games with available tag information",
    "Positive estimated revenue and tag data",
    "Games tagged as Female Protagonist within H1",
    "H1 games without the Female Protagonist tag",
    "Games with country-share data and tag data",
    "Full official GGGI 2025 coverage",
    "At least 80% official GGGI 2025 coverage"
  )
)

continuous_summary <- function(x) {
  x <- as.numeric(x)
  if (any(!is.finite(x))) stop("A Table 2 variable contains nonfinite values.")
  c(
    N = length(x), Mean = mean(x), `St. Dev.` = sd(x), Min = min(x),
    P25 = unname(quantile(x, 0.25)), Median = median(x),
    P75 = unname(quantile(x, 0.75)), Max = max(x)
  )
}

table2_variables <- list(
  "Estimated Revenue (USD)" = h1$revenue,
  "Ln(Revenue)" = h1$log_revenue,
  "Female Protagonist" = h1$tag_female_protagonist,
  "Is Free" = h1$is_free,
  "Early Access" = h1$is_early_access,
  "Current Price (USD)" = h1$price_current
)
if (any(vapply(table2_variables, anyNA, logical(1)))) {
  stop("A Table 2 variable contains missing values in the H1 descriptive sample.")
}
table2 <- rbindlist(lapply(names(table2_variables), function(label) {
  as.data.table(as.list(c(Variable = label, continuous_summary(table2_variables[[label]]))))
}), fill = TRUE)
numeric_table2 <- setdiff(names(table2), "Variable")
table2[, (numeric_table2) := lapply(.SD, as.numeric), .SDcols = numeric_table2]

# ------------------------------------------------------------------------------
# 4. Tables 3 and 4
# ------------------------------------------------------------------------------

distribution_row <- function(data, outcome, label) {
  x <- data[[outcome]]
  x <- x[is.finite(x)]
  data.table(
    Sample = label, N = length(x), Mean = mean(x), `St. Dev.` = sd(x),
    Min = min(x), Q1 = unname(quantile(x, 0.25)), Median = median(x),
    Q3 = unname(quantile(x, 0.75)), Max = max(x)
  )
}

make_h2_descriptive <- function(outcome) {
  rbindlist(list(
    distribution_row(h2_main, outcome, "Main sample - All"),
    distribution_row(
      h2_main[tag_female_protagonist == 1L], outcome,
      "Main sample - Female Protagonist"
    ),
    distribution_row(
      h2_main[tag_female_protagonist == 0L], outcome,
      "Main sample - No Female Protagonist tag"
    ),
    distribution_row(h2_80, outcome, "80% robustness - All"),
    distribution_row(
      h2_80[tag_female_protagonist == 1L], outcome,
      "80% robustness - Female Protagonist"
    ),
    distribution_row(
      h2_80[tag_female_protagonist == 0L], outcome,
      "80% robustness - No Female Protagonist tag"
    )
  ))
}

table3 <- make_h2_descriptive("weighted_gggi_2025")
table4 <- make_h2_descriptive("weighted_economic_participation_2025")
expected_h2_groups <- c(23053L, 2934L, 20119L, 26088L, 3431L, 22657L)
if (!identical(table3$N, expected_h2_groups)) stop("Unexpected group sizes in Table 3.")
if (!identical(table4$N, expected_h2_groups)) stop("Unexpected group sizes in Table 4.")

# ------------------------------------------------------------------------------
# 5. CSV and Excel outputs
# ------------------------------------------------------------------------------

fwrite(table1, file.path(table_dir, "Table_1_Analytical_Samples.csv"), na = "")
fwrite(table2, file.path(table_dir, "Table_2_H1_Descriptive_Statistics.csv"), na = "")
fwrite(table3, file.path(table_dir, "Table_3_H2_Weighted_GGGI.csv"), na = "")
fwrite(table4, file.path(table_dir, "Table_4_H2_Economic_Participation.csv"), na = "")

workbook_file <- file.path(table_dir, "Chapter_4_descriptive_tables.xlsx")
wb <- openxlsx::createWorkbook()
workbook_tables <- list(
  "Table 1 - Samples" = table1,
  "Table 2 - H1" = table2,
  "Table 3 - H2 GGGI" = table3,
  "Table 4 - Economic" = table4
)
header_style <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#0B4EB8", textDecoration = "bold",
  halign = "center", border = "Bottom", borderColour = "#8FB2E3"
)
body_style <- openxlsx::createStyle(
  border = c("top", "bottom", "left", "right"), borderColour = "#D7E4F5"
)
for (sheet_name in names(workbook_tables)) {
  tab <- workbook_tables[[sheet_name]]
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, as.data.frame(tab), headerStyle = header_style)
  openxlsx::addStyle(
    wb, sheet_name, body_style,
    rows = 2:(nrow(tab) + 1L), cols = seq_len(ncol(tab)), gridExpand = TRUE
  )
  openxlsx::freezePane(wb, sheet_name, firstRow = TRUE)
  openxlsx::setColWidths(wb, sheet_name, cols = seq_len(ncol(tab)), widths = "auto")
}
openxlsx::saveWorkbook(wb, workbook_file, overwrite = TRUE)

# ------------------------------------------------------------------------------
# 6. HTML and PNG versions of Tables 1-4
# ------------------------------------------------------------------------------

html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
fmt0 <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")

render_table1 <- function() {
  rows <- paste0(
    "<tr><td class='row-label'>", html_escape(table1$Sample),
    "</td><td class='value'>", html_escape(table1$Value),
    "</td><td>", html_escape(table1[["Short Explanation"]]), "</td></tr>",
    collapse = ""
  )
  paste0(
    "<section class='table1-section'><div class='title'>Table 1: Analytical Samples and Data Availability",
    "<div class='subtitle'>Sample construction for the full game dataset and the H1/H2 analytical samples</div></div>",
    "<table><thead><tr><th>Sample</th><th>Value</th><th>Short Explanation</th></tr></thead><tbody>",
    rows, "</tbody></table></section>"
  )
}

table2_stat_labels <- c("Mean", "St. Dev.", "Min", "P25", "Median", "P75", "Max")
render_table2 <- function() {
  continuous_variables <- c(
    "Estimated Revenue (USD)", "Ln(Revenue)", "Current Price (USD)"
  )
  continuous_rows <- match(continuous_variables, table2$Variable)
  if (anyNA(continuous_rows)) stop("A continuous Table 2 variable could not be found.")
  rows_a <- ""
  for (stat in table2_stat_labels) {
    shown <- vapply(continuous_rows, function(i) fmt2(table2[[stat]][[i]]), character(1))
    rows_a <- paste0(
      rows_a, "<tr><td class='row-label'>", html_escape(stat), "</td>",
      "<td class='value'>", shown[[1]], "</td>",
      "<td class='value'>", shown[[2]], "</td>",
      "<td class='value'>", shown[[3]], "</td></tr>"
    )
  }

  characteristic_labels <- c("Female Protagonist", "Free to Play", "Early Access")
  characteristic_shares <- 100 * c(
    mean(h1$tag_female_protagonist == 1L),
    mean(h1$is_free == 1L),
    mean(h1$is_early_access == 1L)
  )
  rows_b <- paste0(
    "<tr><td class='row-label'>", characteristic_labels,
    "</td><td class='value'>",
    formatC(characteristic_shares, format = "f", digits = 1),
    "%</td></tr>",
    collapse = ""
  )

  paste0(
    "<section><div class='title'>Table 2: Descriptive Statistics - H1 Sample",
    "<div class='subtitle'>Games with positive estimated revenue and available tag data (N = 79,891)</div></div>",
    "<table class='compact-table2'><tbody>",
    "<tr class='panel-title'><td colspan='4'>Panel A: Revenue and Price</td></tr>",
    "<tr class='panel-header'><th>Statistic</th><th>Estimated Revenue (USD)</th>",
    "<th>Ln(Revenue)</th><th>Current Price (USD)</th></tr>",
    rows_a, "</tbody></table>",
    "<table class='compact-binary'><tbody>",
    "<tr class='panel-title'><td colspan='2'>Panel B: Share of Games with Selected Characteristics</td></tr>",
    "<tr class='panel-header'><th>Characteristic</th><th>Share of Sample</th></tr>",
    rows_b, "</tbody></table></section>"
  )
}

render_h2_table <- function(tab, number, title, outcome, note, compact = FALSE) {
  if (compact) {
    stat_cols <- c("N", "Mean", "St. Dev.", "Min", "Q1", "Median", "Q3", "Max")
    panel_specs <- list(
      list(title = "Panel A: Main H2 Sample", rows = 1:3),
      list(title = "Panel B: 80% Coverage Sample", rows = 4:6)
    )
    rows <- paste0(
      "<tr class='outcome'><td colspan='4'>Outcome variable: ",
      html_escape(outcome), "</td></tr>"
    )
    for (panel in panel_specs) {
      panel_rows <- panel$rows
      rows <- paste0(
        rows,
        "<tr class='panel-title'><td colspan='4'>", panel$title, "</td></tr>",
        "<tr class='panel-header'><th>Statistic</th><th>All</th>",
        "<th>Female Protagonist</th><th>No Female Protagonist tag</th></tr>"
      )
      for (stat in stat_cols) {
        if (stat == "N") {
          shown <- vapply(tab$N[panel_rows], fmt0, character(1))
        } else {
          shown <- vapply(tab[[stat]][panel_rows], fmt2, character(1))
        }
        rows <- paste0(
          rows, "<tr><td class='row-label'>", html_escape(stat), "</td>",
          "<td class='value'>", shown[[1]], "</td>",
          "<td class='value'>", shown[[2]], "</td>",
          "<td class='value'>", shown[[3]], "</td></tr>"
        )
      }
    }
    return(paste0(
      "<section><div class='title'>Table ", number, ": ", html_escape(title),
      "<div class='subtitle'>H2 Sample and 80% Coverage</div></div>",
      "<table class='compact-h2'><tbody>", rows, "</tbody></table></section>"
    ))
  }

  stat_cols <- c("Mean", "St. Dev.", "Min", "Q1", "Median", "Q3", "Max")
  rows <- paste0(
    "<tr class='outcome'><td colspan='3'>Outcome variable: ", html_escape(outcome), "</td></tr>"
  )
  for (i in seq_len(nrow(tab))) {
    sample_label <- paste0(html_escape(tab$Sample[[i]]), "<br>(N = ", fmt0(tab$N[[i]]), ")")
    for (j in seq_along(stat_cols)) {
      stat <- stat_cols[[j]]
      first_cell <- if (j == 1L) {
        paste0("<td class='group' rowspan='7'>", sample_label, "</td>")
      } else ""
      rows <- paste0(
        rows, "<tr>", first_cell, "<td>", stat, "</td><td class='value'>",
        fmt2(tab[[stat]][[i]]), "</td></tr>"
      )
    }
  }
  paste0(
    "<section><div class='title'>Table ", number, ": ", html_escape(title),
    "<div class='subtitle'>H2 Sample and 80% Coverage</div></div>",
    "<table><thead><tr><th>Sample</th><th>Statistic</th><th>Value</th></tr></thead><tbody>",
    rows, "</tbody></table><p class='note'><strong>Source:</strong> Gamalytic, World Economic Forum (2025), and author's calculations.",
    "<br><strong>Note:</strong> ", note, "</p></section>"
  )
}

html <- paste0(
  "<!doctype html><html lang='en' dir='ltr'><head><meta charset='utf-8'>",
  "<title>Chapter 4 Descriptive Tables</title><style>",
  "*{box-sizing:border-box}body{font-family:Cambria,Georgia,'Times New Roman',serif;color:#111;background:#f4f7fb;max-width:900px;margin:16px auto;line-height:1.15}",
  "section{width:860px;max-width:100%;margin:0 auto 24px;background:#fff;border:1px solid #bfd2ed;border-radius:12px;overflow:hidden;box-shadow:0 3px 10px rgba(7,59,130,.16);page-break-after:always}",
  ".table1-section{width:470px;max-width:calc(100vw - 32px)}",
  ".title{font-size:22px;color:#fff;background:#073b82;padding:12px;text-align:center;font-weight:700}",
  ".subtitle{font-size:16px;font-style:italic;font-weight:400;margin-top:5px}",
  "table{border-collapse:collapse;width:100%;table-layout:fixed;font-size:15px}",
  "th{background:#0b4eb8;color:#fff;padding:9px 6px;text-align:center;border-right:1px solid #3d73c7}",
  "td{padding:5px 9px;border-right:1px solid #d7e4f5;border-bottom:1px solid #d7e4f5;text-align:center}",
  "tbody tr:nth-child(even) td{background:#edf4fd}tbody tr:nth-child(odd) td{background:#fff}",
  "td.row-label{text-align:left;font-weight:700}td.group{font-weight:700}td.value{color:#0b43c6;font-weight:700}",
  "tr.outcome td{background:#dceafe!important;color:#092f67;font-weight:700}",
  "tr.panel-title td{background:#c9ddf8!important;color:#092f67;font-weight:700;padding:7px}",
  "tr.panel-header th{padding:7px 5px}",
  "table.compact-h2 td{padding:5px 8px}",
  "table.compact-h2 th:first-child,table.compact-h2 td:first-child{width:22%}",
  "table.compact-h2 th:nth-child(2),table.compact-h2 td:nth-child(2){width:19%}",
  "table.compact-h2 th:nth-child(3),table.compact-h2 td:nth-child(3){width:27%}",
  "table.compact-h2 th:nth-child(4),table.compact-h2 td:nth-child(4){width:32%}",
  "table.compact-table2 td{padding:5px 8px}",
  "table.compact-table2 th:first-child,table.compact-table2 td:first-child{width:18%}",
  "table.compact-table2 th:nth-child(2),table.compact-table2 td:nth-child(2){width:32%}",
  "table.compact-table2 th:nth-child(3),table.compact-table2 td:nth-child(3){width:23%}",
  "table.compact-table2 th:nth-child(4),table.compact-table2 td:nth-child(4){width:27%}",
  "table.compact-binary th:first-child,table.compact-binary td:first-child{width:65%}",
  "table.compact-binary th:nth-child(2),table.compact-binary td:nth-child(2){width:35%}",
  ".note{font-size:13px;line-height:1.25;padding:8px 14px 10px;margin:0;text-align:left}.note strong{color:#0747a6}",
  "</style></head><body>",
  render_table1(), render_table2(),
  render_h2_table(
    table3, "3", "Weighted GGGI Descriptive Statistics", "Weighted GGGI (0-100)",
    paste0(
      "GGGI scores are presented on a 0-100 scale. The main sample includes only games with full official coverage ",
      "of the top reported countries. The robustness sample includes games with at least 80% coverage of the top reported countries."
    ),
    compact = TRUE
  ),
  render_h2_table(
    table4, "4", "Economic Participation and Opportunity Descriptive Statistics",
    "Weighted Economic Participation and Opportunity (0-100)",
    paste0(
      "Economic Participation and Opportunity scores are presented on a 0-100 scale. The main sample includes only games ",
      "with full official coverage of the top reported countries. The robustness sample includes games with at least 80% coverage."
    ),
    compact = TRUE
  ),
  "</body></html>"
)

html_file <- file.path(table_dir, "Chapter_4_descriptive_tables.html")
writeLines(html, html_file, useBytes = TRUE)
table_png_files <- c(
  "Table_1_Analytical_Samples.png",
  "Table_2_H1_Descriptive_Statistics.png",
  "Table_3_H2_Weighted_GGGI.png",
  "Table_4_H2_Economic_Participation.png"
)
# Table 1 is intentionally rendered with the original narrow viewport so that
# it remains a tall, portrait-oriented table. Tables 2-4 use the wider viewport
# required by their compact multi-column layouts.
table_vwidths <- c(520L, 1100L, 1100L, 1100L)
html_url <- paste0("file:///", normalizePath(html_file, winslash = "/"))
for (i in seq_along(table_png_files)) {
  webshot2::webshot(
    url = html_url,
    file = file.path(figure_dir, table_png_files[[i]]),
    selector = paste0("section:nth-of-type(", i, ")"),
    zoom = 2, vwidth = table_vwidths[[i]], delay = 0.25
  )
}

# ------------------------------------------------------------------------------
# 7. Figure 1 - genre composition in H1
# ------------------------------------------------------------------------------

genre_order <- c(
  "Action", "Adventure", "Casual", "Simulation", "Multiple genres", "Other",
  "Strategy", "RPG", "Racing", "Sports", "Massively Multiplayer", "Missing",
  "Education"
)
h1[, display_group := fifelse(
  tag_female_protagonist == 1L, "Female Protagonist", "No Female Protagonist tag"
)]
genre_data <- rbindlist(list(
  h1[, .N, by = .(Genre = genre_primary_clean)][
    , Group := "All H1 games"
  ][, .(Group, Genre, N)],
  h1[, .N, by = .(Group = display_group, Genre = genre_primary_clean)]
))
genre_data[is.na(Genre) | Genre == "", Genre := "Missing"]
genre_data[, Share := N / sum(N), by = Group]
genre_data[, Genre_rank := match(Genre, genre_order)]
if (anyNA(genre_data$Genre_rank)) {
  stop(
    "Unexpected genre values: ",
    paste(unique(genre_data[is.na(Genre_rank), Genre]), collapse = ", ")
  )
}
setorder(genre_data, Group, Genre_rank)
genre_data[, Label_y := cumsum(Share) - Share / 2, by = Group]
genre_data[, Group := factor(
  Group, levels = c("All H1 games", "No Female Protagonist tag", "Female Protagonist")
)]
# ggplot2 normally puts the first factor level at the top of a stacked bar.
# Reverse the plotting levels so Action is drawn at the bottom, and specify
# the legend order separately below.
genre_data[, Genre := factor(Genre, levels = rev(genre_order))]

genre_palette <- c(
  "Action" = "#4E79A7", "Adventure" = "#F28E2B", "Casual" = "#59A14F",
  "Simulation" = "#AF7AA1", "Multiple genres" = "#76B7B2", "Other" = "#9C755F",
  "Strategy" = "#EDC948", "RPG" = "#E15759", "Racing" = "#FF9DA7",
  "Sports" = "#499894", "Massively Multiplayer" = "#8CD17D",
  "Missing" = "#BAB0AC", "Education" = "#D4A6C8"
)
genre_plot <- ggplot2::ggplot(
  genre_data, ggplot2::aes(x = Group, y = Share, fill = Genre)
) +
  ggplot2::geom_col(colour = "black", linewidth = 0.25) +
  ggplot2::geom_text(
    data = genre_data[Share >= 0.03],
    ggplot2::aes(
      y = Label_y,
      label = scales::percent(Share, accuracy = 0.1)
    ),
    size = 3.2
  ) +
  ggplot2::scale_fill_manual(
    values = genre_palette,
    breaks = genre_order,
    drop = FALSE
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1), expand = c(0, 0)
  ) +
  ggplot2::labs(
    title = "Genre composition of the H1 regression sample",
    subtitle = "Each column sums to 100%; genres follow the same order across all groups",
    x = NULL, y = "Share within group", fill = "Genre"
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title.position = "plot",
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(angle = 7, hjust = 0.5),
    legend.position = "right"
  )
ggplot2::ggsave(
  file.path(figure_dir, "figure_01_genre_composition_h1.png"),
  genre_plot, width = 12, height = 8, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 8. Figure 2 - estimated-revenue box plot
# ------------------------------------------------------------------------------

h1[, Revenue_group := factor(
  fifelse(
    tag_female_protagonist == 1L,
    "Female Protagonist", "No Female Protagonist tag"
  ),
  levels = c("No Female Protagonist tag", "Female Protagonist")
)]
whiskers <- h1[, {
  q1 <- quantile(revenue, 0.25)
  q3 <- quantile(revenue, 0.75)
  .(upper_whisker = max(revenue[revenue <= q3 + 1.5 * (q3 - q1)]))
}, by = Revenue_group]
whiskers[, x := seq_len(.N)]

revenue_plot <- ggplot2::ggplot(
  h1, ggplot2::aes(x = Revenue_group, y = revenue, fill = Revenue_group)
) +
  ggplot2::geom_boxplot(
    width = 0.58, outlier.alpha = 0.12, outlier.size = 0.25,
    colour = "#333333", linewidth = 0.45
  ) +
  ggplot2::geom_segment(
    data = whiskers,
    ggplot2::aes(
      x = x - 0.34, xend = x + 0.34,
      y = upper_whisker, yend = upper_whisker
    ),
    inherit.aes = FALSE, colour = "#C00000", linewidth = 1.1
  ) +
  ggplot2::scale_fill_manual(values = c("#B7B7B7", "#8064A2"), guide = "none") +
  ggplot2::scale_y_log10(labels = scales::dollar_format(accuracy = 1)) +
  ggplot2::labs(
    title = "Distribution of estimated revenue by Female Protagonist tag",
    subtitle = "H1 regression sample; positive revenue values only",
    x = NULL, y = "Estimated revenue (USD; logarithmic scale)",
    caption = paste0(
      "Red caps mark the upper whisker; points above are outliers. ",
      "Box-plot statistics use original USD revenue values; only the display axis is logarithmic."
    )
  ) +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.caption = ggplot2::element_text(hjust = 0),
    panel.grid.major.x = ggplot2::element_blank()
  )
ggplot2::ggsave(
  file.path(figure_dir, "figure_02_revenue_boxplot.png"),
  revenue_plot, width = 12, height = 8, dpi = 300, bg = "white"
)

# ------------------------------------------------------------------------------
# 9. Final audit
# ------------------------------------------------------------------------------

cat("\n========================================\n")
cat("CHAPTER 4 DESCRIPTIVE OUTPUTS CREATED\n")
cat("========================================\n")
cat("All games:", nrow(all_games), "\n")
cat("Games with tag data:", nrow(games_with_tags), "\n")
cat("H1 descriptive sample:", nrow(h1), "\n")
cat("H2 candidate sample:", nrow(h2_candidates), "\n")
cat("H2 main sample:", nrow(h2_main), "\n")
cat("H2 80% sample:", nrow(h2_80), "\n")
cat("Workbook:", normalizePath(workbook_file, winslash = "/"), "\n")
cat("HTML:", normalizePath(html_file, winslash = "/"), "\n")
cat("Tables and figures saved in results/tables and results/figures.\n")
