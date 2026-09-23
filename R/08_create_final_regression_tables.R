# ==============================================================================
# Create the final econometric tables for Chapter 6
# ==============================================================================
# Run this script only after R/07_run_regressions.R completes successfully.
#
# Outputs:
#   1. One formatted HTML file containing two main-text tables and two appendix tables.
#   2. A separate CSV file for each table.
#   3. PNG images of all four tables.
#
# No regression is re-estimated here. The script reads the audited HC3 results
# from the fixed results/diagnostics/chapter6 directory.
# ==============================================================================

rm(list = ls())

if (!dir.exists("R") || !dir.exists("data") || !dir.exists("results")) {
  stop("Run this script from the repository root (the folder containing R/, data/ and results/).")
}

required_packages <- c("data.table", "webshot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages)) {
  stop("Install the required packages first: ", paste(missing_packages, collapse = ", "))
}
library(data.table)

analysis_dir <- "results/diagnostics"

# ------------------------------------------------------------------------------
# 1. Validate the fixed regression-results directory
# ------------------------------------------------------------------------------

required_input_files <- c(
  "Chapter_6_all_coefficients_HC3.csv",
  "Chapter_6_model_diagnostics.csv",
  "Chapter_6_Female_Protagonist_results.csv",
  "H2_language_control_selection.csv",
  "Chapter_6_audit_log.txt"
)

is_complete_results_dir <- function(path) {
  dir.exists(path) && all(file.exists(file.path(path, required_input_files)))
}

results_dir <- file.path(analysis_dir, "chapter6")
if (!is_complete_results_dir(results_dir)) {
  stop(
    "The fixed results directory is missing or incomplete: ", results_dir,
    ". Run R/07_run_regressions.R successfully first."
  )
}

coefficients <- fread(
  file.path(results_dir, "Chapter_6_all_coefficients_HC3.csv"),
  na.strings = c("", "NA")
)
diagnostics <- fread(
  file.path(results_dir, "Chapter_6_model_diagnostics.csv"),
  na.strings = c("", "NA")
)
female_results <- fread(
  file.path(results_dir, "Chapter_6_Female_Protagonist_results.csv"),
  na.strings = c("", "NA")
)
language_selection <- fread(
  file.path(results_dir, "H2_language_control_selection.csv"),
  na.strings = c("", "NA")
)

required_coefficient_cols <- c(
  "model_id", "term", "estimate", "std_error_hc3", "p_value"
)
required_diagnostic_cols <- c(
  "model_id", "n", "adjusted_r_squared", "max_leverage", "formula"
)
if (length(setdiff(required_coefficient_cols, names(coefficients)))) {
  stop("The coefficient file is missing required columns.")
}
if (length(setdiff(required_diagnostic_cols, names(diagnostics)))) {
  stop("The diagnostics file is missing required columns.")
}

expected_models <- c(paste0("H1_M", 1:5), paste0("H2_M", 1:9))
if (!all(expected_models %in% diagnostics$model_id)) {
  stop("The latest results directory does not contain all planned models.")
}
if (diagnostics[model_id %in% expected_models, any(max_leverage >= 1 - 1e-10)]) {
  stop("At least one model has leverage too close to 1. Do not create final tables.")
}

# Prevent accidental use of an older complete run from before sparse early
# release years were pooled into release_period.
period_models <- c(
  "H1_M3", "H1_M4", "H1_M5",
  "H2_M3", "H2_M4", "H2_M5", "H2_M6", "H2_M7", "H2_M8", "H2_M9"
)
period_formulas <- diagnostics[match(period_models, model_id)]$formula
if (any(!grepl("release_period", period_formulas, fixed = TRUE))) {
  stop(
    "The fixed results directory was created by an older regression ",
    "script. Run R/07_run_regressions.R first."
  )
}

output_dir <- "results/tables"
figure_dir <- "results/figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)

# Create human-readable copies without changing the exact exports.
format_numeric_columns <- function(input, columns) {
  output <- copy(input)
  for (column_name in intersect(columns, names(output))) {
    set(
      output,
      j = column_name,
      value = fifelse(
        is.na(output[[column_name]]),
        "",
        sprintf("%.3f", output[[column_name]])
      )
    )
  }
  output
}

format_p_values <- function(input) {
  output <- copy(input)
  if ("p_value" %in% names(output)) {
    output[, p_value := fifelse(
      is.na(p_value),
      "",
      fifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
    )]
  }
  output
}

readable_coefficients <- format_numeric_columns(
  coefficients,
  c("estimate", "std_error_hc3", "statistic", "conf_low_95", "conf_high_95")
)
readable_coefficients <- format_p_values(readable_coefficients)
fwrite(
  readable_coefficients,
  file.path(results_dir, "Chapter_6_all_coefficients_HC3_readable.csv"),
  na = ""
)

readable_female_results <- format_numeric_columns(
  female_results,
  c(
    "estimate", "std_error_hc3", "statistic", "conf_low_95", "conf_high_95",
    "interpreted_effect", "interpreted_ci_low_95", "interpreted_ci_high_95",
    "r_squared", "adjusted_r_squared", "max_leverage"
  )
)
readable_female_results <- format_p_values(readable_female_results)
fwrite(
  readable_female_results,
  file.path(results_dir, "Chapter_6_Female_Protagonist_results_readable.csv"),
  na = ""
)

readable_diagnostics <- format_numeric_columns(
  diagnostics,
  c("r_squared", "adjusted_r_squared", "max_leverage", "aic", "bic")
)
fwrite(
  readable_diagnostics,
  file.path(results_dir, "Chapter_6_model_diagnostics_readable.csv"),
  na = ""
)

readable_language_selection <- format_numeric_columns(
  language_selection,
  "prevalence"
)
fwrite(
  readable_language_selection,
  file.path(results_dir, "H2_language_control_selection_readable.csv"),
  na = ""
)

# ------------------------------------------------------------------------------
# 2. Formatting helpers
# ------------------------------------------------------------------------------

significance_stars <- function(p) {
  ifelse(
    p < 0.001, "***",
    ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ""))
  )
}

find_coefficient <- function(mid, trm) {
  row <- coefficients[model_id == mid & term == trm]

  # Backward-compatible fallback for an older run in which fread inferred two
  # H2 controls as logical and lm named their coefficients is_freeTRUE and
  # is_early_accessTRUE. The corrected regression script exports 0/1 names.
  if (!nrow(row) && trm %in% c("is_free", "is_early_access")) {
    row <- coefficients[model_id == mid & term == paste0(trm, "TRUE")]
  }

  if (nrow(row) > 1L) stop("Duplicate coefficient: ", mid, " / ", trm)
  row
}

format_coefficient <- function(mid, trm) {
  row <- find_coefficient(mid, trm)
  if (!nrow(row)) return("")
  paste0(
    sprintf("%.3f", row$estimate),
    significance_stars(row$p_value),
    "\n(", sprintf("%.3f", row$std_error_hc3), ")"
  )
}

make_final_table <- function(model_ids, term_labels, control_rows) {
  out <- data.frame(
    Variable = unname(term_labels),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  for (mid in model_ids) {
    out[[mid]] <- vapply(
      names(term_labels),
      function(trm) format_coefficient(mid, trm),
      character(1)
    )
  }

  for (label in names(control_rows)) {
    values <- control_rows[[label]]
    if (length(values) != length(model_ids)) {
      stop("Invalid control-row length: ", label)
    }
    row <- as.data.frame(
      as.list(c(Variable = label, setNames(values, model_ids))),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    out <- rbind(out, row)
  }

  selected_diagnostics <- diagnostics[match(model_ids, model_id)]
  if (anyNA(selected_diagnostics$model_id)) {
    stop("Missing diagnostics for one or more selected models.")
  }

  n_row <- as.data.frame(
    as.list(c(
      Variable = "Observations",
      setNames(
        format(selected_diagnostics$n, scientific = FALSE, trim = TRUE),
        model_ids
      )
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  r2_row <- as.data.frame(
    as.list(c(
      Variable = "Adjusted R-squared",
      setNames(sprintf("%.3f", selected_diagnostics$adjusted_r_squared), model_ids)
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

render_table_html <- function(
  table_data,
  table_number,
  caption,
  column_titles,
  table_note,
  appendix = FALSE
) {
  if (length(column_titles) != ncol(table_data) - 1L) {
    stop("Incorrect number of HTML column titles for ", table_number)
  }

  number_label <- if (appendix) "Appendix Table " else "Table "
  header <- paste0(
    "<tr><th class='variable-head'>Variable</th>",
    paste0(
      "<th><span class='column-number'>(", seq_along(column_titles), ")</span><br>",
      html_escape(column_titles), "</th>",
      collapse = ""
    ),
    "</tr>"
  )

  body <- vapply(seq_len(nrow(table_data)), function(i) {
    label <- html_escape(table_data$Variable[[i]])
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
    paste0(
      "<tr><td class='row-label'>", label, "</td>",
      paste0(cells, collapse = ""), "</tr>"
    )
  }, character(1))

  paste0(
    "<section>",
    "<h2>", number_label, html_escape(table_number), ": ",
    html_escape(caption), "</h2>",
    "<table>", header, paste0(body, collapse = "\n"), "</table>",
    "<p class='table-note'><strong>Notes:</strong> ", table_note, "</p>",
    "</section>"
  )
}

# ------------------------------------------------------------------------------
# 3. Main-text tables
# ------------------------------------------------------------------------------

h1_models <- c("H1_M1", "H1_M3", "H1_M4", "H1_M5")
h1_table <- make_final_table(
  model_ids = h1_models,
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access",
    price_current = "Current price"
  ),
  control_rows = list(
    "Genre controls" = c("No", "Yes", "Yes", "Yes"),
    "Release-period fixed effects" = c("No", "Yes", "Yes", "Yes"),
    "Paid games only" = c("No", "No", "No", "Yes")
  )
)

h2_main_models <- c("H2_M1", "H2_M3", "H2_M4", "H2_M5")
h2_main_table <- make_final_table(
  model_ids = h2_main_models,
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access",
    language_count = "Number of supported languages"
  ),
  control_rows = list(
    "Genre controls" = c("No", "Yes", "Yes", "Yes"),
    "Release-period fixed effects" = c("No", "Yes", "Yes", "Yes"),
    "Language indicators" = c("No", "No", "Yes", "No"),
    "Required coverage rate" = c("100%", "100%", "100%", "At least 80%")
  )
)

# ------------------------------------------------------------------------------
# 4. Appendix tables
# ------------------------------------------------------------------------------

h2_economic_models <- c("H2_M6", "H2_M7", "H2_M8")
h2_economic_table <- make_final_table(
  model_ids = h2_economic_models,
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
    "Required coverage rate" = c("100%", "100%", "At least 80%")
  )
)

russia_models <- c("H2_M3", "H2_M9")
russia_table <- make_final_table(
  model_ids = russia_models,
  term_labels = c(
    tag_female_protagonist = "Female Protagonist tag",
    is_free = "Free to Play",
    is_early_access = "Early Access"
  ),
  control_rows = list(
    "Genre controls" = c("Yes", "Yes"),
    "Release-period fixed effects" = c("Yes", "Yes"),
    "Russia assigned its 2021 score" = c("No", "Yes")
  )
)

# ------------------------------------------------------------------------------
# 5. Save CSV versions
# ------------------------------------------------------------------------------

fwrite(h1_table, file.path(output_dir, "Table_5_H1_Estimated_Revenue.csv"), na = "")
fwrite(h2_main_table, file.path(output_dir, "Table_6_H2_Weighted_GGGI.csv"), na = "")
fwrite(
  h2_economic_table,
  file.path(output_dir, "Appendix_Table_A1_Economic_Participation.csv"),
  na = ""
)
fwrite(
  russia_table,
  file.path(output_dir, "Appendix_Table_A2_Russia_sensitivity.csv"),
  na = ""
)

# ------------------------------------------------------------------------------
# 6. Create one Word-friendly HTML document
# ------------------------------------------------------------------------------

common_note <- paste0(
  "The table reports OLS coefficients. HC3 heteroskedasticity-robust standard ",
  "errors are shown in parentheses. * p&lt;0.05, ** p&lt;0.01, *** p&lt;0.001. ",
  "Release years through 2008 are pooled into a single category."
)
core_controls_note <- paste0(
  common_note,
  " Coefficients for the included genre and release-period controls are omitted for brevity."
)
language_controls_note <- paste0(
  common_note,
  " Coefficients for the included genre, language and release-period controls are omitted for brevity."
)

h1_note <- paste0(
  core_controls_note,
  " The dependent variable is the natural logarithm of estimated revenue. ",
  "To interpret a dummy-variable coefficient in percentage terms, calculate ",
  "100(exp(beta)-1)."
)
h2_note <- paste0(
  language_controls_note,
  " The dependent variable is the 2025 Weighted GGGI score on a 0-100 scale."
)
economic_note <- paste0(
  language_controls_note,
  " The dependent variable is the weighted Economic Participation and ",
  "Opportunity subindex on a 0-100 scale."
)
russia_note <- paste0(
  core_controls_note,
  " In column (2), Russia is assigned its latest available WEF score from ",
  "2021: 70.8. Taiwan and Hong Kong remain without a score."
)

html <- paste0(
  "<!doctype html><html lang='en' dir='ltr'><head><meta charset='utf-8'>",
  "<title>Final Econometric Tables - Chapter 6</title>",
  "<style>",
  "@page{size:A4 portrait;margin:1.8cm;}*{box-sizing:border-box;}",
  "body{font-family:Cambria,Georgia,'Times New Roman',serif;color:#111;background:#f4f7fb;max-width:7.1in;margin:14px auto;line-height:1.15;}",
  "h1{font-size:16pt;margin:0 0 5px;text-align:center;color:#073b82;}",
  ".source{font-size:8.5pt;color:#5b6573;margin:0 0 12px;text-align:center;}",
  "section{width:6.35in;max-width:100%;margin:0 auto 20px;background:#fff;border:1px solid #bfd2ed;border-radius:10px;overflow:hidden;box-shadow:0 2px 8px rgba(7,59,130,.14);page-break-inside:avoid;page-break-after:always;}",
  "h2{font-size:13pt;line-height:1.15;color:#fff;background:#073b82;margin:0;padding:7px 9px;text-align:center;font-weight:700;}",
  "table{border-collapse:collapse;width:100%;table-layout:fixed;font-size:9.7pt;direction:ltr;line-height:1.08;}",
  "th{background:#0b4eb8;color:#fff;border-right:1px solid #3d73c7;border-bottom:1px solid #8fb2e3;padding:4px 3px;text-align:center;font-weight:700;}",
  "th:last-child{border-right:0;}th.variable-head{width:34%;text-align:left;padding-left:7px;}",
  "td{padding:2px 5px;border-right:1px solid #d7e4f5;border-bottom:1px solid #d7e4f5;vertical-align:middle;}",
  "td:last-child{border-right:0;}table tr:nth-child(even) td{background:#edf4fd;}table tr:nth-child(odd) td{background:#fff;}",
  "td.row-label{text-align:left;color:#092f67;font-weight:600;}td.numeric{text-align:center;white-space:nowrap;color:#0b43a5;font-weight:600;}",
  ".se{color:#343a43;font-size:9pt;font-weight:400;}.column-number{font-weight:700;}",
  ".table-note{font-size:8.5pt;line-height:1.2;margin:0;padding:6px 9px 7px;text-align:justify;color:#222;background:#fff;border-top:1px solid #9bb9df;}",
  ".table-note strong{color:#0747a6;}",
  "@media print{body{background:#fff;max-width:none;margin:0;}body>h1,body>.source{display:none;}section{width:100%;margin:0;border-radius:0;box-shadow:none;page-break-after:always;}section:last-of-type{page-break-after:auto;}}",
  "</style></head><body>",
  "<h1>Final Econometric Tables for Chapter 6</h1>",
  "<p class='source'>Results directory: ", html_escape(basename(results_dir)), "</p>",
  render_table_html(
    h1_table,
    "5",
    "Female Protagonist Tag and Estimated Revenue",
    c("Bivariate", "Main model", "Price control", "Paid games"),
    h1_note
  ),
  render_table_html(
    h2_main_table,
    "6",
    "Female Protagonist Tag and Weighted GGGI",
    c("Bivariate", "Main model", "Language controls", "80% coverage"),
    h2_note
  ),
  render_table_html(
    h2_economic_table,
    "A1",
    "Economic Participation and Opportunity",
    c("Main specification", "Language controls", "80% coverage"),
    economic_note,
    appendix = TRUE
  ),
  render_table_html(
    russia_table,
    "A2",
    "Sensitivity Analysis Including Russia",
    c("Main sample (2025 scores)", "Russia included (2021 score)"),
    russia_note,
    appendix = TRUE
  ),
  "</body></html>"
)

html_file <- file.path(output_dir, "Chapter_6_final_tables_for_paper.html")
writeLines(html, html_file, useBytes = TRUE)

# Save each table as a separate high-resolution PNG for insertion into Word.
png_files <- c(
  "Table_5_H1_Estimated_Revenue.png",
  "Table_6_H2_Weighted_GGGI.png",
  "Appendix_Table_A1_Economic_Participation.png",
  "Appendix_Table_A2_Russia_Sensitivity.png"
)
html_url <- paste0("file:///", gsub("\\\\", "/", normalizePath(html_file)))
for (i in seq_along(png_files)) {
  webshot2::webshot(
    url = html_url,
    file = file.path(figure_dir, png_files[[i]]),
    selector = paste0("section:nth-of-type(", i, ")"),
    zoom = 2,
    vwidth = 1200,
    delay = 0.25
  )
}

readme <- c(
  "FINAL CHAPTER 6 TABLES",
  "======================",
  paste("Source results directory:", normalizePath(results_dir, winslash = "/")),
  "",
  "Main-text tables:",
  "- Table 5, columns (1)-(4): H1_M1, H1_M3, H1_M4, H1_M5",
  "- Table 6, columns (1)-(4): H2_M1, H2_M3, H2_M4, H2_M5",
  "",
  "Appendix tables:",
  "- Appendix A1: H2_M6, H2_M7, H2_M8",
  "- Appendix A2: H2_M3 compared with H2_M9",
  "",
  "Supporting file:",
  "- Chapter_6_all_coefficients_HC3_readable.csv",
  "- Chapter_6_Female_Protagonist_results_readable.csv",
  "- Chapter_6_model_diagnostics_readable.csv",
  "- H2_language_control_selection_readable.csv",
  "  Numeric results use three decimal places; p-values below 0.001 are shown as <0.001.",
  "",
  "The script also creates four PNG images in results/figures for insertion into Word."
)
writeLines(
  readme,
  file.path(output_dir, "README_final_tables.txt"),
  useBytes = TRUE
)

cat("\n========================================\n")
cat("FINAL CHAPTER 6 TABLES CREATED\n")
cat("========================================\n")
cat("Source results directory:\n", normalizePath(results_dir, winslash = "/"), "\n", sep = "")
cat("Output directory:\n", normalizePath(output_dir, winslash = "/"), "\n", sep = "")
cat("Main HTML file:\n", normalizePath(html_file, winslash = "/"), "\n", sep = "")
cat("PNG files:\n", paste(file.path(figure_dir, png_files), collapse = "\n"), "\n", sep = "")
cat("Readable CSV files created in:\n", normalizePath(results_dir, winslash = "/"), "\n", sep = "")
cat(paste0(
  "- ",
  c(
    "Chapter_6_all_coefficients_HC3_readable.csv",
    "Chapter_6_Female_Protagonist_results_readable.csv",
    "Chapter_6_model_diagnostics_readable.csv",
    "H2_language_control_selection_readable.csv"
  ),
  collapse = "\n"
), "\n")
