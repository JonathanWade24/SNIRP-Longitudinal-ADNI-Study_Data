# summarize_demographics.R
# Summarize demographics and clinical variables by TBI severity for Table 1
# Uses WM_old.csv in the current directory

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(gt)
library(janitor)

# Read the data
df <- read_csv('Shiny App For Data Exporation/WM_old.csv')

# Get most recent diagnosis for each patient
df_latest_dx <- df %>%
  group_by(rid) %>%
  arrange(rid, desc(examdate)) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(rid, most_recent_dx = dx)

# Filter to one observation per patient for demographics
# Priority: baseline visits first, then earliest visit if no baseline
df_baseline <- df %>%
  group_by(rid) %>%
  arrange(rid, 
          case_when(viscode == 'bl' ~ 1, 
                   viscode == 'sc' ~ 2, 
                   TRUE ~ 3),
          examdate) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  # Join with most recent diagnosis
  left_join(df_latest_dx, by = "rid") %>%
  # Map baseline diagnosis to standard terms
  mutate(
    baseline_dx_mapped = case_when(
      dx_bl == 'CN' ~ 'CN',
      dx_bl %in% c('EMCI', 'LMCI') ~ 'MCI',
      dx_bl == 'AD' ~ 'Dementia',
      dx_bl == 'SMC' ~ 'CN',  # Subjective Memory Complaints mapped to CN
      TRUE ~ dx_bl
    )
  )

cat("Total unique patients included:", nrow(df_baseline), "\n")

# Identify TBI severity column with correct labels and order
df_baseline <- df_baseline %>% mutate(
  TBI_severity = factor(
    case_when(
      new_injury_severity == 0 ~ 'No TBI',
      new_injury_severity == 1 ~ 'TBI without LOC',
      new_injury_severity == 2 ~ 'TBI with LOC',
      TRUE ~ as.character(new_injury_severity)
    ),
    levels = c('No TBI', 'TBI without LOC', 'TBI with LOC')
  )
)

# Helper function for n (%) with safe lookup
n_pct_safe <- function(x, category) {
  tbl <- table(x, useNA = "no")
  pct <- prop.table(tbl) * 100
  res <- paste0(tbl, ' (', sprintf('%.1f', pct), ')')
  names(res) <- names(tbl)
  if (category %in% names(res)) res[[category]] else "0 (0.0)"
}

# Function to perform statistical tests
perform_tests <- function(df) {
  # ANOVA for continuous variables
  age_test <- anova(lm(age ~ TBI_severity, data = df))$`Pr(>F)`[1]
  edu_test <- anova(lm(pteducat ~ TBI_severity, data = df))$`Pr(>F)`[1]
  
  # Chi-square tests for categorical variables
  sex_test <- chisq.test(table(df$ptgender, df$TBI_severity))$p.value
  race_test <- chisq.test(table(df$ptraccat, df$TBI_severity))$p.value
  ethnicity_test <- chisq.test(table(df$ptethcat, df$TBI_severity))$p.value
  baseline_dx_test <- chisq.test(table(df$baseline_dx_mapped, df$TBI_severity))$p.value
  recent_dx_test <- chisq.test(table(df$most_recent_dx, df$TBI_severity))$p.value
  apoe_test <- chisq.test(table(df$apoe4, df$TBI_severity))$p.value
  
  return(list(
    age = age_test,
    education = edu_test,
    sex = sex_test,
    race = race_test,
    ethnicity = ethnicity_test,
    baseline_diagnosis = baseline_dx_test,
    recent_diagnosis = recent_dx_test,
    apoe = apoe_test
  ))
}

# Format p-values
format_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001) return("< 0.001")
  if (p < 0.01) return(sprintf("%.3f", p))
  return(sprintf("%.2f", p))
}

# Get sample sizes for each group
sample_sizes <- df_baseline %>%
  group_by(TBI_severity) %>%
  summarise(n = n(), .groups = 'drop')

# Perform statistical tests
test_results <- perform_tests(df_baseline)

# Summarize function with better variable names and p-values
demo_summary <- df_baseline %>%
  group_by(TBI_severity) %>%
  summarise(
    `Age, years` = paste0(round(mean(age, na.rm=TRUE),1), ' (', round(sd(age, na.rm=TRUE),1), ')'),
    `Sex` = "",
    `  Female` = n_pct_safe(ptgender, 'Female'),
    `  Male` = n_pct_safe(ptgender, 'Male'),
    `Race` = "",
    `  White` = n_pct_safe(ptraccat, 'White'),
    `  Black or African American` = n_pct_safe(ptraccat, 'Black'),
    `  Other` = ifelse(
      length(setdiff(unique(ptraccat), c('White','Black'))) > 0,
      paste(sapply(setdiff(unique(ptraccat), c('White','Black')), function(r) n_pct_safe(ptraccat, r)), collapse=', '),
      "0 (0.0)"
    ),
    `Ethnicity` = "",
    `  Not Hispanic/Latino` = n_pct_safe(ptethcat, 'Not Hisp/Latino'),
    `  Hispanic/Latino` = n_pct_safe(ptethcat, 'Hisp/Latino'),
    `Education, years` = paste0(round(mean(pteducat, na.rm=TRUE),1), ' (', round(sd(pteducat, na.rm=TRUE),1), ')'),
    `Baseline diagnosis` = "",
    `  Cognitively normal (baseline)` = n_pct_safe(baseline_dx_mapped, 'CN'),
    `  Mild cognitive impairment (baseline)` = ifelse(any(!is.na(baseline_dx_mapped) & baseline_dx_mapped == 'MCI'), n_pct_safe(baseline_dx_mapped, 'MCI'), "0 (0.0)"),
    `  Dementia (baseline)` = ifelse(any(!is.na(baseline_dx_mapped) & baseline_dx_mapped == 'Dementia'), n_pct_safe(baseline_dx_mapped, 'Dementia'), "0 (0.0)"),
    `Most recent diagnosis` = "",
    `  Cognitively normal (recent)` = n_pct_safe(most_recent_dx, 'CN'),
    `  Mild cognitive impairment (recent)` = ifelse(any(!is.na(most_recent_dx) & most_recent_dx == 'MCI'), n_pct_safe(most_recent_dx, 'MCI'), "0 (0.0)"),
    `  Dementia (recent)` = ifelse(any(!is.na(most_recent_dx) & most_recent_dx == 'Dementia'), n_pct_safe(most_recent_dx, 'Dementia'), "0 (0.0)"),
    `APOE4 alleles` = "",
    `  0` = n_pct_safe(as.character(apoe4), '0'),
    `  1` = n_pct_safe(as.character(apoe4), '1'),
    `  2` = n_pct_safe(as.character(apoe4), '2'),
    .groups = 'drop'
  )

# Add p-values column
p_values <- data.frame(
  TBI_severity = "P-value",
  `Age, years` = format_p(test_results$age),
  `Sex` = "",
  `  Female` = format_p(test_results$sex),
  `  Male` = "",
  `Race` = "",
  `  White` = format_p(test_results$race),
  `  Black or African American` = "",
  `  Other` = "",
  `Ethnicity` = "",
  `  Not Hispanic/Latino` = format_p(test_results$ethnicity),
  `  Hispanic/Latino` = "",
  `Education, years` = format_p(test_results$education),
  `Baseline diagnosis` = "",
  `  Cognitively normal (baseline)` = format_p(test_results$baseline_diagnosis),
  `  Mild cognitive impairment (baseline)` = "",
  `  Dementia (baseline)` = "",
  `Most recent diagnosis` = "",
  `  Cognitively normal (recent)` = format_p(test_results$recent_diagnosis),
  `  Mild cognitive impairment (recent)` = "",
  `  Dementia (recent)` = "",
  `APOE4 alleles` = "",
  `  0` = format_p(test_results$apoe),
  `  1` = "",
  `  2` = "",
  check.names = FALSE
)

# Combine with main summary
demo_summary <- bind_rows(demo_summary, p_values)

# Print summary
demo_summary %>% print(width=Inf)

# Save to CSV
write_csv(demo_summary, 'Shiny App For Data Exporation/Table1_demographics_by_TBI.csv')

# Transpose the summary so TBI severity is columns and variables are rows
demo_summary_long <- demo_summary %>%
  pivot_longer(-TBI_severity, names_to = "Variable", values_to = "Value") %>%
  pivot_wider(names_from = TBI_severity, values_from = Value) %>%
  clean_names()

# Add sample sizes as first row
n_row <- sample_sizes %>%
  mutate(variable = "N") %>%
  pivot_wider(names_from = TBI_severity, values_from = n) %>%
  clean_names() %>%
  mutate(across(where(is.numeric), as.character)) %>%
  mutate(p_value = "")

demo_summary_final <- bind_rows(n_row, demo_summary_long)

# Create a gt table for publication (vertical format)
summary_gt <- demo_summary_final %>%
  gt() %>%
  tab_header(
    title = "Table 1. Demographics and Clinical Characteristics by TBI Severity"
  ) %>%
  cols_label(
    variable = "",
    no_tbi = "No TBI",
    tbi_without_loc = "TBI without LOC", 
    tbi_with_loc = "TBI with LOC",
    p_value = "P-value"
  ) %>%
  tab_footnote(
    footnote = "Baseline diagnosis from study entry; most recent diagnosis from latest available visit",
    locations = cells_body(columns = variable, rows = variable == "Baseline diagnosis")
  ) %>%
  tab_footnote(
    footnote = "Most recent diagnosis from latest available visit for each patient",
    locations = cells_body(columns = variable, rows = variable == "Most recent diagnosis")
  ) %>%
  tab_footnote(
    footnote = "Continuous variables: ANOVA; Categorical variables: Chi-square test",
    locations = cells_column_labels(columns = p_value)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = variable,
      rows = str_detect(variable, "^[A-Z]") & !str_detect(variable, "^  ")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(indent = px(20))
    ),
    locations = cells_body(
      columns = variable,
      rows = str_detect(variable, "^  ")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      rows = variable == "N"
    )
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(14),
    data_row.padding = px(3),
    table.border.top.style = "hidden",
    table.border.bottom.style = "solid",
    heading.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid",
    table_body.border.bottom.style = "solid"
  ) %>%
  cols_align(
    align = "left",
    columns = variable
  ) %>%
  cols_align(
    align = "center", 
    columns = c(no_tbi, tbi_without_loc, tbi_with_loc, p_value)
  )

# Save as HTML
gt::gtsave(summary_gt, 'Shiny App For Data Exporation/Table1_demographics_by_TBI.html')

# Save as Word document for easy editing
gt::gtsave(summary_gt, 'Shiny App For Data Exporation/Table1_demographics_by_TBI.docx')

# Save as RTF for universal editing
gt::gtsave(summary_gt, 'Shiny App For Data Exporation/Table1_demographics_by_TBI.rtf')

# Export as PNG (high quality)
gt::gtsave(summary_gt, 'Shiny App For Data Exporation/Table1_demographics_by_TBI.png', vwidth = 1200, vheight = 800)

# Also save the data as a nicely formatted CSV for manual editing
demo_summary_final %>%
  write_csv('Shiny App For Data Exporation/Table1_demographics_formatted.csv')

print("Table exported in multiple formats:")
print("- HTML: Table1_demographics_by_TBI.html")
print("- Word: Table1_demographics_by_TBI.docx") 
print("- RTF: Table1_demographics_by_TBI.rtf")
print("- PNG: Table1_demographics_by_TBI.png")
print("- CSV: Table1_demographics_formatted.csv") 