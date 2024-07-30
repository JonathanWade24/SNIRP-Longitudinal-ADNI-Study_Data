# Check and install necessary packages
necessary_packages <- c("tidyverse", "htmltools", "plotly")
new_packages <- necessary_packages[!(necessary_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load the necessary libraries
library(tidyverse)
library(htmltools)
library(plotly)

# This CSS makes each container take up half the width of its parent, allowing for two containers per row
styles <- "
.plot-container {
  float: left;
  width: 50%;
  box-sizing: border-box;
  padding: 10px;
}

/* Clearfix (clear floats) */
.row::after {
  content: '';
  clear: both;
  display: table;
}
"

# Initialize the list with the custom style and a title
plots_html <- list(
  tags$head(tags$style(HTML(styles))),
  tags$h1("Analysis of GM Data (OLD)"))

# Adjusted plotting function for model results with term renaming
plot_model_results_from_csv <- function(data, outcome_variable, filename_prefix) {
  model_summary_df <- data %>%
    filter(Outcome_Variable == outcome_variable) %>%
    filter(!term %in% c("sd__(Intercept)", "sd__Observation")) %>%
    mutate(Term = factor(term,
                         levels = c("(Intercept)",
                                    "new_injury_severity1",
                                    "new_injury_severity2",
                                    "months_since_bl_exam",
                                    "age",
                                    "ptgenderMale",
                                    "field",
                                    "tiv",
                                    "cdrsb_bl",
                                    "new_injury_severity1:months_since_bl_exam",
                                    "new_injury_severity2:months_since_bl_exam"),
                         labels = c("Intercept",
                                    "Injury without LOC",
                                    "Injury with LOC",
                                    "Months Since Baseline",
                                    "Age",
                                    "Gender: Male",
                                    "Field",
                                    "TIV",
                                    "CDRSB_BL",
                                    "Injury w/o LOC * Months Since Baseline",
                                    "Injury w/ LOC 2 * Months Since Baseline")),
           Significance = ifelse(abs(statistic) > 2, "Significant", "Not Significant"))

  # Plotting
  p <- ggplot(model_summary_df, aes(x = Term, y = estimate, ymin = lower, ymax = upper, color = Significance)) +
    geom_pointrange() +
    scale_color_manual(values = c("Significant" = "#1f77b4", "Not Significant" = "#ff7f0e")) +
    coord_flip() +
    labs(x = "", y = "Estimate", title = paste("Fixed Effect Estimates -", filename_prefix, ":", outcome_variable),
         color = "Significance") +
    theme_bw() +
    theme(legend.position = "bottom")

  # Convert ggplot object to plotly object for interactivity
  interactive_plot <- ggplotly(p)

  return(interactive_plot)
}

# Function to create and save HTML file for a specific analysis
generate_html_file <- function(analysis_data, plot_function, filename_prefix) {
  outcome_variables <- unique(analysis_data$Outcome_Variable)
  interactive_plots <- list()

  for(outcome in outcome_variables) {
    interactive_plots[[outcome]] <- plot_function(analysis_data, outcome, filename_prefix)
  }

  # Initialize the list with the custom style and a title
  plots_html <- list(
    tags$head(tags$style(HTML(styles))),
    tags$h1(paste("Analysis of", filename_prefix, "Data (OLD)"))
  )

  # Add plots to the list, wrapping each plot with a div that has the "plot-container" class
  for (i in 1:length(interactive_plots)) {
    plot_html <- plotly::plotly_build(interactive_plots[[i]])
    # Wrap the plot in a div with the "plot-container" class
    plot_container <- div(class = "plot-container", plot_html)
    # Only start a new row for the first plot and every odd plot after
    if (i %% 2 == 1) {
      plots_html[[length(plots_html) + 1]] <- div(class = "row", plot_container)
    } else {
      # Add the second plot of the row directly to the last "row" div
      plots_html[[length(plots_html)]] <- append(plots_html[[length(plots_html)]], list(plot_container))
    }
  }

  # Create and save the HTML file
  html_file <- htmltools::tagList(plots_html)
  htmltools::save_html(html_file, file = paste0(filename_prefix, "_interactive_plots.html"))
}

# Read data files
GM_analysis <- read_csv('Old_gm_Inferential_Statistics_Summary.csv')
WM_analysis <- read_csv('Old-wm_Inferential_Statistics_Summary.csv')
CSF_analysis <- read_csv('Old_csf_Inferential_Statistics_Summary.csv')

# Generate and save HTML files for each analysis
generate_html_file(GM_analysis, plot_model_results_from_csv, "GM")
generate_html_file(WM_analysis, plot_model_results_from_csv, "WM")
generate_html_file(CSF_analysis, plot_model_results_from_csv, "CSF")
generate_html_file(Thick_analysis, plot_model_results_from_csv, "Thick")


cog_plot_model_results_from_csv <- function(data, outcome_variable, filename_prefix) {
  model_summary_df <- data %>%
    filter(Outcome_Variable == outcome_variable) %>%
    filter(!term %in% c("sd__(Intercept)", "sd__Observation")) %>%
    mutate(Term = factor(term,
                         levels = c("(Intercept)",
                                    "new_injury_severity1",
                                    "new_injury_severity2",
                                    "months_since_bl_exam",
                                    "age",
                                    "ptgenderMale",
                                    "apoe41",
                                    "apoe42",
                                    "dxMCI",
                                    "dxDementia",
                                    "new_injury_severity1:months_since_bl_exam",
                                    "new_injury_severity2:months_since_bl_exam"),
                         labels = c("Intercept",
                                    "Injury without LOC",
                                    "Injury with LOC",
                                    "Months Since Baseline",
                                    "Age",
                                    "Gender: Male",
                                    "APOE ε4 Carrier (1 copy)",
                                    "APOE ε4 Carrier (2 copies)",
                                    "Diagnosis: MCI",
                                    "Diagnosis: Dementia",
                                    "Injury w/o LOC * Months Since Baseline",
                                    "Injury w/ LOC 2 * Months Since Baseline")),
           Significance = ifelse(abs(statistic) > 2, "Significant", "Not Significant"))

  # Plotting
  p <- ggplot(model_summary_df, aes(x = Term, y = estimate, ymin = lower_ci, ymax = upper_ci, color = Significance)) +
    geom_pointrange() +
    scale_color_manual(values = c("Significant" = "#1f77b4", "Not Significant" = "#ff7f0e")) +
    coord_flip() +
    labs(x = "", y = "Estimate", title = paste("Fixed Effect Estimates -", filename_prefix, ":", outcome_variable),
         color = "Significance") +
    theme_bw() +
    theme(legend.position = "bottom")

  # Convert ggplot object to plotly object for interactivity
  interactive_plot <- ggplotly(p)

  return(interactive_plot)
}


generate_html_file(CogAnalysis_Final, cog_plot_model_results_from_csv, "Cog")
