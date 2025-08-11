# Enhanced Elastic Net Analysis with Annual Rates and Percentage Change
# This script implements elastic net with clinically meaningful metrics

library(glmnet)
library(caret)
library(pROC)
library(tidyverse)
library(lubridate)
library(janitor)

# Set working directory - already in correct location

cat("=== ENHANCED ELASTIC NET ANALYSIS ===\n")
cat("Features:\n")
cat("- Annual rate of change (per year, not per month)\n")
cat("- Percentage change from baseline ROI volume\n")
cat("- TBI prediction with clinical interpretability\n\n")

# Load and prepare data with annual rates and percentage change
prepare_enhanced_data <- function() {
  
  cat("Loading and preparing data...\n")
  
  # Load datasets
  Severity <- read_csv('Data Import/Final_TBI_With_Severity.csv', show_col_types = FALSE)
  GM <- read_csv('1_to_1Matching/merged_GM_2024-02-23.csv', show_col_types = FALSE)
  WM <- read_csv('1_to_1Matching/merged_WM_2024-02-23.csv', show_col_types = FALSE)
  Thick <- read_csv('1_to_1Matching/merged_Thick_2024-02-23.csv', show_col_types = FALSE)
  CSF <- read_csv('1_to_1Matching/merged_CSF_2024-02-23.csv', show_col_types = FALSE)
  
  # Merge severity data and clean column names
  datasets <- list(GM = GM, WM = WM, Thick = Thick, CSF = CSF)
  datasets <- lapply(datasets, function(df) {
    df %>%
      left_join(Severity %>% select(RID, injury_severity), by = "RID") %>%
      mutate(injury_severity = ifelse(is.na(injury_severity), 0, injury_severity)) %>%
      clean_names()
  })
  
  GM <- datasets$GM
  WM <- datasets$WM
  Thick <- datasets$Thick
  CSF <- datasets$CSF
  
  return(list(GM = GM, WM = WM, Thick = Thick, CSF = CSF))
}

# Enhanced transformation function with annual rates and percentage change
apply_enhanced_transformations <- function(df) {
  
  cat("Applying enhanced transformations...\n")
  
  df_transformed <- df %>%
    mutate(
      # Convert to annual rates (years, not months)
      years_since_bl_exam = as.numeric(difftime(ymd(examdate), ymd(examdate_bl), units = "days")) / 365.25,
      
      # Create TBI severity factors
      new_injury_severity = factor(case_when(
        injury_severity %in% c(0, 1) ~ as.character(injury_severity),
        injury_severity %in% c(2, 3, 4) ~ "2",
        TRUE ~ NA_character_
      )),
      
      # Standard covariates
      ptgender = factor(ptgender, levels = c('Female', 'Male')),
      pteducat = as.numeric(pteducat),
      apoe4 = factor(apoe4, levels = c('0', '1', '2')),
      tbi = as.factor(tbi),
      examdate = ymd(examdate),
      new_injury_severity = factor(new_injury_severity, levels = c("0", "1", "2")),
      
      # Diagnosis handling
      dx = ifelse(is.na(dx), as.character(dx_bl), as.character(dx)),
      dx = case_when(
        dx %in% c('LMCI', 'EMCI', 'SMC') ~ 'MCI',
        TRUE ~ as.character(dx)
      ),
      dx = factor(dx, levels = c("CN", "MCI", "Dementia"))
    )
  
  return(df_transformed)
}

# Function to calculate percentage change from baseline for each ROI
calculate_percentage_changes <- function(data, roi_variables) {
  
  cat("Calculating percentage changes from baseline...\n")
  
  # Ensure all ROI variables are numeric
  for(roi in roi_variables) {
    if(roi %in% names(data)) {
      data[[roi]] <- as.numeric(data[[roi]])
    }
  }
  
  # Filter to ROI variables that actually exist and are numeric
  existing_rois <- roi_variables[roi_variables %in% names(data)]
  numeric_rois <- existing_rois[sapply(existing_rois, function(x) is.numeric(data[[x]]))]
  
  cat("Using", length(numeric_rois), "numeric ROI variables out of", length(roi_variables), "requested\n")
  
  if(length(numeric_rois) == 0) {
    cat("Error: No numeric ROI variables found\n")
    return(NULL)
  }
  
  # Get baseline values (first timepoint for each subject)
  baseline_data <- data %>%
    group_by(rid) %>%
    arrange(rid, years_since_bl_exam) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(rid, all_of(numeric_rois)) %>%
    rename_with(~ paste0(.x, "_baseline"), .cols = all_of(numeric_rois))
  
  # Merge baseline values and calculate percentage changes
  data_with_changes <- data %>%
    left_join(baseline_data, by = "rid")
  
  # Calculate percentage change for each ROI
  for(roi in numeric_rois) {
    baseline_col <- paste0(roi, "_baseline")
    pct_change_col <- paste0(roi, "_pct_change")
    
    if(baseline_col %in% names(data_with_changes)) {
      # Ensure both columns are numeric
      roi_values <- as.numeric(data_with_changes[[roi]])
      baseline_values <- as.numeric(data_with_changes[[baseline_col]])
      
      # Calculate percentage change with protection against division by zero
      data_with_changes[[pct_change_col]] <- 
        ((roi_values - baseline_values) / pmax(abs(baseline_values), 1e-6)) * 100
    }
  }
  
  # Create percentage change variable names
  pct_change_vars <- paste0(numeric_rois, "_pct_change")
  
  return(list(
    data = data_with_changes,
    pct_change_vars = pct_change_vars[pct_change_vars %in% names(data_with_changes)],
    numeric_rois = numeric_rois
  ))
}

# Enhanced elastic net function for TBI prediction using percentage changes
enhanced_elastic_net_tbi_prediction <- function(data, roi_variables, 
                                               alpha_values = seq(0, 1, 0.1),
                                               use_percentage_change = TRUE) {
  
  cat("Running Enhanced Elastic Net for TBI Prediction\n")
  cat("Using:", ifelse(use_percentage_change, "Percentage change", "Raw values"), "\n")
  
  # Calculate percentage changes if requested
  if(use_percentage_change) {
    pct_result <- calculate_percentage_changes(data, roi_variables)
    analysis_data <- pct_result$data
    predictor_vars <- c(pct_result$pct_change_vars, "years_since_bl_exam", "age", "ptgender", "apoe4", "dx")
  } else {
    analysis_data <- data
    predictor_vars <- c(roi_variables, "years_since_bl_exam", "age", "ptgender", "apoe4", "dx")
  }
  
  cat("Predictor variables:", length(predictor_vars), "\n")
  
  # Create model matrix
  formula_str <- paste("~", paste(predictor_vars, collapse = " + "))
  
  # Handle missing data
  complete_data <- analysis_data[complete.cases(analysis_data[, c("new_injury_severity", predictor_vars)]), ]
  cat("Complete cases:", nrow(complete_data), "\n")
  
  if(nrow(complete_data) < 50) {
    cat("Warning: Very few complete cases. Consider imputation.\n")
    return(NULL)
  }
  
  # Create design matrix
  X <- model.matrix(as.formula(formula_str), data = complete_data)[, -1]  # Remove intercept
  
  # Create target variable (binary: TBI vs no TBI)
  y <- ifelse(complete_data$new_injury_severity == "0", 0, 1)
  
  cat("TBI cases:", sum(y), "\n")
  cat("Control cases:", sum(1-y), "\n")
  cat("Features after model matrix creation:", ncol(X), "\n\n")
  
  # Cross-validation for optimal alpha and lambda
  cat("Performing cross-validation for parameter selection...\n")
  
  cv_results <- list()
  
  for(alpha in alpha_values) {
    tryCatch({
      cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = alpha, 
                          nfolds = min(10, nrow(complete_data)), type.measure = "auc")
      
      cv_results[[paste0("alpha_", alpha)]] <- list(
        cv_fit = cv_fit,
        max_auc = max(cv_fit$cvm, na.rm = TRUE),
        lambda_1se = cv_fit$lambda.1se,
        lambda_min = cv_fit$lambda.min,
        alpha = alpha
      )
    }, error = function(e) {
      cat("Error with alpha =", alpha, ":", e$message, "\n")
    })
  }
  
  if(length(cv_results) == 0) {
    cat("Error: No successful cross-validation runs\n")
    return(NULL)
  }
  
  # Find best alpha
  aucs <- sapply(cv_results, function(x) x$max_auc)
  best_alpha_idx <- which.max(aucs)
  best_result <- cv_results[[best_alpha_idx]]
  
  cat("Best alpha:", best_result$alpha, "\n")
  cat("Best AUC:", round(best_result$max_auc, 3), "\n")
  cat("Lambda 1SE:", round(best_result$lambda_1se, 6), "\n\n")
  
  # Fit final model
  final_model <- glmnet(X, y, family = "binomial", 
                       alpha = best_result$alpha, 
                       lambda = best_result$lambda_1se)
  
  # Extract coefficients
  coefficients <- coef(final_model, s = best_result$lambda_1se)
  coef_df <- data.frame(
    variable = rownames(coefficients),
    coefficient = as.vector(coefficients),
    stringsAsFactors = FALSE
  ) %>%
    filter(coefficient != 0, variable != "(Intercept)") %>%
    mutate(
      abs_coefficient = abs(coefficient),
      effect_direction = ifelse(coefficient > 0, "Increases TBI Risk", "Decreases TBI Risk")
    ) %>%
    arrange(desc(abs_coefficient))
  
  cat("Selected features:", nrow(coef_df), "out of", ncol(X), "\n")
  cat("Feature selection ratio:", round(nrow(coef_df)/ncol(X), 3), "\n\n")
  
  # Predictions and performance
  predictions <- predict(final_model, newx = X, s = best_result$lambda_1se, type = "response")
  roc_obj <- roc(y, as.vector(predictions))
  
  cat("Final model performance:\n")
  cat("AUC:", round(auc(roc_obj), 3), "\n")
  
  # Get optimal threshold
  coords_optimal <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity", "accuracy"))
  cat("Optimal threshold:", round(coords_optimal$threshold, 3), "\n")
  cat("Sensitivity:", round(coords_optimal$sensitivity, 3), "\n")
  cat("Specificity:", round(coords_optimal$specificity, 3), "\n")
  cat("Accuracy:", round(coords_optimal$accuracy, 3), "\n\n")
  
  # Feature importance interpretation
  if(nrow(coef_df) > 0) {
    cat("Top 10 predictive features:\n")
    
    # Add clinical interpretation for percentage change features
    if(use_percentage_change) {
      coef_df <- coef_df %>%
        mutate(
          clinical_interpretation = case_when(
            grepl("_pct_change", variable) & coefficient > 0 ~ 
              paste("Volume loss increases TBI risk by", round(coefficient*100, 1), "% per % volume loss"),
            grepl("_pct_change", variable) & coefficient < 0 ~ 
              paste("Volume gain decreases TBI risk by", round(abs(coefficient)*100, 1), "% per % volume gain"),
            TRUE ~ paste("Effect:", round(coefficient, 3))
          )
        )
    }
    
    print(head(coef_df, 10))
  }
  
  return(list(
    model = final_model,
    coefficients = coef_df,
    cv_results = cv_results,
    best_params = best_result,
    performance = list(
      auc = auc(roc_obj),
      roc = roc_obj,
      predictions = predictions,
      optimal_coords = coords_optimal
    ),
    data_info = list(
      n_samples = nrow(complete_data),
      n_features = ncol(X),
      n_selected = nrow(coef_df),
      selection_ratio = nrow(coef_df)/ncol(X),
      use_percentage_change = use_percentage_change
    )
  ))
}

# Function to calculate annual atrophy rates for each subject
calculate_annual_atrophy_rates <- function(data, roi_variables) {
  
  cat("Calculating annual atrophy rates...\n")
  
  # Calculate individual slopes (atrophy rates) for each subject and ROI
  atrophy_rates <- data %>%
    group_by(rid) %>%
    filter(n() >= 2) %>%  # Need at least 2 timepoints
    summarise(
      across(all_of(roi_variables), ~ {
        if(sum(!is.na(.x)) >= 2 && sum(!is.na(years_since_bl_exam)) >= 2) {
          # Linear regression to get slope (rate per year)
          lm_result <- lm(.x ~ years_since_bl_exam)
          coef(lm_result)[2]  # Slope coefficient
        } else {
          NA_real_
        }
      }),
      years_followup = max(years_since_bl_exam, na.rm = TRUE) - min(years_since_bl_exam, na.rm = TRUE),
      n_timepoints = n(),
      .groups = 'drop'
    )
  
  # Add TBI status and other covariates
  baseline_covariates <- data %>%
    group_by(rid) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(rid, new_injury_severity, age, ptgender, apoe4, dx, tbi)
  
  atrophy_rates <- atrophy_rates %>%
    left_join(baseline_covariates, by = "rid")
  
  # Convert to percentage rates (rate per year as % of baseline)
  baseline_values <- data %>%
    group_by(rid) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(rid, all_of(roi_variables))
  
  for(roi in roi_variables) {
    rate_col <- roi
    pct_rate_col <- paste0(roi, "_pct_per_year")
    
    if(rate_col %in% names(atrophy_rates) && roi %in% names(baseline_values)) {
      baseline_merge <- baseline_values %>% select(rid, !!sym(roi))
      names(baseline_merge)[2] <- paste0(roi, "_baseline")
      
      atrophy_rates <- atrophy_rates %>%
        left_join(baseline_merge, by = "rid") %>%
        mutate(
          !!sym(pct_rate_col) := (!!sym(rate_col) / pmax(abs(!!sym(paste0(roi, "_baseline"))), 1e-6)) * 100
        ) %>%
        select(-!!sym(paste0(roi, "_baseline")))
    }
  }
  
  return(atrophy_rates)
}

# Main analysis function
run_enhanced_elastic_net_analysis <- function() {
  
  cat("=== STARTING ENHANCED ELASTIC NET ANALYSIS ===\n\n")
  
  # Load and prepare data
  datasets <- prepare_enhanced_data()
  
  # Apply transformations to each dataset
  transformed_datasets <- lapply(datasets, apply_enhanced_transformations)
  
  # Remove non-ROI columns and get ROI lists
  get_roi_variables <- function(df, start_col = 8) {
    roi_vars <- names(df)[start_col:ncol(df)]
    exclude_pattern <- "rid|examdate|scan_date|field|scan|dx_bl|dx|viscode|ptid|tbi|years_since_bl_exam|months_since_bl_exam|age|ptgender|pteducat|apoe4|injury_severity|new_injury_severity|tiv|cdrsb|_baseline|_pct_change|_pct_per_year"
    roi_vars <- roi_vars[!grepl(exclude_pattern, roi_vars, ignore.case = TRUE)]
    return(roi_vars)
  }
  
  # Process each modality
  results_summary <- list()
  
  for(modality in names(transformed_datasets)) {
    cat("=== PROCESSING", toupper(modality), "===\n")
    
    data <- transformed_datasets[[modality]]
    roi_vars <- get_roi_variables(data)
    
    cat("ROI variables:", length(roi_vars), "\n")
    cat("Total observations:", nrow(data), "\n")
    cat("Unique subjects:", length(unique(data$rid)), "\n\n")
    
    # Run elastic net with percentage changes
    tbi_prediction <- enhanced_elastic_net_tbi_prediction(
      data, roi_vars, use_percentage_change = TRUE
    )
    
    if(!is.null(tbi_prediction)) {
      # Save results
      output_file <- paste0("Enhanced_ElasticNet_", modality, "_TBI_Prediction.csv")
      write_csv(tbi_prediction$coefficients, output_file)
      cat("Results saved to:", output_file, "\n")
      
      # Calculate annual atrophy rates
      cat("Calculating annual atrophy rates...\n")
      atrophy_rates <- calculate_annual_atrophy_rates(data, roi_vars)
      
      atrophy_file <- paste0("Annual_Atrophy_Rates_", modality, ".csv")
      write_csv(atrophy_rates, atrophy_file)
      cat("Atrophy rates saved to:", atrophy_file, "\n")
      
      results_summary[[modality]] <- list(
        prediction = tbi_prediction,
        atrophy_rates = atrophy_rates
      )
    } else {
      cat("Failed to run elastic net for", modality, "\n")
    }
    
    cat("\n", rep("=", 60), "\n\n")
  }
  
  # Create overall summary
  cat("=== OVERALL SUMMARY ===\n")
  
  summary_table <- map_dfr(results_summary, function(x) {
    if(!is.null(x$prediction)) {
      data.frame(
        auc = x$prediction$performance$auc,
        n_features_selected = x$prediction$data_info$n_selected,
        n_total_features = x$prediction$data_info$n_features,
        selection_ratio = x$prediction$data_info$selection_ratio,
        n_subjects = length(unique(x$atrophy_rates$rid)),
        mean_followup_years = mean(x$atrophy_rates$years_followup, na.rm = TRUE)
      )
    }
  }, .id = "modality")
  
  if(nrow(summary_table) > 0) {
    print(summary_table)
    write_csv(summary_table, "Enhanced_ElasticNet_Overall_Summary.csv")
  }
  
  cat("\n=== ENHANCED ELASTIC NET ANALYSIS COMPLETE ===\n")
  
  return(results_summary)
}

# Execute the analysis
results <- run_enhanced_elastic_net_analysis()
