# Elastic Net Analysis for TBI Neuroimaging Data
# This script implements elastic net regularization for feature selection and prediction

library(glmnet)
library(caret)
library(pROC)
library(tidyverse)

# Function to run elastic net for TBI prediction
elastic_net_tbi_analysis <- function(data, roi_variables, target_variable = "new_injury_severity") {
  
  cat("Running Elastic Net Analysis for TBI Prediction\n")
  cat("ROIs:", length(roi_variables), "\n")
  cat("Samples:", nrow(data), "\n\n")
  
  # Prepare data
  predictor_vars <- c(roi_variables, "months_since_bl_exam", "age", "ptgender", "apoe4", "dx")
  
  # Create model matrix
  formula_str <- paste("~", paste(predictor_vars, collapse = " + "))
  
  # Handle missing data
  complete_data <- data[complete.cases(data[, c(target_variable, predictor_vars)]), ]
  cat("Complete cases:", nrow(complete_data), "\n")
  
  # Create design matrix
  X <- model.matrix(as.formula(formula_str), data = complete_data)[, -1]  # Remove intercept
  
  # Create target variable (binary: TBI vs no TBI)
  y <- ifelse(complete_data[[target_variable]] == "0", 0, 1)
  
  cat("TBI cases:", sum(y), "\n")
  cat("Control cases:", sum(1-y), "\n\n")
  
  # Cross-validation for optimal alpha and lambda
  cat("Performing cross-validation for parameter selection...\n")
  
  alpha_values <- seq(0, 1, 0.1)
  cv_results <- list()
  
  for(alpha in alpha_values) {
    cv_fit <- cv.glmnet(X, y, family = "binomial", alpha = alpha, 
                        nfolds = 10, type.measure = "auc")
    
    cv_results[[paste0("alpha_", alpha)]] <- list(
      cv_fit = cv_fit,
      max_auc = max(cv_fit$cvm),
      lambda_1se = cv_fit$lambda.1se,
      lambda_min = cv_fit$lambda.min,
      alpha = alpha
    )
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
    mutate(abs_coefficient = abs(coefficient)) %>%
    arrange(desc(abs_coefficient))
  
  cat("Selected features:", nrow(coef_df), "out of", ncol(X), "\n")
  cat("Feature selection ratio:", round(nrow(coef_df)/ncol(X), 3), "\n\n")
  
  # Predictions and performance
  predictions <- predict(final_model, newx = X, s = best_result$lambda_1se, type = "response")
  roc_obj <- roc(y, as.vector(predictions))
  
  cat("Final model performance:\n")
  cat("AUC:", round(auc(roc_obj), 3), "\n")
  cat("Accuracy at optimal threshold:", round(coords(roc_obj, "best", ret = "accuracy"), 3), "\n")
  
  # Feature importance by brain system
  if(nrow(coef_df) > 0) {
    cat("\nTop 10 predictive features:\n")
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
      predictions = predictions
    ),
    data_info = list(
      n_samples = nrow(complete_data),
      n_features = ncol(X),
      n_selected = nrow(coef_df),
      selection_ratio = nrow(coef_df)/ncol(X)
    )
  ))
}

# Function to run elastic net for continuous outcomes (feature selection)
elastic_net_feature_selection <- function(data, outcome_var, predictor_vars) {
  
  cat("Running Elastic Net Feature Selection for:", outcome_var, "\n")
  
  # Prepare data
  complete_data <- data[complete.cases(data[, c(outcome_var, predictor_vars)]), ]
  
  X <- as.matrix(complete_data[, predictor_vars])
  y <- complete_data[[outcome_var]]
  
  cat("Complete cases:", nrow(complete_data), "\n")
  cat("Predictors:", ncol(X), "\n")
  
  # Cross-validation for parameter selection
  alpha_values <- seq(0, 1, 0.1)
  best_mse <- Inf
  best_result <- NULL
  
  for(alpha in alpha_values) {
    cv_fit <- cv.glmnet(X, y, alpha = alpha, nfolds = 10)
    
    if(min(cv_fit$cvm) < best_mse) {
      best_mse <- min(cv_fit$cvm)
      best_result <- list(
        cv_fit = cv_fit,
        alpha = alpha,
        lambda_1se = cv_fit$lambda.1se,
        mse = best_mse
      )
    }
  }
  
  # Extract selected features
  final_model <- glmnet(X, y, alpha = best_result$alpha, lambda = best_result$lambda_1se)
  coefficients <- coef(final_model, s = best_result$lambda_1se)
  
  selected_features <- rownames(coefficients)[coefficients[, 1] != 0]
  selected_features <- selected_features[selected_features != "(Intercept)"]
  
  cat("Selected features:", length(selected_features), "out of", ncol(X), "\n")
  cat("Best alpha:", best_result$alpha, "\n")
  cat("Best MSE:", round(best_result$mse, 4), "\n\n")
  
  return(list(
    selected_features = selected_features,
    model = final_model,
    best_params = best_result,
    n_selected = length(selected_features),
    total_features = ncol(X),
    selection_ratio = length(selected_features) / ncol(X)
  ))
}

# Example usage function
run_elastic_net_on_existing_data <- function() {
  
  cat("=== ELASTIC NET ANALYSIS EXAMPLE ===\n\n")
  
  # This assumes you have the transformed datasets available
  # Adjust paths as needed
  
  if(exists("Thick_transformed")) {
    
    # Get ROI variables (exclude non-ROI columns)
    roi_vars <- names(Thick_transformed)[8:ncol(Thick_transformed)]
    exclude_pattern <- "rid|examdate|scan_date|field|scan|dx_bl|dx|viscode|ptid|tbi|months_since_bl_exam|age|ptgender|pteducat|apoe4|injury_severity|new_injury_severity|tiv|cdrsb"
    roi_vars <- roi_vars[!grepl(exclude_pattern, roi_vars, ignore.case = TRUE)]
    
    cat("Running elastic net on cortical thickness data...\n")
    
    # TBI prediction
    thickness_tbi_pred <- elastic_net_tbi_analysis(Thick_transformed, roi_vars)
    
    # Save results
    write_csv(thickness_tbi_pred$coefficients, "Thickness_ElasticNet_TBI_Prediction.csv")
    
    cat("Results saved to: Thickness_ElasticNet_TBI_Prediction.csv\n\n")
    
    return(thickness_tbi_pred)
  } else {
    cat("Data not found. Please load your transformed datasets first.\n")
    cat("Example: source('your_data_preparation_script.R')\n")
  }
}

# Print usage instructions
cat("=== ELASTIC NET ANALYSIS FUNCTIONS LOADED ===\n")
cat("Available functions:\n")
cat("1. elastic_net_tbi_analysis(data, roi_variables) - TBI prediction\n")
cat("2. elastic_net_feature_selection(data, outcome, predictors) - Feature selection\n")
cat("3. run_elastic_net_on_existing_data() - Example usage\n\n")
cat("To run: source this file, then call the functions with your data\n")
