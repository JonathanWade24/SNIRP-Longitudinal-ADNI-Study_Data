# Fixed Hierarchical Correction Analysis and Comprehensive Report
# This script applies hierarchical testing and generates a detailed comparison report

library(tidyverse)
library(knitr)

# Set working directory to Final Results
setwd("Final Results")

# Source the hierarchical testing functions (with modifications)
source("../Hierarchical_Testing_Functions.R")

# Modified function to handle different column structures
apply_hierarchical_fdr_flexible <- function(results_df, brain_systems = NULL, roi_column = "Outcome_Variable") {
  
  if(is.null(brain_systems)) {
    brain_systems <- get_brain_systems()
  }
  
  # Calculate p-values from t-statistics if needed
  if("statistic" %in% names(results_df) && !"p.value" %in% names(results_df)) {
    # For t-statistics, calculate two-tailed p-values
    # Assuming large sample approximation (normal distribution)
    results_df$p.value <- 2 * pnorm(-abs(results_df$statistic))
    cat("Calculated p-values from t-statistics\n")
  }
  
  # Check if we have p-values now
  if(!"p.value" %in% names(results_df)) {
    cat("Error: No p.value column found and cannot calculate from available columns\n")
    return(results_df)
  }
  
  # Initialize new columns
  results_df$p.value.system_adjusted <- results_df$p.value
  results_df$brain_system <- "Other"
  
  # Assign brain systems
  for(system_name in names(brain_systems)) {
    system_mask <- results_df[[roi_column]] %in% brain_systems[[system_name]]
    results_df$brain_system[system_mask] <- system_name
  }
  
  # Apply FDR correction within each brain system
  for(system_name in c(names(brain_systems), "Other")) {
    system_mask <- results_df$brain_system == system_name
    if(sum(system_mask) > 0) {
      system_pvals <- results_df$p.value[system_mask]
      results_df$p.value.system_adjusted[system_mask] <- p.adjust(system_pvals, method = "BH")
    }
  }
  
  # Also keep global FDR for comparison
  results_df$p.value.global_adjusted <- p.adjust(results_df$p.value, method = "BH")
  
  return(results_df)
}

# Initialize results storage
all_results <- list()

# Define the results files to process
results_files <- c(
  "Old_thickness_Inferential_Statistics_Summary.csv",
  "Old_csf_Inferential_Statistics_Summary.csv", 
  "Old_gm_Inferential_Statistics_Summary.csv",
  "Old-wm_Inferential_Statistics_Summary.csv",
  "CogAnalysis_Final.csv"
)

# Create a comprehensive analysis report
generate_hierarchical_report <- function() {
  
  cat("====================================================================\n")
  cat("HIERARCHICAL MULTIPLE TESTING CORRECTION - COMPREHENSIVE REPORT\n")
  cat("====================================================================\n")
  cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  cat("Analyst: Enhanced Analysis Pipeline\n\n")
  
  # Process each results file
  for(file in results_files) {
    if(file.exists(file)) {
      
      modality <- case_when(
        grepl("thickness", file) ~ "Cortical Thickness",
        grepl("csf", file) ~ "CSF Volume",
        grepl("gm", file) ~ "Gray Matter Volume", 
        grepl("wm", file) ~ "White Matter Volume",
        grepl("Cog", file) ~ "Cognitive Measures",
        TRUE ~ "Unknown"
      )
      
      cat("Processing:", modality, "\n")
      cat("File:", file, "\n")
      
      # Read and process results
      results_df <- read_csv(file, show_col_types = FALSE)
      
      cat("Original results - Rows:", nrow(results_df), "\n")
      cat("Columns:", paste(names(results_df), collapse = ", "), "\n")
      
      # Apply hierarchical correction with flexible column handling
      enhanced_results <- apply_hierarchical_fdr_flexible(results_df)
      
      # Only proceed if we successfully got p-values
      if("p.value" %in% names(enhanced_results)) {
        
        system_summary <- summarize_hierarchical_results(enhanced_results)
        comparison <- compare_correction_methods(enhanced_results)
        
        # Store results
        all_results[[modality]] <- list(
          original = results_df,
          enhanced = enhanced_results,
          system_summary = system_summary,
          comparison = comparison
        )
        
        # Save enhanced results
        output_file <- gsub("\\.csv$", "_hierarchical.csv", file)
        write_csv(enhanced_results, output_file)
        
        summary_file <- gsub("\\.csv$", "_system_summary.csv", file)
        write_csv(system_summary, summary_file)
        
        cat("Enhanced results saved to:", output_file, "\n")
        cat("System summary saved to:", summary_file, "\n")
        
        # Print immediate comparison for this modality
        cat("\n--- COMPARISON FOR", toupper(modality), "---\n")
        print(comparison)
        
        # Significant findings by brain system
        sig_systems <- system_summary %>%
          filter(n_significant_system_fdr > 0) %>%
          arrange(desc(proportion_system_fdr))
        
        if(nrow(sig_systems) > 0) {
          cat("\nBrain systems with significant effects after hierarchical correction:\n")
          print(sig_systems %>% select(brain_system, n_significant_system_fdr, proportion_system_fdr, power_gain))
        } else {
          cat("\nNo significant effects after hierarchical correction.\n")
        }
        
        # Show some examples of the most significant effects
        if(nrow(enhanced_results) > 0) {
          most_sig <- enhanced_results %>%
            filter(p.value.system_adjusted < 0.05) %>%
            arrange(p.value.system_adjusted) %>%
            head(5)
          
          if(nrow(most_sig) > 0) {
            cat("\nTop 5 most significant effects:\n")
            print(most_sig %>% select(Outcome_Variable, term, estimate, p.value, 
                                    p.value.system_adjusted, brain_system))
          }
        }
        
      } else {
        cat("Skipping detailed analysis - could not obtain p-values\n")
      }
      
      cat("\n", rep("=", 60), "\n\n")
      
    } else {
      cat("File not found:", file, "\n\n")
    }
  }
  
  return(all_results)
}

# Function to create detailed comparison tables
create_detailed_comparison <- function(all_results) {
  
  cat("====================================================================\n")
  cat("DETAILED COMPARISON ACROSS ALL MODALITIES\n")
  cat("====================================================================\n\n")
  
  # Overall summary table
  overall_summary <- map_dfr(all_results, function(x) {
    if(is.null(x$comparison)) return(NULL)
    
    data.frame(
      uncorrected_sig = x$comparison$n_significant[1],
      global_fdr_sig = x$comparison$n_significant[2], 
      hierarchical_fdr_sig = x$comparison$n_significant[3],
      total_tests = nrow(x$enhanced),
      power_gain = x$comparison$n_significant[3] - x$comparison$n_significant[2]
    )
  }, .id = "modality")
  
  if(nrow(overall_summary) > 0) {
    overall_summary <- overall_summary %>%
      mutate(
        uncorrected_prop = round(uncorrected_sig / total_tests, 3),
        global_fdr_prop = round(global_fdr_sig / total_tests, 3),
        hierarchical_fdr_prop = round(hierarchical_fdr_sig / total_tests, 3),
        relative_power_gain_pct = round((power_gain / pmax(global_fdr_sig, 1)) * 100, 1)
      )
    
    cat("OVERALL SUMMARY TABLE:\n")
    print(overall_summary)
    
    cat("\nKEY METRICS:\n")
    cat("Total tests performed:", sum(overall_summary$total_tests), "\n")
    cat("Global FDR significant:", sum(overall_summary$global_fdr_sig), "\n") 
    cat("Hierarchical FDR significant:", sum(overall_summary$hierarchical_fdr_sig), "\n")
    cat("Net power gain:", sum(overall_summary$power_gain), "additional significant findings\n")
    
    if(sum(overall_summary$global_fdr_sig) > 0) {
      cat("Overall relative power gain:", 
          round(sum(overall_summary$power_gain) / sum(overall_summary$global_fdr_sig) * 100, 1), "%\n")
    }
    
    # Save overall summary
    write_csv(overall_summary, "Hierarchical_Correction_Overall_Summary.csv")
    
    cat("\n")
  } else {
    cat("No valid results to compare.\n\n")
  }
  
  return(overall_summary)
}

# Function to identify the most impactful discoveries
identify_key_discoveries <- function(all_results) {
  
  cat("====================================================================\n")
  cat("KEY DISCOVERIES FROM HIERARCHICAL CORRECTION\n")
  cat("====================================================================\n\n")
  
  key_discoveries <- list()
  total_new_discoveries <- 0
  
  for(modality in names(all_results)) {
    results <- all_results[[modality]]
    
    if(is.null(results$enhanced)) next
    
    # Find newly significant results
    newly_significant <- results$enhanced %>%
      filter(p.value.system_adjusted < 0.05, p.value.global_adjusted >= 0.05) %>%
      arrange(p.value.system_adjusted) %>%
      select(Outcome_Variable, term, estimate, p.value, p.value.global_adjusted, 
             p.value.system_adjusted, brain_system)
    
    if(nrow(newly_significant) > 0) {
      cat("### NEW DISCOVERIES IN", toupper(modality), "###\n")
      cat("Found", nrow(newly_significant), "newly significant effects:\n\n")
      
      total_new_discoveries <- total_new_discoveries + nrow(newly_significant)
      
      # Group by brain system
      system_discoveries <- newly_significant %>%
        group_by(brain_system) %>%
        summarise(
          n_discoveries = n(),
          median_p_hierarchical = median(p.value.system_adjusted),
          median_p_global = median(p.value.global_adjusted),
          .groups = 'drop'
        ) %>%
        arrange(desc(n_discoveries))
      
      print(system_discoveries)
      
      cat("\nTop newly significant effects:\n")
      print(head(newly_significant, min(10, nrow(newly_significant))))
      
      key_discoveries[[modality]] <- newly_significant
      
      # Save newly significant results
      write_csv(newly_significant, paste0(gsub(" ", "_", modality), "_new_discoveries.csv"))
      
      cat("\n", rep("-", 40), "\n\n")
    } else {
      cat("### ", toupper(modality), "###\n")
      cat("No new discoveries (hierarchical correction did not find additional significant effects)\n\n")
    }
  }
  
  cat("TOTAL NEW DISCOVERIES ACROSS ALL MODALITIES:", total_new_discoveries, "\n\n")
  
  return(key_discoveries)
}

# Function to analyze brain system patterns
analyze_brain_system_patterns <- function(all_results) {
  
  cat("====================================================================\n")
  cat("BRAIN SYSTEM PATTERN ANALYSIS\n")
  cat("====================================================================\n\n")
  
  # Combine all system summaries
  all_system_summaries <- map_dfr(all_results, function(x) {
    if(!is.null(x$system_summary)) {
      x$system_summary
    }
  }, .id = "modality")
  
  if(nrow(all_system_summaries) > 0) {
    
    # System-wise analysis across modalities
    system_analysis <- all_system_summaries %>%
      group_by(brain_system) %>%
      summarise(
        total_rois = sum(n_rois),
        total_significant = sum(n_significant_system_fdr),
        avg_proportion = mean(proportion_system_fdr),
        n_modalities = n(),
        total_power_gain = sum(power_gain),
        .groups = 'drop'
      ) %>%
      arrange(desc(total_significant))
    
    cat("BRAIN SYSTEMS RANKED BY SIGNIFICANT EFFECTS:\n")
    print(system_analysis)
    
    cat("\nMOST AFFECTED BRAIN SYSTEMS:\n")
    top_systems <- head(system_analysis, 3)
    for(i in 1:nrow(top_systems)) {
      system <- top_systems$brain_system[i]
      cat(sprintf("%d. %s: %d significant effects across %d modalities (%.1f%% average proportion)\n",
                  i, system, top_systems$total_significant[i], 
                  top_systems$n_modalities[i], top_systems$avg_proportion[i] * 100))
    }
    
    # Save brain system analysis
    write_csv(system_analysis, "Brain_System_Pattern_Analysis.csv")
    
    cat("\n")
  } else {
    cat("No system summaries available for analysis.\n\n")
  }
  
  return(all_system_summaries)
}

# Function to create a summary of effect directions
analyze_effect_directions <- function(all_results) {
  
  cat("====================================================================\n")
  cat("EFFECT DIRECTION ANALYSIS\n")
  cat("====================================================================\n\n")
  
  for(modality in names(all_results)) {
    results <- all_results[[modality]]
    
    if(is.null(results$enhanced)) next
    
    # Analyze significant effects by direction and term
    sig_effects <- results$enhanced %>%
      filter(p.value.system_adjusted < 0.05) %>%
      mutate(
        effect_direction = case_when(
          estimate > 0 ~ "Positive",
          estimate < 0 ~ "Negative",
          TRUE ~ "Zero"
        )
      )
    
    if(nrow(sig_effects) > 0) {
      
      cat("### EFFECT DIRECTIONS IN", toupper(modality), "###\n")
      
      direction_summary <- sig_effects %>%
        group_by(term, effect_direction) %>%
        summarise(
          n_effects = n(),
          mean_estimate = mean(estimate),
          median_p = median(p.value.system_adjusted),
          .groups = 'drop'
        ) %>%
        arrange(term, desc(n_effects))
      
      print(direction_summary)
      cat("\n")
    }
  }
}

# Main execution
cat("Starting hierarchical correction analysis...\n\n")

# Run the complete analysis
all_results <- generate_hierarchical_report()
overall_summary <- create_detailed_comparison(all_results)
key_discoveries <- identify_key_discoveries(all_results)
brain_system_patterns <- analyze_brain_system_patterns(all_results)
analyze_effect_directions(all_results)

cat("====================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("====================================================================\n")
cat("Files generated:\n")
cat("- *_hierarchical.csv: Enhanced results with hierarchical correction\n")
cat("- *_system_summary.csv: Brain system summaries\n")
cat("- Hierarchical_Correction_Overall_Summary.csv: Overall comparison\n")
cat("- *_new_discoveries.csv: Newly significant findings\n")
cat("- Brain_System_Pattern_Analysis.csv: System-wise patterns\n")
cat("\nHierarchical correction analysis complete!\n")
