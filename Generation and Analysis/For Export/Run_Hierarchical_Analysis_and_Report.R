# Hierarchical Correction Analysis and Comprehensive Report
# This script applies hierarchical testing and generates a detailed comparison report

library(tidyverse)
library(knitr)
library(DT)

# Set working directory to Final Results
setwd("Final Results")

# Source the hierarchical testing functions
source("../Hierarchical_Testing_Functions.R")

# Initialize results storage
all_results <- list()
comparison_summary <- data.frame()

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
      
      # Check if p.value column exists
      if(!"p.value" %in% names(results_df)) {
        cat("Warning: No p.value column found in", file, "\n\n")
        next
      }
      
      cat("Original results - Rows:", nrow(results_df), "\n")
      
      # Apply hierarchical correction
      enhanced_results <- apply_hierarchical_fdr(results_df)
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
        relative_power_gain = round(power_gain / global_fdr_sig, 3)
      )
    
    cat("OVERALL SUMMARY TABLE:\n")
    print(overall_summary)
    
    cat("\nTOTAL ACROSS ALL MODALITIES:\n")
    cat("Total tests performed:", sum(overall_summary$total_tests), "\n")
    cat("Global FDR significant:", sum(overall_summary$global_fdr_sig), "\n") 
    cat("Hierarchical FDR significant:", sum(overall_summary$hierarchical_fdr_sig), "\n")
    cat("Net power gain:", sum(overall_summary$power_gain), "additional significant findings\n")
    cat("Overall relative power gain:", 
        round(sum(overall_summary$power_gain) / sum(overall_summary$global_fdr_sig), 3), "\n\n")
    
    # Save overall summary
    write_csv(overall_summary, "Hierarchical_Correction_Overall_Summary.csv")
  }
  
  return(overall_summary)
}

# Function to identify the most impactful discoveries
identify_key_discoveries <- function(all_results) {
  
  cat("====================================================================\n")
  cat("KEY DISCOVERIES FROM HIERARCHICAL CORRECTION\n")
  cat("====================================================================\n\n")
  
  key_discoveries <- list()
  
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
      
      cat("\nTop 5 most significant new discoveries:\n")
      print(head(newly_significant, 5))
      
      key_discoveries[[modality]] <- newly_significant
      
      # Save newly significant results
      write_csv(newly_significant, paste0(gsub(" ", "_", modality), "_new_discoveries.csv"))
      
      cat("\n", rep("-", 40), "\n\n")
    } else {
      cat("### ", toupper(modality), "###\n")
      cat("No new discoveries (hierarchical correction was more conservative)\n\n")
    }
  }
  
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
  }
  
  return(all_system_summaries)
}

# Function to create effect size analysis for new discoveries
analyze_effect_sizes <- function(key_discoveries) {
  
  cat("====================================================================\n")
  cat("EFFECT SIZE ANALYSIS OF NEW DISCOVERIES\n")
  cat("====================================================================\n\n")
  
  if(length(key_discoveries) > 0) {
    
    all_discoveries <- bind_rows(key_discoveries, .id = "modality")
    
    if(nrow(all_discoveries) > 0) {
      
      # Effect size summary by modality and term
      effect_summary <- all_discoveries %>%
        group_by(modality, term) %>%
        summarise(
          n_effects = n(),
          mean_effect = mean(estimate, na.rm = TRUE),
          median_effect = median(estimate, na.rm = TRUE), 
          sd_effect = sd(estimate, na.rm = TRUE),
          min_p = min(p.value.system_adjusted, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(modality, min_p)
      
      cat("EFFECT SIZE SUMMARY FOR NEW DISCOVERIES:\n")
      print(effect_summary)
      
      # Identify largest effect sizes
      large_effects <- all_discoveries %>%
        mutate(abs_effect = abs(estimate)) %>%
        arrange(desc(abs_effect)) %>%
        head(10)
      
      cat("\nLARGEST EFFECT SIZES AMONG NEW DISCOVERIES:\n")
      print(large_effects %>% select(modality, Outcome_Variable, term, estimate, 
                                   p.value.system_adjusted, brain_system))
      
      # Save effect size analysis
      write_csv(effect_summary, "New_Discoveries_Effect_Size_Summary.csv")
      write_csv(large_effects, "Largest_Effect_Sizes_New_Discoveries.csv")
      
      cat("\n")
    }
  } else {
    cat("No new discoveries to analyze.\n\n")
  }
}

# Main execution
cat("Starting hierarchical correction analysis...\n\n")

# Run the complete analysis
all_results <- generate_hierarchical_report()
overall_summary <- create_detailed_comparison(all_results)
key_discoveries <- identify_key_discoveries(all_results)
brain_system_patterns <- analyze_brain_system_patterns(all_results)
analyze_effect_sizes(key_discoveries)

cat("====================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("====================================================================\n")
cat("Files generated:\n")
cat("- *_hierarchical.csv: Enhanced results with hierarchical correction\n")
cat("- *_system_summary.csv: Brain system summaries\n")
cat("- Hierarchical_Correction_Overall_Summary.csv: Overall comparison\n")
cat("- *_new_discoveries.csv: Newly significant findings\n")
cat("- Brain_System_Pattern_Analysis.csv: System-wise patterns\n")
cat("- New_Discoveries_Effect_Size_Summary.csv: Effect size analysis\n")
cat("- Largest_Effect_Sizes_New_Discoveries.csv: Top effect sizes\n")
cat("\nHierarchical correction analysis complete!\n")
