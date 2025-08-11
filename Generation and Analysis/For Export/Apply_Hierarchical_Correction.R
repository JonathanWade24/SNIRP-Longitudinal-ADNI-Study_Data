# Apply Hierarchical Testing Correction to Existing Results
# Run this script to enhance your existing analysis results

# Load the hierarchical testing functions
source("Hierarchical_Testing_Functions.R")

# Apply hierarchical correction to all existing final results
cat("Applying hierarchical multiple testing correction to existing results...\n\n")

# List of existing results files
results_files <- c(
  "Thick_final.csv",
  "CSF_final.csv", 
  "GM_final.csv",
  "WM_final.csv",
  "Cog_final.csv"
)

# Apply correction to each file
enhanced_results_list <- list()

for(file in results_files) {
  if(file.exists(file)) {
    cat("Processing:", file, "\n")
    
    enhanced_results_list[[file]] <- apply_hierarchical_to_existing_results(file)
    
    cat("Enhanced results saved to:", enhanced_results_list[[file]]$output_file, "\n")
    cat(rep("=", 60), "\n\n")
    
  } else {
    cat("File not found:", file, "\n\n")
  }
}

# Create overall comparison across modalities
cat("=== OVERALL COMPARISON ACROSS MODALITIES ===\n")

if(length(enhanced_results_list) > 0) {
  
  overall_comparison <- map_dfr(enhanced_results_list, function(x) {
    x$summary %>%
      summarise(
        total_rois = sum(n_rois),
        total_significant_global = sum(n_significant_global_fdr),
        total_significant_hierarchical = sum(n_significant_system_fdr),
        power_gain = total_significant_hierarchical - total_significant_global,
        .groups = 'drop'
      )
  }, .id = "modality")
  
  overall_comparison$modality <- gsub("_final.csv", "", overall_comparison$modality)
  
  print(overall_comparison)
  
  # Save overall comparison
  write_csv(overall_comparison, "Overall_Hierarchical_Comparison.csv")
  
  cat("\nTotal power gain across all modalities:", 
      sum(overall_comparison$power_gain), "additional significant findings\n")
}

cat("\n=== HIERARCHICAL CORRECTION COMPLETE ===\n")
cat("Enhanced results files have been created with '_hierarchical.csv' suffix\n")
cat("System summaries have been created with '_system_summary.csv' suffix\n")
