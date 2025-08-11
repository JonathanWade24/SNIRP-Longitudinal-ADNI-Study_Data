# Hierarchical Multiple Testing Correction Functions
# Author: Enhanced Analysis Pipeline
# Date: 2024
# 
# This script provides functions to apply hierarchical FDR correction
# based on anatomically-informed brain system groupings

# Load required libraries
library(tidyverse)

# Define brain system hierarchies
get_brain_systems <- function() {
  list(
    # Frontal System
    frontal = c("right_mfg_middle_frontal_gyrus", "left_mfg_middle_frontal_gyrus",
                "right_sfg_superior_frontal_gyrus", "left_sfg_superior_frontal_gyrus",
                "right_msfg_superior_frontal_gyrus_medial_segment", "left_msfg_superior_frontal_gyrus_medial_segment",
                "right_mfc_medial_frontal_cortex", "left_mfc_medial_frontal_cortex",
                "right_fo_frontal_operculum", "left_fo_frontal_operculum",
                "right_frp_frontal_pole", "left_frp_frontal_pole",
                "right_op_ifg_opercular_part_of_the_inferior_frontal_gyrus", 
                "left_op_ifg_opercular_part_of_the_inferior_frontal_gyrus",
                "right_or_ifg_orbital_part_of_the_inferior_frontal_gyrus",
                "left_or_ifg_orbital_part_of_the_inferior_frontal_gyrus",
                "right_tr_ifg_triangular_part_of_the_inferior_frontal_gyrus",
                "left_tr_ifg_triangular_part_of_the_inferior_frontal_gyrus"),
    
    # Temporal System
    temporal = c("right_mtg_middle_temporal_gyrus", "left_mtg_middle_temporal_gyrus",
                 "right_stg_superior_temporal_gyrus", "left_stg_superior_temporal_gyrus",
                 "right_itg_inferior_temporal_gyrus", "left_itg_inferior_temporal_gyrus",
                 "right_tmp_temporal_pole", "left_tmp_temporal_pole",
                 "right_ttg_transverse_temporal_gyrus", "left_ttg_transverse_temporal_gyrus",
                 "right_pt_planum_temporale", "left_pt_planum_temporale",
                 "right_pp_planum_polare", "left_pp_planum_polare"),
    
    # Parietal System  
    parietal = c("right_spl_superior_parietal_lobule", "left_spl_superior_parietal_lobule",
                 "right_po_g_postcentral_gyrus", "left_po_g_postcentral_gyrus",
                 "right_m_po_g_postcentral_gyrus_medial_segment", 
                 "left_m_po_g_postcentral_gyrus_medial_segment",
                 "right_smg_supramarginal_gyrus", "left_smg_supramarginal_gyrus",
                 "right_an_g_angular_gyrus", "left_an_g_angular_gyrus",
                 "right_po_parietal_operculum", "left_po_parietal_operculum"),
    
    # Occipital System
    occipital = c("right_sog_superior_occipital_gyrus", "left_sog_superior_occipital_gyrus",
                  "right_mog_middle_occipital_gyrus", "left_mog_middle_occipital_gyrus",
                  "right_iog_inferior_occipital_gyrus", "left_iog_inferior_occipital_gyrus",
                  "right_ocp_occipital_pole", "left_ocp_occipital_pole",
                  "right_calc_calcarine_cortex", "left_calc_calcarine_cortex",
                  "right_cun_cuneus", "left_cun_cuneus"),
    
    # Limbic System
    limbic = c("right_hippocampus", "left_hippocampus",
               "right_amygdala", "left_amygdala",
               "right_phg_parahippocampal_gyrus", "left_phg_parahippocampal_gyrus",
               "right_ent_entorhinal_area", "left_ent_entorhinal_area",
               "right_a_cg_g_anterior_cingulate_gyrus", "left_a_cg_g_anterior_cingulate_gyrus",
               "right_p_cg_g_posterior_cingulate_gyrus", "left_p_cg_g_posterior_cingulate_gyrus",
               "right_m_cg_g_middle_cingulate_gyrus", "left_m_cg_g_middle_cingulate_gyrus"),
    
    # Subcortical System
    subcortical = c("right_caudate", "left_caudate",
                    "right_putamen", "left_putamen", 
                    "right_pallidum", "left_pallidum",
                    "right_thalamus_proper", "left_thalamus_proper",
                    "right_accumbens_area", "left_accumbens_area",
                    "right_ventral_dc", "left_ventral_dc"),
    
    # Ventricular System
    ventricular = c("x3rd_ventricle", "x4th_ventricle",
                    "right_lateral_ventricle", "left_lateral_ventricle",
                    "right_inf_lat_vent", "left_inf_lat_vent", "csf")
  )
}

# Function to apply hierarchical FDR correction
apply_hierarchical_fdr <- function(results_df, brain_systems = NULL, roi_column = "Outcome_Variable") {
  
  if(is.null(brain_systems)) {
    brain_systems <- get_brain_systems()
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

# Function to summarize hierarchical testing results
summarize_hierarchical_results <- function(results_df, alpha = 0.05) {
  
  summary_stats <- results_df %>%
    group_by(brain_system) %>%
    summarise(
      n_rois = n(),
      n_significant_uncorrected = sum(p.value < alpha, na.rm = TRUE),
      n_significant_system_fdr = sum(p.value.system_adjusted < alpha, na.rm = TRUE),
      n_significant_global_fdr = sum(p.value.global_adjusted < alpha, na.rm = TRUE),
      proportion_system_fdr = n_significant_system_fdr / n_rois,
      proportion_global_fdr = n_significant_global_fdr / n_rois,
      power_gain = n_significant_system_fdr - n_significant_global_fdr,
      .groups = 'drop'
    ) %>%
    arrange(desc(proportion_system_fdr))
  
  return(summary_stats)
}

# Function to compare correction methods
compare_correction_methods <- function(results_df, alpha = 0.05) {
  
  comparison <- data.frame(
    method = c("Uncorrected", "Global FDR", "Hierarchical FDR"),
    n_significant = c(
      sum(results_df$p.value < alpha, na.rm = TRUE),
      sum(results_df$p.value.global_adjusted < alpha, na.rm = TRUE),
      sum(results_df$p.value.system_adjusted < alpha, na.rm = TRUE)
    )
  )
  
  comparison$proportion <- comparison$n_significant / nrow(results_df)
  
  return(comparison)
}

# Function to apply hierarchical correction to existing analysis results
# This can be used to retrofit existing CSV results files
apply_hierarchical_to_existing_results <- function(results_file, output_file = NULL) {
  
  # Read existing results
  results_df <- read_csv(results_file)
  
  # Apply hierarchical correction
  enhanced_results <- apply_hierarchical_fdr(results_df)
  
  # Generate summary
  summary_stats <- summarize_hierarchical_results(enhanced_results)
  
  # Save enhanced results
  if(is.null(output_file)) {
    output_file <- gsub("\\.csv$", "_hierarchical.csv", results_file)
  }
  
  write_csv(enhanced_results, output_file)
  
  # Save summary
  summary_file <- gsub("\\.csv$", "_system_summary.csv", output_file)
  write_csv(summary_stats, summary_file)
  
  # Print comparison
  cat("=== HIERARCHICAL CORRECTION RESULTS ===\n")
  print(compare_correction_methods(enhanced_results))
  
  cat("\n=== SYSTEM-WISE SUMMARY ===\n")
  print(summary_stats)
  
  return(list(
    enhanced_results = enhanced_results,
    summary = summary_stats,
    output_file = output_file
  ))
}

# Example usage:
# 
# # Apply to existing results
# thickness_enhanced <- apply_hierarchical_to_existing_results("Thick_final.csv")
# csf_enhanced <- apply_hierarchical_to_existing_results("CSF_final.csv")
# gm_enhanced <- apply_hierarchical_to_existing_results("GM_final.csv")
# wm_enhanced <- apply_hierarchical_to_existing_results("WM_final.csv")
#
# # Or apply directly to results dataframe
# brain_systems <- get_brain_systems()
# enhanced_results <- apply_hierarchical_fdr(my_results_df, brain_systems)
# summary_stats <- summarize_hierarchical_results(enhanced_results)
