# --------------------------------------------------------------
# Script Summary:
# This script loads the necessary R libraries, imports MRI segmentation data, 
# and processes it for further analysis. The main steps include:
# 1. Data Import from different CSV files.
# 2. Extracting RID and Date from dataset columns.
# 3. Applying transformations to imported data.
# 4. Saving transformed data for further Python processing.
# 5. Executing a Python script for matching.
#
# The end result of this script should be 4 files in the same directory 
# named "merged_[type of data]_[date].csv"
# Author: Jonathan Wade
# Date: 8/28/2023
# --------------------------------------------------------------


# Load necessary libraries
library(tidyverse)
library(usethis)
library(janitor)
library(stringr)
library(reticulate)

# ---------------------
# Data Import
# ---------------------

# Read in the data
TBI_IDs <- read_csv('Data Import/tbi_likely_massive_further_refined.csv')%>%
  select(RID, hasTBI)%>%
  unique()
MissingField<- read_csv('Data Import/filtered_names_FIELD.csv')
VerifiedTBI <- read_csv('Data Import/Verified RIDs.csv')
dfADNI <- read_csv('Data Import/ADNIMERGE_03Aug2023.csv')
dfGM <- read_csv('Data Import/9-20-2023/GM/ROI_neuromorphometrics_Vgm.csv')
dfWM <- read_csv('Data Import/9-20-2023/WM/ROI_neuromorphometrics_Vwm.csv')
dfCSF <- read_csv('Data Import/9-20-2023/CSF/ROI_neuromorphometrics_Vcsf.csv')
dfThick <- read_csv('Data Import/9-20-2023/Cortical Thickness/ROI_aparc_a2009s_thickness.csv')

# Read in the TIV data
dfTIV <- read.table('Data Import/9-20-2023/TIV.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE, col.names = c("TIVnames", "TIV"))
dfTIV
# Functions to extract RID and Date
extract_rid <- function(name) {
  return(str_extract(name, "\\d{4}"))
}

extract_date <- function(name) {
  date <- str_extract(name, "\\d{4}\\d{2}\\d{2}(?=_SCAN)")
  if (!is.na(date)) return(date)
  
  date <- str_extract(name, "\\d{4}-\\d{2}-\\d{2}(?=_SCAN)")
  if (!is.na(date)) return(gsub("-", "", date))
  
  return(NA)
}

# Assuming df is your transformed dataframe and TBI_IDs is loaded in your environment
add_TBI_column <- function(df) {
  # Convert RID in TBI_IDs to character
  TBI_IDs$RID <- as.character(TBI_IDs$RID)
  
  df <- left_join(df, TBI_IDs, by = "RID")
  
  # Convert hasTBI to logical TRUE/FALSE if it's not already
  df$TBI <- df$hasTBI == TRUE
  
  # Drop hasTBI column as we now have TBI column
  df$hasTBI <- NULL
  
  return(df)
}

# Function to replace NA values in FIELD column based on MissingField dataframe
replace_missing_fields <- function(df) {
  # Join with MissingField dataframe on 'names' column
  df <- left_join(df, MissingField, by = "names", suffix = c("", ".replace"))
  
  # Replace NAs in FIELD column with corresponding values from FIELD.replace
  df$FIELD[is.na(df$FIELD)] <- df$FIELD.replace[is.na(df$FIELD)]
  
  # Replace values in FIELD column where FIELD = 1 to 1.5
  df$FIELD[df$FIELD == 1] <- 1.5
  
  # Drop the FIELD.replace column
  df$FIELD.replace <- NULL
  
  return(df)
}

# Transformation function
data_transform <- function(df) {
  df <- df %>%
    mutate(
      RID = sapply(names, extract_rid),
      FIELD = ifelse(str_detect(names, "(?<=_)[0-9.]+T"), as.numeric(str_extract(names, "(?<=_)[0-9.]+(?=T)")), NA),
      DATE = sapply(names, extract_date),
      SCAN = as.numeric(str_extract(names, "(?<=_SCAN_)[0-9]+$")),
      isAvg = str_detect(names, "^avg_")
    )
  df <- df %>%
    mutate(RID = str_replace_all(RID, "^r", "")) %>%
    mutate(RID = str_replace_all(RID, "^0*", ""))
  
  df$DATE <- as.Date(df$DATE, format="%Y%m%d")
  df$DATE <- format(df$DATE, "%m/%d/%Y")
  
  return(df)
}

dataframes <- list(GM = dfGM, WM = dfWM, CSF = dfCSF, Thick = dfThick)

# Applying transformations to each dataframe
dataframes <- lapply(dataframes, data_transform)


# Applying transformations to each dataframe
transformed_dataframes <- lapply(dataframes, data_transform)

# Assigning back to the individual dataframes
dfGM <- transformed_dataframes$GM
dfWM <- transformed_dataframes$WM
dfCSF <- transformed_dataframes$CSF
dfThick <- transformed_dataframes$Thick

# Extract RID and Date from TIV data
dfTIV <- dfTIV %>%
  mutate(
    RID = sapply(TIVnames, extract_rid),
    DATE = sapply(TIVnames, extract_date)
  ) %>%
  mutate(DATE = as.Date(DATE, format="%Y%m%d"),  # Convert to date type
         DATE = format(DATE, "%m/%d/%Y"))  # Convert back to "MM/DD/YYYY" format
# Remove leading zeros from the RID values in dfTIV
dfTIV$RID <- sapply(dfTIV$RID, function(x) as.character(as.integer(x)))

colnames(dfWM)
# Merge TIV data with each MRI dataframe
dfGM <- left_join(dfGM, dfTIV, by = c("RID", "DATE"))
dfWM <- left_join(dfWM, dfTIV, by = c("RID", "DATE"))
dfCSF <- left_join(dfCSF, dfTIV, by = c("RID", "DATE"))
dfThick <- left_join(dfThick, dfTIV, by = c("RID", "DATE"))


# Add TBI column to each dataframe
dfGM <- add_TBI_column(dfGM)
dfWM <- add_TBI_column(dfWM)
dfCSF <- add_TBI_column(dfCSF)
dfThick <- add_TBI_column(dfThick)

dfThick <- dfThick%>%
  filter(isAvg == F)

# Apply the replace_missing_fields function to necessary datasets
dfGM <- replace_missing_fields(dfGM)
dfWM <- replace_missing_fields(dfWM)
dfCSF <- replace_missing_fields(dfCSF)
dfThick <- replace_missing_fields(dfThick)

transformed_dataframes$Thick <- dfThick
transformed_dataframes$GM <- dfGM
transformed_dataframes$WM <- dfWM
transformed_dataframes$WM <- dfCSF

# Save the transformed dataframes for Python processing
lapply(names(transformed_dataframes), function(name) {
  write_csv(transformed_dataframes[[name]], paste0("df_", name, ".csv"))
})



# Convert date columns to Date type
dfADNI$EXAMDATE <- as.Date(dfADNI$EXAMDATE, format = "%m/%d/%Y")

# Convert RID to character
dfADNI$RID <- as.character(dfADNI$RID)

df_clinical <- dfADNI %>% 
  filter(RID %in% transformed_dataframes$GM$RID)%>%
  arrange(RID, EXAMDATE)%>%
  unique()

write_csv(df_clinical, 'df_clinical.csv')

# Execute the Python script
py_run_file('1_to_1_matching/1_to_1_matching.py')


