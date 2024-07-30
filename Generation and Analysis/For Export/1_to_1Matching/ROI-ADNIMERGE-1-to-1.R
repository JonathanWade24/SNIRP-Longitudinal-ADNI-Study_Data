# --------------------------------------------------------------
# Script Summary:
# This script loads the necessary R libraries, imports MRI segmentation data, 
# and processes it for further analysis. Main steps include:
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
# Function Definitions
# ---------------------

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

add_TBI_column <- function(df) {
  TBI_IDs$RID <- as.character(TBI_IDs$RID)
  df <- left_join(df, TBI_IDs, by = "RID")
  df$TBI <- df$hasTBI == TRUE
  df$hasTBI <- NULL
  return(df)
}

replace_missing_fields <- function(df) {
  df <- left_join(df, MissingField, by = "names", suffix = c("", ".replace"))
  df$FIELD[is.na(df$FIELD)] <- df$FIELD.replace[is.na(df$FIELD)]
  df$FIELD[df$FIELD == 1] <- 1.5
  df$FIELD.replace <- NULL
  return(df)
}

data_transform <- function(df) {
  df <- df %>%
    mutate(
      RID = sapply(names, extract_rid),
      FIELD = ifelse(str_detect(names, "(?<=_)[0-9.]+T"), as.numeric(str_extract(names, "(?<=_)[0-9.]+(?=T)")), NA),
      DATE = sapply(names, extract_date),
      SCAN = as.numeric(str_extract(names, "(?<=_SCAN_)[0-9]+$")),
      isAvg = str_detect(names, "^avg_")
    ) %>%
    mutate(RID = str_replace_all(RID, "^r", "")) %>%
    mutate(RID = str_replace_all(RID, "^0*", ""))
  
  df$DATE <- as.Date(df$DATE, format="%Y%m%d")
  df$DATE <- format(df$DATE, "%m/%d/%Y")
  
  return(df)
}

# ---------------------
# Data Import
# ---------------------

TBI_IDs <- read_csv('Data Import/Final_TBI_With_Severity.csv') %>%
  select(RID, hasTBI) %>%
  unique()

MissingField <- read_csv('Data Import/filtered_names_FIELD.csv')
dfADNI <- read_csv('Data Import/ADNIMERGE_03Aug2023.csv')
dfGM <- read_csv('Data Import/2-23-2024/ROI_neuromorphometrics_Vgm_merged.csv')
dfWM <- read_csv('Data Import/2-23-2024/ROI_neuromorphometrics_Vwm_merged.csv')
dfCSF <- read_csv('Data Import/2-23-2024/ROI_neuromorphometrics_Vcsf_merged.csv')
dfThick <- read_csv('Data Import/2-23-2024/ROI_aparc_a2009s_thickness_merged.csv')

dfTIV <- read.table('Data Import/2-23-2024/TIV.txt', header = TRUE, sep = "\t", stringsAsFactors = FALSE, col.names = c("TIVnames", "TIV"))

# ---------------------
# Data Transformations and Processes
# ---------------------

dataframes <- list(GM = dfGM, WM = dfWM, CSF = dfCSF, Thick = dfThick)

# Applying transformations to each dataframe
transformed_dataframes <- lapply(dataframes, data_transform)

dfGM <- transformed_dataframes$GM
dfWM <- transformed_dataframes$WM
dfCSF <- transformed_dataframes$CSF
dfThick <- transformed_dataframes$Thick

dfTIV <- dfTIV %>%
  mutate(
    RID = sapply(TIVnames, extract_rid),
    DATE = sapply(TIVnames, extract_date)
  ) %>%
  mutate(DATE = as.Date(DATE, format="%Y%m%d"), 
         DATE = format(DATE, "%m/%d/%Y"))

dfTIV$RID <- sapply(dfTIV$RID, function(x) as.character(as.integer(x)))

dfGM <- left_join(dfGM, dfTIV, by = c("RID", "DATE"))
dfWM <- left_join(dfWM, dfTIV, by = c("RID", "DATE"))
dfCSF <- left_join(dfCSF, dfTIV, by = c("RID", "DATE"))
dfThick <- left_join(dfThick, dfTIV, by = c("RID", "DATE"))

dfGM <- add_TBI_column(dfGM)
dfWM <- add_TBI_column(dfWM)
dfCSF <- add_TBI_column(dfCSF)
dfThick <- add_TBI_column(dfThick)

dfThick <- dfThick %>% filter(isAvg == F)

dfGM <- replace_missing_fields(dfGM)
dfWM <- replace_missing_fields(dfWM)
dfCSF <- replace_missing_fields(dfCSF)
dfThick <- replace_missing_fields(dfThick)

transformed_dataframes$GM <- dfGM
transformed_dataframes$WM <- dfWM
transformed_dataframes$CSF <- dfCSF
transformed_dataframes$Thick <- dfThick

lapply(names(transformed_dataframes), function(name) {
  write_csv(transformed_dataframes[[name]], paste0("df_", name, ".csv"))
})

dfADNI$EXAMDATE <- as.Date(dfADNI$EXAMDATE, format = "%m/%d/%Y")
dfADNI$RID <- as.character(dfADNI$RID)

df_clinical <- dfADNI %>% 
  filter(RID %in% transformed_dataframes$GM$RID) %>%
  arrange(RID, EXAMDATE) %>%
  unique()

write_csv(df_clinical, 'df_clinical.csv')

# Execute the Python script
py_run_file('1_to_1_matching.py')
