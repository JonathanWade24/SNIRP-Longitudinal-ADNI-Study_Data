# --------------------------------------------------------------
# Script Summary:
# This Python script processes and merges MRI segmentation data. 
# The primary purpose is to integrate scan data with clinical information, with
# the goal of aligning imaging (scan) dates to their closest clinical exam dates. 
# Key steps include:
# 1. Reading data from pre-processed CSV files generated from ROI-ADNIMERGE-1-to-1.R.
# 2. Filtering out certain rows, prioritizing certain field values, and deduplication.
# 3. Renaming columns for clarity.
# 4. Identifying the closest clinical exam date for each scan date.
# 5. Merging scan data with clinical data based on closest dates.
# 6. Reordering columns and saving the merged dataset with the current date appended.
#
# It's intended to be used in conjunction with the R script 'ROI-ADNIMERGE-1-to-1.R' 
# for seamless data processing.
#
# Author: Jonathan Wade
# Date: 08/28/2023
# --------------------------------------------------------------

import pandas as pd
from datetime import date

# Get current date in "YYYY-MM-DD" format
current_date = date.today().strftime('%Y-%m-%d')

def process_and_merge(df_name):
    # Load the data
    df_clinical = pd.read_csv("df_clinical.csv")
    df_data = pd.read_csv(f"df_{df_name}.csv")

    # Filtering out 'isAvg' rows and preferring FIELD '3T' over '1.5T' for the same RID and DATE
    df_data = df_data[df_data['isAvg'] == False]
    df_data = df_data.sort_values(by=['RID', 'DATE', 'FIELD'], ascending=[True, True, False])
    df_data = df_data.drop_duplicates(subset=['RID', 'DATE'], keep='first')

    # Rename DATE to "SCAN_DATE" in df_data
    df_data.rename(columns={'DATE': 'SCAN_DATE'}, inplace=True)

    # Define a function to get the closest date from df_clinical for a given row in df_data
    def get_closest_date(data_row, clinical_df):
        rid = data_row['RID']
        data_date = data_row['SCAN_DATE']

        # Filter clinical_df for the given RID
        filtered_clinical = clinical_df[clinical_df['RID'] == rid]

        # If no matching RID is found in clinical_df, return None
        if filtered_clinical.empty:
            return None, None

        # Calculate the difference between SCAN_DATE and all EXAMDATEs in filtered_clinical
        filtered_clinical['date_diff'] = (pd.to_datetime(filtered_clinical['EXAMDATE']) - pd.to_datetime(data_date)).abs()

        # Sort by date_diff and take the topmost row (i.e., the closest date)
        closest_row = filtered_clinical.sort_values(by='date_diff').iloc[0]

        return closest_row['EXAMDATE'], closest_row['VISCODE']

    # Apply the function to df_data to get the closest EXAMDATE for each SCAN_DATE
    df_data['Closest_EXAMDATE'], df_data['Closest_VISCODE'] = zip(*df_data.apply(lambda row: get_closest_date(row, df_clinical), axis=1))

    # Merge the datasets on RID and the closest EXAMDATE
    merged_df = pd.merge(df_data, df_clinical, left_on=['RID', 'Closest_EXAMDATE'], right_on=['RID', 'EXAMDATE'], how='left')

    # Reorder columns
    first_columns = ['RID', 'EXAMDATE', 'SCAN_DATE', 'FIELD', 'SCAN', 'DX_bl']
    other_columns = [col for col in merged_df.columns if col not in first_columns]
    merged_df = merged_df[first_columns + other_columns]

    # Drop the additional columns
    merged_df = merged_df.drop(columns=['Closest_EXAMDATE', 'Closest_VISCODE'])

    # Save the merged dataframe with date appended
    merged_df.to_csv(f'merged_{dataset}_{current_date}.csv', index=False)


# Iterate over the datasets
datasets = ["GM", "WM", "CSF", "Thick"]
for dataset in datasets:
    process_and_merge(dataset)
