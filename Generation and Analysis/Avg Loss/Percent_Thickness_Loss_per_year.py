"""
Author: jonathan Wade
Data: 8/29/2023

Script Summary:

- This script processes cortical thickness data from the file 'df_Thick.csv'.
- The main goal is to compute the average loss of cortical thickness per year for each patient (RID).
- The data is categorized based on the magnetic field strength of the MRI scans (1.5T and 3T).
- The following steps are executed:
    1. Load the 'df_Thick.csv' file.
    2. Convert the DATE column to a datetime format for easier processing.
    3. Compute the average loss of cortical thickness per year and the time span between the first and last scans for each RID.
    4. Separate the computed data based on the magnetic field strength (1.5T and 3T).
    5. Save the results in two separate CSV files - 'average_loss_1_5T.csv' for 1.5T scans and 'average_loss_3T.csv' for 3T scans.

Note: This script assumes the presence of 'df_Thick.csv' in the same directory and writes the output files to the same location.
"""

import pandas as pd
from tqdm import tqdm

# Load the clinical data
adnimerge_df = pd.read_csv('ADNIMERGE_03Aug2023.csv')
adnimerge_df['EXAMDATE'] = pd.to_datetime(adnimerge_df['EXAMDATE'])

# Define all the required functions

def compute_avg_loss_and_time_distance(group, columns_to_exclude, is_thickness_data=False):
    group = group.sort_values(by='DATE')
    first_index = group.index[0]
    last_index = group.index[-1]

    timepoint_1 = group.at[first_index, 'DATE']
    timepoint_2 = group.at[last_index, 'DATE']
    
    # Ensure timepoints are in datetime format
    timepoint_1 = pd.to_datetime(timepoint_1)
    timepoint_2 = pd.to_datetime(timepoint_2)

    years = (timepoint_2 - timepoint_1).days / 365.25

    avg_loss = {
        'RID': group.at[first_index, 'RID'],
        'TBI': group.at[first_index, 'TBI']
    }

    columns = [col for col in group.columns if col not in columns_to_exclude]
    for col in columns:
        initial_value = group.at[first_index, col]
        final_value = group.at[last_index, col]

        if pd.isna(initial_value) or pd.isna(final_value) or initial_value == 0 or years == 0:
            avg_loss[col] = None
        else:
            avg_loss[col] = ((initial_value - final_value) / initial_value) * 100 / years

    avg_loss['Timepoint_Distance'] = years
    avg_loss['timepoint_1'] = timepoint_1
    avg_loss['timepoint_2'] = timepoint_2

    return pd.Series(avg_loss)

def get_closest_row(rid, target_date):
    rid_data = adnimerge_df[adnimerge_df['RID'] == rid]
    closest_date_idx = (rid_data['EXAMDATE'] - target_date).abs().idxmin()
    return rid_data.loc[closest_date_idx]

def merge_timepoint_data(row):
    tp1_data = get_closest_row(row['RID'], row['timepoint_1'])
    for col in tp1_data.index:
        row[f'timepoint_1_{col}'] = tp1_data[col]

    tp2_data = get_closest_row(row['RID'], row['timepoint_2'])
    for col in tp2_data.index:
        row[f'timepoint_2_{col}'] = tp2_data[col]

    return row

def compute_avg_loss_and_time_distance_modified(dataset_df, columns_to_exclude, is_thickness_data=False):
    avg_loss_data = dataset_df.groupby('RID').apply(compute_avg_loss_and_time_distance, columns_to_exclude, is_thickness_data).reset_index(drop=True)
    merged_data = avg_loss_data.apply(merge_timepoint_data, axis=1)
    return merged_data


def process_data(dataset_name, field_strength):
    dataset_df = pd.read_csv(f'df_{dataset_name}.csv')
    
    # Ensure DATE column is in datetime format
    dataset_df['DATE'] = pd.to_datetime(dataset_df['DATE'])
    
    exclude_columns = ['RID', 'DATE', 'FIELD', 'TIV', 'names', 'TIVnames', 'TBI'] if dataset_name != "Thick" else ['RID', 'DATE', 'FIELD', 'TBI', 'names']
    columns = [col for col in dataset_df.columns if col not in exclude_columns]
    
    if dataset_name != "Thick":
        for col in columns:
            dataset_df[col] = dataset_df[col] / dataset_df['TIV']
    
    for field_strength in [1.5, 3.0]:
        filtered_data = dataset_df[dataset_df['FIELD'] == field_strength]
        result_df = compute_avg_loss_and_time_distance_modified(filtered_data, exclude_columns, dataset_name == "Thick")
        result_df.to_csv(f'average_{dataset_name}_loss_{field_strength}_with_timepoints.csv', index=False)

datasets = ["GM", "WM", "CSF", "Thick"]
for dataset_name in tqdm(datasets, desc="Overall Progress"):  # <-- Step 2: Add progress bar for datasets
    print(f"Processing {dataset_name} data...")  # <-- Step 3: Print statement
    try:
        for field_strength in tqdm([1.5, 3.0], desc=f"{dataset_name} Field Strength"):  # <-- Step 4: Add progress bar for field strengths
            print(f"Processing {dataset_name} data for {field_strength}T...")  # <-- Step 5: Print statement
            process_data(dataset_name, field_strength)
    except Exception as e:
        error_message = f"Error encountered while processing {dataset_name}: {str(e)}"
        print(error_message)
