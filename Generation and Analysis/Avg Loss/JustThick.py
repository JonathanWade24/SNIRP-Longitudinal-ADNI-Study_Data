import pandas as pd
from tqdm import tqdm

# Load the 'df_Thick.csv' and 'ADNIMERGE_03Aug2023.csv' data
thick_df = pd.read_csv('df_Thick.csv')
thick_df['DATE'] = pd.to_datetime(thick_df['DATE'])

adnimerge_df = pd.read_csv('ADNIMERGE_03Aug2023.csv')
adnimerge_df['EXAMDATE'] = pd.to_datetime(adnimerge_df['EXAMDATE'])

import pandas as pd
from tqdm import tqdm

# Load the data
thick_df = pd.read_csv('df_Thick.csv', low_memory=False)  # Use low_memory=False to suppress dtype warning
thick_df['DATE'] = pd.to_datetime(thick_df['DATE'])

adnimerge_df = pd.read_csv('ADNIMERGE_03Aug2023.csv', low_memory=False)
adnimerge_df['EXAMDATE'] = pd.to_datetime(adnimerge_df['EXAMDATE'])

def compute_avg_loss_and_time_distance(group, columns_to_exclude):
    group = group.sort_values(by='DATE')
    first_index = group.index[0]
    last_index = group.index[-1]

    timepoint_1 = group.at[first_index, 'DATE']
    timepoint_2 = group.at[last_index, 'DATE']

    years = (timepoint_2 - timepoint_1).days / 365.25

    avg_loss = {
        'RID': group['RID'].iloc[0],  # Use iloc to access the RID value
        'TBI': group['TBI'].iloc[0]
    }

    columns = [col for col in group.columns if col not in columns_to_exclude]
    for col in columns:
        initial_value = pd.to_numeric(group.at[first_index, col], errors='coerce')
        final_value = pd.to_numeric(group.at[last_index, col], errors='coerce')

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

def compute_avg_loss_for_thick():
    # Ensure 'DATE' column is in datetime format
    thick_df['DATE'] = pd.to_datetime(thick_df['DATE'])
    
    exclude_columns = ['RID', 'DATE', 'FIELD', 'TBI', 'names']
    
    for field_strength in [1.5, 3.0]:
        filtered_data = thick_df[thick_df['FIELD'] == field_strength]
        avg_loss_data = filtered_data.groupby('RID').apply(compute_avg_loss_and_time_distance, exclude_columns).reset_index(drop=True)
        result_df = avg_loss_data.apply(merge_timepoint_data, axis=1)
        result_df.to_csv(f'average_Thick_loss_{field_strength}T_with_timepoints.csv', index=False)

compute_avg_loss_for_thick()
