import pandas as pd
import numpy as np
import nibabel as nib
from nilearn import plotting, datasets
import xml.etree.ElementTree as ET
import matplotlib.pyplot as plt
import os

# Define paths
xml_file_path = "labels_cat12_neuromorphometrics.xml"
atlas_file_path = "cat12_neuromorphometrics.nii"

# Parse XML for ROI mappings
tree = ET.parse(xml_file_path)
root = tree.getroot()
roi_mapping = {label.find('name').text.strip(): int(label.find('index').text) for label in root.findall('.//label')}

# Load the atlas data
atlas_img = nib.load(atlas_file_path)
atlas_data = atlas_img.get_fdata()

# Ensure output directory exists
output_dir = 'hd_figures/MNI/'  # Simplified output directory path
os.makedirs(output_dir, exist_ok=True)
# Directory containing CSV files
csv_directory = 'separated_by_terms_csvs/'
csv_files = [f for f in os.listdir(csv_directory) if f.endswith('.csv')]

# Titles for the figures based on the CSV filenames
titles = {
    'CSF_final_new_injury_severity1.csv': 'CSF TBI without LOC',
    'CSF_final_new_injury_severity1_months_since_bl_exam.csv': 'CSF Attenuation Associated with TBI without LOC',
    'CSF_final_new_injury_severity2.csv': 'CSF TBI with LOC',
    'CSF_final_new_injury_severity2_months_since_bl_exam.csv': 'CSF Attenuation Associated with TBI with LOC',
    'GM_final_new_injury_severity1.csv': 'GM TBI without LOC',
    'GM_final_new_injury_severity1_months_since_bl_exam.csv': 'GM Attenuation Associated with TBI without LOC',
    'GM_final_new_injury_severity2.csv': 'GM TBI with LOC',
    'GM_final_new_injury_severity2_months_since_bl_exam.csv': 'GM Attenuation Associated with TBI with LOC',
    'WM_final_new_injury_severity1.csv': 'WM TBI without LOC',
    'WM_final_new_injury_severity1_months_since_bl_exam.csv': 'WM Attenuation Associated with TBI without LOC',
    'WM_final_new_injury_severity2.csv': 'WM TBI with LOC',
    'WM_final_new_injury_severity2_months_since_bl_exam.csv': 'WM Attenuation Associated with TBI with LOC'
}

# Load the MNI T1 template
mni_t1_template = datasets.load_mni152_template()

# Process each CSV file
for csv_file in os.listdir(csv_directory):
    file_path = os.path.join(csv_directory, csv_file)
    if os.path.isfile(file_path) and csv_file in titles:
        # Load dataset
        df = pd.read_csv(file_path)
        
        # Initialize a new statistical map
        stat_map = np.zeros_like(atlas_data)
        
       # Apply statistical estimates to the stat map
        for _, row in df.iterrows():
            roi_name = row['Outcome_Variable']
            estimate = row['estimate']
    
            # Skip processing for "Cerebral White Matter" outcomes
            if "Cerebral White Matter" in roi_name:
             continue
    
            if roi_name in roi_mapping:
                index = roi_mapping[roi_name]
                stat_map[atlas_data == index] = estimate
        
        # Determine colormap and normalization based on estimates range
        min_estimate, max_estimate = df['estimate'].min(), df['estimate'].max()
        if min_estimate < 0 and max_estimate <= 0:
            # Only negative estimates
            cmap = 'Blues_r'
            vmax = 0  # Max set to 0 as there are only negative values
            vmin = min_estimate  # Minimum value in the dataset
        elif min_estimate >= 0 and max_estimate > 0:
            # Only positive estimates
            cmap = 'Reds'
            vmax = max_estimate  # Maximum value in the dataset
            vmin = 0  # Min set to 0 as there are only positive values
        else:
            # Ranges that include both positive and negative
            cmap = 'coolwarm'
            vmax = max(abs(min_estimate), abs(max_estimate))
            vmin = -vmax
        # Create NIfTI image from statistical map
        stat_map_img = nib.Nifti1Image(stat_map, atlas_img.affine)
        
        # Generate and save the plot using the MNI T1 template
        if "months_since_bl_exam" in csv_file:
            units = " (mL/month)"
        else:
            units = " (mL)"
        title_with_units = titles[csv_file] + units
        display = plotting.plot_stat_map(stat_map_img, bg_img=mni_t1_template, display_mode='tiled',
                                         colorbar=True, cmap=cmap, threshold=0, vmax=vmax, vmin=vmin,
                                         title=title_with_units, draw_cross= False)
        
        print(stat_map_img.get_fdata().min(), stat_map_img.get_fdata().max())
        
        # Correctly define the figure_path using the modified title
        safe_title = title_with_units.replace(' ', '_').replace('/', '_per_')
        figure_path = os.path.join(output_dir, safe_title + '.png')
        
        display.savefig(figure_path, dpi=300)
        display.close()