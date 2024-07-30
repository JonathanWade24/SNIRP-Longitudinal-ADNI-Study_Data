import pandas as pd
import numpy as np
import nibabel as nib
import xml.etree.ElementTree as ET
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
output_dir = 'hd_figures/Nifti/'  # Simplified output directory path for NIfTI files
os.makedirs(output_dir, exist_ok=True)

# Directory containing CSV files
csv_directory = 'separated_by_terms_csvs/'
csv_files = [f for f in os.listdir(csv_directory) if f.endswith('.csv')]

# Process each CSV file
for csv_file in os.listdir(csv_directory):
    file_path = os.path.join(csv_directory, csv_file)
    if os.path.isfile(file_path):
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
        
        # Create NIfTI image from statistical map
        stat_map_img = nib.Nifti1Image(stat_map, atlas_img.affine)
        
        # Save the NIfTI image to a file
        nifti_file_name = os.path.splitext(csv_file)[0] + '_stat_map.nii'
        nifti_file_path = os.path.join(output_dir, nifti_file_name)
        nib.save(stat_map_img, nifti_file_path)

        print(f'Stat map saved to {nifti_file_path}')
