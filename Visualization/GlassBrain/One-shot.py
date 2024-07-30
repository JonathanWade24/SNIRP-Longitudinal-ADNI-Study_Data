import pandas as pd
import numpy as np
import nibabel as nib
from nilearn import plotting
import xml.etree.ElementTree as ET

# ROI Mapping from XML
xml_file_path = "labels_cat12_neuromorphometrics.xml"
tree = ET.parse(xml_file_path)
root = tree.getroot()

roi_mapping = {}
for label in root.findall('.//label'):
    index = int(label.find('index').text)
    name = label.find('name').text.strip()
    roi_mapping[name] = index

# Load atlas data
atlas_img = nib.load("cat12_neuromorphometrics.nii")
atlas_data = atlas_img.get_fdata()

# Initialize a statistical map
stat_map = np.zeros_like(atlas_data)

# Load your dataset
filtered_df = pd.read_csv('WM_fixed.csv')

# Significance Threshold
p_value_threshold = 0.05

for _, row in filtered_df.iterrows():
    roi_name = row['Outcome_Variable']
    estimate = row['estimate']
    p_value = row['p.value']  # Assuming you have a p-value column
    if roi_name in roi_mapping and p_value < p_value_threshold:
        index = roi_mapping[roi_name]
        stat_map[atlas_data == index] = estimate

# Invert the statistical map if necessary
inverted_stat_map = -1 * stat_map

# Create the NIfTI image from the statistical map
stat_map_img = nib.Nifti1Image(stat_map, atlas_img.affine)

# Adjust the colormap to have a midpoint at 0 if your data includes negative values
from nilearn.plotting.cm import _cmap_d as nilearn_cmaps  # To get nilearn-specific colormaps if needed

# Define the maximum absolute value for the color scale to create a balanced colormap
vmax = max(-stat_map.min(), stat_map.max())

# If using nilearn's colormap:
cmap = 'Reds'  # This is an example of a diverging colormap in nilearn

display = plotting.plot_glass_brain(
    stat_map_img,
    display_mode='lyrz',
    colorbar=True,
    cmap=cmap,
    black_bg=False,
    plot_abs=True,
    threshold = 0,
    vmax=vmax,
    vmin= 0,  # Ensures the color map is centered at zero
    symmetric_cbar='auto',  # Automatically adjusts the color bar to be symmetric
    title="Areas with significant decreases in WM volume associated with LOC Injuries",
    annotate=True
)

plotting.show()
