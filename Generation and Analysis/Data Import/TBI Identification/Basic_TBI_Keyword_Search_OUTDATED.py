"""
Script Name: Basic TBI Search
Author: Jonathan Wade
Last Updated: 9/05/2023

Script Summary:

This script identifies potential Traumatic Brain Injury (TBI) events from a dataset based on specific keywords.

Features:
1. Recognizes basic TBI-related keywords.
2. Combines two columns of the dataset to ensure full context for keyword search (the additional column usually contains extra comments from the initial history).
3. Produces a filtered dataset containing potential TBI events while account for basic negation like "no head injury"

NOTE that this script is meant to load the csv "Medical History Composite MASSIVE.csv" which should be present in the working directory before running.
NOTE that a manual review of this output is required to ensure false positives are exlcuded from our TBI population.
"""
import pandas as pd

# Load the dataset
file_path = 'Medical History Composite MASSIVE.csv'  # Change this to your file path
df = pd.read_csv(file_path)

# TBI-related keywords
tbi_keywords = [
    "head injury", "concussion", "brain injury", "traumatic brain injury", 
    "TBI", "cranial trauma", "mild traumatic brain injury", "mtbi", "hit head",
    "loss of consciousness", "LOC", "brain trauma", "skull fracture",
    "head trauma", "head impact", "brain contusion"
]

# Negation keywords
negations = ["no", "not", "denies", "deny", "denied", "without", "negative"]

# Function to determine if a description likely indicates TBI
def is_tbi(description):
    description = description.lower()
    for keyword in tbi_keywords:
        if keyword in description:
            for negation in negations:
                if negation + " " + keyword in description:
                    return False
                if keyword == "loc" and (negation + " loss of consciousness" in description or negation + " loc" in description):
                    return False
            return True
    return False

# Apply the function to the 'Description' column
df['TBI_Likely'] = df['Description'].apply(is_tbi)

# Save the dataframe with the new 'TBI_Likely' column to a new CSV file
df.to_csv('TBI RID Search Results.csv', index=False)  # Change this to your desired save path
