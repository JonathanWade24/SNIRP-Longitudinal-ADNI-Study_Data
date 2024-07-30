"""
Script Name: Refined TBI Search
Author: Jonathan Wade
Last Updated: 9/12/2023

Script Summary:

This script identifies potential Traumatic Brain Injury (TBI) events from a dataset based on specific keywords while also considering negations.
It provides a more nuanced approach by excluding entries that negate the presence of a TBI even if they contain a TBI-related keyword.

Features:
1. Recognizes an expanded list of TBI-related keywords.
2. Combines two columns of the dataset to ensure full context for keyword search (the additional column usually contains extra comments from the initial history).
3. Checks for common negation patterns to reduce false positives.
4. Prioritizes entries where only one TBI keyword is negated but another is present.
5. Produces a filtered dataset containing potential TBI events with reduced false positives.

NOTE that this script is meant to load the csv "Medical History Composite MASSIVE.csv" which should be present in the working directory before running.
NOTE that a manual review of this output is required to ensure false positives are exlcuded from our TBI population.
"""

# Import necessary libraries
import pandas as pd
import re

# Define TBI-related keywords
tbi_keywords_updated = [
    "head injury", "concussion", "brain injury", "traumatic brain injury", 
    "TBI", "cranial trauma", "mild traumatic brain injury", "mtbi", "hit head",
    "loss of consciousness", "LOC", "brain trauma", "skull fracture",
    "head trauma", "head impact", "brain contusion", "whiplash"
]

# Define negation patterns
negation_patterns = [
    r'\bno\b\s+\bLOC\b',
    r'\bdenies\b.*\bhitting head\b',
    r'\bdid not\b.*\bhit head\b'
]

# Function to check for TBI keywords and exclude negations with the further refined approach
def further_refined_tbi_check(text):
    lower_text = text.lower()
    
    # Store whether any TBI keyword without negation is present
    positive_indication_found = False
    
    # If there's a TBI keyword in the text
    for keyword in tbi_keywords_updated:
        if keyword in lower_text:
            
            # Check for negation patterns for the current keyword
            negation_found = False
            for pattern in negation_patterns:
                combined_pattern = pattern + ".*" + keyword
                if re.search(combined_pattern, lower_text) or re.search(keyword + ".*" + pattern, lower_text):
                    negation_found = True
                    break
            
            # If keyword is present without a negation
            if not negation_found:
                positive_indication_found = True
                break
    
    return positive_indication_found

# Load data
massive_data = pd.read_csv("Medical History Composite MASSIVE.csv")

# Append third column to the second column
massive_data['MHCOMMEN'] = massive_data['MHCOMMEN'].fillna('') + ' ' + massive_data['Unnamed: 2'].fillna('')

# Identify TBI likely entries using the further refined approach
massive_data['TBI_likely_further_refined'] = massive_data['MHCOMMEN'].apply(further_refined_tbi_check)

# Filter out TBI likely entries using the further refined approach
tbi_likely_massive_further_refined_df = massive_data[massive_data['TBI_likely_further_refined']]

# Export the identified entries to a CSV
tbi_likely_massive_further_refined_df.to_csv("TBI Search Results.csv", index=False)
