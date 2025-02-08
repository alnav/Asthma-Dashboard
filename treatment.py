# Define the treatment mapping
asthma_treatment_map = {
    "Intermittent": "PRN_LABA-ICS",  # As-needed low dose ICS-formoterol
    "Mild": "LD_LABA-ICS",  # Low dose maintenance ICS-formoterol
    "Moderate": "MD_LABA-ICS",  # Medium dose maintenance ICS-formoterol
    "Severe": "HD_LABA-ICS_LAMA"  # High dose ICS-formoterol + LAMA
}

# Function to assign treatment based on severity
def assign_treatment(patient):
    """
    Assigns asthma treatment based on the severity level.

    :param patient: Dictionary containing at least the key 'severity'
    :return: Updated patient dictionary with 'treatment' assigned
    """
    treatment = asthma_treatment_map.get(patient["Asthma Severity"])  # Default to biologics for unclassified cases
    patient["treatment"] = treatment
    return patient

