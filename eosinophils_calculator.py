import random

def calculate_eosinophils(patient):
    """
    Calculate eosinophil levels based on asthma severity.

    Parameters:
        patient (dict): Dictionary containing patient information with keys:
            - asthma_severity (str): "Intermittent", "Mild", "Moderate", "Severe"
    
    Returns:
        dict: Updated patient dictionary with eosinophil levels.
    """
    
    # Eosinophil levels based on asthma severity (example values)
    EOSINOPHIL_LEVELS = {
        "Intermittent": 0.2,  # cells/µL
        "Mild": 0.4,  # cells/µL
        "Moderate": 1,  # cells/µL
        "Severe": 2,  # cells/µL
    }

    # Extract asthma severity
    asthma_severity = patient["Asthma Severity"]

    # Base eosinophil level
    base_eosinophil_level = EOSINOPHIL_LEVELS.get(asthma_severity, 0.4)  # Default to 150 if severity not found

    # Introduce slight randomness around the base eosinophil level
    eosinophil_level = base_eosinophil_level + random.uniform(-0.1 * base_eosinophil_level, 0.1 * base_eosinophil_level)

    # Update the patient dictionary with eosinophil levels
    patient["eosinophil_level"] = round(eosinophil_level, 2)

    return patient