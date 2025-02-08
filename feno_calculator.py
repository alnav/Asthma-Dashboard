import random

def calculate_feno(patient):
    """
    Calculate FeNO levels based on asthma severity and IgE levels.

    Parameters:
        patient (dict): Dictionary containing patient information with keys:
            - "Asthma Severity" (str): "Intermittent", "Mild", "Moderate", "Severe"
            - "total_ige" (float): Total IgE levels (optional, but used for further adjustment)
    
    Returns:
        dict: Updated patient dictionary with FeNO levels.
    """

    # Base FeNO levels in parts per billion (ppb) based on severity
    BASE_FENO_LEVELS = {
        "Intermittent": 15,  # Normal range
        "Mild": 20,
        "Moderate": 35,
        "Severe": 50,
    }

    # Additional FeNO increase based on total IgE level
    IGE_FENO_MULTIPLIER = {
        "low": 1.0,   # Normal IgE
        "moderate": 1.2,  # Slightly elevated IgE
        "high": 1.5,  # High IgE
        "very_high": 2.0,  # Very high IgE
    }

    # Extract severity and IgE values
    asthma_severity = patient.get("Asthma Severity", "Intermittent")
    total_ige = patient.get("total_ige")  # Default if IgE is not provided

    # Assign FeNO based on severity
    base_feno = BASE_FENO_LEVELS.get(asthma_severity)

    # Determine FeNO adjustment based on IgE levels
    if total_ige < 100:
        ige_category = "low"
    elif 100 <= total_ige < 200:
        ige_category = "moderate"
    elif 200 <= total_ige < 300:
        ige_category = "high"
    else:
        ige_category = "very_high"

    # Apply multiplier to FeNO based on IgE levels
    feno = base_feno * IGE_FENO_MULTIPLIER[ige_category]

    # Introduce slight randomness for variation
    feno *= random.uniform(0.9, 1.1)

    # Update patient dictionary with FeNO levels
    patient["FeNO_ppb"] = round(feno, 2)

    return patient