def calculate_fev1(patient):
    """
    Calculate FEV1 based on various parameters.

    Parameters:
        patient (dict): Dictionary containing patient information with keys:
            - age (int): Age in years
            - sex (str): "Male" or "Female"
            - height_cm (float): Height in cm
            - ethnicity (str): "White", "Black", "Asian", or "Other"
            - fvc_actual (float): Measured FVC (L)
            - asthma_severity (str): "Intermittent", "Mild Persistent", "Moderate Persistent", "Severe Persistent"
    
    Returns:
        dict: Actual FEV1, predicted FEV1, FEV1/FVC ratio, and FEV1% predicted.
    """
    
    # Constants
    MIN_FEV1_FVC_RATIO = 0.30
    ETHNICITY_CORRECTION = {"Black": 0.88, "Asian": 0.94, "Other": 0.92, "White": 1.0}
    FEV1_FVC_RATIOS = {
        "Male": {"White": 0.75, "Black": 0.70, "Asian": 0.74, "Other": 0.74},
        "Female": {"White": 0.78, "Black": 0.73, "Asian": 0.77, "Other": 0.77},
    }
    FEV1_FVC_REDUCTION = {
        "Intermittent": 0.0,  # No change (normal ratio)
        "Mild": -0.05,  # Slight decrease
        "Moderate": -0.10,  # Moderate decrease
        "Severe": -0.15,  # Significant decrease
    }

    # Extract patient information
    age = patient["Age"]
    sex = patient["Sex"]
    height_cm = patient["Height (cm)"]
    ethnicity = patient["Ethnicity"]
    fvc_actual = patient["fvc_actual"]
    asthma_severity = patient["Asthma Severity"]

    # Convert height to meters
    height_m = height_cm / 100

    # Predicted FEV1 based on ERS equations
    if sex == "Male":
        fev1_predicted = (4.30 * height_m) - (0.029 * age) - 2.49
    else:  # Female
        fev1_predicted = (3.95 * height_m) - (0.025 * age) - 2.60

    # Apply ethnicity correction factor
    fev1_predicted *= ETHNICITY_CORRECTION.get(ethnicity, 1.0)

    # Get normal FEV1/FVC ratio based on sex and ethnicity
    fev1_fvc_ratio_normal = FEV1_FVC_RATIOS.get(sex, {}).get(ethnicity, 0.75)


    # Apply asthma severity impact on FEV1/FVC
    fev1_fvc_ratio = max(MIN_FEV1_FVC_RATIO, fev1_fvc_ratio_normal + FEV1_FVC_REDUCTION.get(asthma_severity, 0))


    # Calculate actual FEV1 using predicted FEV1 and adjusted FEV1/FVC ratio
    fev1_actual = fev1_predicted * fev1_fvc_ratio

    # Calculate FEV1 percent predicted
    fev1_percent_predicted = (fev1_actual / fev1_predicted) * 100

    # Update the patient dictionary with FVC results
    patient["fev1_actual"] = round(fev1_actual, 2)
    patient["fev1_predicted"] = round(fev1_predicted, 2)
    patient["fev1_fvc_ratio"] = round(fev1_fvc_ratio,2)
    patient["fev1_percent_predicted"] = round(fev1_percent_predicted,2)

    return patient