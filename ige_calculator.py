import random

def calculate_ige(patient):
    """
    Calculate total IgE and specific IgE levels for various allergens.

    Parameters:
        patient (dict): Dictionary containing patient information with keys:
            - asthma_severity (str): "Intermittent", "Mild", "Moderate", "Severe"
    
    Returns:
        dict: Updated patient dictionary with IgE levels.
    """
    
    # Base IgE levels (example values in IU/mL)
    BASE_IGE_LEVELS = {
        "total": 100,  # Total IgE
        "pollen": 20,  # IgE to pollen
        "cats": 15,  # IgE to cats
        "dogs": 10,  # IgE to dogs
        "mould": 5,  # IgE to mould
        "grass": 25,  # IgE to grass
        "house_dust_mites": 30,  # IgE to house dust mites
    }

    # Probability of having very high IgE levels based on severity
    HIGH_IGE_PROBABILITY = {
        "Intermittent": 0.0,
        "Mild": 0.10,
        "Moderate": 0.20,
        "Severe": 0.30,
    }

    # Extract asthma severity
    asthma_severity = patient["Asthma Severity"]

    # Determine if the patient falls into the category with very high IgE levels
    if random.random() < HIGH_IGE_PROBABILITY.get(asthma_severity, 0.0):
        # Significantly increase IgE levels for patients with high IgE probability
        ige_levels = {key: value * random.uniform(2, 5.0) for key, value in BASE_IGE_LEVELS.items()}
    else:
        # Introduce slight randomness around the base IgE levels for other patients
        ige_levels = {key: value + random.uniform(-0.1 * value, 0.1 * value) for key, value in BASE_IGE_LEVELS.items()}

    # Update the patient dictionary with IgE levels
    patient["total_ige"] = round(ige_levels["total"], 2)
    patient["ige_pollen"] = round(ige_levels["pollen"], 2)
    patient["ige_cats"] = round(ige_levels["cats"], 2)
    patient["ige_dogs"] = round(ige_levels["dogs"], 2)
    patient["ige_mould"] = round(ige_levels["mould"], 2)
    patient["ige_grass"] = round(ige_levels["grass"], 2)
    patient["ige_house_dust_mites"] = round(ige_levels["house_dust_mites"], 2)

    return patient