import numpy as np

def progress_patient(patient):
    # Helper function to change a value by +/- 10%
    def change_by_10_percent(value):
        return round(value * (1 + np.random.uniform(-0.1, 0.1)),2)
    
    # List of keys to modify by +/- 10%
    keys_to_modify = [
        "fvc_actual", "fvc_predicted", "fvc_percent_predicted",
        "fev1_actual", "fev1_predicted", "fev1_fvc_ratio",
        "fev1_percent_predicted", "eosinophil_level", "total_ige",
        "ige_pollen", "ige_cats", "ige_dogs", "ige_mould",
        "ige_grass", "ige_house_dust_mites", "pef", "FeNO_ppb"
    ]
    
    # Create a new patient dictionary with updated values
    new_patient = patient.copy()
    new_patient["Age"] += 1  # Increment age by 1
    
    for key in keys_to_modify:
        new_patient[key] = change_by_10_percent(patient[key])
    
    return new_patient