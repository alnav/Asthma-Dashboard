import numpy as np

def progress_patient(patient):
    def change_by_10_percent(value):
        return round(value * (1 + np.random.uniform(-0.1, 0.1)), 2)
    
    keys_to_modify = [
        "fvc_actual", "fvc_predicted", "fvc_percent_predicted",
        "fev1_predicted", "fev1_fvc_ratio",
        "fev1_percent_predicted", "eosinophil_level", "total_ige",
        "ige_pollen", "ige_cats", "ige_dogs", "ige_mould",
        "ige_grass", "ige_house_dust_mites", "pef", "FeNO_ppb"
    ]
    
    new_patient = patient.copy()
    new_patient["Age"] += 1
    
    # Modify FEV1 based on adherence
    adherence = patient.get("Adherence", 0)
    if adherence > 80:
        fev1_decline = -0.9  # Increase
    elif 50 <= adherence <= 80:
        fev1_decline = 0.06  # Slow decline
    else:
        fev1_decline = 0.09  # Rapid decline
        
    new_patient["fev1_actual"] = round(patient["fev1_actual"] - fev1_decline, 2)

    # Modify adherence by +/- 20% if not 100%
    if adherence != 100:
        adherence_change = np.random.uniform(-20, 20)
        new_adherence = adherence + adherence_change
        new_patient["Adherence"] = round(min(max(new_adherence, 0), 100), 1)
    
    # Update other variables
    for key in keys_to_modify:
        new_patient[key] = change_by_10_percent(patient[key])
    
    return new_patient