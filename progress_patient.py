import numpy as np
from datetime import datetime, timedelta

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

    # Modify ACT score based on adherence
    current_act = patient.get("act_score", 20)  # Default 20 if not present
    if adherence > 80:
        act_change = np.random.uniform(0, 2)  # Improvement
    elif 50 <= adherence <= 80:
        act_change = np.random.uniform(-1, 1)  # Stable
    else:
        act_change = np.random.uniform(-2, 0)  # Decline
        
    new_act = round(min(max(current_act + act_change, 5), 25))
    new_patient["act_score"] = new_act
    
    # Update other variables
    for key in keys_to_modify:
        new_patient[key] = change_by_10_percent(patient[key])
    
    # Add review date, within 1 month of previous date, same year
    previous_review_date = datetime.strptime(
        patient.get("Review Date", datetime.now().strftime("%Y-%m-%d")), 
        "%Y-%m-%d"
    )
    
    # Add one year
    new_review_date = previous_review_date + timedelta(days=365)
    
    # Add random variation of ±1 month (30 days)
    month_variation = np.random.randint(-30, 31)
    new_review_date = new_review_date + timedelta(days=month_variation)
    
    # Ensure date stays in same year
    if new_review_date.year > previous_review_date.year + 1:
        new_review_date = datetime(new_review_date.year - 1, 12, 31)
    elif new_review_date.year < previous_review_date.year + 1:
        new_review_date = datetime(new_review_date.year + 1, 1, 1)
    
    new_patient["Review Date"] = new_review_date.strftime("%Y-%m-%d")

    return new_patient