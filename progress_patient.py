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
    
    # Convert review date string to date object if it's a string
    previous_review_date = (
        datetime.strptime(patient["review_date"], "%Y-%m-%d").date()
        if isinstance(patient["review_date"], str)
        else patient["review_date"]
    )
    
    # Add one year
    new_review_date = (previous_review_date + timedelta(days=365))
    
    # Add random variation of ±1 month (30 days)
    month_variation = np.random.randint(-30, 31)
    new_review_date = new_review_date + timedelta(days=month_variation)
    
    # Ensure date stays in same year
    if new_review_date.year > previous_review_date.year + 1:
        new_review_date = datetime(new_review_date.year - 1, 12, 31).date()
    elif new_review_date.year < previous_review_date.year + 1:
        new_review_date = datetime(new_review_date.year + 1, 1, 1).date()
    
    new_patient["review_date"] = new_review_date  # Store as date object

    # Generate potential exacerbation dates
    exacerbation_dates = []
    
    # Base probability based on asthma severity
    severity_probabilities = {
        "Mild": 0.1,
        "Moderate": 0.2,
        "Severe": 0.4
    }
    base_probability = severity_probabilities.get(patient["Asthma Severity"], 0.2)
    
    # Increase probability based on risk factors
    if adherence < 50:
        base_probability += 0.2
    if current_act < 15:
        base_probability += 0.2
    if patient["eosinophil_level"] > 0.45:
        base_probability += 0.1
    if patient["Smoking Status"] == "Current Smoker":
        base_probability += 0.15
        
    # Check for exacerbations (max 4 per year for severe asthma)
    max_exacerbations = 4 if patient["Asthma Severity"] == "Severe" else 3
    
    while (np.random.random() < base_probability and 
           len(exacerbation_dates) < max_exacerbations):
        # Generate random date between previous and new review
        days_between = (new_review_date - previous_review_date).days
        random_days = np.random.randint(0, days_between)
        exacerbation_date = previous_review_date + timedelta(days=random_days)
        
        # Ensure minimum 30 days between exacerbations
        if not exacerbation_dates or min(
            abs((existing_date - exacerbation_date).days)
            for existing_date in exacerbation_dates) >= 30:
            exacerbation_dates.append(exacerbation_date)
            base_probability *= 0.6
    
    # Store dates as list of date objects
    new_patient["exacerbation_dates"] = exacerbation_dates if exacerbation_dates else None

    return new_patient