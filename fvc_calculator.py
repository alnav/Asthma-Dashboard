import numpy as np

def calculate_fvc(patient):
    ethnicity_factors = {
        "White": 1.00,
        "Black": 0.88,
        "Asian": 0.94,
        "Mixed": 0.96,
        "Other": 0.96
    }
    
    sex = patient["Sex"]
    age = patient["Age"]
    height = patient["Height (cm)"]
    ethnicity = patient["Ethnicity"]
    
    height_ln = np.log(height)
    age_ln = np.log(age)
    
    ethnicity_factor = ethnicity_factors.get(ethnicity, 1.00)
    
    if sex == "Male":
        fvc_pred = np.exp(-11.800108 + (2.620864 * height_ln) - (0.010504 * age_ln)) * ethnicity_factor
    else:  # Female
        fvc_pred = np.exp(-10.002108 + (2.341474 * height_ln) - (0.010204 * age_ln)) * ethnicity_factor
    
    fvc_measured = fvc_pred * np.random.uniform(0.9, 1.1)
    percent_predicted = round((fvc_measured / fvc_pred) * 100, 2)
    
# Update the patient dictionary with FVC results
    patient["fvc_actual"] = round(fvc_measured, 2)
    patient["fvc_predicted"] = round(fvc_pred, 2)
    patient["fvc_percent_predicted"] = percent_predicted

    return patient