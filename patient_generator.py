import numpy as np

# Function to create a single patient
def new_patient():
    # Randomly assign sex
    sex = np.random.choice(["Male", "Female"])
    
    # Generate age (18-80 years)
    age = np.random.randint(18, 80)
    
    # Generate height based on sex (in cm)
    height = np.round(
        np.random.normal(175, 7.5) if sex == "Male" else np.random.normal(162, 7.5), 1
    )
    
    # Generate weight based on sex (in kg)
    weight = np.round(
        np.random.normal(78, 10) if sex == "Male" else np.random.normal(65, 8), 1
    )
    
    # Calculate BMI (kg/m²)
    bmi = np.round(weight / ((height / 100) ** 2), 1)

    # Ethnicity categories with approximate population distribution
    ethnicities = ["White", "Black", "Asian", "Mixed", "Other"]
    ethnicity_probs = [0.75, 0.10, 0.10, 0.04, 0.01]
    ethnicity = np.random.choice(ethnicities, p=ethnicity_probs)

    # Smoking status categories
    smoking_statuses = ["Never Smoked", "Former Smoker", "Current Smoker"]
    smoking_probs = [0.7, 0.2, 0.1]  # Approximate smoking prevalence
    smoking_status = np.random.choice(smoking_statuses, p=smoking_probs)

    # One-hot encode smoking status
    smoking_status_encoded = {
        "Never Smoked": [1, 0, 0],
        "Former Smoker": [0, 1, 0],
        "Current Smoker": [0, 0, 1]
    }
    smoking_status_one_hot = smoking_status_encoded[smoking_status]

    # Asthma severity categories and probabilities
    asthma_severities = [
        "Intermittent",
        "Mild",
        "Moderate",
        "Severe"
    ]
    severity_probs = [0.40, 0.30, 0.20, 0.10]  # Adjusted probabilities
    asthma_severity = np.random.choice(asthma_severities, p=severity_probs)

    # Return patient object as a dictionary
    return {
        "Sex": sex,
        "Age": age,
        "Height (cm)": height,
        "Weight (kg)": weight,
        "BMI": bmi,
        "Ethnicity": ethnicity,
        "Smoking Status": smoking_status,
        "Smoking Status Encoded": smoking_status_one_hot,
        "Asthma Severity": asthma_severity
    }
