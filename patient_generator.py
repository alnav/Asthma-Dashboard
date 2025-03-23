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
    
    # Generate ACT score based on asthma severity with random variation
    # ACT scores range from 5-25, with higher scores indicating better control
    # The median scores for each severity level are estimated based on clinical observations
    act_score_means = {
        "Intermittent": 22,  # Well controlled (20-25)
        "Mild": 19,          # Borderline controlled (16-19)
        "Moderate": 15,      # Not well controlled (15-19)
        "Severe": 11         # Poorly controlled (5-14)
    }
    
    # Standard deviations to add randomness while keeping scores mostly in the expected range
    act_score_stds = {
        "Intermittent": 1.5,
        "Mild": 2.0,
        "Moderate": 2.5,
        "Severe": 3.0
    }
    
    # Calculate raw ACT score with randomness
    raw_act_score = np.random.normal(
        act_score_means[asthma_severity], 
        act_score_stds[asthma_severity]
    )
    
    # Ensure ACT score is within valid range (5-25) and is an integer
    act_score = int(np.clip(round(raw_act_score), 5, 25))

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
        "Asthma Severity": asthma_severity,
        "act_score": act_score
    }
