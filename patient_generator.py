import numpy as np
from datetime import datetime, timedelta
import random

def generate_nhs_number():
    """Generate a valid NHS number with check digit"""
    while True:
        # Generate the first 9 digits randomly
        digits = [random.randint(0, 9) for _ in range(9)]
        
        # Apply weighting: multiply each digit by its weight (10 to 2)
        total = sum(d * (10 - i) for i, d in enumerate(digits))
        
        # Calculate check digit
        check_digit = 11 - (total % 11)
        
        # If check digit is 11, set to 0
        # If check digit is 10, number is invalid - try again
        if check_digit == 11:
            check_digit = 0
        elif check_digit == 10:
            continue
            
        digits.append(check_digit)
        nhs_number = ''.join(map(str, digits))
        return f"{nhs_number[:3]} {nhs_number[3:6]} {nhs_number[6:]}"


# Function to create a single patient
def new_patient():

    # Define a realistic job list
    job_list = [
    "Construction Worker", "Factory Operative", "Cleaner", "Gardener", "Mechanic",
    "Warehouse Packer", "Farm Labourer", "Delivery Driver", "Plumber", "Electrician",
    "Nurse", "Healthcare Assistant", "GP Receptionist", "Physiotherapist", "Pharmacist",
    "Care Home Worker", "Radiographer", "Occupational Therapist", "Administrative Assistant",
    "Call Centre Agent", "Customer Service Rep", "Data Entry Clerk", "HR Officer",
    "Receptionist", "IT Support Technician", "Project Manager", "Procurement Assistant",
    "Teacher", "Teaching Assistant", "School Administrator", "Librarian",
    "University Lecturer", "Social Worker", "Police Officer", "Firefighter", "Council Officer",
    "Chef", "Kitchen Porter", "Waiter", "Bartender", "Barista", "Hotel Housekeeper",
    "Hairdresser", "Beautician", "Retail Assistant", "Supermarket Cashier",
    "Software Developer", "Civil Engineer", "Accountant", "Architect", "Lab Technician",
    "Statistician", "Mechanical Engineer", "Graphic Designer", "Surveyor", "Environmental Consultant",
    "Musician", "Actor", "Photographer", "Writer", "Yoga Instructor", "Personal Trainer",
    "Influencer", "Taxi Driver", "Freelance Translator", "Antique Dealer"
    ]

    # Define pet distribution
    pet_distribution = {
    "No Pets": 0.38,
    "Dog": 0.27,
    "Cat": 0.24,
    "Dog and Cat": 0.03,
    "Small Mammal": 0.025,
    "Fish": 0.03,
    "Bird": 0.015,
    "Reptile": 0.005,
    "Other": 0.005
    }
    
    # Generate random review date in 2020
    start_date = datetime(2020, 1, 1)
    end_date = datetime(2020, 12, 31)
    days_between = (end_date - start_date).days
    random_days = np.random.randint(0, days_between + 1)
    review_date = (start_date + timedelta(days=random_days)).date()

    # Randomly assign sex
    sex = np.random.choice(["Male", "Female"])
    
    # Generate age (18-80 years)
    age = np.random.randint(18, 80)
    
    # Calculate birth date based on age (assuming today is August 1, 2025)
    reference_date = datetime(2025, 8, 1)
    birth_year = reference_date.year - age
    birth_month = np.random.randint(1, 13)
    
    # Adjust birth year if birth month is after August
    if birth_month > reference_date.month:
        birth_year -= 1
    
    # Get max days for the chosen month
    if birth_month in [4, 6, 9, 11]:
        max_days = 30
    elif birth_month == 2:
        # Simple leap year calculation
        max_days = 29 if birth_year % 4 == 0 and (birth_year % 100 != 0 or birth_year % 400 == 0) else 28
    else:
        max_days = 31
        
    birth_day = np.random.randint(1, max_days + 1)
    birth_date = datetime(birth_year, birth_month, birth_day).strftime("%Y-%m-%d")
    
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

    # Randomly assign a job
    job = np.random.choice(job_list) + ", Retired" if age > 65 else np.random.choice(job_list)

    # Randomly assign a pet based on the distribution
    pet = np.random.choice(list(pet_distribution.keys()), p=list(pet_distribution.values()))

    # Add lab simulation
    pneumococcal_ab = np.random.uniform(1.3, 50) if np.random.rand() < 0.85 else np.random.uniform(0.5, 1.3)
    tetanus_ab = np.random.uniform(0.1, 3.0) if np.random.rand() < 0.9 else np.random.uniform(0.01, 0.1)
    c3 = np.random.uniform(0.9, 1.8) if np.random.rand() < 0.95 else np.random.uniform(0.4, 0.89)
    c4 = np.random.uniform(0.1, 0.4) if np.random.rand() < 0.95 else np.random.uniform(0.01, 0.09)
    anca = "Negative" if np.random.rand() < 0.95 else "Positive"
    ana = "Negative" if np.random.rand() < 0.85 else np.random.choice(["Low positive (1:40)", "Low positive (1:80)"])
    quantiferon = "Negative" if np.random.rand() < 0.95 else "Positive"

    names_by_ethnicity_and_sex = {
    "White": {
        "Male": [
            "James", "Oliver", "William", "George", "Thomas", "Henry",
            "Jack", "Daniel", "Harry", "Liam", "Edward", "Matthew"
        ],
        "Female": [
            "Emily", "Charlotte", "Sophia", "Amelia", "Isla", "Grace",
            "Lucy", "Molly", "Ella", "Olivia", "Chloe", "Hannah"
        ]
    },
    "Black": {
        "Male": [
            "Ethan", "Jayden", "Elijah", "Malik", "Isaiah", "Micah",
            "Kwame", "Omari", "Tariq", "Zion", "Jabari", "Darius"
        ],
        "Female": [
            "Aaliyah", "Imani", "Zara", "Ayanna", "Nia", "Brielle",
            "Sade", "Chanel", "Aminata", "Tiana", "Keisha", "Nyla"
        ]
    },
    "Asian": {
        "Male": [
            "Arjun", "Ravi", "Kiran", "Anil", "Farhan", "Haruto",
            "Rakesh", "Ali", "Deep", "Jin", "Ahmed", "Dev"
        ],
        "Female": [
            "Mei", "Aisha", "Sakura", "Priya", "Nisha", "Tanvi",
            "Min", "Lina", "Deepa", "Yuna", "Rani", "Sara"
        ]
    },
    "Mixed": {
        "Male": [
            "Liam", "Noah", "Lucas", "Cameron", "Leo", "Miles",
            "Aiden", "Kai", "Zion", "Jalen", "Jude", "Ezra"
        ],
        "Female": [
            "Amara", "Layla", "Maya", "Sienna", "Elena", "Naomi",
            "Talia", "Ava", "Jasmine", "Zoe", "Luna", "Skye"
        ]
    },
    "Other": {
        "Male": [
            "Alex", "Jordan", "Morgan", "Casey", "Riley", "Skyler",
            "Phoenix", "River", "Dakota", "Quinn", "Ariel", "Devon"
        ],
        "Female": [
            "Taylor", "Morgan", "Casey", "Riley", "Dakota", "Quinn",
            "Reese", "Rowan", "Justice", "Oakley", "Ariel", "Sky"
        ]
    }
    }

    surnames_by_ethnicity = {
    "White": [
        "Smith", "Jones", "Taylor", "Brown", "Williams", "Wilson",
        "Evans", "Thomas", "Roberts", "Walker", "White", "Hall",
        "Wright", "Green", "Baker", "Carter", "Mitchell", "King"
    ],
    "Black": [
        "Johnson", "Jackson", "Davis", "Harris", "Robinson", "Walker",
        "Lewis", "Young", "Allen", "Brooks", "Campbell", "Stewart",
        "Simpson", "Grant", "Nelson", "Ellis", "Morris", "Foster"
    ],
    "Asian": [
        "Chen", "Patel", "Khan", "Lee", "Singh", "Kim",
        "Ahmed", "Nguyen", "Wong", "Sharma", "Ali", "Islam",
        "Hussain", "Jain", "Zhang", "Iqbal", "Rao", "Tan"
    ],
    "Mixed": [
        "Anderson", "Thomas", "Moore", "Martin", "White", "Clark",
        "Lewis", "Jackson", "James", "Bell", "Hill", "Adams",
        "Ward", "Turner", "Morgan", "Murphy", "Watson", "Palmer"
    ],
    "Other": [
        "Young", "Scott", "Green", "Adams", "Hill", "Baker",
        "Cooper", "Hughes", "Murray", "Reed", "Ford", "Shaw",
        "Spencer", "May", "Francis", "Austin", "Dixon", "Marsh"
    ]
    }
    
    first_name = np.random.choice(names_by_ethnicity_and_sex[ethnicity][sex])
    surname = np.random.choice(surnames_by_ethnicity[ethnicity])
    full_name = f"{first_name} {surname}"

    # Generate NHS number
    nhs_number = generate_nhs_number()

    # Return patient object as a dictionary
    return {
        "Name": full_name,
        "nhs_number": nhs_number,
        "birth_date": birth_date,
        "Sex": sex,
        "Age": age,
        "Job": job,
        "Pet": pet,
        "Height (cm)": height,
        "Weight (kg)": weight,
        "BMI": bmi,
        "Ethnicity": ethnicity,
        "Smoking Status": smoking_status,
        "Smoking Status Encoded": smoking_status_one_hot,
        "Asthma Severity": asthma_severity,
        "act_score": act_score,
        "pneumococcal_abs": round(pneumococcal_ab, 2),
        "tetanus_abs": round(tetanus_ab, 3),
        "c3": round(c3, 2),
        "c4": round(c4, 2),
        "anca": anca,
        "ana": ana,
        "quantiferon": quantiferon,
        "review_date": review_date.strftime("%Y-%m-%d"),
        "exacerbation_dates": []
    }
