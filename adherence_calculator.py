import random

def assign_adherence(patient):
    """
    Assign adherence based on population distribution:
    - 20% of patients have 100% adherence
    - 30% of patients have 50-80% adherence
    - 50% of patients have <50% adherence

    Parameters:
        patient (dict): Dictionary containing patient information

    Returns:
        dict: Updated patient dictionary with adherence %.
    """
    # Generate random number to determine group
    group = random.random()
    
    # Assign adherence based on group
    if group < 0.2:  # 20% chance
        adherence = 100
    elif group < 0.5:  # 30% chance (0.5 - 0.2 = 0.3)
        adherence = random.uniform(50, 80)
    else:  # 50% chance
        adherence = random.uniform(0, 50)
    
    # Round to 1 decimal place
    patient["Adherence"] = round(adherence, 1)
    
    return patient