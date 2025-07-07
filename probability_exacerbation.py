import numpy as np

def estimate_exacerbation_risk(fev1_percent_pred, eosinophils, total_ige, asthma_severity, smoking_status, feno, adherence):
    """
    Estimate the probability of an asthma exacerbation in the next month
    using a logistic regression-based approximation.
    
    Parameters:
    - fev1_percent_pred: FEV1 % predicted
    - eosinophils: Blood eosinophil count (cells/µL)
    - total_ige: Total IgE level (IU/mL)
    - asthma_severity: 1 for Mild, 2 for Moderate, 3 for Severe
    - smoking_status: One-hot encoded smoking status [Never Smoked, Former Smoker, Current Smoker]

    Returns:
    - Probability of exacerbation (0-1)
    """
   # Logistic regression coefficients (from literature-based approximation)
    beta_0 = -2.5  # Baseline risk
    beta_fev1 = -0.03  # Protective effect of higher FEV1 % predicted
    beta_eos = 0.001  # Higher eosinophils increase risk
    beta_ige = 0.001  # IgE contribution to risk
    beta_severity = 1.2  # Severe asthma significantly increases risk
    beta_smoking = [0.0, 0.5, 1.0]  # Coefficients for smoking status
    beta_feno = 0.01  # FeNO contribution to risk
    beta_adherence = -0.5  # Better adherence reduces

    # Compute the log-odds
    log_odds = (
        beta_0 
        + beta_fev1 * fev1_percent_pred
        + beta_eos * eosinophils
        + beta_ige * total_ige
        + beta_severity * asthma_severity
        + np.dot(beta_smoking, smoking_status)
        + beta_feno * feno
        + beta_adherence * adherence
    )

    # Convert to probability using logistic function
    probability = 1 / (1 + np.exp(-log_odds))
    return probability
