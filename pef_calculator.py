def calculate_pef(patient):
    """
    Estimate Peak Expiratory Flow (PEF) from FEV1 using the Nunn & Gregg formula.

    :param fev1: Forced Expiratory Volume in 1 second (FEV1) in liters
    :return: Estimated Peak Expiratory Flow (PEF) in L/min
    """
    fev1 = patient["fev1_actual"]
    patient["pef"] = round(367 * (fev1 ** 0.77), 2)


