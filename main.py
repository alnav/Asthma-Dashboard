import csv
from patient_generator import new_patient
from fvc_calculator import calculate_fvc
from fev1_calculator import calculate_fev1
from eosinophils_calculator import calculate_eosinophils
from ige_calculator import calculate_ige
from pef_calculator import calculate_pef
from treatment import assign_treatment
from feno_calculator import calculate_feno
from adherence_calculator import assign_adherence
from progress_patient import progress_patient

def main():
    dataset = []

    for i in range(1, 1001):
        patient = new_patient()
        patient["ID"] = i

        calculate_fvc(patient)
        calculate_fev1(patient)
        calculate_eosinophils(patient)
        calculate_ige(patient)
        calculate_pef(patient)
        calculate_feno(patient)
        assign_adherence(patient)
        assign_treatment(patient)
        dataset.append(patient)

        # Progress the patient 5 times and add each progression to the dataset
        for _ in range(5):
            patient = progress_patient(patient)
            dataset.append(patient)

    # Export dataset to CSV
    keys = dataset[0].keys()
    with open('patient_dataset.csv', 'w', newline='') as output_file:
        dict_writer = csv.DictWriter(output_file, fieldnames=keys)
        dict_writer.writeheader()
        dict_writer.writerows(dataset)

    print("Dataset exported to patient_dataset.csv")

if __name__ == "__main__":
    main()