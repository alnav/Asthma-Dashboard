import unittest
from fev1_calculator import calculate_fev1

class TestFEV1Calculator(unittest.TestCase):
    def test_calculate_fev1(self):
        result = calculate_fev1(
            age=30,
            sex="Male",
            height_cm=175,
            ethnicity="White",
            fvc_actual=4.5,
            asthma_severity="Intermittent"
        )
        self.assertAlmostEqual(result["fev1_actual"], 3.375, places=2)
        self.assertAlmostEqual(result["fev1_predicted"], 4.30 * 1.75 - 0.029 * 30 - 2.49, places=2)
        self.assertAlmostEqual(result["fev1_fvc_ratio"], 0.75, places=2)
        self.assertAlmostEqual(result["fev1_percent_predicted"], (3.375 / (4.30 * 1.75 - 0.029 * 30 - 2.49)) * 100, places=2)

    def test_calculate_fev1_with_asthma(self):
        result = calculate_fev1(
            age=30,
            sex="Female",
            height_cm=160,
            ethnicity="Black",
            fvc_actual=3.5,
            asthma_severity="Moderate Persistent"
        )
        self.assertAlmostEqual(result["fev1_actual"], 2.205, places=2)
        self.assertAlmostEqual(result["fev1_predicted"], (3.95 * 1.60 - 0.025 * 30 - 2.60) * 0.88, places=2)
        self.assertAlmostEqual(result["fev1_fvc_ratio"], 0.63, places=2)
        self.assertAlmostEqual(result["fev1_percent_predicted"], (2.205 / ((3.95 * 1.60 - 0.025 * 30 - 2.60) * 0.88)) * 100 * 0.70, places=2)

if __name__ == '__main__':
    unittest.main()