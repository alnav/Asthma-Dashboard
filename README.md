# Asthma Dashboard

A clinical dashboard to enhance asthma care by providing data visualization and predictive analytics.
[Click here to see it in action!](https://19545x-alessio-navarra.shinyapps.io/dashboard/)

## Features

- Patient data visualization with interactive plots
- Real-time metrics display including:
  - ACT (Asthma Control Test) scores
  - Lung function (FEV1, FVC)
  - FeNO and eosinophil levels
  - Medication adherence tracking
- Risk prediction model for asthma exacerbations (proof of concept)
- PDF/Text report generation
- Patient history tracking

## Technologies Used

- R Shiny for the web interface
- Python for data processing and calculations
- Plotly for interactive visualizations
- HTML/CSS for styling

## Setup

1. Install required R packages:
```r
install.packages(c("shiny", "readr", "ggplot2", "dplyr", "plotly", 
                  "flexdashboard", "rmarkdown", "knitr"))
```

2. Install required Python packages:
```python
pip install pandas numpy scikit-learn
```

3. Clone the repository:
```bash
git clone https://github.com/yourusername/asthma-dashboard.git
```

4. Run the dashboard:
```r
shiny::runApp()
```

## Contact

For questions or feedback, please contact [Dr Alessio Navarra](mailto:alessio.navarra1@nhs.net)