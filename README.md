# Deforestation Prediction Model

A machine learning system to create incentive-preserving baseline predictions for forest emissions.

## Project Overview

This project implements a prediction model for forest-related CO2 emissions that can be used as part of a "Jurisdictional Reward Fund" system. The model predicts a country's expected deforestation rates based on characteristics that are not easily influenced by government policy, thus preserving incentives for good forest management.

### Objective

The primary goal is to create a model that can accurately predict a country's forest-related CO2 emissions using only data from other countries. This approach ensures that a country cannot influence its own baseline prediction, making it suitable for incentive programs that reward countries for keeping emissions below the predicted baseline.

### Methodology

The model uses machine learning to establish relationships between various socioeconomic, geographic, and demographic variables and forest-related CO2 emissions. For each country being evaluated:

1. Train a model on data from all other countries
2. Use the trained model to predict emissions for the target country
3. Compare actual emissions to predicted baseline

## Data Sources

The project combines data from multiple sources:

- **FAOSTAT**: Forest emissions, land area, agriculture/forestry capital, population, GDP, and more
- **UNFCCC**: Alternative emissions data (limited availability)
- **Temperature Data**: Monthly average surface temperatures from Copernicus Climate Change Service
- **World Bank Climate Change Knowledge Portal**: Additional climate variables

## Key Features

- Leave-one-out cross-validation approach (train on all countries except the target)
- Comparison of multiple modeling approaches (Random Forest, GLM, CSRF)
- Evaluation using RMSE, R², and other metrics
- Variable importance analysis
- Visualization of predictions vs. actual emissions

## Project Structure

- **Data Processing Scripts**:
  - `process_faostat_data.R`: Processes FAOSTAT datasets
  - `process_temperature_data.R`: Processes temperature data and calculates heating/cooling degree days
  - `combine_data.R`: Merges all data sources using UN M49 country codes
  - `prepare_all_data.R`: Main pipeline to run all data processing steps
  - `fetch_world_bank.py`: Downloads climate data from World Bank API

- **Analysis Scripts**:
  - `predict_forest_emissions.Rmd`: Main analysis notebook
  - `tune_RF_parameters_script.Rmd`: Hyperparameter tuning for Random Forest model -- not currently in use

- **Data Directory**:
  - Raw data files from FAOSTAT and other sources
  - Processed data files ready for modeling
  - Metadata files

## Models Used

1. **Random Forest**: Non-parametric ensemble method suitable for complex relationships
2. **Generalized Linear Model (GLM)**: Linear modeling approach
3. **Country Similarity Random Forest (CSRF)**: Custom approach that weights countries by similarity. See: `https://www.jstor.org/stable/24737234`

## Variables

The most effective predictors identified include:

```r
drivers <- c(
  "year",
  "pop_f",         # Female population
  "pop_m",         # Male population
  "pop_rural",     # Rural population
  "ag_fish_forest_cap_formation",  # Capital formation in agriculture/forestry/fishing
  "ag_fish_forest_consumption",    # Consumption in agriculture/forestry/fishing
  "gdp_percap"            # Gross Domestic Product (per capita)
)
```

## Key Findings

- Random Forest and CSRF models outperform GLM and baseline predictions
- Data quality issues exist in the FAOSTAT Tier 1 deforestation records
- Prediction quality is currently highly variable country-by-country. Some countries fit well, others don't. This may relate to Tier 1 data quality issues.
- You can inspect the results in this public google document:
`https://docs.google.com/document/d/1zLgyCeKdBxyoO3fVo4y1QxJPJJJ8rKXUvJRlwY7VNRY/edit?tab=t.0`

## Setup and Usage

### Requirements

- R (version 4.0 or higher)
- Python 3 (for data fetching scripts)
- Required R packages: tidyverse, randomForest, janitor, pdp, ranger
- Required Python packages: requests, pandas

### Installation

1. Clone this repository
2. Install required R packages:
   ```r
   install.packages(c("tidyverse", "randomForest", "janitor", "pdp", "ranger"))
   ```

### Data Preparation

1. Run the data preparation pipeline:
   ```bash
   Rscript prepare_all_data.R
   ```
   
   This will:
   - Process temperature data
   - Process FAOSTAT data
   - Combine all data sources
   - Generate the final dataset (`data/all_variables.RDS`)

### Running the Analysis

Open the main analysis notebook in rstudio: `predict_forest_emissions.Rmd' and run it. You will see the results for a selection of countries.

## Future Work

- Improve data quality by incorporating more accurate forest emissions datasets
- Add climate variables (precipitation, NPP) that may improve predictions
- Explore sub-national predictions (state/province level)
- Implement more sophisticated weighting schemes for country similarity

