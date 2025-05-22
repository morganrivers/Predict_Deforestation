#!/usr/bin/env Rscript

# Main script to run all data processing steps
cat("Starting data processing pipeline...\n")

# Install required packages if not already installed
required_packages <- c("dplyr", "readr", "tidyr", "janitor")
new_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(new_packages)) {
  cat("Installing required packages:", paste(new_packages, collapse=", "), "\n")
  install.packages(new_packages, repos = "https://cloud.r-project.org")
}

# Set working directory to the project folder
setwd("/home/dmrivers/Code/lennart_predicting_forest_emissions")

# Step 1: Process temperature data
cat("\nStep 1: Processing temperature data...\n")
source("process_temperature_data.R")

# Step 2: Process FAOSTAT data
cat("\nStep 2: Processing FAOSTAT data...\n")
source("process_faostat_data.R")

# Step 3: Combine all data
cat("\nStep 3: Combining all processed data...\n")
source("combine_data.R")

cat("\nAll data processing complete! The dataset is ready for analysis.\n")
cat("You can now run the predict_forest_emissions.Rmd file to perform the analysis.\n")