#!/usr/bin/env Rscript

# Script to process temperature data and calculate heating and cooling degree days
library(dplyr)
library(tidyr)
library(readr)

# Set working directory to the project folder
setwd("/home/dmrivers/Code/Predict_Deforestation")

# Read the temperature data
temp_data <- read_csv("data/monthly-average-surface-temperatures-by-year.csv", show_col_types = FALSE)

# The data is in a different format than expected
# The 'Year' column in this dataset actually represents the month (1-12)
# The actual years are column names (2025, 2024, etc.)

# First, let's reshape the data to a more usable format
temp_reshaped <- temp_data %>%
  # Convert to long format where years become values
  pivot_longer(
    cols = -c(Entity, Code, Year),
    names_to = "actual_year",
    values_to = "temp_celsius"
  ) %>%
  # Convert year to numeric
  mutate(actual_year = as.numeric(actual_year)) %>%
  # Rename the 'Year' column to 'month'
  rename(month = Year) %>%
  # Filter for years 1990 and later
  filter(actual_year >= 1990) %>%
  filter(actual_year <= 2024)

# Calculate HDD and CDD
# HDD: Sum of (18°C - avg_temp) for months where avg_temp < 18°C
# CDD: Sum of (avg_temp - 18°C) for months where avg_temp > 18°C
temp_with_degree_days <- temp_reshaped %>%
  mutate(
    hdd = pmax(18 - temp_celsius, 0),  # Heating degree days
    cdd = pmax(temp_celsius - 18, 0)   # Cooling degree days
  )

# Aggregate by country and year
annual_degree_days <- temp_with_degree_days %>%
  group_by(Entity, Code, actual_year) %>%
  summarize(
    hdd = sum(hdd, na.rm = TRUE),  # Sum HDDs for the year
    cdd = sum(cdd, na.rm = TRUE),  # Sum CDDs for the year
    .groups = "drop"
  ) %>%
  # Rename columns to match expected format
  rename(country = Entity, country_code = Code, year = actual_year)

# Save the processed data
write_csv(annual_degree_days, "data/processed_temperature_data.csv")

cat("Temperature data processed and saved to data/processed_temperature_data.csv\n")