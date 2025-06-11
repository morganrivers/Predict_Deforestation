#!/usr/bin/env Rscript

# Script to process FAOSTAT data files
library(dplyr)
library(readr)
library(tidyr)
library(janitor)

# Set working directory to the project folder
setwd("/home/dmrivers/Code/Predict_Deforestation")

# Function to read and process FAOSTAT data
process_faostat <- function(file_path) {
  # Read the data
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Filter to years 1990 and later
  data <- data %>% filter(as.numeric(Year) >= 1990) %>% filter(as.numeric(Year) <= 2024)
  
  return(data)
}

# Process Population data
pop_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_Pop.csv") %>%
  # Select relevant columns
  select(Area, "Area Code (M49)", Element, Year, Value) %>%
  # Multiply values by 1000 as noted
  mutate(Value = Value * 1000) %>%
  # Create separate columns for different population types
  pivot_wider(
    names_from = Element,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year,
    pop_total = total_population_both_sexes,
    pop_m = total_population_male,
    pop_f = total_population_female,
    pop_rural = rural_population,
    pop_urban = urban_population
  )

# Process GDP and Capital Formation data
gdp_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_GDP_Cap_Formation.csv") %>%
  # Select relevant columns
  select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
  # Filter for GDP and Capital Formation with specific element
  filter(Element == "Value US$, 2015 prices") %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year,
    gdp = gross_domestic_product,
    fixed_cap_formation = gross_fixed_capital_formation
  ) %>%
  select(-element)

# Process Mountain Area data
mountain_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_Mountain_Area.csv") %>%
  # Select relevant columns
  select(Area,"Area Code (M49)", Item, Year, Value) %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year
  )

# Rename mountain area columns
if("x15_4_2_mountain_area_total" %in% names(mountain_data)) {
  mountain_data <- mountain_data %>% rename(mtn_area_tot = x15_4_2_mountain_area_total)
}
if("x15_4_2_mountain_area_nival" %in% names(mountain_data)) {
  mountain_data <- mountain_data %>% rename(mtn_area_nival = x15_4_2_mountain_area_nival)
}
if("x15_4_2_mountain_area_alpine" %in% names(mountain_data)) {
  mountain_data <- mountain_data %>% rename(mtn_area_alpine = x15_4_2_mountain_area_alpine)
}
if("x15_4_2_mountain_area_montane" %in% names(mountain_data)) {
  mountain_data <- mountain_data %>% rename(mtn_area_montane = x15_4_2_mountain_area_montane)
}
if("x15_4_2_mountain_area_remaining_moutain_area" %in% names(mountain_data)) {
  mountain_data <- mountain_data %>% rename(mtn_area_remaining = x15_4_2_mountain_area_remaining_moutain_area)
}

# Process Agriculture, Forestry and Fishing Capital data
ag_cap_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_Ag_For_Fish_Cap_Form_Stocks.csv") %>%
  # Select relevant columns
  select(Area, "Area Code (M49)", Element, Item, Year, Value) %>%
  filter(Element == "Value US$, 2015 prices") %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year
  ) %>%
  select(-element)

# Rename agriculture columns
if("gross_fixed_capital_formation_agriculture_forestry_and_fishing" %in% names(ag_cap_data)) {
  ag_cap_data <- ag_cap_data %>% 
    rename(ag_fish_forest_cap_formation = gross_fixed_capital_formation_agriculture_forestry_and_fishing)
}
if("consumption_of_fixed_capital_agriculture_forestry_and_fishing" %in% names(ag_cap_data)) {
  ag_cap_data <- ag_cap_data %>% 
    rename(ag_fish_forest_consumption = consumption_of_fixed_capital_agriculture_forestry_and_fishing)
}
if("net_capital_stocks_agriculture_forestry_and_fishing" %in% names(ag_cap_data)) {
  ag_cap_data <- ag_cap_data %>% 
    rename(ag_fish_forest_net_capital = net_capital_stocks_agriculture_forestry_and_fishing)
}

# Process Forestland data
forestland_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_Forestland.csv")

# Extract the elements we need
forestland_area <- forestland_data %>%
  filter(Element == "Area", Source == "FAO TIER 1", Item == "Forestland") %>%
  select(Area, "Area Code (M49)", Year, Value) %>%
  rename(country = Area, m49_code = "Area Code (M49)", year = Year, forestland_area = Value)

forestland_emissions <- forestland_data %>%
  filter(Element == "Net emissions/removals (CO2) (Forest land)", 
         Source == "FAO TIER 1", 
         Item == "Forestland") %>%
  select(Area,"Area Code (M49)", Year, Value) %>%
  rename(country = Area,m49_code="Area Code (M49)", year = Year, net_emissions_removal_co2 = Value)


unfccc_emissions <- forestland_data %>%
  filter(Element == "Net emissions/removals (CO2) (Forest land)", 
         Source == "UNFCCC", 
         Item == "Forestland") %>%
  select(Area,"Area Code (M49)", Year, Value) %>%
  rename(country = Area,m49_code="Area Code (M49)", year = Year, unfccc_emissions = Value)

# Process CO2 Forest Conversion data (or use forestland as a fallback)
co2_conversion_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_CO2_Forest_Conversion.csv")

# Extract only Net Forest conversion data (not Forestland)
net_forest_area <- co2_conversion_data %>%
  filter(Element == "Area", Source == "FAO TIER 1") %>%
  filter(Item == "Net Forest conversion") %>%
  select(Area,"Area Code (M49)", Year, Value) %>%
  rename(country = Area,m49_code="Area Code (M49)", year = Year, net_forest_conversion_area = Value)

net_forest_emissions <- co2_conversion_data %>%
  filter(Element == "Net emissions/removals (CO2) (Forest land)", 
         Source == "FAO TIER 1") %>%
  filter(Item == "Net Forest conversion") %>%
  select(Area,"Area Code (M49)", Year, Value) %>%
  rename(country = Area, m49_code="Area Code (M49)", year = Year, net_forest_conversion_co2 = Value)



land_area_data <- process_faostat("data/FAOSTAT_data_en_5-21-2025_Area.csv") %>%
  # Select relevant columns
  select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
  # Filter for GDP and Capital Formation with specific element
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year,
    land_area = value,
  )

gdp_data <- process_faostat("data/FAOSTAT_data_en_5-20-2025_GDP_Cap_Formation.csv") %>%
  # Select relevant columns
  select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
  # Filter for GDP and Capital Formation with specific element
  filter(Element == "Value US$, 2015 prices") %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year,
    gdp = gross_domestic_product,
    fixed_cap_formation = gross_fixed_capital_formation
  ) %>%
  select(-element)

ag_price_data <- process_faostat("data/FAOSTAT_data_en_6-11-2025_Agricultural_Prices.csv") %>%
  # Select relevant columns
  select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
  # Filter for GDP and Capital Formation with specific element
  filter(Element == "Producer Price (USD/tonne)") %>%
  pivot_wider(
    names_from = Item,
    values_from = Value,
    names_repair = "unique"
  ) %>%
  clean_names() %>%
  rename(
    country = area,
    m49_code = area_code_m49,
    year = year,
    cattle_meat = meat_of_cattle_with_the_bone_fresh_or_chilled,
    soy = soya_beans,
    milk = raw_milk_of_cattle,
    sugar = sugar_cane,
    palm = palm_oil,
    coffee = coffee_green
  ) %>%
  mutate(across(c(cattle_meat, soy, milk, sugar, palm, coffee), log)) %>%
  select(-element)


# land_area_data <- process_faostat("data/FAOSTAT_data_en_6-11-2025_Wood_Prices_Quantities.csv") %>%
#   # Select relevant columns
#   select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
#   # Filter for GDP and Capital Formation with specific element
#   clean_names() %>%
#   rename(
#     country = area,
#     m49_code = area_code_m49,
#     year = year,
#     land_area = value,
#   )

# land_area_data <- process_faostat("data/FAOSTAT_data_en_6-11-2025_Agricultural_Production.csv") %>%
#   # Select relevant columns
#   select(Area,"Area Code (M49)", Element, Item, Year, Value) %>%
#   # Filter for GDP and Capital Formation with specific element
#   clean_names() %>%
#   rename(
#     country = area,
#     m49_code = area_code_m49,
#     year = year,
#     land_area = value,
#   )



# Save the processed data files
write_csv(pop_data, "data/processed_population_data.csv")
write_csv(gdp_data, "data/processed_gdp_data.csv")
write_csv(mountain_data, "data/processed_mountain_data.csv")
write_csv(ag_cap_data, "data/processed_ag_cap_data.csv")
write_csv(forestland_emissions, "data/processed_forestland_emissions_data.csv")
write_csv(unfccc_emissions, "data/processed_unfccc_emissions_data.csv")
write_csv(forestland_area, "data/processed_forestland_area_data.csv")
write_csv(land_area_data, "data/processed_land_area_data.csv")
write_csv(ag_price_data, "data/processed_ag_price_data.csv")

cat("All FAOSTAT data processed and saved to data/ directory\n")