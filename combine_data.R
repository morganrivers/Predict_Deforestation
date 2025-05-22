#!/usr/bin/env Rscript

# ---------------------------------------------------------------------------
# combine_data.R  – Create the final all_variables.RDS using a common M49 key
# ---------------------------------------------------------------------------
# 2025‑05‑21
# This script rewrites the previous “combine all datasets” routine so that
# every source is harmonised on the official UN M49 area code.  The lookup
# table is generated once from the UNSD country/area list and then attached
# to every dataset that is originally keyed by ISO‑alpha‑3.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(purrr)
})

# ---------------------------------------------------------------------------
# 0 . Paths
# ---------------------------------------------------------------------------
root <- "/home/dmrivers/Code/Predict_Deforestation"  # project root
setwd(root)
raw   <- file.path(root, "data")
# ---------------------------------------------------------------------------
# 1 . Master country lookup  (UNSD M49 ↔ ISO‑2/ISO‑3)
# ---------------------------------------------------------------------------
lookup <- read_csv(
  file.path(raw, "2022-09-24__CSV_UNSD_M49.csv"),
  quote       = "",                 # <‑‑ disable quoting: rows are fully quoted
  col_types   = cols(.default = "c"),
  trim_ws     = TRUE
) %>%
  rename_with(~ gsub('"', '', .x)) %>%            # clean quotes from header names
  mutate(across(everything(), ~ gsub('"', '', .x))) %>%  # …and from values
  transmute(
    m49      = as.integer(`M49 Code`),
    iso2     = `ISO-alpha2 Code`,
    iso3     = `ISO-alpha3 Code`,
    name_off = `Country or Area`
  ) %>%
  filter(!is.na(m49) & !is.na(iso3)) %>%
  distinct(iso3, .keep_all = TRUE)                # guard against dup ISO‑3

# Helper to attach M49 to any dataset that has ISO‑3 (column “country_code”)
add_m49 <- function(df, by_iso_col = "country_code") {
  if (!by_iso_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in dataset %s", by_iso_col, deparse(substitute(df))))
  }
  left_join(df, lookup, by = setNames("iso3", by_iso_col)) %>%
    relocate(m49, .before = 1)  # put m49 first for readability
}

# ---------------------------------------------------------------------------
# 2 . Load processed datasets & attach m49
# ---------------------------------------------------------------------------
read_proc_no_m49 <- function(name, iso_col = "country_code") {
  read_csv(file.path(raw, paste0("processed_", name, "_data.csv")), show_col_types = FALSE) %>%
    add_m49(by_iso_col = iso_col)
}

# read_proc <- function(name) {
#   read_csv(file.path(raw, paste0("processed_", name, "_data.csv")), show_col_types = FALSE) %>%
#     rename(m49 = m49_code)
# }
read_proc <- function(name) {
  read_csv(file.path(raw, paste0("processed_", name, "_data.csv")), show_col_types = FALSE) %>%
    rename(m49 = m49_code) %>%
    mutate(m49 = as.integer(m49))
}



temp_data <- read_proc_no_m49("temperature") %>%
  filter(!is.na(m49))  # Remove rows without m49 codes

pop_data        <- read_proc("population")
gdp_data        <- read_proc("gdp")
mountain_data   <- read_proc("mountain")
ag_cap_data     <- read_proc("ag_cap")
forestland_emissions_data <- read_proc("forestland_emissions")
forestland_area_data <- read_proc("forestland_area")
land_area_data <- read_proc("land_area")
unfccc_emissions_data <- read_proc("unfccc_emissions")

# ---------------------------------------------------------------------------
# 3 . Standardise year & list for coverage checks
# ---------------------------------------------------------------------------
standardise_year <- function(df) mutate(df, year = as.character(year))

datasets <- list(
  temp       = temp_data       %>% standardise_year(),
  pop        = pop_data        %>% standardise_year(),
  gdp        = gdp_data        %>% standardise_year(),
  mountain   = mountain_data   %>% standardise_year(),
  ag_cap     = ag_cap_data     %>% standardise_year(),
  co2_conv   = forestland_emissions_data   %>% standardise_year(),
  forest_area   = forestland_area_data   %>% standardise_year(),
  unfccc_emissions   = unfccc_emissions_data   %>% standardise_year(),
  land_area   = land_area_data   %>% standardise_year()
)

# ---------------------------------------------------------------------------
# 5 . Left‑join every dataset on (m49, year)
# ---------------------------------------------------------------------------
print("ALLDATASETS")
for (name in names(datasets)) {
  cat("\nDataset:", name, "\n")
  print(colnames(datasets[[name]]))
}

all_cols <- lapply(datasets, colnames)
common_cols <- Reduce(intersect, all_cols)
cat("\nColumns common to all datasets:\n")
print(common_cols)

cat("\nColumns in each dataset:\n")
for (name in names(all_cols)) {
  cat("\n", name, ":\n", sep = "")
  print(all_cols[[name]])
}

# want to keep 'country' only from the first dataset ('temp')
# So for all other datasets, drop 'country' column before the join
datasets_cleaned <- datasets
for (name in names(datasets_cleaned)[-1]) {
  datasets_cleaned[[name]] <- datasets_cleaned[[name]] %>% select(-country)
}

combined_data <- reduce(
  datasets_cleaned,
  # function(x, y) full_join(x, y, by = c("m49", "year"), relationship = "many-to-many")
  function(x, y) inner_join(x, y, by = c("m49", "year"))
)



all_colnames <- unlist(all_cols)
dup_cols <- unique(all_colnames[duplicated(all_colnames)])
cat("\nColumns appearing in more than one dataset:\n")
print(dup_cols)


# # ---------------------------------------------------------------------------
# # 6 . Back‑fill a human‑readable name (optional)
# # ---------------------------------------------------------------------------
# combined_data <- combined_data %>%
#   left_join(select(lookup, m49, country = name_off), by = "m49") %>%
#   relocate(country, .after = m49)

# print("Combined data")
# print(colnames(combined_data))
# print(str(combined_data))

# ---------------------------------------------------------------------------
# 7 . Transformations & quality checks (unchanged logic)
# ---------------------------------------------------------------------------
combined_data <- combined_data %>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    lgdp  = ifelse(is.na(gdp)        | gdp        <= 0, NA, log(gdp)),
    lpop  = ifelse(is.na(pop_total)  | pop_total  <= 0, NA, log(pop_total)),
    lpop_f  = ifelse(is.na(pop_f)  | pop_f  <= 0, NA, log(pop_f)),
    lpop_m  = ifelse(is.na(pop_m)  | pop_m <= 0, NA, log(pop_m)),
    lpop_rural  = ifelse(is.na(pop_rural)  | pop_rural <= 0, NA, log(pop_rural)),
    lpop_urban  = ifelse(is.na(pop_urban)  | pop_urban <= 0, NA, log(pop_urban)),
    lpop_urban  = ifelse(is.na(pop_urban)  | pop_urban <= 0, NA, log(pop_urban)),
    lfixed_cap_formation  = ifelse(is.na(fixed_cap_formation)  | fixed_cap_formation <= 0, NA, log(fixed_cap_formation)),
    lfixed_cap_formation_sq  = ifelse(is.na(lgdp), NA, lfixed_cap_formation^2),
    lgdp_sq = ifelse(is.na(lgdp), NA, lgdp^2),
    gdp_sq = ifelse(is.na(gdp), NA, gdp^2),
    gdp_percap = gdp / pop_total,
    lland_area  = ifelse(is.na(land_area)        | land_area        <= 0, NA, log(land_area)),
    log_net_emissions_removal_co2 = case_when(
      is.na(net_emissions_removal_co2)        ~ NA_real_,
      net_emissions_removal_co2 == 0          ~ 0,
      TRUE                                    ~ log(abs(net_emissions_removal_co2)) * sign(net_emissions_removal_co2)
    )
  )

# Duplicate guard
duplicates <- combined_data %>%
  group_by(m49, year) %>% mutate(n = n()) %>% ungroup() %>% filter(n > 1)
if (nrow(duplicates) > 0) {
  warning(sprintf("%d duplicated country‑year rows found – keeping first occurrence", nrow(duplicates)))
  combined_data <- combined_data %>% group_by(m49, year) %>% slice(1) %>% ungroup()
}

# ---------------------------------------------------------------------------
# 8 . Save & summarise
# ---------------------------------------------------------------------------
outfile <- file.path(raw, "all_variables_hdd_cdd_defined.RDS")

saveRDS(combined_data, outfile)
cat("Combined dataset written to", outfile, "\n\n")

cat("Summary:\n")
cat("  Countries:", length(unique(combined_data$m49)), "\n")
cat("  Years:    ", min(combined_data$year), "to", max(combined_data$year), "\n")
cat("  Obs:      ", nrow(combined_data), "\n")
cat("  Vars:     ", paste(names(combined_data), collapse = ", "), "\n")




# SAME THING BUT USING INNER JOIN
cat("\n\nNOW RUNNING THE FULL JOIN")
combined_data <- reduce(
  datasets_cleaned,
  function(x, y) full_join(x, y, by = c("m49", "year"), relationship = "many-to-many")
  # function(x, y) inner_join(x, y, by = c("m49", "year"))
)



all_colnames <- unlist(all_cols)
dup_cols <- unique(all_colnames[duplicated(all_colnames)])
cat("\nColumns appearing in more than one dataset:\n")
print(dup_cols)


# # ---------------------------------------------------------------------------
# # 6 . Back‑fill a human‑readable name (optional)
# # ---------------------------------------------------------------------------
# combined_data <- combined_data %>%
#   left_join(select(lookup, m49, country = name_off), by = "m49") %>%
#   relocate(country, .after = m49)

# print("Combined data")
# print(colnames(combined_data))
# print(str(combined_data))

# ---------------------------------------------------------------------------
# 7 . Transformations & quality checks (unchanged logic)
# ---------------------------------------------------------------------------
combined_data <- combined_data %>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    lgdp  = ifelse(is.na(gdp)        | gdp        <= 0, NA, log(gdp)),
    lpop  = ifelse(is.na(pop_total)  | pop_total  <= 0, NA, log(pop_total)),
    lpop_f  = ifelse(is.na(pop_f)  | pop_f  <= 0, NA, log(pop_f)),
    lpop_m  = ifelse(is.na(pop_m)  | pop_m <= 0, NA, log(pop_m)),
    lpop_rural  = ifelse(is.na(pop_rural)  | pop_rural <= 0, NA, log(pop_rural)),
    lpop_urban  = ifelse(is.na(pop_urban)  | pop_urban <= 0, NA, log(pop_urban)),
    lpop_urban  = ifelse(is.na(pop_urban)  | pop_urban <= 0, NA, log(pop_urban)),
    lfixed_cap_formation  = ifelse(is.na(fixed_cap_formation)  | fixed_cap_formation <= 0, NA, log(fixed_cap_formation)),
    lfixed_cap_formation_sq  = ifelse(is.na(lgdp), NA, lfixed_cap_formation^2),
    lgdp_sq = ifelse(is.na(lgdp), NA, lgdp^2),
    gdp_sq = ifelse(is.na(gdp), NA, gdp^2),
    gdp_percap = gdp / pop_total,
    lland_area  = ifelse(is.na(land_area)        | land_area        <= 0, NA, log(land_area)),
    log_net_emissions_removal_co2 = case_when(
      is.na(net_emissions_removal_co2)        ~ NA_real_,
      net_emissions_removal_co2 == 0          ~ 0,
      TRUE                                    ~ log(abs(net_emissions_removal_co2)) * sign(net_emissions_removal_co2)
    )
  )

# Duplicate guard
duplicates <- combined_data %>%
  group_by(m49, year) %>% mutate(n = n()) %>% ungroup() %>% filter(n > 1)
if (nrow(duplicates) > 0) {
  warning(sprintf("%d duplicated country‑year rows found – keeping first occurrence", nrow(duplicates)))
  combined_data <- combined_data %>% group_by(m49, year) %>% slice(1) %>% ungroup()
}

# ---------------------------------------------------------------------------
# 8 . Save & summarise
# ---------------------------------------------------------------------------
outfile <- file.path(raw, "all_variables.RDS")

saveRDS(combined_data, outfile)
cat("Combined dataset written to", outfile, "\n\n")

cat("Summary:\n")
cat("  Countries:", length(unique(combined_data$m49)), "\n")
cat("  Years:    ", min(combined_data$year), "to", max(combined_data$year), "\n")
cat("  Obs:      ", nrow(combined_data), "\n")
cat("  Vars:     ", paste(names(combined_data), collapse = ", "), "\n")
