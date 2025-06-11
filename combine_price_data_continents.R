#!/usr/bin/env Rscript
# ---------------------------------------------------------------
# make_region_ag_price_data.R
#   Aggregate FAOSTAT ag-price data by UN region/year
#   while keeping one row per country
#   + make one plot per commodity
# ---------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(janitor)
  library(ggplot2)
  library(purrr)
})


# ────────────────────────────────────────────────────────────────
# Helper to print Afghanistan rows at any stage
# ────────────────────────────────────────────────────────────────
print_afg <- function(df, tag) {
  afg <- df %>% filter(m49_code == 4)   # Afghanistan’s M49 code = 4
  cat("\n──────────────── Afghanistan:", tag, "────────────────\n")
  if (nrow(afg) == 0) {
    cat("(no rows found)\n")
  } else {
    print(afg %>% select(region, year, any_of(lcommodities)), n = Inf)
  }
}


# ---------------------------------------------------------------
# 0. Paths
# ---------------------------------------------------------------
root <- "/home/dmrivers/Code/Predict_Deforestation"   # adjust if needed
setwd(root)


in_file      <- "data/processed_ag_price_data.csv"
in_file_wood <- "data/processed_wood_price_data.csv"
unsd_lookup  <- "data/2022-09-24__CSV_UNSD_M49.csv"
out_csv      <- "data/processed_all_prices_data.csv"
plot_dir     <- "plots/region_ag_prices"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------
# 1. Read country-level price table
# ---------------------------------------------------------------
ag <- read_csv(in_file, show_col_types = FALSE) |>
  mutate(m49_code = as.integer(m49_code))   # ← make types identical
wood <- read_csv(in_file_wood, show_col_types = FALSE)   # ← make types identical
wood <- wood %>%                     # keeps the object you already read
  rename(lwood_pr = wood_price_frac)     #  ✱ standardise the column name

# After raw import (no imputation yet)
print_afg(ag, "raw import")

# ---------------------------------------------------------------
# 2. UNSD M49 lookup
# ---------------------------------------------------------------
lookup <- read_csv(
  unsd_lookup,
  quote     = "",           # whole file is quoted
  col_types = cols(.default = "c"),
  trim_ws   = TRUE
) |>
  rename_with(~ gsub('"', "", .x)) |>
  mutate(across(everything(), ~ gsub('"', "", .x))) |>
  transmute(
    m49_code = as.integer(`M49 Code`),
    region   = `Region Name`,
    subregion = `Sub-region Name`
  ) |>
  filter(!is.na(m49_code) & !is.na(region))

ag <- ag %>%
  left_join(wood, by = "year")       #  ✱ add the global wood price to each row
# ---------------------------------------------------------------
# 3. Attach region / sub-region
# ---------------------------------------------------------------
ag <- ag |>
  left_join(lookup, by = "m49_code")


# Log-transform and rename commodity columns (except wood)
commodities      <- c("cattle_meat", "soy", "milk", "sugar", "palm", "coffee")

# If wood is already log and named e.g. 'wood' or 'lwood_pr', leave as is.
# If not, add it to 'commodities_new' with correct name.

# Apply log10 and rename
ag <- ag %>%
  mutate(across(
    all_of(commodities),
    ~log10(.x),
    .names = "l{.col}_pr"
  )) %>%
  # remove the original non-log columns
  select(-all_of(commodities))



lcommodities <- c("lcattle_meat_pr", "lsoy_pr", "lmilk_pr", "lsugar_pr", "lpalm_pr", "lcoffee_pr", "lwood_pr")

# ---------------------------------------------------------------
# 4. Replace each country’s values with continent (region) means
#    – mean() but NA if *all* missing
# ---------------------------------------------------------------
# mean_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
mean_na <- function(x) mean(x, na.rm = TRUE)

ag_region_filled <- ag |>
  group_by(region, year) |>
  mutate(across(all_of(lcommodities), mean_na)) |>
  ungroup()


# ---------------------------------------------------------------
# 5. Intelligent fill of missing region-year values
# ---------------------------------------------------------------

years <- sort(unique(ag$year))
regions <- unique(ag$region)
fill_series <- function(y, g) {
  idx <- seq_along(y)
  y_out <- y
  
  # (a) region has NO data at all ------------------------------
  if (all(is.na(y))) return(g)
  
  # (b) fill leading NAs ---------------------------------------
  first_real <- which(!is.na(y_out))[1]
  if (first_real > 1) {
    for (k in (first_real - 1):1) {
      y_out[k] <- y_out[first_real] + (g[k] - g[first_real])
    }
  }
  
  # (c) fill trailing NAs --------------------------------------
  last_real <- tail(which(!is.na(y_out)), 1)
  if (last_real < length(y)) {
    for (k in (last_real + 1):length(y)) {
      y_out[k] <- y_out[last_real] + (g[k] - g[last_real])
    }
  }
  
  # (d) fill internal gaps -------------------------------------
  na_run <- which(is.na(y_out))
  while (length(na_run) > 0) {
    i <- na_run[1]
    prev_idx <- max(which(!is.na(y_out) & idx < i))
    next_idx <- min(which(!is.na(y_out) & idx > i))
    
    # safety check in case one side is missing
    if (is.infinite(prev_idx) || is.infinite(next_idx)) break
    
    r_s <- y_out[prev_idx];  r_e <- y_out[next_idx]
    g_s <- g[prev_idx];      g_e <- g[next_idx]
    
    same_sign <- sign(r_e - r_s) == sign(g_e - g_s)
    
    fill_idx <- prev_idx:next_idx
    if (same_sign && g_e != g_s) {
      y_out[fill_idx] <- r_s + (g[fill_idx] - g_s)
      # scale global shape to fit the endpoints
      # y_out[fill_idx] <- r_s + (r_e - r_s) *
      #   (g[fill_idx] - g_s) / (g_e - g_s)
    } else {
      # simple linear interpolation
      y_out[fill_idx] <- approx(
        x    = c(prev_idx, next_idx),
        y    = c(r_s,       r_e),
        xout = fill_idx
      )$y
    }
    na_run <- which(is.na(y_out))
  }
  y_out
}

## 5-A. region-year mean table ----------------------------------
region_tbl <- ag %>%
  group_by(region, year) %>%
  summarise(across(all_of(lcommodities), mean_na), .groups = "drop") %>%
  arrange(region, year)

## 5-B. loop over regions & commodities -------------------------
filled_list <- lapply(regions, function(reg) {
  reg_df   <- region_tbl %>% filter(region == reg)
  other_df <- region_tbl %>% filter(region != reg) %>%
                group_by(year) %>%
                summarise(across(all_of(lcommodities), mean_na),
                          .groups = "drop") %>%
                arrange(year)
  
  for (comm in lcommodities) {
    y <- reg_df[[comm]]
    g <- other_df[[comm]]
    reg_df[[comm]] <- fill_series(y, g)
  }
  reg_df
})

region_filled <- bind_rows(filled_list)

## 5-C. attach filled values back to every country row ----------
ag_region_filled <- ag %>%
  select(-all_of(lcommodities)) %>%          # drop old values
  left_join(region_filled, by = c("region", "year"))
# ---------------------------------------------------------------
# 5-bis-A.  build lagged Δlog₁₀ price signals on REGION table ---
# ---------------------------------------------------------------
region_deltas <- region_filled %>%
  group_by(region) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(across(
    all_of(lcommodities),
    list(
      `_1y`  = ~ lag(.x, 1) - lag(.x, 2),          # (t-2 → t-1)
      `_3yr` = ~ (lag(.x, 1) - lag(.x, 4)) / 3     # mean of 3 annual Δs
    ),
    .names = "{.col}{.fn}"
  )) %>%
  # fill ALL leading NAs in the new delta columns with first non-NA
  mutate(across(
    matches("_(1y|3yr)$"),
    ~ {
      first_ok <- which(!is.na(.x))[1]
      if (!is.na(first_ok) && first_ok > 1) .x[1:(first_ok - 1)] <- .x[first_ok]
      .x
    }
  )) %>%
  ungroup() %>%
  select(region, year, ends_with("_1y"), ends_with("_3yr"))

# ---------------------------------------------------------------
# 5-bis-B.  Expand to ALL M49 countries & fill with region means -
# ---------------------------------------------------------------
all_countries <- lookup %>%                         # every country in 2022 file
  distinct(m49_code, region, subregion)

country_year_grid <- tidyr::expand_grid(            # full grid
  m49_code = all_countries$m49_code,
  year     = years
) %>%
  left_join(all_countries, by = "m49_code")         # add region / subregion

# ➊ merge existing country rows (may have NAs)
ag_region_filled <- country_year_grid %>%
  left_join(ag_region_filled,
            by = c("m49_code", "year", "region", "subregion"))

# ➋ bring in region-level log-prices for fallback
ag_region_filled <- ag_region_filled %>%
  left_join(region_filled,
            by     = c("region", "year"),
            suffix = c("", "_region"))

for (col in lcommodities) {                         # coalesce one column at a time
  reg_col <- paste0(col, "_region")
  ag_region_filled[[col]] <- dplyr::coalesce(ag_region_filled[[col]],
                                            ag_region_filled[[reg_col]])
}

ag_region_filled <- ag_region_filled %>%
  select(-ends_with("_region"))                     # drop helper cols

# ➌ attach region-level deltas (_1y, _3yr)
ag_region_filled <- ag_region_filled %>%
  left_join(region_deltas, by = c("region", "year"))


# ---------------------------------------------------------------
# 6. Write filled table
# ---------------------------------------------------------------
write_csv(ag_region_filled, out_csv, na = "")
cat(sprintf("✓ Region-filled CSV written: %s\n", out_csv))
# ---------------------------------------------------------------
# 7. Make one plot per commodity: region trends through time
# ---------------------------------------------------------------
agg_region <- region_filled %>%                       # <-- use FILLED data
  pivot_longer(
    cols      = all_of(lcommodities),
    names_to  = "commodity",
    values_to = "value"
  )


# ---------------------------------------------------------------
# 7-bis.  Plot 1-yr and 3-yr Δ(log₁₀ price) by region ---------
# ---------------------------------------------------------------
delta_cols <- grep("_pr_(1y|3yr)$", names(ag_region_filled), value = TRUE)

delta_long <- ag_region_filled %>%
  pivot_longer(
    cols = all_of(delta_cols),
    names_to = c("commodity", "horizon"),
    names_pattern = "(l.+?_pr)_(1y|3yr)",
    values_to = "delta"
  )

plot_delta <- function(comm, horiz) {
  p <- delta_long %>%
    filter(commodity == comm, horizon == horiz) %>%
    ggplot(aes(x = year, y = delta, group = region, colour = region)) +
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
      geom_line(na.rm = TRUE) +
      geom_point(na.rm = TRUE, size = 2) +
      labs(
        title  = paste0("Δ log₁₀ price (", horiz, "): ",
                        gsub("^l|_pr$", "", comm)),
        y      = "Δ log₁₀ USD / tonne",
        colour = NULL
      ) +
      theme_bw() +
      theme(legend.position = "right")

  ggsave(
    filename = file.path(
      plot_dir,
      paste0("region_", gsub("^l|_pr$", "", comm), "_", horiz, ".pdf")
    ),
    plot  = p,
    width = 7, height = 5
  )
}

walk(unique(delta_long$commodity),
     \(comm) walk(c("1y", "3yr"), \(h) plot_delta(comm, h)))

cat(sprintf("✓ Delta plots (1-yr & 3-yr) written to: %s/\n", plot_dir))


# Step 1: Mark imputed vs. observed for plotting ----------------

# Make a region-year-observed table for each commodity
region_obs_tbl <- ag %>%
  group_by(region, year) %>%
  summarise(across(all_of(lcommodities), mean_na), .groups = "drop") %>%
  arrange(region, year)

# Long form for both filled and observed
agg_region <- region_filled %>%
  pivot_longer(cols = all_of(lcommodities),
               names_to = "commodity",
               values_to = "value") %>%
  left_join(
    region_obs_tbl %>%
      pivot_longer(cols = all_of(lcommodities),
                   names_to = "commodity",
                   values_to = "obs_value"),
    by = c("region", "year", "commodity")
  ) %>%
  mutate(
    imputed = is.na(obs_value),          # TRUE if value is imputed
    value_log10 = value
  )

# Step 2: Plot with size/linetype/shape mapping  ----------------
plot_one <- function(comm) {
  p <- agg_region %>%
    filter(commodity == comm) %>%
    ggplot(aes(x = year, y = value_log10, group = region, colour = region)) +
      geom_line(aes(size = imputed, linetype = imputed), na.rm = TRUE) +
      geom_point(aes(shape = imputed, fill = imputed), size = 2.7, stroke = 1.4, na.rm = TRUE) +
      # mappings for imputed vs observed
      scale_size_manual(values = c("FALSE" = 0.5, "TRUE" = 1.6), guide = "none") +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "solid"), guide = "none") +
      # SHAPE and FILL share the same key, both appear in the legend
      scale_shape_manual(values = c("FALSE" = 21, "TRUE" = 21),
                         breaks = c(FALSE, TRUE),
                         labels = c("Observed", "Imputed"),
                         name   = "") +
      scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "black"),
                        breaks = c(FALSE, TRUE),
                        labels = c("Observed", "Imputed"),
                        name   = "") +
      labs(
        title  = paste("Producer price (log₁₀):", gsub("_", " ", comm)),
        y      = "log₁₀ USD / tonne",
        colour = NULL,
        shape  = NULL
      ) +
      theme_bw() +
      theme(legend.position = "right") +
      annotate("text", x = Inf, y = -Inf,
               hjust = 1.01, vjust = -0.2, size = 3.7, fontface = "italic",
               label = "Bold black points/lines = imputed values")

  ggsave(
    filename = file.path(plot_dir, paste0("region_", comm, ".pdf")),
    plot     = p,
    width    = 7, height = 5
  )
}

walk(lcommodities, plot_one)

cat(sprintf("✓ Commodity plots written to: %s/\n", plot_dir))
