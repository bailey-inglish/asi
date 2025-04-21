# File name: ces_simple_clean.r
# Purpose:   Using the Cooperative Election Survey for more robust cumulative
#            content.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)

# Data
setwd("disparities")
ces_orig <- read_dta("raw_data/cumulative_2006-2023.dta")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")
ces <- left_join(ces_orig, fips_conv, c("state" = "fips")) %>%
  mutate(
    locality = name,
    state_fips = state,
    state_abbr = abbr,
    .keep = "unused"
  )

# All binary recodes
var_bin_recodes <- tribble(
  ~name, ~var, ~min, ~max, ~na_start,
  # var, 0, 999, 1000 <- inclusive bounds
  "any_college_educ", "educ", 3, 6, NA,
  "50k_plus", "faminc", 6, 12, 13,
  "30_under", "age", 18, 30, NA,
  "65_plus", "age", 65, 90, NA,
  "female", "gender", 2, 2, NA,
  "white", "race", 1, 1, NA,
  "black", "race", 2, 2, NA,
  "aapi", "race", 4, 4, NA,
  "latino", "race", 3, 3, NA
)

# Rectangularize

prop_totals <- tibble(
  expand.grid(year = unique(ces$year), locality = c(unique(ces$locality), "United States"))
)

for (name in var_bin_recodes$name) {
  vmin <- var_bin_recodes$min[var_bin_recodes$name == name]
  vmax <- var_bin_recodes$max[var_bin_recodes$name == name]
  vna <- var_bin_recodes$na_start[var_bin_recodes$name == name]
  v <- var_bin_recodes$var[var_bin_recodes$name == name]

  ces[, str_c("is_", name)] <- c(NA, TRUE)[(ces[, v] <= vmax) + 1]
  ces[, str_c("is_", name)] <- ces[, v] <= vmax & ces[, v] >= vmin
  if (!is.na(vna)) {
    ces[is.na(ces[, v]), v] <- vna
    ces[ces[, v] >= vna, str_c("is_", name)] <- NA
  }

  # State level
  in_group_count <- ces %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(year, locality, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(weight))
  overall_count <- ces %>%
    group_by(year, locality) %>%
    summarize(ovr_count = sum(weight))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, year, locality, str_c("prop_", name))
  state_r <- ratio_tab

  # Federal level
  in_group_count <- ces %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(year, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(weight))
  overall_count <- ces %>%
    group_by(year) %>%
    summarize(ovr_count = sum(weight))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, year, str_c("prop_", name)) %>%
    mutate(locality = "United States")
  fed_r <- ratio_tab

  ovr_r <- rows_append(state_r, fed_r)
  prop_totals <- left_join(prop_totals, ovr_r, by = c("year", "locality"))
}

# Pick vars we need
ces_fin <- select(
  ces,
  year,
  locality,
  voted = vv_turnout_gvm,
  registered = vv_regstatus,
  weight,
  starts_with("is_")
)

# Write final outputs
write_csv(ces_fin, "final_data/ces_reduced_2006-2022.csv")
write_dta(ces_fin, "final_data/ces_reduced_2006-2022.dta")
write_csv(prop_totals, "final_data/ces_state_proportions_2006-2022.csv")
write_dta(prop_totals, "final_data/ces_state_proportions_2006-2022.dta")
