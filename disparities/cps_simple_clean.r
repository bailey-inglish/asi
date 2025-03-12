# File name: cps_simple_clean.r
# Purpose:   Creates binary dummy variables for factors to be regressed and
#            summarized in a later script.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)
library(ipumsr)
setwd("disparities")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00022.xml") %>% read_ipums_micro()

# VOREG recode to account for voter universe specification
cps <- mutate(cps, registered = VOTED == 2 | VOREG == 2)

# Reweight according to Hur & Achen using UF Election Lab actual turnout data
act_turn <- read_csv("raw_data/actual_turnout.csv")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")
act_turn <- left_join(act_turn, fips_conv, by = c("STATE_ABV" = "abbr")) %>%
  select(YEAR, fips, locality = name, vep_turnout = VEP_TURNOUT_RATE)

raw_turn <- filter(
  cps,
  VOTED == 1 | VOTED == 2,
  CITIZEN < 5,
  AGE >= 18
) %>%
  select(YEAR, VOSUPPWT, STATEFIP, VOTED) %>%
  group_by(STATEFIP, YEAR) %>%
  reframe(
    est_vep_turnout = sum(VOSUPPWT * (VOTED == 2)) / sum(VOSUPPWT)
  )

fin_turn <- left_join(
  act_turn,
  raw_turn,
  by = c("fips" = "STATEFIP", "YEAR" = "YEAR")
) %>%
  mutate(
    adj_voter_wt = (vep_turnout / est_vep_turnout),
    adj_non_voter_wt = ((1 - vep_turnout) / (1 - est_vep_turnout))
  )

cps <- left_join(
  cps,
  fin_turn,
  by = c("STATEFIP" = "fips", "YEAR" = "YEAR")
)

cps$adj_vosuppwt <- rep(NA, nrow(cps))
cps$adj_vosuppwt[cps$VOTED == 2] <- cps$VOSUPPWT[cps$VOTED == 2] * cps$adj_voter_wt[cps$VOTED == 2]
cps$adj_vosuppwt[cps$VOTED == 1] <- cps$VOSUPPWT[cps$VOTED == 1] * cps$adj_non_voter_wt[cps$VOTED == 1]

# We only need the voters now!
cps <- filter(cps, VOTED == 1 | VOTED == 2)

var_bin_recodes <- tribble(
  ~name, ~var, ~min, ~max,
  # var, 0, 999, <- inclusive bounds
  "any_college_educ", "EDUC", 80, 125,
  "5_plus_years_at_address", "VOTERES", 33, 899,
  "50k_plus", "FAMINC", 820, 899,
  "30_under", "AGE", 18, 30,
  "65_plus", "AGE", 65, 90,
  "female", "SEX", 2, 2,
  "in_metro", "METRO", 2, 4,
  "white", "RACE", 100, 100,
  "black", "RACE", 200, 200,
  "aapi", "RACE", 650, 652,
  "latino", "HISPAN", 1, 899
)

cps$METRO[cps$METRO == 0] <- 999

prop_totals <- tibble(
  year = rep(unique(cps$YEAR), length(unique(cps$locality)) + 1),
  locality = rep(c(unique(cps$locality), "United States"), length(unique(cps$YEAR)))
)

for (name in var_bin_recodes$name) {
  vmin <- var_bin_recodes$min[var_bin_recodes$name == name]
  vmax <- var_bin_recodes$max[var_bin_recodes$name == name]
  v <- var_bin_recodes$var[var_bin_recodes$name == name]

  cps[, str_c("is_", name)] <- c(NA, TRUE)[(cps[, v] <= vmax) + 1]
  cps[, str_c("is_", name)] <- cps[, v] <= vmax & cps[, v] >= vmin

  # State level
  in_group_count <- cps %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(YEAR, locality, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps %>%
    group_by(YEAR, locality) %>%
    summarize(ovr_count = sum(adj_vosuppwt))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, YEAR, locality, str_c("prop_", name))
  state_r <- ratio_tab

  # Federal level
  in_group_count <- cps %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(YEAR, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps %>%
    group_by(YEAR) %>%
    summarize(ovr_count = sum(adj_vosuppwt))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, YEAR, str_c("prop_", name)) %>%
    mutate(locality = "United States")
  fed_r <- ratio_tab

  ovr_r <- rows_append(state_r, fed_r)
  prop_totals <- left_join(prop_totals, ovr_r, by = c("year" = "YEAR", "locality"))
}

# Pick out only the variables we analyze (comment out this part for debugging)
cps <- select(
  cps,
  year = YEAR,
  locality,
  voted = VOTED,
  registered,
  adj_vosuppwt,
  starts_with("is_")
)

# Write final outputs
write_csv(cps, "final_data/cps_reduced_ipums_1994-2022.csv")
write_dta(cps, "final_data/cps_reduced_ipums_1994-2022.dta")
write_csv(prop_totals, "final_data/cps_state_proportions_1994-2022.csv")
write_dta(prop_totals, "final_data/cps_state_proportions_1994-2022.dta")