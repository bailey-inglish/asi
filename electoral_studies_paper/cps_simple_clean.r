# File name: cps_simple_clean.r
# Purpose:   Creates binary dummy variables for factors to be regressed and
#            summarized in a later script.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)
library(ipumsr)
setwd("electoral_studies_paper")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00022.xml") %>% read_ipums_micro()

# VOREG recode to account for voter universe specification
cps <- mutate(cps, registered = VOTED == 2 | VOREG == 2)

# Hispanic simple recoding
cps$is_hispanic <- rep(TRUE, nrow(cps))
cps$is_hispanic[cps$HISPAN == 0] <- FALSE
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$is_white <- rep(FALSE, nrow(cps))
cps$is_white[cps$RACE == 100] <- TRUE

cps$is_black <- rep(FALSE, nrow(cps))
cps$is_black[cps$RACE == 200] <- TRUE

cps$is_aapi <- rep(FALSE, nrow(cps))
cps$is_aapi[is.element(cps$RACE, c(650, 651, 652))] <- TRUE

# Race/ethnicity combined simple recoding
cps$is_white_non_hisp <- rep(FALSE, nrow(cps))
cps$is_white_non_hisp[cps$RACE == 100 & cps$is_hispanic == FALSE] <- TRUE

# EDUC simplified recode
cps$is_college_educ <- rep(NA, nrow(cps))
cps$is_college_educ[cps$EDUC > 1] <- FALSE
cps$is_college_educ[cps$EDUC > 73] <- TRUE

# VOTERES harmonization
cps$is_long_res <- rep(NA, nrow(cps))
cps$is_long_res[cps$VOTERES < 900] <- TRUE
cps$is_long_res[cps$VOTERES < 33] <- FALSE

# FAMINC simple recoding
cps$is_higher_income <- rep(NA, nrow(cps))
cps$is_higher_income[cps$FAMINC < 900] <- TRUE
cps$is_higher_income[cps$FAMINC < 720] <- FALSE

# AGE simple recoding
cps$is_over_30 <- rep(NA, nrow(cps))
cps$is_over_30[cps$AGE < 900] <- TRUE
cps$is_over_30[cps$AGE <= 30] <- FALSE

cps$is_65_or_older <- rep(NA, nrow(cps))
cps$is_65_or_older[cps$AGE < 900] <- TRUE
cps$is_65_or_older[cps$AGE < 65] <- FALSE

# SEX simple recoding
cps$is_female <- rep(NA, nrow(cps))
cps$is_female[cps$SEX == 1] <- FALSE
cps$is_female[cps$SEX == 2] <- TRUE

# METRO simple recoding
cps$is_in_metro <- rep(NA, nrow(cps))
cps$is_in_metro[is.element(cps$METRO, c(2, 3, 4))] <- TRUE
cps$is_in_metro[cps$METRO == 1] <- FALSE

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
  "college_educ", "EDUC", 73, 125,
  "long_res", "VOTERES", 33, 899,
  "higher_income", "FAMINC", 720, 899,
  "30_under", "AGE", 18, 30,
  "65_plus", "AGE", 30, 90,
  "female", "SEX", 2, 2,
  "in_metro", "METRO", 2, 4,
  "white", "RACE", 100, 100,
  "black", "RACE", 200, 200,
  "aapi", "RACE", 650, 652,
  "latino", "HISPAN", 1, 899
)

cps$METRO[cps$METRO == 0] <- 999

for (name in var_bin_recodes$name) {
  print(name)
  vmin <- var_bin_recodes$min[var_bin_recodes$name == name]
  vmax <- var_bin_recodes$max[var_bin_recodes$name == name]
  v <- var_bin_recodes$var[var_bin_recodes$name == name]

  cps[, str_c("is_", name)] <- c(NA, TRUE)[cps[, v] <= vmax]
  cps[, str_c("is_", name)] <- cps[, v] <= vmax & cps[, v] >= vmin

  in_group_count <- cps %>%
    filter(!!sym(v) %in% codes) %>%
    group_by(YEAR, STATEFIP, !!sym(v)) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps %>%
    group_by(YEAR, STATEFIP) %>%
    summarize(ovr_count = sum(adj_vosuppwt))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, YEAR, STATEFIP, str_c("prop_", name))
  cps <- left_join(cps, ratio_tab, by = c("YEAR", "STATEFIP"))
}

# Now post reweighting, add some potential covariates of interest before we
# get rid of all of the technical variables in the next step.
add_pct_by_codes <- function(variable, codebook, cps) {
  # variable = original VAR in cps dataset
  # codebook = tribble(
  #   ~name, ~code,
  #   "group1", c(100),
  #   "group2", c(200, 300),
  #   etc.
  # )
  for (name in codebook$name) {
    codes <- codebook[codebook$name == name, ]$codes[[1]]
    variable = sym(variable)
    in_group_count <- cps %>%
      filter(!!variable %in% codes) %>%
      group_by(YEAR, STATEFIP, !!variable) %>%
      summarize(ig_count = sum(adj_vosuppwt))
    overall_count <- cps %>%
      group_by(YEAR, STATEFIP) %>%
      summarize(ovr_count = sum(adj_vosuppwt))
    ratio_tab <- left_join(overall_count, in_group_count)
    ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
    ratio_tab <- select(ratio_tab, YEAR, STATEFIP, str_c("prop_", name))
    cps <- left_join(cps, ratio_tab, by = c("YEAR", "STATEFIP"))
  }
  return(cps)
}

cps_c <- cps

race_cb <- tribble(
  ~name, ~codes,
  "white", c(100),
  "black", c(200),
  "aapi", c(650, 651, 652)
)
cps_c <- add_pct_by_codes("RACE", race_cb, cps)
for (name in race_cb$name) {
  codes <- race_cb[race_cb$name == name, ]$codes[[1]]
  in_group_count <- cps %>%
    filter(RACE %in% codes) %>%
    group_by(YEAR, STATEFIP, RACE) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps %>%
    group_by(YEAR, STATEFIP) %>%
    summarize(ovr_count = sum(adj_vosuppwt))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, YEAR, STATEFIP, str_c("prop_", name))
  cps_c <- left_join(cps, ratio_tab, by = c("YEAR", "STATEFIP"))
}

hisp_cb <- tribble(
  ~name, ~codes,
  "latino", c(TRUE)
)
cps_c <- add_pct_by_codes("is_hispanic", hisp_cb, cps_c)

race_eth_comb_cb <- tribble(
  ~name, ~codes,
  "white_non_hisp", c(TRUE)
)
cps_c <- add_pct_by_codes("is_white_non_hisp", race_eth_comb_cb, cps_c)

educ_cb <- tribble(
  ~name, ~codes,
  "college_educ", c(TRUE)
)
cps_c <- add_pct_by_codes("is_college_educ", educ_cb, cps_c)

metro_cb <- tribble(
  ~name, ~codes,
  "metro", c(TRUE)
)
cps_c <- add_pct_by_codes("is_in_metro", metro_cb, cps_c)

age_cb <- tribble(
  ~name, ~codes,
  "over_65", c(65:90)
)
cps_c <- add_pct_by_codes("AGE", age_cb, cps_c)

# Pick out only the variables we analyze (comment out this part for debugging)
cps <- select(
  cps,
  year = YEAR,
  locality,
  voted = VOTED,
  registered,
  adj_vosuppwt,
  starts_with("is_", "prop_")
)

# Write final outputs
write_csv(cps, "final_data/cps_reduced_ipums_1994-2022.csv")
write_dta(cps, "final_data/cps_reduced_ipums_1994-2022.dta")
