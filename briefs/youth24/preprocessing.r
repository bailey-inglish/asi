# File name: cps_processing.r
# Purpose:   Takes 2024 CPS data and creates clean PUMS-level results, then
#            computes summary tables.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(ipumsr)
setwd("briefs/youth24")

### CPS REDUCED
# Import data
cps <- read_ipums_ddi("raw_data/cps_00024.xml") %>% read_ipums_micro()
act_turn <- read_csv("raw_data/actual_turnout.csv")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")
act_turn <- left_join(act_turn, fips_conv, by = c("STATE_ABV" = "abbr")) %>%
  select(YEAR, fips, locality = name, vep_turnout = VEP_TURNOUT_RATE) %>%
  filter(YEAR >= 1994)

# VOREG recode to account for voter universe specification
cps <- mutate(cps, registered = VOTED == 2 | VOREG == 2)

# Reweight according to Hur & Achen using UF Election Lab actual turnout data
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
  expand.grid(year = unique(cps$YEAR), locality = c(unique(cps$locality), "United States"))
)

cps_reduced <- cps

for (name in var_bin_recodes$name) {
  vmin <- var_bin_recodes$min[var_bin_recodes$name == name]
  vmax <- var_bin_recodes$max[var_bin_recodes$name == name]
  v <- var_bin_recodes$var[var_bin_recodes$name == name]

  cps_reduced[, str_c("is_", name)] <- c(NA, TRUE)[(cps[, v] <= vmax) + 1]
  cps_reduced[, str_c("is_", name)] <- cps[, v] <= vmax & cps[, v] >= vmin

  # State level
  in_group_count <- cps_reduced %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(YEAR, locality, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps_reduced %>%
    group_by(YEAR, locality) %>%
    summarize(ovr_count = sum(adj_vosuppwt))
  ratio_tab <- left_join(overall_count, in_group_count)
  ratio_tab[, str_c("prop_", name)] <- ratio_tab[, "ig_count"] / ratio_tab[, "ovr_count"]
  ratio_tab <- select(ratio_tab, YEAR, locality, str_c("prop_", name))
  state_r <- ratio_tab

  # Federal level
  in_group_count <- cps_reduced %>%
    filter(!!sym(str_c("is_", name)) == TRUE) %>%
    group_by(YEAR, !!sym(str_c("is_", name))) %>%
    summarize(ig_count = sum(adj_vosuppwt))
  overall_count <- cps_reduced %>%
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
cps_reduced <- select(
  cps_reduced,
  year = YEAR,
  locality,
  voted = VOTED,
  registered,
  adj_vosuppwt,
  starts_with("is_")
)

# Write final outputs
write_csv(cps_reduced, "final_data/cps_reduced_ipums_1994-2024.csv")
write_csv(prop_totals, "final_data/cps_state_proportions_1994-2024.csv")

### CPS EXPANDED
# Uses upper bound year to approx immigrant years in the US (note that higher)
# Hispanic simple recoding
cps$is_hispanic <- rep("Hispanic/Latino", nrow(cps))
cps$is_hispanic[cps$HISPAN == 0] <- "Non-Hispanic/Latino"
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$race_cluster <- rep(NA, nrow(cps))
cps$race_cluster[cps$RACE == 100] <- "White"
cps$race_cluster[cps$RACE == 200] <- "Black"
cps$race_cluster[cps$RACE == 300] <- "American Indian"
cps$race_cluster[is.element(cps$RACE, 650:652)] <- "Asian/Pacific Islander"
cps$race_cluster[cps$RACE >= 700] <- "Multiracial"

# CITIZEN NA recode
cps$CITIZEN[cps$CITIZEN == 9] <- NA

# VOREG recode to account for voter universe specification
cps <- mutate(cps, is_registered = VOTED == 2 | VOREG == 2)

# EDUC clustering recode
cps$edu_cluster <- rep(NA, nrow(cps))
cps$edu_cluster[cps$EDUC <= 72 & cps$EDUC > 1] <- "Less than HS Diploma" # 1 is coded as NIU! Boo :(
cps$edu_cluster[cps$EDUC == 73] <- "HS Diploma or Equivalent"
cps$edu_cluster[cps$EDUC == 81] <- "Some College But No Degree"
cps$edu_cluster[is.element(cps$EDUC, c(91, 92))] <- "Associate's Degree"
cps$edu_cluster[cps$EDUC >= 111] <- "Bachelor's Degree Or Higher"

# VOTERES harmonization
cps$vote_res_harmonized <- rep(NA, nrow(cps))
cps$vote_res_harmonized[cps$VOTERES >= 10 & cps$VOTERES <= 13] <- "0-1 years"
cps$vote_res_harmonized[cps$VOTERES == 20] <- "1-2 years"
cps$vote_res_harmonized[cps$VOTERES == 31] <- "2-4 years"
cps$vote_res_harmonized[cps$VOTERES == 33] <- "5+ years"

# One final round of clustering on selected variables
income_conv <- tribble(
  ~FAMINC, ~income_range,
  100, "<$30k",
  210, "<$30k",
  300, "<$30k",
  430, "<$30k",
  470, "<$30k",
  500, "<$30k",
  600, "<$30k",
  710, "<$30k",
  720, "$30-50k",
  730, "$30-50k",
  740, "$30-50k",
  820, "$50-100k",
  830, "$50-100k",
  840, ">$70k",
  841, "$50-100k",
  842, "$100-150k",
  843, ">$150k",
  996, NA,
  997, NA,
  999, NA
)

metro_conv <- tribble(
  ~METRO, ~metro_status,
  1, "Rural",
  2, "Urban",
  3, "Suburban",
  4, "Suburban"
)

age_conv <- tibble(
  AGE = c(18:29, 30:44, 45:59, 60:100),
  age_cluster = c(rep("18-29", 12), rep("30-44", 15), rep("45-59", 15), rep("60+", 41))
)

sex_conv <- tibble(
  SEX = c(1, 2),
  sex_name = c("Male", "Female")
)

cps <- cps %>%
  left_join(income_conv) %>%
  left_join(metro_conv) %>%
  left_join(age_conv) %>%
  left_join(sex_conv)

# Combine race and ethnicity
cps$eth_race_comb_cluster <- cps$race_cluster
cps$eth_race_comb_cluster[cps$is_hispanic == "Hispanic/Latino"] <- "Hispanic/Latino"

# Remove original variables
cps_expanded <- select(
  cps,
  YEAR,
  VOTED,
  AGE,
  FAMINC,
  FAMSIZE,
  17:18, # make sure this still works
  23:33
)

# Export `cps` with original variables for reproducability and bugtesting
write_csv(cps, "final_data/cps_expanded_ipums_repro_1994-2024.csv")

### CPS EXTRAS
# New dataset(s)
poverty_line <- read_csv("raw_data/poverty_line.csv")

# Poverty line variable addition
max_income_conv <- tribble(
  ~FAMINC, ~max_inc,
  100, 5000,
  210, 7499,
  300, 9999,
  430, 12499,
  470, 14999,
  500, 19999,
  600, 24999,
  710, 29999,
  720, 34999,
  730, 39999,
  740, 49999,
  820, 59999,
  830, 74999,
  840, 100000, # dummy value
  841, 99999,
  842, 149999,
  843, 1000000, # dummy value, no ceiling
  996, NA,
  997, NA,
  999, NA
)

# scale down FAMSIZE
cps_expanded[cps_expanded$FAMSIZE > 8, ]$FAMSIZE <- 8
cps_expanded <- cps_expanded %>%
  left_join(
    max_income_conv,
    by = "FAMINC"
  ) %>%
  left_join(
    poverty_line,
    by = c(
      "YEAR" = "year",
      "FAMSIZE" = "hh_size"
    )
  ) %>%
  mutate(
    is_in_poverty = max_inc < pov_line
  ) %>%
  select(
    !max_inc,
    !pov_line
  )

# Generations coding (according to Pew)
generation_conv <- tibble(
  birth_year = c(
    1901:1927,
    1928:1945,
    1946:1964,
    1965:1980,
    1981:1996,
    1997:2012,
    2013:2024
  ),
  generation = c(
    rep("Greatest", 27),
    rep("Silent", 18),
    rep("Boomer", 19),
    rep("Generation X", 16),
    rep("Millenial", 16),
    rep("Generation Z", 16),
    rep("Generation Alpha", 12)
  )
)

cps_expanded <- cps_expanded %>%
  mutate(
    birth_year = YEAR - AGE
  ) %>%
  left_join(
    generation_conv,
    by = "birth_year"
  ) %>%
  select(!birth_year)

# Write final outputs
cps_expanded <- cps_expanded %>% select(!FAMINC, !FAMSIZE)
write_csv(cps_expanded, "final_data/cps_expanded_ipums_1994-2024.csv")
