# Setup
library(tidyverse)
library(haven)
library(ipumsr)
setwd("cps_cleaning")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00018.xml") %>% read_ipums_micro()

# Uses upper bound year to approx immigrant years in the US (note that higher)
# values are more subject to varition, see codebook.
cps$min_immi_years_in_us <- rep(NA, nrow(cps))
cps[cps$YRIMMIG != 0, "min_immi_years_in_us"] <- cps[cps$YRIMMIG != 0, "YEAR"] - cps[cps$YRIMMIG != 0, "YRIMMIG"]

# Hispanic simple recoding
cps$is_hispanic <- rep("Hispanic/Latino", nrow(cps))
cps$is_hispanic[cps$HISPAN == 0] <- "Non-Hispanic/Latino"
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$race_cluster <- rep("Multiracial", nrow(cps))
cps$race_cluster[cps$RACE == 100] <- "White"
cps$race_cluster[cps$RACE == 200] <- "Black"
cps$race_cluster[cps$RACE == 300] <- "American Indian"
cps$race_cluster[is.element(cps$RACE, 650:652)] <- "Asian/Pacific Islander"

# Geo + citizen recodings
cps$METFIPS[cps$METFIPS == 99998] <- NA
cps$METRO[cps$METRO == 0] <- NA
cps$CITIZEN[cps$CITIZEN == 9] <- NA

# Census-designated regional comparison
cps$region_name <- rep(NA, nrow(cps))
cps$region_name[is.element(cps$REGION, c(11, 12))] <- "Northeast"
cps$region_name[is.element(cps$REGION, c(21, 22))] <- "Midwest"
cps$region_name[is.element(cps$REGION, c(31, 32, 33))] <- "South"
cps$region_name[is.element(cps$REGION, c(41, 42))] <- "West"

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

# Reweight according to Hur & Achen - 2008 CT, TX, MS ballot recode fyi
act_turn <- read_csv("raw_data/actual_turnout.csv")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")
act_turn <- left_join(act_turn, fips_conv, by = c("STATE_ABV" = "abbr")) %>%
  select(YEAR, fips, state_name = name, vep_turnout = VEP_TURNOUT_RATE)

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

fin_turn <- left_join(act_turn, raw_turn, by = c("fips" = "STATEFIP", "YEAR" = "YEAR")) %>%
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

cps <- filter(cps, VOTED == 1 | VOTED == 2)

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
  AGE = c(18:29, 30:44, 45:59, 60:85),
  age_cluster = c(rep("18-29", 12), rep("30-44", 15), rep("45-59", 15), rep("60+", 26))
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
cps$eth_race_comb_cluster <- rep("Other", nrow(cps))
cps$eth_race_comb_cluster[cps$race_cluster == "White"] <- "White"
cps$eth_race_comb_cluster[cps$race_cluster == "Black"] <- "Black"
cps$eth_race_comb_cluster[cps$race_cluster == "Asian/Pacific Islander"] <- "Asian/Pacific Islander"
cps$eth_race_comb_cluster[cps$is_hispanic == "Hispanic/Latino"] <- "Hispanic/Latino"

# Write final outputs
write_csv(cps, "final_data/cps_clean_ipums_2008-2022.csv")
write_dta(cps, "final_data/cps_clean_ipums_2008-2022.dta")
