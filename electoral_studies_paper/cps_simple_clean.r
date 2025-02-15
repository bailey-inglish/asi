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

# Hispanic simple recoding
cps$is_hispanic <- rep(TRUE, nrow(cps))
cps$is_hispanic[cps$HISPAN == 0] <- FALSE
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$is_white <- rep(FALSE, nrow(cps))
cps$is_white[cps$RACE == 100] <- TRUE

# VOREG recode to account for voter universe specification
cps <- mutate(cps, registered = VOTED == 2 | VOREG == 2)

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
