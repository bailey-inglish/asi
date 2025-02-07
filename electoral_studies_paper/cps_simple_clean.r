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
cps <- read_ipums_ddi("raw_data/cps_00017.xml") %>% read_ipums_micro()

# Hispanic simple recoding
cps$is_hispanic <- rep(TRUE, nrow(cps))
cps$is_hispanic[cps$HISPAN == 0] <- FALSE
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$is_white <- rep(FALSE, nrow(cps))
cps$is_white[cps$RACE == 100] <- TRUE

# VOREG recode to account for voter universe specification
cps$CITIZEN[cps$CITIZEN == 9] <- NA
cps <- mutate(cps, is_registered = VOTED == 2 | VOREG == 2)

# EDUC simplified recode
cps$is_college_educ <- rep(NA, nrow(cps))
cps$is_college_educ[cps$EDUC <= 72 & cps$EDUC > 1] <- FALSE
cps$is_college_educ[cps$EDUC == 73] <- FALSE
cps$is_college_educ[cps$EDUC == 81] <- TRUE
cps$is_college_educ[is.element(cps$EDUC, c(91, 92))] <- TRUE
cps$is_college_educ[cps$EDUC == 111] <- TRUE
cps$is_college_educ[cps$EDUC > 123] <- TRUE

# VOTERES harmonization
cps$is_long_res <- rep(NA, nrow(cps))
cps$is_long_res[cps$VOTERES >= 10 & cps$VOTERES <= 13] <- FALSE
cps$is_long_res[cps$VOTERES == 20] <- FALSE
cps$is_long_res[cps$VOTERES == 31] <- FALSE
cps$is_long_res[cps$VOTERES == 33] <- TRUE

# One final round of clustering on selected variables
income_conv <- tribble(
  ~FAMINC, ~is_higher_income,
  100, FALSE,
  210, FALSE,
  300, FALSE,
  430, FALSE,
  470, FALSE,
  500, FALSE,
  600, FALSE,
  710, FALSE,
  720, FALSE,
  730, FALSE,
  740, FALSE,
  820, TRUE,
  830, TRUE,
  840, TRUE,
  841, TRUE,
  842, TRUE,
  843, TRUE,
  996, NA,
  997, NA,
  999, NA
)

metro_conv <- tribble(
  ~METRO, ~is_metro,
  1, FALSE,
  2, TRUE,
  3, FALSE,
  4, FALSE
)

age_conv <- tibble(
  AGE = c(18:29, 30:44, 45:59, 60:85),
  is_over_30 = c(rep(FALSE, 12), rep(TRUE, 15), rep(TRUE, 15), rep(TRUE, 26))
)

sex_conv <- tibble(
  SEX = c(1, 2),
  is_female = c(FALSE, TRUE)
)

vetstat_conv <- tibble(
  VETSTAT = c(0, 1, 2),
  is_veteran = c(NA, FALSE, TRUE)
)

diff_conv <- tibble(
  DIFFANY = c(0, 1, 2),
  is_disabled = c(NA, FALSE, TRUE)
)

cps <- cps %>%
  left_join(income_conv) %>%
  left_join(metro_conv) %>%
  left_join(age_conv) %>%
  left_join(sex_conv) %>%
  left_join(diff_conv) %>%
  left_join(vetstat_conv)


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

# Pick out only the variables we analyze (comment out for debugging prn)
cps <- select(
  cps,
  year = YEAR,
  state_name,
  voted = VOTED,
  adj_vosuppwt,
  starts_with("is_")
)

# Write final outputs
write_csv(cps, "final_data/cps_reduced_ipums_1994-2022.csv")
write_dta(cps, "final_data/cps_reduced_ipums_1994-2022.dta")
