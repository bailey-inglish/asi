# Setup
library(tidyverse)
library(haven)
library(ipumsr)
setwd("cps_cleaning")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00014.xml") %>% read_ipums_micro()

# Uses upper bound year to approx immigrant years in the US (note that higher)
# values are more subject to varition, see codebook.
cps$min_immi_years_in_us <- rep(NA, nrow(cps))
cps[cps$YRIMMIG != 0, "min_immi_years_in_us"] <- cps[cps$YRIMMIG != 0, "YEAR"] - cps[cps$YRIMMIG != 0, "YRIMMIG"]

# Hispanic simple recoding
cps$is_hispanic <- cps$HISPAN != 0
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$race_cluster <- rep("multiracial", nrow(cps))
cps$race_cluster[cps$RACE == 100] <- "white"
cps$race_cluster[cps$RACE == 200] <- "black"
cps$race_cluster[cps$RACE == 300] <- "native american"
cps$race_cluster[cps$RACE == 651] <- "asian"

# Geo + citizen recodings
cps$METFIPS[cps$METFIPS == 99998] <- NA
cps$METRO[cps$METRO == 0] <- NA
cps$CBSASZ[cps$CBSASZ == 0] <- NA
cps$CITIZEN[cps$CITIZEN == 9] <- NA

# VOREG recode to account for voter universe specification
cps <- mutate(cps, is_registered = VOTED == 2 | VOREG == 2)

# EDUC clustering recode
cps$edu_cluster <- rep(NA, nrow(cps))
cps$edu_cluster[cps$EDUC <= 72 & cps$EDUC > 1] <- "Less than HS Diploma" # 1 is coded as NIU! Boo :(
cps$edu_cluster[cps$EDUC == 73] <- "HS Diploma or Equivalent"
cps$edu_cluster[cps$EDUC == 81] <- "Some College, No Degree"
cps$edu_cluster[is.element(cps$EDUC, c(91, 92))] <- "Associate's Degree"
cps$edu_cluster[cps$EDUC == 111] <- "Bachelor's Degree"
cps$edu_cluster[cps$EDUC > 123] <- "Advanced Degree"

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
  select(YEAR, fips, vep_turnout = VEP_TURNOUT_RATE)

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

cps <- filter(VOTED == 1 | VOTED == 2)

# Write final outputs
write_csv(cps, "final_data/cps_clean_ipums_2008-2022.csv")
write_dta(cps, "final_data/cps_clean_ipums_2008-2022.dta")

test <- filter(cps, YEAR == 2020) %>% group_by(STATEFIP) %>%
  summarize(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  )
