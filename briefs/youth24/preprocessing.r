# File name: cps_processing.r
# Purpose:   Takes 1980-2024 CPS data and creates clean PUMS-level results.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(ipumsr)
setwd("briefs/youth24")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00025.xml") %>% read_ipums_micro() %>%
  filter(YEAR >= 1980) # some missing turnout data for 1980-fixed
act_turn <- read_csv("raw_data/actual_turnout.csv")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")
act_turn <- left_join(act_turn, fips_conv, by = c("STATE_ABV" = "abbr")) %>%
  select(YEAR, fips, locality = name, vep_turnout) %>%
  filter(YEAR >= 1980)
poverty_line <- read_csv("raw_data/poverty_line.csv")

# VOREG recode to account for voter universe specification
cps <- mutate(cps, registered = VOTED == 2 | VOREG == 2)

# Reweight according to Hur & Achen using UF Election Lab actual turnout data
raw_turn <- filter(
  cps,
  VOTED == 1 | VOTED == 2 # already filtered to citizen = T & age > 18 by census
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

# EDUC clustering recode
cps$edu_cluster <- rep(NA, nrow(cps))
cps$edu_cluster[cps$EDUC <= 72 & cps$EDUC > 1] <- "Less than HS Diploma" # 1 is coded as NIU! Boo :(
cps$edu_cluster[cps$EDUC == 73] <- "HS Diploma or Equivalent"
cps$edu_cluster[is.element(cps$EDUC, c(80, 81))] <- "Some College But No Degree"
cps$edu_cluster[is.element(cps$EDUC, c(90, 91, 92, 100))] <- "Associate's Degree"
cps$edu_cluster[cps$EDUC >= 110] <- "Bachelor's Degree Or Higher"

# One final round of cohorting on selected variables
income_conv <- tribble(
  ~FAMINC, ~income_range,
  100, "<$25k",
  110, "<$25k",
  120, "<$25k",
  130, "<$25k",
  140, "<$25k",
  150, "<$25k",
  210, "<$25k",
  220, "<$25k",
  231, "<$25k",
  300, "<$25k",
  430, "<$25k",
  440, "<$25k",
  460, "<$25k",
  470, "<$25k",
  500, "<$25k",
  540, "<$25k",
  550, "<$25k",
  600, "<$25k",
  700, "$25-50k",
  710, "$25-50k",
  720, "$25-50k",
  730, "$25-50k",
  740, "$25-50k",
  800, ">50k",
  820, "$50-75k",
  830, "$50-75k",
  840, ">$75k",
  841, "$75-100k",
  842, "$100-150k",
  843, ">$150k",
  995, NA,
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

# Poverty line variable addition
max_income_conv <- tribble(
  ~FAMINC, ~max_inc,
  100, 4999,
  110, 999,
  120, 1999,
  130, 2999,
  140, 3999,
  150, 4999,
  210, 7499,
  220, 5999,
  231, 7499,
  300, 9999,
  430, 12499,
  440, 11999,
  460, 14999,
  470, 14999,
  500, 19999,
  540, 17499,
  550, 19999,
  600, 24999,
  700, 49999,
  710, 29999,
  720, 34999,
  730, 39999,
  740, 49999,
  800, 999999999, # upper bound dummy value
  820, 59999,
  830, 74999,
  840, 999999999, # upper bound dummy value
  841, 99999,
  842, 1499999,
  843, 999999999, # upper bound dummy value
  995, NA,
  996, NA,
  997, NA,
  999, NA
)

# scale down FAMSIZE
cps[cps$FAMSIZE > 8, ]$FAMSIZE <- 8
cps <- cps %>%
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
    1881:1900,
    1901:1927,
    1928:1945,
    1946:1964,
    1965:1980,
    1981:1996,
    1997:2012,
    2013:2024
  ),
  generation = c(
    rep("Lost", 20),
    rep("Greatest", 27),
    rep("Silent", 18),
    rep("Boomer", 19),
    rep("Generation X", 16),
    rep("Millenial", 16),
    rep("Generation Z", 16),
    rep("Generation Alpha", 12)
  )
)

cps <- cps %>%
  mutate(
    birth_year = YEAR - AGE
  ) %>%
  left_join(
    generation_conv,
    by = "birth_year"
  ) %>%
  select(!birth_year)

# Write final outputs
cps_final <- cps %>% select(
  !c(STATEFIP,
    METRO,
    FAMINC,
    SEX,
    RACE,
    FAMSIZE,
    CITIZEN,
    HISPAN,
    EDUC,
    VOREG,
    VOTERES,
    VOSUPPWT,
    ends_with("_turnout"),
    ends_with("_wt")
  )
) %>%
  mutate(
    year = YEAR,
    age = AGE,
    voted = VOTED == 2,
    .keep = "unused"
  )
write_csv(cps_final, "final_data/revised_ipums_final80-24.csv")

# diagnostics
for (n in names(cps_final)) {
  print(n)
  print(sum(is.na(cps_final[, n])))
  print("- - - - - - - - - - - - - - - - - - - -")
}
