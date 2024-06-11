library(tidyverse)
library(ipumsr)
library(forcats)

setwd("C:/Users/baile/Desktop/asi/sdr/prep")
options(tibble.width = Inf)

fips_conv <- read_csv("fips_name_abbr.csv") # https://census.gov
pums <- read_ipums_ddi("cps_00006.xml") %>% read_ipums_micro() # https://cps.ipums.org/cps-action/data_requests/download
turnout_actual <- read_csv("turnout_time_series_by_state.csv") # https://election.lab.ufl.edu/dataset/1980-2022-general-election-turnout-rates-v1-1/

pums_summary <-
  left_join(
    pums,
    fips_conv,
    by = c("STATEFIP" = "fips")
  ) %>%
  mutate(
    young = as.logical(AGE >= 18 & AGE <= 24),
    hispan = as.logical(HISPAN != 0),
    state = name
  ) %>%
  select(YEAR, WTFINL, state, young, hispan, VOTED, VOREG) %>%
  group_by(state, YEAR) %>%
  summarize(
    votes = sum((VOTED == 2) * WTFINL),
    reg_no_vote = sum((VOREG == 2) * WTFINL),
    tot_reg = votes + reg_no_vote,
    vap_est = sum((VOTED != 99) * WTFINL),
    young_votes = sum((VOTED == 2 & young == TRUE) * WTFINL),
    young_vap_est = sum((VOTED != 99 & young == TRUE) * WTFINL),
    hisp_votes = sum((VOTED == 2 & hispan == TRUE) * WTFINL),
    hisp_vap_est = sum((VOTED != 99 & hispan == TRUE) * WTFINL)
  )

felon_rates <-
  mutate(
    turnout_actual,
    YEAR = YEAR,
    state = STATE,
    mod_felon_rate = INELIGIBLE_FELONS_TOTAL / ((1 - turnout_actual$NONCITIZEN_PCT) * VAP),
    .keep = "none"
  )

pums_comp <-
  left_join(
    pums_summary,
    felon_rates,
    by = c("YEAR", "state")
  ) %>%
  mutate(
    vep_est = vap_est * (1 - mod_felon_rate),
    turnout_est = votes / vep_est,
    young_vep_est = young_vap_est * (1 - mod_felon_rate),
    young_turnout_est = young_votes / young_vep_est,
    hisp_vep_est = hisp_vap_est * (1 - mod_felon_rate),
    hisp_turnout_est = hisp_votes / hisp_vep_est
  )

pums_adj <-
  left_join(
    select(
      pums_comp,
      state,
      YEAR,
      turnout_est,
      young_turnout_est,
      hisp_turnout_est
    ),
    select(
      turnout_actual,
      YEAR,
      state = STATE,
      turnout = VEP_TURNOUT_RATE
    ),
    by = c("YEAR", "state")
  ) %>%
  mutate(
    error = (turnout_est - turnout) / turnout,
    young_turnout_adj = young_turnout_est / (1 + error),
    hisp_turnout_adj = hisp_turnout_est / (1 + error)
  )

pums_tidy <-
  select(
    pums_adj,
    Year = YEAR,
    State = state,
    Overall = turnout,
    Young = young_turnout_adj,
    Hispanic = hisp_turnout_adj,
  ) %>%
  pivot_longer(
    c(Overall, Hispanic, Young),
    values_to = "Turnout",
    names_to = "Group"
  )

setwd("C:/Users/baile/Desktop/asi/sdr/live")
write_csv(pums_tidy, "turnout_by_year_state_group.csv")
