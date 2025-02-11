# File name: vri_refactor.r
# Purpose:   Reorganizes a cleaned PUMS file into a summary-level table of VRI
#            by each selected variable and year/state/etc.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)
setwd("electoral_studies_paper")

cps <- read_csv("final_data/cps_reduced_ipums_2008-2022.csv") %>%
  select(!is_registered) # is_registered is redundant to calculate VRI

cps_c <- cps %>%
  pivot_longer(
    cols = starts_with("is_"),
    names_to = "grouping_var",
    values_to = "gvar_value"
  ) %>%
  filter(!is.na(gvar_value))

state_vri <- group_by(cps_c, grouping_var, year, locality, gvar_value) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (voted == 2))
  ) %>%
  left_join(
    group_by(cps_c, grouping_var, year, locality) %>%
      reframe(
        total_vep = sum(adj_vosuppwt),
        total_voters = sum(adj_vosuppwt * (voted == 2))
      ),
    by = c("year", "locality", "grouping_var")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * vep_in_group / total_vep,
    pct_of_voters = 100 * voters_in_group / total_voters,
    vri = 100 * (pct_of_voters - pct_of_ve_population) / pct_of_ve_population
  ) %>%
  select(year, locality, grouping_var, gvar_value, vri)

fed_vri <- group_by(cps_c, grouping_var, year, gvar_value) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (voted == 2))
  ) %>%
  left_join(
    group_by(cps_c, grouping_var, year) %>%
      reframe(
        total_vep = sum(adj_vosuppwt),
        total_voters = sum(adj_vosuppwt * (voted == 2))
      ),
    by = c("year", "grouping_var")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * vep_in_group / total_vep,
    pct_of_voters = 100 * voters_in_group / total_voters,
    vri = 100 * (pct_of_voters - pct_of_ve_population) / pct_of_ve_population
  ) %>%
  select(year, grouping_var, gvar_value, vri)


ovr_vri <- fed_vri %>%
  mutate(
    locality = "United States"
  ) %>%
  rows_append(state_vri)

t_ovr_vri <- filter(ovr_vri, gvar_value == TRUE) %>%
  mutate(t_vri = vri, .keep = "unused") %>%
  select(!gvar_value)
f_ovr_vri <- filter(ovr_vri, gvar_value == FALSE) %>%
  mutate(f_vri = vri, .keep = "unused") %>%
  select(!gvar_value)
ovr_vri <- left_join(f_ovr_vri, t_ovr_vri) %>%
  mutate(vri_ratio = -1 * (f_vri / t_vri), .keep = "unused")

# Add in covariates of interest
# SDR status and years from NCSL
# https://www.ncsl.org/elections-and-campaigns/same-day-voter-registration
sdr_years <- read_csv("raw_data/sdr_years.csv")
ovr_vri <- mutate(ovr_vri, has_sdr = FALSE)

for (state_name in sdr_years$state_name) {
  sdr_start <- sdr_years[sdr_years$state_name == state_name, "sdr_start"] %>%
    as.numeric()
  years <- sdr_start:2022
  for (y in years) {
    if (length(ovr_vri[ovr_vri$year == y & ovr_vri$locality == state_name, "has_sdr"]) > 0) {
      ovr_vri[ovr_vri$year == y & ovr_vri$locality == state_name, "has_sdr"] <- TRUE
    }
  }
}

# General election total federal votes from the FEC (converted to logical)
# https://www.fec.gov/resources/cms-content/documents/federalelections20XX.pdf
election_results <- read_csv("raw_data/election_results.csv")
ovr_vri <- left_join(ovr_vri, election_results, by = c("locality" = "name", "year" = "year"))

write_csv(ovr_vri, "final_data/vri_2008-2022.csv")
write_dta(ovr_vri, "final_data/vri_2008-2022.dta")
