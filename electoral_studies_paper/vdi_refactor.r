# File name: vdi_refactor.r
# Purpose:   Reorganizes a cleaned PUMS file into a summary-level table of VDI
#            by each selected variable and year/state/etc.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)
setwd("electoral_studies_paper")

cps <- read_csv("final_data/cps_reduced_ipums_2008-2022.csv") %>%
  select(!is_registered) # is_registered is redundant to calculate vdi

cps_c <- cps %>%
  pivot_longer(
    cols = starts_with("is_"),
    names_to = "grouping_var",
    values_to = "gvar_value"
  ) %>%
  filter(!is.na(gvar_value))

state_vdi <- group_by(cps_c, grouping_var, year, locality, gvar_value) %>%
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
    pct_of_ve_population = vep_in_group / total_vep,
    pct_of_voters = voters_in_group / total_voters,
    vdi = abs(pct_of_voters - pct_of_ve_population) / pct_of_ve_population
  ) %>%
  select(year, locality, grouping_var, gvar_value, vdi)

fed_vdi <- group_by(cps_c, grouping_var, year, gvar_value) %>%
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
    pct_of_ve_population = vep_in_group / total_vep,
    pct_of_voters = voters_in_group / total_voters,
    vdi = abs(pct_of_voters - pct_of_ve_population) / pct_of_ve_population
  ) %>%
  select(year, grouping_var, gvar_value, vdi)


ovr_vdi <- fed_vdi %>%
  mutate(
    locality = "United States"
  ) %>%
  rows_append(state_vdi)

# Add in covariates of interest
# SDR status and years from NCSL
# https://www.ncsl.org/elections-and-campaigns/same-day-voter-registration
sdr_years <- read_csv("raw_data/sdr_years.csv")
ovr_vdi <- mutate(ovr_vdi, has_sdr = FALSE)

for (state_name in sdr_years$state_name) {
  sdr_start <- sdr_years[sdr_years$state_name == state_name, "sdr_start"] %>%
    as.numeric()
  years <- sdr_start:2022
  for (y in years) {
    if (length(ovr_vdi[ovr_vdi$year == y & ovr_vdi$locality == state_name, "has_sdr"]) > 0) {
      ovr_vdi[ovr_vdi$year == y & ovr_vdi$locality == state_name, "has_sdr"] <- TRUE
    }
  }
}

# General election total federal votes from the FEC (converted to logical)
# https://www.fec.gov/resources/cms-content/documents/federalelections20XX.pdf
election_results <- read_csv("raw_data/election_results.csv")
ovr_vdi <- left_join(ovr_vdi, election_results, by = c("locality" = "name", "year" = "year"))

t_ovr_vdi <- filter(ovr_vdi, gvar_value == TRUE) %>%
  mutate(t_vdi = vdi, .keep = "unused") %>%
  select(!gvar_value)
f_ovr_vdi <- filter(ovr_vdi, gvar_value == FALSE) %>%
  mutate(f_vdi = vdi, .keep = "unused") %>%
  select(!gvar_value)
ovr_vdi <- left_join(f_ovr_vdi, t_ovr_vdi) %>%
  mutate(vdi_ratio = f_vdi + t_vdi, .keep = "unused")

write_csv(ovr_vdi, "final_data/vdi_ratio_2008-2022.csv")
write_dta(ovr_vdi, "final_data/vdi_ratio_2008-2022.dta")