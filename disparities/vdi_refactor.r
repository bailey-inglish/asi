# File name: vdi_refactor.r
# Purpose:   Reorganizes a cleaned PUMS file into a summary-level table of VDI
#            by each selected variable and year/state/etc.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
library(haven)
setwd("disparities")

cps <- read_csv("final_data/cps_reduced_ipums_1994-2022.csv")
props <- read_csv("final_data/cps_state_proportions_1994-2022.csv")
fips_conv <- read_csv("raw_data/fips_name_abbr.csv")

# Reframe the data to be in terms of grouping variables, filter out NA values
cps_c <- cps %>%
  pivot_longer(
    cols = starts_with("is_"),
    names_to = "grouping_var",
    values_to = "gvar_value"
  ) %>%
  filter(
    !is.na(gvar_value),
    year >= 2004
  )

# Calculate the state and federal VRI values for each group
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
    pct_of_ve_population = vep_in_group / total_vep,
    pct_of_voters = voters_in_group / total_voters,
    vri = (pct_of_voters - pct_of_ve_population) / pct_of_ve_population
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
    pct_of_ve_population = vep_in_group / total_vep,
    pct_of_voters = voters_in_group / total_voters,
    vri = (pct_of_voters - pct_of_ve_population) / pct_of_ve_population
  ) %>%
  select(year, grouping_var, gvar_value, vri)

# Combine the two tibbles
ovr_vri <- fed_vri %>%
  mutate(
    locality = "United States"
  ) %>%
  rows_append(state_vri) %>%
  left_join(
    props,
    by = c("year", "locality")
  )

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
# https://utexas.box.com/s/wp4lwnqpvrewnfpf5qxrvz641ikccgrb
election_results <- read_csv("raw_data/fed_elec2004-2022.csv")
ovr_vri <- left_join(
  ovr_vri,
  election_results,
  by = c("locality", "year")
)

# Midterm/presidential year elections
ovr_vri$midterm <- round(ovr_vri$year / 4) == ovr_vri$year / 4

# Presidential incumbency status on the ballot
ovr_vri$dem_pres_incumb <- is.element(ovr_vri$year, c(1996, 2012))
ovr_vri$dem_pres_in_office <- is.element(ovr_vri$year, c(1994, 1996, 1998, 2000, 2010, 2012, 2014, 2016, 2022, 2024))

# Add region flags using Census region specifications
reg_conv <- read_csv("raw_data/region_conv.csv")
ovr_vri <- left_join(ovr_vri, reg_conv, by = c("locality" = "name"))

# Cost of Voting Index
covi <- read_csv("raw_data/covi_1996-2024.csv") %>%
  left_join(fips_conv, by = c("state" = "abbr")) %>%
  select(
    locality = name,
    year,
    final_covi
  )

ovr_vri <- left_join(ovr_vri, covi, by = c("year", "locality"))

# Calculate the VDI as the gap between the over and underrepesented VRIs
t_ovr_vri <- filter(ovr_vri, gvar_value == TRUE) %>%
  mutate(t_vri = abs(vri), .keep = "unused") %>%
  select(!gvar_value)
f_ovr_vri <- filter(ovr_vri, gvar_value == FALSE) %>%
  mutate(f_vri = abs(vri), .keep = "unused") %>%
  select(!gvar_value)
ovr_vdi <- inner_join(f_ovr_vri, t_ovr_vri) %>%
  mutate(vdi = f_vri + t_vri, .keep = "unused")

# Write final outputs
write_csv(ovr_vdi, "final_data/vdi_2004-2022.csv")
write_dta(ovr_vdi, "final_data/vdi_2004-2022.dta")