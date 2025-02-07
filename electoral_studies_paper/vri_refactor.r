# File name: vri_refactor.r
# Purpose:   Reorganizes a cleaned PUMS file into a summary-level table of VRI
#            by each selected variable and year/state/etc.
# Author:    Bailey Inglish

# Setup
library(tidyverse)
setwd("electoral_studies_paper")

cps <- read_csv("final_data/cps_reduced_ipums_1994-2022.csv")

cps_c <- cps %>%
  pivot_longer(
    cols = starts_with("is_"),
    names_to = "grouping_var",
    values_to = "gvar_value"
  ) %>%
  filter(!is.na(gvar_value))

state_vri <- group_by(cps_c, grouping_var, year, state_name, gvar_value) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (voted == 2))
  ) %>%
  left_join(
    group_by(cps_c, grouping_var, year, state_name) %>%
      reframe(
        total_vep = sum(adj_vosuppwt),
        total_voters = sum(adj_vosuppwt * (voted == 2))
      ),
    by = c("year", "state_name", "grouping_var")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * vep_in_group / total_vep,
    pct_of_voters = 100 * voters_in_group / total_voters,
    vri = 100 * (pct_of_voters - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = gvar_value
  ) %>%
  select(
    year,
    state_name,
    vri,
    starts_with("is_")
  )

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
    vri = 100 * (pct_of_voters - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = gvar_value
  ) %>%
  select(
    year,
    vri,
    starts_with("is_")
  )

filter(state_vri_c, !is.na(is_college_educ)) %>%
  ggplot() +
    geom_boxplot(
      aes(
        x = is_college_educ,
        y = vri
      )
    )
# permutation tests?