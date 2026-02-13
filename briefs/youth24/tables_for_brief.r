# File name: selected_plots.r
# Purpose:   Saves final tables used in 2024 TCVR Youth Brief
# Author:    Bailey Inglish

# Setup
library(tidyverse)

setwd("briefs/youth24")

cps <- read_csv("final_data/revised_ipums_final80-24.csv")

cps_young <- filter(cps, age >= 18, age <= 29)
cps_young_tx <- filter(cps_young, locality == "Texas")

cps_24_young <- filter(cps, year == 2024, age >= 18, age <= 29)
cps_24_young_tx <- filter(cps_24_young, locality == "Texas")

# 4: % of voters in each age group for eligible population vs. actual voters
#    in the US and TX, just for 2024

# 4-1 - United States
filter(cps, year == 2024) %>%
  group_by(age_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      filter(cps, year == 2024)$adj_vosuppwt *
        (filter(cps, year == 2024)$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = age_cluster,
    `U.S. Eligible` = prop_eligible,
    `U.S. Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`U.S. Eligible`, `U.S. Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table4-1.csv")

# 4-2 - Texas
filter(cps, year == 2024, locality == "Texas") %>%
  group_by(age_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps, year == 2024, locality == "Texas")$adj_vosuppwt),
    total_voted = sum(
      filter(cps, year == 2024, locality == "Texas")$adj_vosuppwt *
        (filter(cps, year == 2024, locality == "Texas")$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = age_cluster,
    `TX Eligible` = prop_eligible,
    `TX Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`TX Eligible`, `TX Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table4-2.csv")

# 5: age group turnout over time, us + tx, midterm + pres

# 5-1 - US and midterm
group_by(cps, age_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, age_cluster, turnout) %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = turnout
  ) %>%
  write_csv("final_data/tables/table5-1.csv")

# 5-2 - TX and midterm
filter(cps, locality == "Texas") %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, age_cluster, turnout) %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = turnout
  ) %>%
  write_csv("final_data/tables/table5-2.csv")

# 5-3 - US and presidential
group_by(cps, age_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, age_cluster, turnout) %>%
  filter(year %% 4 == 0) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = turnout
  ) %>%
  write_csv("final_data/tables/table5-3.csv")

# 5-4 - TX and presidential
filter(cps, locality == "Texas") %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, age_cluster, turnout) %>%
  filter(year %% 4 == 0) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = turnout
  ) %>%
  write_csv("final_data/tables/table5-4.csv")

# 6 - turnout by age 18-25 generationally
upper_bound_gen_years <- tibble(
  generation = c("Lost", "Greatest", "Silent", "Boomer", "Generation X", "Millenial", "Generation Z", "Generation Alpha"),
  end_year = c(1900, 1927, 1945, 1964, 1980, 1996, 2012, 2024)
)

cps %>%
  group_by(generation, age) %>%
  reframe(
    eligible = sum(adj_vosuppwt),
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    turnout = 100 * round((voted / eligible), 3)
  ) %>%
  left_join(upper_bound_gen_years, by = "generation") %>%
  mutate(is_stable = (2024 >= age + end_year)) %>%
  select(generation, age, turnout) %>%
  filter(age <= 25) %>%
  pivot_wider(
    names_from = generation,
    values_from = turnout
  ) %>%
  select(age, Boomer, `Generation X`, Millenial, `Generation Z`) %>%
  arrange(age) %>%
  write_csv("final_data/tables/table6.csv")

# 8/9: breakdown of youth voters (18-29) by gender, race/ethnicity, income,
#      educational attainment [2024 only us vs tx]

# 8-1 - US gender
cps_24_young %>%
  group_by(sex_name) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young$adj_vosuppwt *
        (cps_24_young$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = sex_name,
    `U.S. Eligible` = prop_eligible,
    `U.S. Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`U.S. Eligible`, `U.S. Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table8-1.csv")

# 8-2 - TX gender
cps_24_young_tx %>%
  group_by(sex_name) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young_tx, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young_tx$adj_vosuppwt *
        (cps_24_young_tx$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = sex_name,
    `TX Eligible` = prop_eligible,
    `TX Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`TX Eligible`, `TX Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table8-2.csv")

# 8-3 - US race/ethnicity
cps_24_young %>%
  group_by(eth_race_comb_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young$adj_vosuppwt *
        (cps_24_young$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = eth_race_comb_cluster,
    `U.S. Eligible` = prop_eligible,
    `U.S. Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`U.S. Eligible`, `U.S. Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  write_csv("final_data/tables/table8-3.csv")

# 8-4 - TX race/ethnicity
cps_24_young_tx %>%
  group_by(eth_race_comb_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young_tx, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young_tx$adj_vosuppwt *
        (cps_24_young_tx$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = eth_race_comb_cluster,
    `TX Eligible` = prop_eligible,
    `TX Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`TX Eligible`, `TX Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  write_csv("final_data/tables/table8-4.csv")

# 9-1 - US income
cps_24_young %>%
  group_by(income_range) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young$adj_vosuppwt *
        (cps_24_young$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = income_range,
    `U.S. Eligible` = prop_eligible,
    `U.S. Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`U.S. Eligible`, `U.S. Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table9-1.csv")

# 9-2 - TX income
cps_24_young_tx %>%
  group_by(income_range) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young_tx, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young_tx$adj_vosuppwt *
        (cps_24_young_tx$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = income_range,
    `TX Eligible` = prop_eligible,
    `TX Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`TX Eligible`, `TX Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table9-2.csv")

# US education
cps_24_young %>%
  group_by(edu_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young$adj_vosuppwt *
        (cps_24_young$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = edu_cluster,
    `U.S. Eligible` = prop_eligible,
    `U.S. Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`U.S. Eligible`, `U.S. Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table9-3.csv")

# TX education
cps_24_young_tx %>%
  group_by(edu_cluster) %>%
  summarize(
    total_eligible = sum(filter(cps_24_young_tx, year == 2024)$adj_vosuppwt),
    total_voted = sum(
      cps_24_young_tx$adj_vosuppwt *
        (cps_24_young_tx$voted == TRUE)
    ),
    prop_eligible = 100 * round(sum(adj_vosuppwt) / total_eligible, 3),
    prop_voted = 100 * round(sum(adj_vosuppwt * (voted == TRUE)) / total_voted, 3)
  ) %>%
  select(
    grouping_var = edu_cluster,
    `TX Eligible` = prop_eligible,
    `TX Voters` = prop_voted
  ) %>%
  pivot_longer(
    cols = c(`TX Eligible`, `TX Voters`),
    names_to = "Measure",
    values_to = "Value"
  ) %>%
  pivot_wider(
    names_from = grouping_var,
    values_from = Value
  ) %>%
  rename(`Eligible/Voters` = Measure) %>%
  write_csv("final_data/tables/table9-4.csv")

# 10-11: turnout by race, midterm + pres, us + tx [18-29 only]

# 10-1 - US midterm
cps_young %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, eth_race_comb_cluster, turnout) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = turnout
  ) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  arrange(year) %>%
  write_csv("final_data/tables/table10-1.csv")

# 10-2 - TX midterm
cps_young_tx %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, eth_race_comb_cluster, turnout) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = turnout
  ) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  arrange(year) %>%
  write_csv("final_data/tables/table10-2.csv")

# 11-1 - US presidential
cps_young %>%
  filter(year %% 4 == 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, eth_race_comb_cluster, turnout) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = turnout
  ) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  arrange(year) %>%
  write_csv("final_data/tables/table11-1.csv")

# 11-2 - TX presidential
cps_young_tx %>%
  filter(year %% 4 == 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  select(year, eth_race_comb_cluster, turnout) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = turnout
  ) %>%
  mutate(
    Latino = `Hispanic/Latino`,
    AAPI = `Asian/Pacific Islander`,
    Other = `American Indian` + Multiracial,
    .keep = "unused"
  ) %>%
  arrange(year) %>%
  write_csv("final_data/tables/table11-2.csv")

# 12: turnout by poverty status, midterm + pres, us + tx

# 12-1 - midterm
cps %>%
  filter(year %% 2 == 0 & year %% 4 != 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty - US", "Not in Poverty - US")
  ) %>%
  select(year, group_var, turnout) %>%
  pivot_wider(
    names_from = group_var,
    values_from = turnout
  ) %>%
  inner_join(
    filter(cps, locality == "Texas") %>%
      filter(year %% 2 == 0 & year %% 4 != 0, !is.na(is_in_poverty)) %>%
      group_by(is_in_poverty, year) %>%
      reframe(
        voted = sum(adj_vosuppwt * (voted == TRUE)),
        eligible = sum(adj_vosuppwt),
        turnout = 100 * round(voted / eligible, 3)
      ) %>%
      mutate(
        group_var = ifelse(is_in_poverty, "In Poverty - TX", "Not in Poverty - TX")
      ) %>%
      select(year, group_var, turnout) %>%
      pivot_wider(
        names_from = group_var,
        values_from = turnout
      ),
    by = "year"
  ) %>%
  write_csv("final_data/tables/table12-1.csv")

# 12-2 - presidential
cps %>%
  filter(year %% 4 == 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted = sum(adj_vosuppwt * (voted == TRUE)),
    eligible = sum(adj_vosuppwt),
    turnout = 100 * round(voted / eligible, 3)
  ) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty - US", "Not in Poverty - US")
  ) %>%
  select(year, group_var, turnout) %>%
  pivot_wider(
    names_from = group_var,
    values_from = turnout
  ) %>%
  inner_join(
    filter(cps, locality == "Texas") %>%
      filter(year %% 4 == 0, !is.na(is_in_poverty)) %>%
      group_by(is_in_poverty, year) %>%
      reframe(
        voted = sum(adj_vosuppwt * (voted == TRUE)),
        eligible = sum(adj_vosuppwt),
        turnout = 100 * round(voted / eligible, 3)
      ) %>%
      mutate(
        group_var = ifelse(is_in_poverty, "In Poverty - TX", "Not in Poverty - TX")
      ) %>%
      select(year, group_var, turnout) %>%
      pivot_wider(
        names_from = group_var,
        values_from = turnout
      ),
    by = "year"
  ) %>%
  write_csv("final_data/tables/table12-2.csv")

# 15 - vri by age group, us + tx, midterm + pres

# 15-1 - us midterm
cps %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps, year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(age_cluster, year, vri) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table15-1.csv")

# 15-2 - tx midterm
cps %>% 
  filter(locality == "Texas") %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(age_cluster, year, vri) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table15-2.csv")

# 15-3 - us presidential
cps %>%
  filter(year %% 4 == 0) %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps, year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(age_cluster, year, vri) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table15-3.csv")

# 15-4 - tx presidential
cps %>% 
  filter(locality == "Texas") %>%
  filter(year %% 4 == 0) %>%
  group_by(age_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(age_cluster, year, vri) %>%
  pivot_wider(
    names_from = age_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table15-4.csv")

# 16-17: vri by race, us + tx, midterm + pres

# 16-1 - us midterm
cps_young %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps_young, year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(eth_race_comb_cluster, year, vri) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table16-1.csv")

# 16-2 - tx midterm
cps_young_tx %>%
  filter(year %% 2 == 0 & year %% 4 != 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps_young_tx, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(eth_race_comb_cluster, year, vri) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table16-2.csv")

# 17-1 - us presidential
cps_young %>% 
  filter(year %% 4 == 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps_young, year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(eth_race_comb_cluster, year, vri) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table17-1.csv")

# 17-2 - tx presidential
cps_young_tx %>% 
  filter(year %% 4 == 0) %>%
  group_by(eth_race_comb_cluster, year) %>%
  summarize(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps_young_tx, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  select(eth_race_comb_cluster, year, vri) %>%
  pivot_wider(
    names_from = eth_race_comb_cluster,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table17-2.csv")

# 18: vri by poverty status, us + tx, midterm + pres

# 18-1 - us midterm
cps %>%
  filter(year %% 2 == 0 & year %% 4 != 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps, year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty", "Not in Poverty")
  ) %>%
  select(year, group_var, vri) %>%
  pivot_wider(
    names_from = group_var,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table18-1.csv")

# 18-2 - tx midterm
cps %>%
  filter(locality == "Texas") %>%
  filter(year %% 2 == 0 & year %% 4 != 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 2 == 0 & year %% 4 != 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty", "Not in Poverty")
  ) %>%
  select(year, group_var, vri) %>%
  pivot_wider(
    names_from = group_var,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table18-2.csv")

# 18-3 - us presidential
cps %>%
  filter(year %% 4 == 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    group_by(cps, year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty", "Not in Poverty")
  ) %>%
  select(year, group_var, vri) %>%
  pivot_wider(
    names_from = group_var,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table18-3.csv")

# 18-4 - tx presidential
cps %>%
  filter(locality == "Texas") %>%
  filter(year %% 4 == 0, !is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, year) %>%
  reframe(
    voted_in_group = sum(adj_vosuppwt * (voted == TRUE)),
    eligible_in_group = sum(adj_vosuppwt)
  ) %>%
  left_join(
    filter(cps, locality == "Texas") %>%
      group_by(year) %>%
      filter(year %% 4 == 0) %>%
      summarize(
        voted_total = sum(adj_vosuppwt * (voted == TRUE)),
        eligible_total = sum(adj_vosuppwt)
      ),
    by = "year"
  ) %>%
  mutate(vri = 100 * round(((voted_in_group / voted_total) / (eligible_in_group / eligible_total)) - 1, 3)) %>%
  mutate(
    group_var = ifelse(is_in_poverty, "In Poverty", "Not in Poverty")
  ) %>%
  select(year, group_var, vri) %>%
  pivot_wider(
    names_from = group_var,
    values_from = vri
  ) %>%
  write_csv("final_data/tables/table18-4.csv")
