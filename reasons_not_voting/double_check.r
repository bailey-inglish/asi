library(tidyverse)
library(ipumsr)
setwd("reasons_not_voting")

# Import data
cps <- read_ipums_ddi("cps_00027.xml") %>% read_ipums_micro()

# VOYNOTREG (Reason why respondent did not register to vote)
cps_voynotreg <- cps %>%
  filter(VOTED == 1 | VOTED == 2) %>%
  filter(VOYNOTREG %in% c(1:8, 97)) %>%
  group_by(YEAR, VOYNOTREG) %>%
  summarise(
    count = sum(WTFINL),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(YEAR, VOYNOTREG, proportion) %>%
  arrange(YEAR, VOYNOTREG)

# VOWHYNOT (Reason why eligible voter did not vote)
cps_vowhynot <- cps %>%
  filter(VOTED == 1 | VOTED == 2) %>%
  filter(VOWHYNOT %in% c(1:15, 97)) %>%
  group_by(YEAR, VOWHYNOT) %>%
  summarise(
    count = sum(WTFINL),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(YEAR, VOWHYNOT, proportion) %>%
  arrange(YEAR, VOWHYNOT)

# VOTEHOW (Method of voting in the most recent November election)
cps_votehow <- cps %>%
  filter(VOTED == 2) %>%
  filter(VOTEHOW %in% c(1:6, 97)) %>%
  group_by(YEAR, VOTEHOW) %>%
  summarise(
    count = sum(WTFINL),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(YEAR, VOTEHOW, proportion) %>%
  arrange(YEAR, VOTEHOW)

# VOTEWHEN (Voted on or before election day)
cps_votewhen <- cps %>%
  filter(VOTED == 2) %>%
  filter(VOTEWHEN %in% c(1:3, 97)) %>%
  group_by(YEAR, VOTEWHEN) %>%
  summarise(
    count = sum(WTFINL),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(YEAR, VOTEWHEN, proportion) %>%
  arrange(YEAR, VOTEWHEN)

# VOREGHOW (Method of registering to vote)
cps_voreghow <- cps %>%
  filter(registered == TRUE) %>%
  filter(VOREGHOW %in% c(1:6, 97)) %>%
  group_by(YEAR, VOREGHOW) %>%
  summarise(
    count = sum(WTFINL),
    .groups = "drop"
  ) %>%
  group_by(YEAR) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(YEAR, VOREGHOW, proportion) %>%
  arrange(YEAR, VOREGHOW)

# Question: In 2020, what were the rasons why REGISTERED voters did not vote?
cps_vowhynot_2020_registered <- cps %>%
  filter(YEAR == 2020) %>%
  filter(VOREG == 2) %>%
  filter(VOWHYNOT %in% c(1:15, 97)) %>%
  group_by(VOWHYNOT) %>%
  summarise(
    count = n(),
    .groups = "drop"
  ) %>%
  mutate(
    total = sum(count),
    proportion = count / total
  ) %>%
  select(VOWHYNOT, proportion) %>%
  arrange(VOWHYNOT)
