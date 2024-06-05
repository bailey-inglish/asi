# Libraries
library(tidyverse)
library(ipumsr)
library(forcats)

# Options
setwd("C:/Users/baile/Desktop/asi/turnout 2.0")
options(tibble.width = Inf)

# Load data
fips_conv <- read_csv("fips_name_abbr.csv")
pums <- read_ipums_ddi("cps_00005.xml") %>% read_ipums_micro()
turnout_actual <- read_csv("turnout_expanded.csv")

# Clean data
pums_clean <- filter(pums, VOTED != 99) %>%
  left_join(
    fips_conv,
    by = c("STATEFIP" = "fips")
  ) %>%
  mutate(
    VOTED = as.logical(VOTED == 2),
    HISPAN = as.logical(HISPAN != 0),
    STATE = name,
    CITIZEN = (CITIZEN != 5 & CITIZEN != 9) # technically redundant but still included for repro
  ) %>%
  select(WTFINL, STATE, CITIZEN, AGE, HISPAN, VOTED)

# Compute summary leaderboards
turnout_ovr <- group_by(pums_clean, STATE) %>%
  summarize(
    vap = sum(WTFINL),
    votes = sum(VOTED * WTFINL)
  ) %>%
  left_join(
    select(turnout_actual, state, felon),
    by = c("STATE" = "state")
  ) %>%
  mutate(
    vep = vap - felon,
    turnout_est = votes / vep
  )

turnout_young <- filter(pums_clean, AGE >= 18 & AGE <= 24) %>%
  group_by(STATE) %>%
  summarize(
    turnout_est = sum(VOTED * WTFINL) / sum(WTFINL),
    pop = sum(WTFINL),
    votes = sum(VOTED * WTFINL)
  ) %>%
  arrange(desc(turnout_est))

turnout_hisp <- filter(pums_clean, HISPAN == TRUE) %>%
  group_by(STATE) %>%
  summarize(
    turnout_est = sum(VOTED * WTFINL) / sum(WTFINL),
    pop = sum(WTFINL),
    votes = sum(VOTED * WTFINL)
  ) %>%
  arrange(desc(turnout_est))

# Join overall data to compare with actual for validity testing
comp <- left_join(turnout_actual, turnout_ovr, by = c("state" = "STATE"))

# VAP error - mean 8% under
mean((comp$voting_age_pop - comp$vap) / comp$voting_age_pop)
ggplot(comp) +
  geom_col(
    aes(
      x = state,
      y = 100 * (voting_age_pop - vap) / voting_age_pop
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  labs(
    title = "Error in voting age population",
    subtitle = "For the November 2020 election",
    x = "State",
    y = "(Actual - calculated) as percent of actual (%)"
  )

# VEP error - mean 9%
mean((comp$voting_age_pop - comp$vep) / comp$voting_age_pop)
ggplot(comp) +
  geom_col(
    aes(
      x = state,
      y = 100 * (voting_age_pop - vep) / voting_age_pop
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  labs(
    title = "Error in voting eligible population",
    subtitle = "For the November 2020 election",
    x = "State",
    y = "(Actual - calculated) as percent of actual (%)"
  )

# Ballot error - average 0%
mean((comp$ballots / comp$votes) / comp$ballots)
ggplot(comp) +
  geom_col(
    aes(
      x = fct_reorder(state, (ballots - votes) / ballots),
      y = 100 * (ballots - votes) / ballots
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  labs(
    title = "Error in ballot vs. 'I voted' count",
    subtitle = "For the November 2020 election",
    x = "State",
    y = "(Actual - calculated) as percent of actual (%)"
  )

# Raw turnout - range: 55% (OK) to 84% (DC)
range(turnout_ovr$turnout_est)
ggplot(turnout_ovr) +
  geom_col(
    aes(
      x = fct_reorder(STATE, turnout_est),
      y = turnout_est * 100
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )  +
  labs(
    title = "Calculated Estimate of Voter Turnout",
    subtitle = "For the November 2020 election",
    x = "State",
    y = "Voter turnout (%)"
  )

# Turnout error - mean 0%
mean(comp$elig_turnout - comp$turnout_est)
ggplot(comp) +
  geom_col(
    aes(
      x = fct_reorder(state, elig_turnout - turnout_est),
      y = 100 * (elig_turnout - turnout_est)
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  ) +
  labs(
    title = "Error in actual vs. CPS-expected turnout",
    subtitle = "For the November 2020 Election",
    x = "State",
    y = "Actual - calculated (% pts)"
  )

# Young voters - range: 31% (OK) to 81% (DC)
range(turnout_young$turnout_est)
ggplot(turnout_young) +
  geom_col(
    aes(
      x = fct_reorder(STATE, turnout_est),
      y = turnout_est * 100
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )  +
  labs(
    title = "Calculated Estimate of Voter Turnout",
    subtitle = "For Voters Aged 18-24 the November 2020 Election",
    x = "State",
    y = "Voter turnout (%)"
  )

# Hispanic turnout - range: 30% (OK) - 81% (DC)
range(turnout_hisp$turnout_est)
ggplot(turnout_hisp) +
  geom_col(
    aes(
      x = fct_reorder(STATE, turnout_est),
      y = turnout_est * 100
    )
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )  +
  labs(
    title = "Calculated Estimate of Voter Turnout",
    subtitle = "For Hispanic/Latino Voters the November 2020 Election",
    x = "State",
    y = "Voter turnout (%)"
  )
