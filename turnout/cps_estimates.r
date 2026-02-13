############################### SETUP ##################################

# Libraries
library(tidyverse)
library(ipumsr)
library(forcats)

# Options
setwd("C:/Users/baile/Desktop/asi/turnout")
options(tibble.width = Inf)

# Load data
fips_conv <- read_csv("fips_name_abbr.csv") # https://census.gov
pums <- read_ipums_ddi("cps_00005.xml") %>% read_ipums_micro() # https://cps.ipums.org/cps-action/data_requests/download
turnout_actual <- read_csv("turnout_expanded.csv") # https://docs.google.com/spreadsheets/d/1h_2pR1pq8s_I5buZ5agXS9q1vLziECztN2uWeR6Czo0/edit

############################ CALCULATIONS ##############################

# Clean data
pums_clean <- filter(pums, VOTED != 99) %>%
  left_join(
    fips_conv,
    by = c("STATEFIP" = "fips")
  ) %>%
  mutate(
    VOTED = as.logical(VOTED == 2),
    HISPAN = as.logical(HISPAN != 0),
    STATE = name
  ) %>%
  select(WTFINL, STATE, AGE, HISPAN, VOTED)

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
  ) %>%
  arrange(desc(turnout_est))

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

# Error adjustment (proposed)
turnout_young <- left_join(
  turnout_young,
  mutate(
    comp,
    state = state,
    error = comp$elig_turnout - comp$turnout_est,
    .keep = "none"
  ),
  by = c("STATE" = "state")
) %>%
  mutate(
    turnout_adj = turnout_est + error
  ) %>%
  select(-error)

turnout_hisp <- left_join(
  turnout_hisp,
  mutate(
    comp,
    state = state,
    error = comp$elig_turnout - comp$turnout_est,
    .keep = "none"
  ),
  by = c("STATE" = "state")
) %>%
  mutate(
    turnout_adj = turnout_est + error
  ) %>%
  select(-error)

############################### PLOTS ##################################

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

# VEP error - mean 2%
mean((comp$elig_pop - comp$vep) / comp$elig_pop)
ggplot(comp) +
  geom_col(
    aes(
      x = state,
      y = 100 * (comp$elig_pop - vep) / comp$elig_pop
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

# Raw turnout - range: 55% (AR) to 84% (DC)
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
      x = fct_reorder(STATE, turnout_adj),
      y = turnout_adj * 100
    ),
    alpha = 0.4
  ) +
  geom_point(
    aes(
      x = fct_reorder(STATE, turnout_adj),
      y = turnout_est * 100
    ),
    col = "black",
    pch = 20
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
    y = "Voter turnout (%)",
    caption = "• = Raw subgroup turnout\n▨ = Subgroup turnout adjusted for overall statewide error"
  )

# Hispanic turnout - range: 30% (OK) - 81% (DC)
range(turnout_hisp$turnout_est)
ggplot(turnout_hisp) +
  geom_col(
    aes(
      x = fct_reorder(STATE, turnout_adj),
      y = turnout_adj * 100
    ),
    alpha = 0.4
  ) +
  geom_point(
    aes(
      x = fct_reorder(STATE, turnout_adj),
      y = turnout_est * 100
    ),
    col = "black",
    pch = 20
  ) +
  theme(
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    )
  )  +
  labs(
    title = "Adjusted Estimate of Voter Turnout",
    subtitle = "For Hispanic/Latino Voters the November 2020 Election",
    x = "State",
    y = "Voter turnout (%)",
    caption = "• = Raw subgroup turnout\n▨ = Subgroup turnout adjusted for overall statewide error"
  )
