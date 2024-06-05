# Libraries
library(tidyverse)
library(ipumsr)
library(forcats)

# Options
setwd("C:/Users/baile/Desktop/asi/turnout 2.0")
options(tibble.width = Inf)

# Load data
fips_conv <- read_csv("fips_name_abbr.csv")
pums <- read_ipums_ddi("cps_00004.xml") %>% read_ipums_micro()
turnout_actual <- read_csv("turnout_expanded.csv")

# Clean data
pums_clean <- filter(pums, (VOTED == 1 | VOTED == 2)) %>%
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
    turnout_est = sum(VOTED * WTFINL) / sum(WTFINL),
    pop = sum(WTFINL),
    votes = sum(VOTED * WTFINL)
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

# VAP error
ggplot(comp) +
  geom_col(
    aes(
      x = state,
      y = 100 * (voting_age_pop - pop) / voting_age_pop
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

# Ballot error
ggplot(comp) +
  geom_col(
    aes(
      x = state,
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
    title = "Error in ballot/vote count",
    subtitle = "For the November 2020 election",
    x = "State",
    y = "(Actual - calculated) as percent of actual (%)"
  )

# Raw turnout
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
    y = "Voter turnout (%)",
    caption = "Note: currently as a % of voting age population rather than VEP :("
  )

# Turnout error
ggplot(comp) +
  geom_col(
    aes(
      x = fct_reorder(state, (ballots / voting_age_pop) - turnout_est),
      y = 100 * ((ballots / voting_age_pop) - turnout_est)
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
    y = "Actual - calculated (% pts)",
    caption = "Note: Only using VAP rather than VEP for now :("
  )

# Young voters
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
    y = "Voter turnout (%)",
    caption = "Note: currently as a % of voting age population rather than VEP :("
  )

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
    y = "Voter turnout (%)",
    caption = "Note: currently as a % of voting age population rather than VEP :("
  )
