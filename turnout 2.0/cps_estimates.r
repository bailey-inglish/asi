library(tidyverse)
library(ipumsr)
library(forcats)

setwd("C:/Users/baile/Desktop/asi/turnout 2.0")
options(tibble.width = Inf)

fips_conv <- read_csv("fips_name_abbr.csv")
pums <- read_ipums_ddi("cps_00004.xml") %>% read_ipums_micro()
turnout_actual <- read_csv("turnout_expanded.csv")

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

comp <- left_join(turnout_actual, turnout_ovr, by = c("state" = "STATE"))

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
  )

