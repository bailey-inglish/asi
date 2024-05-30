library(tidyverse)

setwd("C:/Users/baile/Desktop/asi/random cps testing")

age <- read_csv("age.csv") # https://data.census.gov/table/ACSST5Y2020.S0101?q=age%20by%20state&moe=false&tp=true
race <- read_csv("race.csv") # https://data.census.gov/table/DECENNIALCD1182020.P9?q=race%20by%20state&tp=false
turnout <- read_csv("turnout.csv") # https://docs.google.com/spreadsheets/d/1h_2pR1pq8s_I5buZ5agXS9q1vLziECztN2uWeR6Czo0/edit#gid=2030096602

age$label[3 + 13 * (0:51)] <- age$label[1 + 13 * (0:51)]
age <- age[3 + 13 * (0:51), ]
age$young <- as.integer(str_remove_all(age$young, ","))
age$total <- as.integer(str_remove_all(age$total, ","))
age$prop_young <- age$young / age$total

race$state[2 * (1:52)] <- race$state[2 * (1:52) - 1]
race <- race[2 * (1:52), ]
race$prop_hisp <- race$hisp / race$total

demo <- tibble(
  state = race$state,
  prop_young = age$prop_young,
  prop_hisp = race$prop_hisp,
)
demo <- filter(demo, state != "Puerto Rico")
demo <- inner_join(demo, turnout, by = "state")

summary(lm(turnout ~ prop_young + prop_hisp, data = demo))

ggplot(demo) +
  geom_point(
    aes(
      x = prop_young,
      y = prop_hisp,
      size = turnout
    ),
    pch = 20
  ) +
  scale_x_reverse() +
  scale_y_reverse() +
  labs(
    title = "Voter turnout by age and ethnicity",
    subtitle = "In the 2020 election by state",
    x = "Proportion of adults in state who are aged 18-24",
    y = "Proportion of residents in state who are Hispanic or Latino",
    size = "Proportion of eligible\nadults who voted"
  )

ggplot(demo, aes(x = prop_young, y = turnout)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Voter turnout vs. age",
    subtitle = "In the 2020 election by state",
    x = "Proportion of adults in state who are aged 18-24",
    y = "Proportion of eligible\nadults who voted"
  )

ggplot(demo, aes(x = prop_hisp, y = turnout)) +
  geom_point() +
  geom_smooth() +
  labs(
    title = "Voter turnout vs. ethnicity",
    subtitle = "In the 2020 election by state",
    x = "Proportion of residents in state who are Hispanic or Latino",
    y = "Proportion of eligible\nadults who voted"
  )
