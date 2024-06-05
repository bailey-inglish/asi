# issue: future data?
library(tidyverse)
library(ipumsr)
library(forcats)

setwd("C:/Users/baile/Desktop/asi/turnout")

fips_conv <- read_csv("fips_name_abbr.csv")

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)

options(tibble.width = Inf)
head(data)

pums <- filter(data, !is.na(VOTED))

pums_easy <- filter(pums, (VOTED == 1 | VOTED == 2)) %>%
  select(WTFINL, STATEFIP, AGE, HISPAN, VOTED) %>%
  mutate(
    VOTED = as.logical(VOTED == 2),
    HISPAN = as.logical(HISPAN != 0)
  )

tx_pums <- filter(pums_easy, STATEFIP == 48)

turnout_young <- filter(pums_easy, AGE >= 18 & AGE <= 24) %>%
  group_by(STATEFIP) %>%
  summarize(
    turnout = sum(VOTED * WTFINL) / sum(WTFINL),
    pop = sum(WTFINL)
  ) %>%
  arrange(desc(turnout))

turnout_hisp <- filter(pums_easy, HISPAN == TRUE) %>%
  group_by(STATEFIP) %>%
  summarize(
    turnout = sum(VOTED * WTFINL) / sum(WTFINL),
    pop = sum(WTFINL)
  ) %>%
  arrange(desc(turnout))

print(turnout_hisp, n = 51) # TX is 21st place
print(turnout_young, n = 51) # TX is 40th place

turnout_calc <- group_by(pums_easy, STATEFIP) %>%
  summarize(
    est_turnout = sum(VOTED * WTFINL) / sum(WTFINL),
    est_pop = sum(WTFINL)
  )

turnout_actual <- read_csv("turnout_expanded.csv")
turnout_actual <- left_join(turnout_actual, fips_conv, by = c("state" = "name"))

turnout_comp <- left_join(turnout_actual, turnout_calc, by = c("fips" = "STATEFIP"))

ggplot(turnout_comp) +
  geom_col(
    aes(
      x = fct_reorder(
        state,
        turnout - est_turnout
      ),
      y = (turnout - est_turnout) * 100
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
    title = "Error in actual vs. CPS turnout",
    subtitle = "For the November 2020 Election",
    x = "State",
    y = "Difference between actual\nand reported turnout (% pts)",
    caption = "Note: calculated turnout is out of voting age population;\nactual turnout uses voting eligible population"
  )

ggplot(turnout_comp) +
  geom_col(
    aes(
      x = fct_reorder(
        state,
        est_turnout
      ),
      y = est_turnout * 100
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
    title = "Estimated turnout using CPS",
    subtitle = "For the November 2020 Election",
    x = "State",
    y = "Calculated turnout (%)",
    caption = "Note: calculated turnout is out of voting age population"
  )

table(pums$VOTED)

raw_turnout_act <- mutate(
  read_csv("turnout_expanded.csv"),
  basic_turnout = ballots / voting_age_pop
)