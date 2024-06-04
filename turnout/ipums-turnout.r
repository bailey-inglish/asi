library(tidyverse)
library(ipumsr)
library(forcats)

setwd("C:/Users/baile/Desktop/asi/turnout")

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)

head(data)

pums <- filter(data, !is.na(VOTED))

fips_conv <- read_csv("fips_name_abbr.csv")

pums_easy <- filter(pums, (VOTED == 1 | VOTED == 2)) %>%
  select(WTFINL, STATEFIP, AGE, HISPAN, VOTED) %>%
  mutate(
    VOTED = as.logical(VOTED - 1),
    HISPAN = as.logical(HISPAN != 0)
  )

tx_pums <- filter(pums_easy, STATEFIP == 48)

turnout_young <- filter(pums_easy, AGE >= 18 & AGE <= 24) %>%
  group_by(STATEFIP) %>%
  summarize(turnout = sum(VOTED * WTFINL) / sum(WTFINL)) %>%
  arrange(desc(turnout))

turnout_hisp <- filter(pums_easy, HISPAN == TRUE) %>%
  group_by(STATEFIP) %>%
  summarize(turnout = sum(VOTED * WTFINL) / sum(WTFINL)) %>%
  arrange(desc(turnout))

turnout_hisp$state <- fips_conv$name[turnout_hisp$STATEFIP == fips_conv$fips]

ggplot(turnout_young) +
  geom_col(
    aes(
      x = fct_reorder(, turnout),
      y = turnout
    )
  )
