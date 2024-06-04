library(tidyverse)
library(ipumsr)
library(forcats)

setwd("C:/Users/baile/Desktop/asi/turnout")

fips_conv <- read_csv("fips_name_abbr.csv")

ddi <- read_ipums_ddi("cps_00001.xml")
data <- read_ipums_micro(ddi)

head(data)

pums <- filter(data, !is.na(VOTED))

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

print(turnout_hisp, n = 51) # TX is 21st place
print(turnout_young, n = 51) # TX is 40th place

ggplot(turnout_young) +
  geom_col(
    aes(
      x = STATEFIP,
      y = turnout
    )
  )
