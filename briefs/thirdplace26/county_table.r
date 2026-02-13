## File:   county_table.r
## Desc:   Tabulates CES for variables of interest and aggregates TPI by countyfips. Then
##         joins the tables together.
## Output: tpi_ces_by_county.csv

# Libraries
setwd("briefs/thirdplace26")
library(tidyverse)
library(haven)

# Load data
ces <- read_dta("raw_data/ces.dta") %>% filter(year == 2024)
tpi <- read_csv("raw_data/third_place.csv")
zcta_conv <- read_delim("raw_data/zcta_tract_conv.txt", delim = "|")

# Tabulate CES by countyfips for variables of interest (used vvweight)
# Vars of interest:
  # validated turnout (vv_turnout_gvm) | 1 = voted, 2 = did not vote,
  #      3 = no record
  # partisanship (pid3) | 1 = dem, 2 = rep, 3 = ind, 4 = other, 5 = not sure
  # news interest (newsint) | 1 = Most of the time, 2 = Some of time,
  #      3 = Only now and then, 4 = Hardly at all, 7 = Don't Know
# Transformed vars:
  # turnout: # in each zcta / # of 1s and 2s [float] (exclude no record)
  # partisanship: % dem > % rep [bool] (exclude others and not sure)
  # news interest: % high (1-2) [float] (exclude don't know)
# Geo vars: countyfips, state
ces_by_county <- ces %>%
  filter(!is.na(county_fips)) %>%
  group_by(state, county_fips) %>%
  summarize(
    turnout = sum(vvweight * (vv_turnout_gvm == 1), na.rm = TRUE) /
      sum(vvweight * (vv_turnout_gvm %in% c(1, 2)), na.rm = TRUE),
    pct_dem = sum(vvweight * (pid3 == 1), na.rm = TRUE) /
      sum(vvweight * (pid3 %in% c(1, 2)), na.rm = TRUE),
    pct_rep = sum(vvweight * (pid3 == 2), na.rm = TRUE) /
      sum(vvweight * (pid3 %in% c(1, 2)), na.rm = TRUE),
    news_interest = sum(vvweight * (newsint %in% c(1, 2)), na.rm = TRUE) /
      sum(vvweight * (newsint %in% c(1, 2, 3, 4)), na.rm = TRUE)
  ) %>%
  select(
    county_fips = county_fips,
    turnout = turnout,
    prop_dem = pct_dem,
    prop_rep = pct_rep,
    news_interest = news_interest
  )

# Aggregate TPI by county (first five digits of geoid), summing numeric vars except income, state_fips, pct_bachelors, percentile
num_cols <- tpi %>% select(where(is.numeric)) %>% names()
exclude <- c("geoid", "state_fips", "median_income", "pct_bachelors", "third_place_percentile")
sum_cols <- setdiff(num_cols, exclude)

tpi_by_county <- tpi %>%
  mutate(geoid_str = sprintf("%.0f", geoid),
         county = substr(geoid_str, 1, 5)) %>%
  group_by(county) %>%
  summarise(
    across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
    state_fips = first(state_fips),
    .groups = "drop"
  ) %>%
  rename(county_fips = county)

# Join CES and TPI by countyfips
tpi_ces_by_county <- tpi_by_county %>%
  left_join(ces_by_county, by = "county_fips")

# Write output
write_csv(tpi_ces_by_county, "final_data/tpi_ces_by_county.csv")
