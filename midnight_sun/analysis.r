# File name: analysis.r
# Project:   midnight_sun
# Purpose:   Do some fun tricks with AK CVR data. Not especially organized.
# Author:    Bailey Inglish
# Source(s): general_elections_2024.csv <- Alaska Elections Division

## Setup
# Libaries
library(tidyverse)

# Directory
setwd("midnight_sun")

# Files
ge <- read_csv("general_election_2024.csv")

## Scripts
# Crossover voting
ge_c <- filter(ge, is.element(Pres1Id, c(542, 543)), !is.na(BM2Name)) %>%
  mutate(crossover = (Pres1Id == 542 & BM2Name == "YES") | (Pres1Id == 543 & BM2Name == "NO"))

tot_cross_prop <- sum(ge_c$crossover) / nrow(ge_c)
blank_prop <- sum(ge_c$BM2Name == "[Blank]") / nrow(ge_c)
dem_prop <- sum(ge_c$Pres1Id == 542 & ge_c$crossover == TRUE) / sum(ge_c$Pres1Id == 542)
rep_prop <- sum(ge_c$Pres1Id == 543 & ge_c$crossover == TRUE) / sum(ge_c$Pres1Id == 543)

ge_tp <- filter(ge, is.element(Pres1Id, c(554, 540, 547, 545, 546, 541)), !is.na(BM2Name))
third_party_prop <- sum(ge_tp$BM1Name == "NO") / nrow(ge_tp)

# Intra-party support for rcv
rep_rcv_maintain <- nrow(filter(ge, Pres1Id == 543, BM2Name == "NO")) / nrow(filter(ge, Pres1Id == 543))
dem_rcv_maintain <- nrow(filter(ge, Pres1Id == 542, BM2Name == "NO")) / nrow(filter(ge, Pres1Id == 542))

# Senate district review
cross_pct <- filter(
  ge_c,
  !is.na(PrecinctName)
) %>%
  group_by(
    PrecinctName
  ) %>%
  summarize(
    prop_cross = sum(crossover) / n()
  ) %>%
  arrange(
    desc(prop_cross)
  )

2 <- filter(
  ge,
  PrecinctExternalId == "40-006"
) %>%
  select(
    starts_with("Pres"),
    starts_with("BM2")
  )
