# Setup
library(tidyverse)
library(haven)
library(ipumsr)
setwd("cps_cleaning")

# Import data
cps <- read_ipums_ddi("raw_data/cps_00010.xml") %>% read_ipums_micro()

# Uses upper bound year to approx immigrant years in the US (note that higher)
# values are more subject to varition, see codebook.
cps$min_immi_years_in_us <- rep(NA, nrow(cps))
cps[cps$YRIMMIG != 0, "min_immi_years_in_us"] <- cps[cps$YRIMMIG != 0, "YEAR"] - cps[cps$YRIMMIG != 0, "YRIMMIG"]

# Hispanic simple recoding
cps$is_hispanic <- cps$HISPAN != 0
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$race_cluster <- rep("multiracial", nrow(cps))
cps$race_cluster[cps$RACE == 100] <- "white"
cps$race_cluster[cps$RACE == 200] <- "black"
cps$race_cluster[cps$RACE == 300] <- "native american"
cps$race_cluster[cps$RACE == 651] <- "asian"

# Employment simplifying recode
cps$employment <- rep(NA, nrow(cps))
cps$employment[cps$EMPSTAT == 1] <- "armed forces"
cps$employment[cps$EMPSTAT == 10 | cps$EMPSTAT == 12] <- "employed"
cps$employment[cps$EMPSTAT == 21 | cps$EMPSTAT == 22] <- "unemployed"
cps$employment[cps$EMPSTAT > 30] <- "not in labor force"

# Work status simplifying recode
cps$work_status <- rep(NA, nrow(cps))
cps$work_status[is.element(cps$WKSTAT, c(11, 14, 15))] <- "full-time"
cps$work_status[is.element(cps$WKSTAT, c(12, 21, 22, 41))] <- "part-time"
cps$work_status[is.element(cps$WKSTAT, c(13, 41, 50, 60))] <- "not working"

# Large block of NA recoding
cps$COUNTY[cps$COUNTY == 0] <- NA
cps$METFIPS[cps$METFIPS == 99998] <- NA
cps$METRO[cps$METRO == 0] <- NA
cps$CBSASZ[cps$CBSASZ == 0] <- NA
cps$MARST[cps$MARST == 9] <- NA
cps$CITIZEN[cps$CITIZEN == 9] <- NA

cps <- mutate(cps, is_registered = VOTED == 2 | VOREG == 2)

# Write final outputs
write_csv(cps, "final_data/cps_clean_ipums_2008-2022.csv")
write_dta(cps, "final_data/cps_clean_ipums_2008-2022.dta")
