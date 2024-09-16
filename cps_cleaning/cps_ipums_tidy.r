# Setup options(dplyr.width = Inf)
library(tidyverse)
library(haven)
library(ipumsr)
setwd("cps_cleaning")

# Beautiful function that lets me use haven in ggplot <3
label_x <- function(p) {
  b <- ggplot_build(p)
  x <- b$plot$data[[b$plot$labels$x]]

  p + scale_x_continuous(
    attributes(x)$label, 
    breaks = attributes(x)$labels, 
    labels = names(attributes(x)$labels)
  )
}

# Angle lbl- theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Import data
cps_raw <- read_ipums_ddi("raw_data/cps_00009.xml") %>% read_ipums_micro()
# https://www.test.census.gov/data/tables/time-series/demo/income-poverty/historical-income-households.html
income_cutoffs <- read_csv("income_cutoffs_08-23.csv")

cps <- cps_raw

quintize_income <- function(cps) {
  cps$income_cluster <- rep(NA, nrow(cps))
  for (year in 2008 + 2 * 0:7) {
    if (is.element(year, c(2008, 2010, 2012, 2014))) {
      cps[cps$FAMINC < 600 & cps$YEAR == year, "income_cluster"] <- "lowest income"
      cps[cps$FAMINC >= 600 & cps$FAMINC <= 730 & cps$YEAR == year, "income_cluster"] <- "lower income"
      cps[cps$FAMINC >= 740 & cps$FAMINC <= 830 & cps$YEAR == year, "income_cluster"] <- "middle income"
      cps[cps$FAMINC == 841 & cps$YEAR == year, "income_cluster"] <- "higher income"
      cps[cps$FAMINC > 841 & cps$FAMINC < 900 & cps$YEAR == year, "income_cluster"] <- "higest income"
    } else if (year == 2016) {
      cps[cps$FAMINC < 710 & cps$YEAR == year, "income_cluster"] <- "lowest income"
      cps[cps$FAMINC >= 710 & cps$FAMINC <= 740 & cps$YEAR == year, "income_cluster"] <- "lower income"
      cps[cps$FAMINC >= 820 & cps$FAMINC <= 830 & cps$YEAR == year, "income_cluster"] <- "middle income"
      cps[cps$FAMINC == 841 & cps$YEAR == year, "income_cluster"] <- "higher income"
      cps[cps$FAMINC > 841 & cps$FAMINC < 900 & cps$YEAR == year, "income_cluster"] <- "higest income"
    }
    else if (is.element(year, c(2018, 2020))) {
      cps[cps$FAMINC < 710 & cps$YEAR == year, "income_cluster"] <- "lowest income"
      cps[cps$FAMINC >= 710 & cps$FAMINC <= 740 & cps$YEAR == year, "income_cluster"] <- "lower income"
      cps[cps$FAMINC >= 820 & cps$FAMINC <= 830 & cps$YEAR == year, "income_cluster"] <- "middle income"
      cps[cps$FAMINC >= 841 & cps$FAMINC <= 842 & cps$YEAR == year, "income_cluster"] <- "higher income"
      cps[cps$FAMINC > 842 & cps$FAMINC < 900 & cps$YEAR == year, "income_cluster"] <- "higest income"
    }
    else if (year == 2022) {
      cps[cps$FAMINC < 720 & cps$YEAR == year, "income_cluster"] <- "lowest income"
      cps[cps$FAMINC >= 720 & cps$FAMINC <= 820 & cps$YEAR == year, "income_cluster"] <- "lower income"
      cps[cps$FAMINC >= 830 & cps$FAMINC <= 841 & cps$YEAR == year, "income_cluster"] <- "middle income"
      cps[cps$FAMINC == 842 & cps$YEAR == year, "income_cluster"] <- "higher income"
      cps[cps$FAMINC > 842 & cps$FAMINC < 900 & cps$YEAR == year, "income_cluster"] <- "higest income"
    }
  }
  return(cps)
}

cps <- quintize_income(cps)

qcps %>% group_by(income_cluster) %>% summarize(avg_reg = mean(VOTED ==2 | VOREG == 2)) %>%
  ggplot() +
    geom_col(
      aes(
        y = avg_reg,
        x = income_cluster
      )
    )

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
cps$employment[cps$EMPSTAT == 10 | cps$EMPSTAT == 12] <- "unemployed"
cps$employment[cps$EMPSTAT > 30] <- "not in labor force"

# Work status simplifying recode

# Massive block of NA recoding
cps$COUNTY[cps$COUNTY == 0] <- NA
cps$METFIPS[cps$METFIPS == 99998] <- NA
cps$METRO[cps$METRO == 0] <- NA
cps$CBSASZ[cps$CBSASZ == 0] <- NA
cps$MARST[cps$MARST == 9] <- NA
cps$CITIZEN[cps$CITIZEN == 9] <- NA
