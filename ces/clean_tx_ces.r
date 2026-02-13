# Cumulative harmonized dataset for Texas 2018-2023
# Still considering whether it's just worth it to use individual 18-23 datasets
# Guide: https://dataverse.harvard.edu/file.xhtml?fileId=10277001&version=9.0
library(tidyverse)
options(dplyr.width = Inf)

ces <- read_rds("ces/cumulative_2006-2023.rds")

tx_ces <- filter(
  ces,
  state == "Texas",
  year >= 2018
)

tx_ces <- mutate(tx_ces, tookpost = as.logical(tookpost))

write_csv(tx_ces, "ces.csv")
