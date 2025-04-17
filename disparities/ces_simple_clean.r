# File name: ces_simple_clean.r
# Purpose:   Using the Cooperative Election Survey for more robust cumulative
#            content.
# Author:    Bailey Inglish

# Setup
library(tidyverse)

# Data
setwd("disparities")
ces <- read_rds("raw_data/cumulative_2006-2023.rds")
