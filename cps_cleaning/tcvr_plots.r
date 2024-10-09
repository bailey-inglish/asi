## Code for ggplot plots used in the TCVR
# Bailey Inglish
library(tidyverse)
library(forcats)

cps <- read_csv("cps_cleaning/final_data/cps_clean_ipums_2008-2022.csv")

## Section 1: State of Texas Turnout
# TX vs. US
turn_tx <- filter(cps, STATEFIP == 48) %>%
  group_by(YEAR) %>%
  summarize(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  ) %>%
  cbind("region" = rep("TX", 8))

turn_us <- cps %>%
  group_by(YEAR) %>%
  summarize(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  ) %>%
  cbind("region" = rep("US", 8))

turn_comb <- rows_append(turn_tx, turn_us)
turn_comb$elec_type <- rep("Presidential", 8)
turn_comb$elec_type[is.element(turn_comb$YEAR, c(2010, 2014, 2018, 2022))] <- "Midterm"

ggplot(turn_comb) +
  geom_line(
    aes(
      x = YEAR,
      y = turnout * 100,
      col = region,
      lty = elec_type
    )
  ) +
  labs(
    title = "Voter Turnout: Texas vs. US",
    subtitle = "Presidential and Midterm Elections 2008-2022",
    x = "Year",
    y = "VEP Turnout (%)",
    col = "Region",
    lty = "Election Type"
  ) +
  theme_bw() +
  scale_color_manual(values = c("#bf5700", "black"))

# Census-designated regional comparison

# Conference comparison