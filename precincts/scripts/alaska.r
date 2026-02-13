# File name: alaska.r
# Purpose:   Given the 2024 election returns for Alaska, evaluate the
#            relationships between presidential preference and support for
#            ranked-choice voting.
# Author:    Bailey Inglish

# Libraries and files
library(tidyverse)
library(kableExtra)

setwd("precincts")

ak <- read_csv("returns/alaska.csv") # Source: AK Elections Division 2024 Results

# Reframe the data from ak -> ak2 (creative name I know :P)
ak2 <- pivot_wider(
  ak,
  names_from = cand_party,
  id_cols = starts_with("pct") | starts_with("reg"),
  values_from = votes
) %>%
  filter(!is.na(YES)) %>%
  mutate(
    total_pres = DEM + RFK + LIB + ASP + GRN + CON + REP + AUR,
    total_rcv = YES + NO,
    prop_dem = DEM / total_pres,
    prop_rep = REP / total_pres,
    prop_rfk = RFK / total_pres,
    prop_yes = YES / total_rcv,
    prop_no = NO / total_rcv,
    exception = (prop_yes > 0.6 & prop_dem > 0.6) | (prop_no > 0.6 & prop_rep > 0.6)
  )

write_csv(ak2, "products/ak2.csv")

# Print out pretty tables of the results for each precint
for (i in 1:6) {
  filter(ak2, exception == TRUE) %>%
    arrange(desc(prop_yes)) %>%
    mutate(
      pct_name,
      reg_vot,
      total_pres,
      total_rcv,
      prop_dem = round(prop_dem * 100, 1),
      prop_rep = round(prop_rep * 100, 1),
      prop_keep_rcv = round(prop_no * 100, 1),
      prop_axe_rcv = round(prop_yes * 100, 1),
      .keep = "none"
    ) %>%
    slice(i) %>%
    kbl(
      col.names = c(
        "Precinct",
        "Reg. Voters",
        "Pres. Votes",
        "RCV Votes",
        "% Dem",
        "% Rep",
        "% For RCV",
        "% Against RCV"
      )
    ) %>%
    kable_styling() %>%
    print()
}

# Pretty pictures of the overall trends!
ggplot(
  ak2,
  aes(
    x = 100 * prop_dem,
    y = 100 * prop_yes,
    pch = exception
  )
) +
  geom_point(col = "blue") +
  geom_smooth(col = "darkblue") +
  labs(
    title = "Democratic Opposition to RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Harris/Walz",
    y = "% Abolish RCV"
  )

ggplot(
  ak2,
  aes(
    x = 100 * prop_rep,
    y = 100 * prop_no
  )
) +
  geom_point(col = "red") +
  geom_smooth(col = "darkred") +
  labs(
    title = "Republican Support for RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Trump/Vance",
    y = "% Maintain RCV"
  )

ggplot(
  ak2,
  aes(
    x = 100 * prop_dem,
    y = 100 * prop_no
  )
) +
  geom_point(col = "blue") +
  geom_smooth(col = "darkblue") +
  labs(
    title = "Democratic Support for RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Harris/Walz",
    y = "% Maintain RCV"
  )

ggplot(
  ak2,
  aes(
    x = 100 * prop_rep,
    y = 100 * prop_yes
  )
) +
  geom_point(col = "red") +
  geom_smooth(col = "darkred") +
  labs(
    title = "Republican Opposition to RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Trump/Vance",
    y = "% Abolish RCV"
  )

ggplot(
  ak2,
  aes(
    x = 100 * prop_rfk,
    y = 100 * prop_yes
  )
) +
  geom_point(col = "purple") +
  geom_smooth(col = "purple4") +
  labs(
    title = "RFK Opposition to RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Kennedy/Shanahan",
    y = "% Abolish RCV"
  )

ggplot(
  ak2,
  aes(
    x = 100 * prop_rfk,
    y = 100 * prop_no
  )
) +
  geom_point(col = "purple") +
  geom_smooth(col = "purple4") +
  labs(
    title = "RFK Support for RCV",
    subtitle = "Alaska General Election 2024",
    x = "% Kennedy/Shanahan",
    y = "% Maintain RCV",
    note = "Each dot represents a precinct of n = 0-5851 registered voters"
  )
