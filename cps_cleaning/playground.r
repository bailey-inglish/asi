library(tidyverse)
library(forcats)
library(RColorBrewer)

cps <- read_csv("cps_cleaning/final_data/cps_clean_ipums_2008-2022.csv")

filter(cps, is_hispanic == "Hispanic/Latino") %>%
  summarize(
    voter_ct_ig = sum((VOTED == 2) * adj_vosuppwt)
  )

cps_c <- filter(cps, !is.na(income_range))
cps_c$old <- c("Less than 40", "Over 40")[(cps_c$AGE > 40) + 1]

income_power_groups_age <- cps_c %>%
  group_by(income_range, old, YEAR) %>%
  reframe(
    vep_in_group = round(sum(adj_vosuppwt), 2),
    voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
  ) %>%
  left_join(
    group_by(cps_c, old, YEAR) %>%
      reframe(
        total_vep = round(sum(adj_vosuppwt), 2),
        total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
      ),
    by = c("YEAR", "old")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
    pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
    vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
  )

ggplot(
  income_power_groups_age,
  aes(
    y = vri,
    x = YEAR,
    col = fct_reorder(income_range, desc(vri))
  )
) +
  geom_line() +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  facet_grid(
    ~old,
  ) +
  labs(
    title = paste("Voting Representation by Age and Income"),
    x = "Year",
    y = "Voter Representation Index (%)",
    col = ""
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = 2008 + (0:7 * 2)
  ) +
  scale_colour_viridis_d(begin = 0, end = 0.85) +
  scale_y_continuous(
    limits = c(-65, 65),
    breaks = -10:10 * 10
  )

income_power_groups_age2 <- cps_c %>%
  group_by(income_range, old, YEAR) %>%
  reframe(
    vep_in_group = round(sum(adj_vosuppwt), 2),
    voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
  ) %>%
  left_join(
    group_by(cps_c, YEAR) %>%
      reframe(
        total_vep = round(sum(adj_vosuppwt), 2),
        total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
      ),
    by = c("YEAR")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
    pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
    vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
  )

ggplot(
  income_power_groups_age2,
  aes(
    y = vri,
    x = YEAR,
    col = fct_reorder(income_range, desc(vri)),
    lty = old
  )
) +
  geom_line() +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  labs(
    title = paste("Voting Representation by Age and Income"),
    x = "Year",
    y = "Voter Representation Index (%)",
    col = "",
    lty = ""
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = 2008 + (0:7 * 2)
  ) +
  scale_colour_viridis_d(begin = 0, end = 0.85) +
  scale_y_continuous(
    limits = c(-65, 65),
    breaks = -10:10 * 10
  )

cps_c <- filter(cps, !is.na(income_range))
cps_c$high_earner <- c("Less than 50k", "Over 50k")[(cps_c$FAMINC > 740) + 1]

earner_power_groups <- cps_c %>%
  group_by(high_earner, YEAR) %>%
  reframe(
    vep_in_group = round(sum(adj_vosuppwt), 2),
    voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
  ) %>%
  left_join(
    group_by(cps_c, YEAR) %>%
      reframe(
        total_vep = round(sum(adj_vosuppwt), 2),
        total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
      ),
    by = c("YEAR")
  ) %>%
  mutate(
    pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
    pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
    vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
  )
