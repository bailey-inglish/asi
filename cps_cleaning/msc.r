library(tidyverse)
library(forcats)
library(RColorBrewer)
library(kableExtra)

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
  ) %>%
  filter(YEAR >= 2020)

earner_power_groups_tx <- filter(cps_c, STATEFIP == 48) %>%
  group_by(high_earner, YEAR) %>%
  reframe(
    vep_in_group = round(sum(adj_vosuppwt), 2),
    voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
  ) %>%
  left_join(
    group_by(filter(cps_c, STATEFIP == 48), YEAR) %>%
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
  ) %>%
  filter(YEAR >= 2020)

ut_tab <- filter(cps, STATEFIP == 49) %>%
  group_by(age_cluster, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
  ) %>%
  left_join(
    group_by(filter(cps, STATEFIP == 49), YEAR) %>%
      summarize(
        total_vep = sum(adj_vosuppwt),
        total_voters = sum(adj_vosuppwt * (VOTED == 2))
      ),
    by = "YEAR"
  ) %>%
  mutate(
    pct_of_ve_population = 100 * vep_in_group / total_vep,
    pct_of_electorate = 100 * voters_in_group / total_voters,
    vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
  )
kbl(ut_tab) %>% kable_styling()

sum(filter(cps, VOTED == 2, YEAR == 2020, STATEFIP == 48)$adj_vosuppwt) / sum(filter(cps, VOTED == 2, YEAR == 2020)$adj_vosuppwt)
sum(filter(cps, YEAR == 2020, STATEFIP == 48)$adj_vosuppwt) / sum(filter(cps, YEAR == 2020)$adj_vosuppwt)

vars_of_interest <- tibble(
  var = c("age_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "metro_status", "eth_race_comb_cluster", "sex_name"),
  name = c("Age Cluster", "Length of Residence", "Educational Attainment", "Income Range", "Metro Status", "Race/Ethnicity", "Sex")
)

for (gvar in vars_of_interest$var) {
  cps_c <- filter(cps, !is.na(!!sym(gvar)))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

  rand <- sample(unique(cps$STATEFIP), 1)
  print(unique(cps$state_name[cps$STATEFIP == rand]))
  rand_tab <- filter(cps_c, STATEFIP == rand) %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
    ) %>%
    left_join(
      group_by(filter(cps_c, STATEFIP == rand), YEAR) %>%
        summarize(
          total_vep = sum(adj_vosuppwt),
          total_voters = sum(adj_vosuppwt * (VOTED == 2))
        ),
      by = "YEAR"
    ) %>%
    mutate(
      pct_of_ve_population = 100 * vep_in_group / total_vep,
      pct_of_electorate = 100 * voters_in_group / total_voters,
      vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
    )
  print(rand_tab)

  tx_tab <- filter(cps_c, STATEFIP == 48) %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
    ) %>%
    left_join(
      group_by(filter(cps_c, STATEFIP == 48), YEAR) %>%
        summarize(
          total_vep = sum(adj_vosuppwt),
          total_voters = sum(adj_vosuppwt * (VOTED == 2))
        ),
      by = "YEAR"
    ) %>%
    mutate(
      pct_of_ve_population = 100 * vep_in_group / total_vep,
      pct_of_electorate = 100 * voters_in_group / total_voters,
      vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
    )
  print(tx_tab)
  us_tab <- cps_c %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
    ) %>%
    left_join(
      group_by(cps_c, YEAR) %>%
        summarize(
          total_vep = sum(adj_vosuppwt),
          total_voters = sum(adj_vosuppwt * (VOTED == 2))
        ),
      by = "YEAR"
    ) %>%
    mutate(
      pct_of_ve_population = 100 * vep_in_group / total_vep,
      pct_of_electorate = 100 * voters_in_group / total_voters,
      vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
    )
  print(us_tab)
}
