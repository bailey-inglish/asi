# File name: selected_plots.r
# Purpose:   Generates 2024-era plots for VRI and turnout for selected
#            characteristics of demographic groups.
# Author:    Bailey Inglish


library(tidyverse)
library(aod)
setwd("briefs/youth24")

fancy_cps <- read_csv("final_data/revised_cps_expanded_ipums_1982-2024.csv")
covi <- read_csv("raw_data/covi96-24.csv")

## Longitudinal analysis
# Line plots of VRI over time for each group/grouping variable
vars_of_interest <- tibble(
  var = c("age_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "metro_status", "eth_race_comb_cluster", "sex_name"),
  name = c("Age Cohort", "Length of Residence", "Educational Attainment", "Income Range", "Metro Status", "Race/Ethnicity", "Gender")
)

# Midterm flag
fancy_cps$elec_type <- c("Midterm", "Presidential")[(round(fancy_cps$YEAR / 4) == fancy_cps$YEAR / 4) + 1]

for (gvar in vars_of_interest$var) {
  cps_c <- filter(fancy_cps, !is.na(!!sym(gvar)))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

  us_tab <- cps_c %>%
    group_by(!!sym(gvar), YEAR, elec_type) %>%
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

  if (gvar == "income_range") {
    var_spec_min <- 2004
  } else {
    var_spec_min <- 1994
  }
  us_tab <- filter(us_tab, YEAR >= var_spec_min)

  p <- ggplot(
    us_tab,
    aes(
      y = vri,
      x = YEAR,
      col = fct_reorder(!!sym(gvar), vri)
    )
  ) +
    geom_line() +
    geom_point() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "right",
      panel.background = element_rect(fill = "grey85")
    ) +
    labs(
      title = paste("Voter Representation by", gname),
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
      breaks = 1994 + (0:15 * 2),
      limits = c(1994, 2024)
    ) +
    scale_colour_viridis_d(begin = 0, end = 0.85) +
    scale_y_continuous(
      limits = c(-65, 65),
      breaks = -10:10 * 10
    ) +
    facet_wrap(~elec_type)
  print(p)
  print(filter(us_tab, YEAR == 2024) %>% select(vri, starts_with("pct")), n = 100)
}

# Line plot of turnout by age group over time
for (loc in c("United States", "Texas")) {
  if (loc != "United States") {
    fancy_cps_c <- filter(fancy_cps, locality == loc)
  } else {
    fancy_cps_c <- fancy_cps
  }
  for (gvar in vars_of_interest$var) {
    cps_c <- filter(fancy_cps_c, !is.na(!!sym(gvar)))
    gname <- vars_of_interest$name[vars_of_interest$var == gvar]

    us_tab <- cps_c %>%
      group_by(!!sym(gvar), YEAR, elec_type) %>%
      reframe(
        vep_in_group = sum(adj_vosuppwt),
        voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
      ) %>%
      mutate(
        turnout = 100 * (voters_in_group / vep_in_group)
      )

    if (gvar == "income_range") {
      var_spec_min <- 2004
    } else {
      var_spec_min <- 1994
    }
    us_tab <- filter(us_tab, YEAR >= var_spec_min)

    p <- ggplot(
      us_tab,
      aes(
        y = turnout,
        x = YEAR,
        col = fct_reorder(!!sym(gvar), turnout)
      )
    ) +
      geom_line() +
      geom_point() +
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "right",
        panel.background = element_rect(fill = "grey85")
      ) +
      labs(
        title = paste("Voter Turnout by", gname),
        subtitle = loc,
        x = "Year",
        y = "Voter Turnout (%)",
        col = ""
      ) +
      geom_hline(
        yintercept = 0,
        col = "black",
        linewidth = 0.3,
        alpha = 0.5
      ) +
      scale_x_continuous(
        breaks = 1994 + (0:15 * 2),
        limits = c(1994, 2024)
      ) +
      scale_colour_viridis_d(begin = 0, end = 0.85) +
      scale_y_continuous(
        limits = c(0, 100),
        breaks = 0:10 * 10
      ) +
      facet_wrap(~elec_type)
    print(p)
  }
}

# Race x Age
race_by_age <- group_by(fancy_cps, eth_race_comb_cluster, age_cluster, YEAR, elec_type) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
  ) %>%
  mutate(
    turnout = 100 * (voters_in_group / vep_in_group)
  ) %>%
  filter(!is.na(eth_race_comb_cluster))

ggplot(
  race_by_age,
  aes(
    y = turnout,
    x = YEAR,
    col = fct_reorder(age_cluster, turnout)
  )
) +
  geom_line() +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "right",
    panel.background = element_rect(fill = "grey85")
  ) +
  labs(
    title = paste("Voter Turnout by Race and Age"),
    x = "Year",
    y = "Voter Turnout (%)",
    col = ""
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = 1994 + (0:15 * 2),
    limits = c(1994, 2024)
  ) +
  scale_colour_viridis_d(begin = 0, end = 0.85) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = 0:10 * 10
  ) +
  facet_grid(elec_type~eth_race_comb_cluster)

# Generation w/ x as Age
upper_bound_gen_years <- tibble(
  generation = c("Greatest", "Silent", "Boomer", "Generation X", "Millenial", "Generation Z", "Generation Alpha"),
  end_year = c(1927, 1945, 1964, 1980, 1996, 2012, 2024)
)
for (y in (1994 + (0:15 * 2))) {
  gen_by_age <- fancy_cps %>%
    filter(YEAR <= y) %>%
    group_by(generation, AGE) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
      turnout = 100 * (voters_in_group / vep_in_group)
    ) %>%
    left_join(
      upper_bound_gen_years,
      by = "generation"
    ) %>%
    mutate(
      is_stable = (y >= AGE + end_year)
    )

  p <- ggplot(
    gen_by_age,
    aes(
      y = turnout,
      x = AGE,
      col = fct_reorder(generation, turnout),
      linetype = is_stable,
      shape = is_stable
    )
  ) +
    geom_line() +
    geom_point() +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "right",
      panel.background = element_rect(fill = "grey85")
    ) +
    labs(
      title = paste("Voter Turnout by Generation and Age in", y),
      x = "Age",
      y = "Voter Turnout (%)",
      col = ""
    ) +
    geom_hline(
      yintercept = 0,
      col = "black",
      linewidth = 0.3,
      alpha = 0.5
    ) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = 0:10 * 10
    ) +
    scale_x_continuous(
      breaks = 20 + (0:7 * 10),
      limits = c(18, 90)
    )
  #print(p)
  if (y == 2024) {
    write_csv(gen_by_age, "final_data/gen_by_age2024.csv")
  }
}

# COVI vs. VRI for 2024
cov_tab <- fancy_cps %>%
  filter(YEAR == 2024, locality != "District of Columbia") %>%
  group_by(age_cluster, locality, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(fancy_cps, YEAR) %>%
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
  ) %>%
  left_join(
    select(
      covi,
      locality,
      YEAR,
      cov_rank
    ),
    by = c("locality", "YEAR")
  ) %>%
  mutate(lab_name = paste(cov_rank, "-", locality))

# Turnout
ggplot(
  cov_tab,
  aes(
    x = fct_reorder(lab_name, cov_rank),
    y = turnout,
    col = age_cluster,
    group = age_cluster
  )
) +
  geom_line() + geom_point() +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = 0:10 * 10
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "right",
    panel.background = element_rect(fill = "grey85")
  ) +
  labs(
    title = paste("Voter Turnout by State"),
    subtitle = "Ordered by Cost of Voting Index (2024 general election only)",
    x = "State & COVI Rank",
    y = "Voter Turnout (%)",
    col = "Age Cohort"
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  )

# VRI
ggplot(
  cov_tab,
  aes(
    x = fct_reorder(lab_name, cov_rank),
    y = vri,
    col = age_cluster,
    group = age_cluster
  )
) +
  geom_smooth() +
  scale_y_continuous(
    limits = c(-65, 65),
    breaks = -10:10 * 10
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "right",
    panel.background = element_rect(fill = "grey85")
  ) +
  labs(
    title = paste("Voter Representation by State"),
    subtitle = "Ordered by Cost of Voting Index (2024 general election only)",
    x = "State & COVI Rank",
    y = "VRI",
    col = "Age Cohort"
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  )

## Poverty Line Level Analysis
for (loc in c("Texas", "the US")) {
  if (loc == "the US") {
    cps_c <- fancy_cps
  } else {
    cps_c <- filter(fancy_cps, locality == loc)
  }
  pov_tab <- filter(cps_c, !is.na(is_in_poverty)) %>%
    group_by(is_in_poverty, YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
      turnout = voters_in_group / vep_in_group * 100
    ) %>%
    left_join(
      group_by(fancy_cps, YEAR) %>%
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

  p <- ggplot(
    pov_tab,
    aes(
      x = YEAR,
      y = vri,
      fill = is_in_poverty
    )
  ) +
    geom_col(
      position = "dodge"
    ) +
    scale_y_continuous(
      limits = c(-75, 75),
      breaks = -10:10 * 10
    ) +
    labs(
      title = "VRI for Voters by Age and Economic Status",
      subtitle = paste("Across General Elections 1994-2024 in", loc),
      x = "Year",
      y = "VRI",
      fill = "In poverty?"
    ) +
    geom_hline(
      yintercept = 0,
      col = "black",
      linewidth = 0.3,
      alpha = 0.5
    ) +
    scale_x_continuous(
      breaks = 1994 + (0:15 * 2),
      limits = c(1993, 2025)
    ) +
    facet_wrap(~age_cluster) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      legend.position = "right",
      panel.background = element_rect(fill = "grey85")
    )
  print(p)
}

# Generic VRI and turnout table
# tx only
tx_tab <- fancy_cps %>%
  filter(locality == "Texas") %>%
  group_by(age_cluster, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(fancy_cps, YEAR) %>%
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
  ) %>%
  select(
    YEAR,
    age_cluster,
    turnout,
    vri
  )
write_csv(tx_tab, "final_data/vri-turnout-tx.csv")

us_tab <- fancy_cps %>%
  group_by(age_cluster, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(fancy_cps, YEAR) %>%
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
  ) %>%
  select(
    YEAR,
    age_cluster,
    turnout,
    vri
  )

write_csv(us_tab, "final_data/vri-turnout-us.csv")

## Race and age Line Level Analysis
for (loc in c("Texas", "the US")) {
  if (loc == "the US") {
    cps_c <- fancy_cps
  } else {
    cps_c <- filter(fancy_cps, locality == loc)
  }
  race_tab <- filter(cps_c, !is.na(eth_race_comb_cluster)) %>%
    group_by(age_cluster, eth_race_comb_cluster, YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
      turnout = voters_in_group / vep_in_group * 100
    ) %>%
    left_join(
      group_by(fancy_cps, YEAR) %>%
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
  write_csv(race_tab, str_c("final_data/age_vs_race_vri-", loc, ".csv"))
}

# Generation breakdown csvs by age but also with VRI
upper_bound_gen_years <- tibble(
  generation = c("Lost", "Greatest", "Silent", "Boomer", "Generation X", "Millenial", "Generation Z", "Generation Alpha"),
  end_year = c(1900, 1927, 1945, 1964, 1980, 1996, 2012, 2024)
)

gen_by_age <- fancy_cps %>%
  group_by(generation, AGE) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = 100 * (voters_in_group / vep_in_group)
  ) %>%
  left_join(
    group_by(fancy_cps, generation) %>%
      summarize(
        total_vep = sum(adj_vosuppwt),
        total_voters = sum(adj_vosuppwt * (VOTED == 2))
      )
  ) %>%
  mutate(
    pct_of_ve_population = 100 * vep_in_group / total_vep,
    pct_of_electorate = 100 * voters_in_group / total_voters,
    vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population # (True - Obs) / True
  ) %>%
  left_join(
    upper_bound_gen_years,
    by = "generation"
  ) %>%
  mutate(
    is_stable = (2024 >= AGE + end_year)
  ) %>%
  select(generation, AGE, turnout) %>%
  filter(AGE <= 25)

write_csv(gen_by_age, "final_data/6revised_gen_by_age_turnout.csv")

ggplot(
  gen_by_age,
  aes(
    y = turnout,
    x = AGE,
    col = fct_reorder(generation, turnout),
    linetype = !is_stable,
    shape = !is_stable
  )
) +
  geom_line() +
  geom_point() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position = "right",
    panel.background = element_rect(fill = "grey85")
  ) +
  labs(
    title = "Turnout by Generation and Age in 2024",
    x = "Age",
    y = "Turnout (%)",
    col = "",
    shape = "Data Still\nBeing Collected",
    linetype = "Data Still\nBeing Collected",
  ) +
  geom_hline(
    yintercept = 0,
    col = "black",
    linewidth = 0.3,
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = 20 + (0:9 * 10),
    limits = c(18, 100)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = 0:10 * 10
  )

# page 10/15
pov_tab_tx <- fancy_cps %>%
  filter(!is.na(is_in_poverty), locality == "Texas") %>%
  group_by(is_in_poverty, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(filter(fancy_cps, locality == "Texas"), YEAR) %>%
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
pov_tab_tx$elec_type <- c("Midterm", "Presidential")[(round(pov_tab_tx$YEAR / 4) == pov_tab_tx$YEAR / 4) + 1]

pov_tab_us <- fancy_cps %>%
  filter(!is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(filter(fancy_cps, !is.na(is_in_poverty)), YEAR) %>%
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
pov_tab_us$elec_type <- c("Midterm", "Presidential")[(round(pov_tab_us$YEAR / 4) == pov_tab_us$YEAR / 4) + 1]

# page 15
select(pov_tab_tx, vri, YEAR, is_in_poverty) %>%
  filter(YEAR >= 2014) %>%
  write_csv("final_data/15_TX_revised_pov_vri.csv")
select(pov_tab_us, vri, YEAR, is_in_poverty) %>%
  filter(YEAR >= 2014) %>%
  write_csv("final_data/15_US_revised_pov_vri.csv")

# page 10
select(pov_tab_us, turnout, YEAR, is_in_poverty, elec_type) %>%
  mutate(locality = "United States") %>%
  filter(YEAR >= 1988) %>%
  rows_append(
    select(pov_tab_tx, turnout, YEAR, is_in_poverty, elec_type) %>%
      mutate(locality = "Texas") %>%
      filter(YEAR >= 1988)
  ) %>%
  write_csv("final_data/10_revised_pov_turnout.csv")

for (l in c("Texas", "United States")) {
  age_tab <- fancy_cps %>%
    filter(YEAR >= 1984) %>%
    group_by(is_in_poverty, YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
      turnout = voters_in_group / vep_in_group * 100
    ) %>%
    left_join(
      group_by(filter(fancy_cps, !is.na(is_in_poverty)), YEAR) %>%
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
}


age_tab <- fancy_cps %>%
  filter(!is.na(is_in_poverty)) %>%
  group_by(is_in_poverty, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(filter(fancy_cps, !is.na(is_in_poverty)), YEAR) %>%
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

# young men vs young women
# Generic VRI and turnout table
# tx only
age_tab_tx <- fancy_cps %>%
  filter(AGE < 30) %>%
  filter(locality == "Texas") %>%
  group_by(sex_name, YEAR) %>%
  reframe(
    vep_in_group = sum(adj_vosuppwt),
    voters_in_group = sum(adj_vosuppwt * (VOTED == 2)),
    turnout = voters_in_group / vep_in_group * 100
  ) %>%
  left_join(
    group_by(filter(fancy_cps, AGE < 30, locality == "Texas"), YEAR) %>%
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
  ) %>%
  select(
    YEAR,
    sex_name,
    turnout,
    vri
  )

ggplot(
  age_tab_tx,
  aes(
    x = YEAR,
    y = turnout,
    col = sex_name
  )
) +
  geom_point() +
  geom_line()
