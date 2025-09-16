# File name: selected_plots.r
# Purpose:   Generates 2024-era plots for VRI and turnout for selected
#            characteristics of demographic groups.
# Author:    Bailey Inglish


library(tidyverse)
library(aod)
setwd("briefs/youth24")

fancy_cps <- read_csv("final_data/cps_expanded_ipums_1994-2024.csv")
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
      turnout = 100 * (voters_in_group / vep_in_group),
      pct_of_ve_population = 100 * vep_in_group / total_vep,
      pct_of_electorate = 100 * voters_in_group / total_voters,
      vri = 100 * (pct_of_electorate - pct_of_ve_population) / pct_of_ve_population,
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
  print(p)
}
