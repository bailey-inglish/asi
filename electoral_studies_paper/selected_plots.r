library(tidyverse)
library(aod)
setwd("electoral_studies_paper")

vdi <- read_csv("final_data/vdi_2004-2022.csv")
fancy_cps <- read_csv("../cps_cleaning/final_data/cps_clean_ipums_2008-2022.csv")

## Longitudinal analysis
# Line plots of VRI over time for each group/grouping variable
vars_of_interest <- tibble(
  var = c("age_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "metro_status", "eth_race_comb_cluster", "sex_name"),
  name = c("Age Cohort", "Length of Residence", "Educational Attainment", "Income Range", "Metro Status", "Race/Ethnicity", "Gender")
)

for (gvar in vars_of_interest$var) {
  cps_c <- filter(fancy_cps, !is.na(!!sym(gvar)))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

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
      breaks = 2008 + (0:7 * 2)
    ) +
    scale_colour_viridis_d(begin = 0, end = 0.85) +
    scale_y_continuous(
      limits = c(-65, 65),
      breaks = -10:10 * 10
    )
  print(p)
}

# Line plot of turnout by age group over time
for (gvar in vars_of_interest$var) {
  cps_c <- filter(fancy_cps, !is.na(!!sym(gvar)))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

  us_tab <- cps_c %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = sum(adj_vosuppwt),
      voters_in_group = sum(adj_vosuppwt * (VOTED == 2))
    ) %>%
    mutate(
      turnout = 100 * (voters_in_group / vep_in_group)
    )

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
      breaks = 2008 + (0:7 * 2)
    ) +
    scale_colour_viridis_d(begin = 0, end = 0.85) +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = 0:10 * 10
    )
  print(p)
}
