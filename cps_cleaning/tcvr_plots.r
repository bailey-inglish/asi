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
  scale_color_manual(values = c("#bf5700", "black"))

# Census-designated regional comparison
cps$region <- rep(NA, nrow(cps))
cps$region[is.element(cps$REGION, c(11, 12))] <- "Northeast"
cps$region[is.element(cps$REGION, c(21, 22))] <- "Midwest"
cps$region[is.element(cps$REGION, c(31, 32, 33))] <- "South"
cps$region[is.element(cps$REGION, c(41, 42))] <- "West"

turn_reg <- group_by(cps, region, YEAR) %>%
  reframe(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  )

turn_reg <- rows_append(turn_reg, turn_tx)
turn_reg$elec_type <- rep("Presidential", nrow(turn_reg))
turn_reg$elec_type[is.element(turn_reg$YEAR, c(2010, 2014, 2018, 2022))] <- "Midterm"

ggplot(turn_reg) +
  geom_line(
    aes(
      x = YEAR,
      y = turnout * 100,
      col = region,
      lty = elec_type
    )
  ) +
  labs(
    title = "Voter Turnout: Texas vs. US Regions",
    subtitle = "Presidential and Midterm Elections 2008-2022",
    x = "Year",
    y = "VEP Turnout (%)",
    col = "Region",
    lty = "Election Type"
  ) +
  scale_color_manual(values = c("darkred", "darkblue", "darkgreen", "#bf5700", "gold"))

# Conference comparison - for fun
# WIP come back later

## Voting Strength and VRI
# Recodes for vis
vars_of_interest <- tibble(
  var = c("age_cluster", "race_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "metro_status", "is_hispanic"),
  name = c("Age Cluster", "Race Cluster", "Length of Residence", "Educational Attainment", "Income Range", "Metro Status", "Ethnicity")
)

cps$is_hispanic <- c("TRUE" = "Hispanic/Latino", "FALSE" = "Non-Hispanic/Latino")[as.character(cps$is_hispanic)]

for (gvar in vars_of_interest$var) {
  cps_c <- filter(cps, !is.na(!!sym(gvar)), is.element(YEAR, 2016:2022))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

  tx_tab <- filter(cps_c, STATEFIP == 48) %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = round(sum(adj_vosuppwt), 2),
      voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
    ) %>%
    left_join(
      group_by(filter(cps_c, STATEFIP == 48), YEAR) %>%
        summarize(
          total_vep = round(sum(adj_vosuppwt), 2),
          total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
        ),
      by = "YEAR"
    ) %>%
    mutate(
      pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
      pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
      vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
    )

  us_tab <- cps_c %>%
    group_by(!!sym(gvar), YEAR) %>%
    reframe(
      vep_in_group = round(sum(adj_vosuppwt), 2),
      voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
    ) %>%
    left_join(
      group_by(cps_c, YEAR) %>%
        summarize(
          total_vep = round(sum(adj_vosuppwt), 2),
          total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
        ),
      by = "YEAR"
    ) %>%
    mutate(
      pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
      pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
      vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
    )

  cps_grouped <- rows_append(
    cbind(tx_tab, "scope" = rep("TX", nrow(tx_tab))),
    cbind(us_tab, "scope" = rep("US", nrow(us_tab)))
  )

  p <- ggplot(cps_grouped) +
    geom_col(
      aes(
        x = fct_reorder(
          !!sym(gvar),
          vri
        ),
        y = vri,
        fill = scope
      ),
      position = "dodge",
      width = 0.7
    ) +
    scale_fill_manual(values = c("#bf5700", "#3f3f3f")) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_grid(
      ~YEAR
    ) +
    labs(
      title = paste("Voter Representation by", gname),
      x = gname,
      y = "Voting Representation Index (%)",
      fill = "Region"
    ) +
    geom_hline(
      yintercept = 0,
      col = "black",
      linewidth = 0.25
    )

  if (gvar == "race_cluster") {
    p <- p +
      labs(
        caption = "Note: very small sample for Native American (n = 3,490 across all four\nsurveys) and multiracial respondents (n = 5,472 across all four surveys)"
      )
  }
  print(p)
}

## Bonus: plots for gifs!
income_power_groups <- cps %>%
  group_by(income_range, YEAR) %>%
  reframe(
    vep_in_group = round(sum(adj_vosuppwt), 2),
    voters_in_group = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
  ) %>%
  left_join(
    group_by(cps, YEAR) %>%
      summarize(
        total_vep = round(sum(adj_vosuppwt), 2),
        total_voters = round(sum(adj_vosuppwt * (VOTED == 2)), 2)
      ),
    by = "YEAR"
  ) %>%
  mutate(
    pct_of_ve_population = 100 * round(vep_in_group / total_vep, 3),
    pct_of_electorate = 100 * round(voters_in_group / total_voters, 3),
    vri = 100 * round((pct_of_electorate - pct_of_ve_population) / pct_of_ve_population, 3) # (True - Obs) / True
  )

for (y in 2008 + (2 * 0:7)) {
  p <- ggplot(filter(income_power_groups, YEAR == y, !is.element(income_range, c("Don't know", "Refused")))) +
    geom_col(
      aes(
        x = fct_reorder(
          income_range,
          c("<30k", "$30-50k", "$50-100k", "$100-150k", ">$150k")
        ),
        y = pct_of_electorate,
        fill = income_range
      ),
      alpha = 0.75
    ) +
    geom_point(
      aes(
        x = income_range,
        y = pct_of_ve_population
      ),
      shape = 4
    ) +
    labs(
      title = "Electoral vs. Population Representation",
      subtitle = paste("In the", y, "general election"),
      x = "Income group",
      y = "Share of Electorate (%)",
      fill = "Income group",
      caption = "Note: X = Share of population (%)"
    ) +
    theme(legend.position = "none") +
    scale_y_continuous(
      breaks = 0:7 * 5,
      limits = c(0, 35)
    )
  print(p)
}

for (y in 2008 + (2 * 0:7)) {
  p <- ggplot(filter(income_power_groups, YEAR == y, !is.element(income_range, c("Don't know", "Refused")))) +
    geom_col(
      aes(
       x = fct_reorder(
          income_range,
          c("<30k", "$30-50k", "$50-100k", "$100-150k", ">$150k")
        ),
        y = vri,
        fill = income_range
      ),
      alpha = 0.75
    ) +
    labs(
      title = "Electoral vs. Population Representation",
      subtitle = paste("In the", y, "general election"),
      x = "Income group",
      y = "Voting Representation Index (%)",
      fill = "Income group"
    ) +
    theme(legend.position = "none") +
    scale_y_continuous(
      breaks = -10:10 * 10,
      limits = c(-40, 40)
    ) +
    geom_hline(
      yintercept = 0,
      col = "black",
      linewidth = 0.25
    )
  print(p)
}
