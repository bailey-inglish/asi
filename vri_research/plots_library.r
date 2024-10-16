library(tidyverse)

cps <- read_csv("vri_research/final_data/cps_expanded_ipums_1994-2022.csv")

year_count <- 1 + (max(cps$YEAR) - min(cps$YEAR)) / 2
year_range_str <- str_c(min(cps$YEAR), "-", max(cps$YEAR))

## Section 1: State of Texas Turnout
# TX vs. US
turn_tx <- filter(cps, STATEFIP == 48) %>%
  group_by(YEAR) %>%
  summarize(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  ) %>%
  cbind("region_name" = rep("Texas", year_count))

turn_us <- cps %>%
  group_by(YEAR) %>%
  summarize(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  ) %>%
  cbind("region_name" = rep("United States", year_count))

turn_comb <- rows_append(turn_tx, turn_us)
turn_comb$elec_type <- rep("Presidential", year_count)
turn_comb$elec_type[is.element(turn_comb$YEAR, 2022 - 0:25 * 4)] <- "Midterm"

ggplot(turn_comb) +
  geom_line(
    aes(
      x = YEAR,
      y = turnout * 100,
      col = region_name,
      lty = elec_type
    )
  ) +
  labs(
    title = "Voter Turnout: Texas vs. US",
    subtitle = paste("Presidential and Midterm Elections year", year_range_str),
    x = "Year",
    y = "VEP Turnout (%)",
    col = "Region",
    lty = "Election Type"
  ) +
  scale_color_manual(values = c("#bf5700", "black")) +
  scale_x_continuous(
    breaks = 2024 - 0:25 * 4
  )

turn_reg <- group_by(cps, region_name, YEAR) %>%
  reframe(
    turnout = sum(adj_vosuppwt * (VOTED == 2)) / sum(adj_vosuppwt)
  )

turn_reg <- rows_append(turn_reg, turn_tx)
turn_reg$elec_type <- rep("Presidential", nrow(turn_reg))
turn_reg$elec_type[is.element(turn_reg$YEAR, 2022 - 0:25 * 4)] <- "Midterm"

ggplot(turn_reg) +
  geom_line(
    aes(
      x = YEAR,
      y = turnout * 100,
      col = region_name,
      lty = elec_type
    )
  ) +
  labs(
    title = "Voter Turnout: Texas vs. US Regions",
    subtitle = paste("Presidential and Midterm Elections", year_range_str),
    x = "Year",
    y = "VEP Turnout (%)",
    col = "Region",
    lty = "Election Type"
  ) +
  scale_color_manual(values = c("darkred", "darkblue", "darkgreen", "#bf5700", "gold")) +
  scale_x_continuous(
    breaks = 2024 - 0:25 * 4
  )

# Conference comparison - for fun
# WIP come back later

## Longitudinal analysis
# Line plots of VRI over time for each group/grouping variable
vars_of_interest <- tibble(
  var = c("age_cluster", "race_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "metro_status", "is_hispanic", "sex_name", "veteran_status", "any_difficulty"),
  name = c("Age Cluster", "Race Cluster", "Length of Residence", "Educational Attainment", "Income Range", "Metro Status", "Ethnicity", "Sex", "Military Service", "Disability")
)

for (gvar in vars_of_interest$var) {
  cps_c <- filter(cps, !is.na(!!sym(gvar)))
  gname <- vars_of_interest$name[vars_of_interest$var == gvar]

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

  cps_grouped <- rows_append(
    cbind(tx_tab, "scope" = rep("Texas", nrow(tx_tab))),
    cbind(us_tab, "scope" = rep("United States", nrow(us_tab)))
  )

  p <- ggplot(cps_grouped) +
    geom_line(
      aes(
        col = !!sym(gvar),
        y = vri,
        x = YEAR
      )
    ) +
    geom_point(
      aes(
        col = !!sym(gvar),
        y = vri,
        x = YEAR
      )
    ) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    facet_grid(
      ~scope
    ) +
    labs(
      title = paste("Voter Representation by", gname),
      subtitle = paste("November general elections", year_range_str),
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
      breaks = 2024 - (0:25 * 4)
    )

  if (is.element(gvar, c("edu_cluster", "income_range"))) {
    p <- p +
      scale_y_continuous(
        limits = c(-70, 150),
        breaks = -20:20 * 10
      )
  } else {
    p <- p +
      scale_y_continuous(
        limits = c(-70, 70),
        breaks = -20:20 * 10
      )
  }
  print(p)
}

# Note: y-scale had been enlarged due to high turnout\namong advanced degree holders (N < 110 each year)