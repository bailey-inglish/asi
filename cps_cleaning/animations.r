library(tidyverse)
library(gganimate)
library(gifski)
library(ggthemes)
library(forcats)
library(magick)

setwd("cps_cleaning")
logo <- image_read("raw_data/strauss_rgb.png")
cps <- read_csv("final_data/cps_clean_ipums_2008-2022_CUSTOM.csv")

vars_of_interest <- tibble(
  var = c("age_cluster", "vote_res_harmonized", "edu_cluster", "income_range", "eth_race_comb_cluster", "sex_name"),
  name = c("Age Cohort", "Length of Residence", "Education Level", "Income Range", "Race and Ethnicity", "Gender")
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

  p <- ggplot(
    cps_grouped,
    aes(
      y = vri,
      x = fct_reorder(!!sym(gvar), vri),
      fill = scope
    )
  ) +
    geom_col(position = "dodge") +
    geom_hline(
      yintercept = 0,
      col = "black",
      linewidth = 0.3,
      alpha = 0.5
    ) +
    scale_fill_manual(values = c("#e2b065", "#83c5cf")) +
    scale_y_continuous(
      limits = c(-65, 65),
      breaks = -10:10 * 20
    ) +
    theme_fivethirtyeight() +
    theme(
      legend.position = "bottom",
      plot.tag = element_text(
        face = "bold",
        color = "#a3a3a3",
        size = 60
      ),
      axis.title.x = element_text(
        color = "#2b2b2b",
        size = 10
      ),
      axis.title.y = element_text(
        color = "#2b2b2b",
        size = 10
      ),
      axis.text.x = element_text(
        color = "#393939",
        size = 12
      ),
      axis.text.y = element_text(
        color = "#393939",
        size = 12
      ),
      plot.tag.position = "bottomright",
      plot.tag.location = "panel"
    ) +
    transition_time(YEAR) +
    labs(
      title = paste("Voting Representation Disparities by", gname),
      fill = "",
      tag = "{round((frame_time - 0.5) / 2) * 2}",
      caption = "Full report available at annettestrauss.org\nData: IPUMS CPS, UF Election Lab"
    ) +
    xlab(gname) +
    ylab("Electoral Over-/Under-Representation (VRI %)")

  if (gvar == "edu_cluster") {
    p <- p +
      theme(axis.text.x = element_text(angle = 20, vjust = 1, hjust = 1))
  }

  animate(p, height = 800, width = 800, fps = 30, duration = 15, start_pause = 30, end_pause = 30, res = 100)
  anim_save(paste(gname, "_comp", ".gif", sep = ""))

  main_gif <- image_read(paste(gname, "_comp", ".gif", sep = ""))
  new_gif <- image_composite(
    main_gif,
    image_resize(logo, geometry_size_percent(12)),
    offset = "+20+740"
  )
  image_write_gif(new_gif, paste(gname, "_comp", ".gif", sep = ""))
}
