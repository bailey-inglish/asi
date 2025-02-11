library(tidyverse)
setwd("electoral_studies_paper")

ovr_vri <- read_csv("final_data/vri_2008-2022.csv")

ggplot(ovr_vri) +
  geom_histogram(
    aes(
      x = log(vri_ratio)
    ),
    position = position_dodge(),
    bins = 50
  ) +
  facet_wrap(~grouping_var)

filter(ovr_vri, grouping_var == "is_college_educ", !is.na(more_dem_votes)) %>%
  ggplot() +
  geom_density(
    aes(
      x = vri_ratio,
      fill = more_dem_votes,
      col = more_dem_votes
    ),
    alpha = 0.5
  )

for (gvar in unique(ovr_vri$grouping_var)) {
  print(gvar)
  cor(filter(ovr_vri, grouping_var == gvar, year < 2022)$more_dem_votes, filter(ovr_vri, grouping_var == gvar, year < 2022)$vri_ratio) %>%
    print()
  print("- - - - - - - - - - - - - - - - - - - - -")
}

for (gvar in unique(ovr_vri$grouping_var)) {
  print(gvar)
  cor(filter(ovr_vri, grouping_var == gvar)$has_sdr, filter(ovr_vri, grouping_var == gvar)$vri_ratio) %>%
    print()
  print("- - - - - - - - - - - - - - - - - - - - -")
}
