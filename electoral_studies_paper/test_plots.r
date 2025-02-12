library(tidyverse)
setwd("electoral_studies_paper")

ovr_vri <- read_csv("final_data/vri_ratio_2008-2022.csv")

vdi <- read_csv("final_data/vdi_ratio_2008-2022.csv")

for (gvar in unique(ovr_vri$grouping_var)) {
  for (loc in unique(ovr_vri$locality)) {
    sub <- filter(ovr_vri, locality == loc & grouping_var == gvar)
    
  }
}

gvar <- "is_over_30"
loc <- "United States"
sub <- filter(vdi, locality != loc & grouping_var == gvar)
mod <- lm(more_dem_votes ~ vdi_ratio + locality + has_sdr, data = sub)
anova(mod)

ggplot(vdi) +
  geom_boxplot(
    aes(
      fill = more_dem_votes,
      col = more_dem_votes,
      y = vdi_ratio,
      x = locality
    ),
    alpha = 0.2
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggplot(vdi) +
  geom_histogram(
    aes(
      x = (vdi_ratio)
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

ggplot(ovr_vri) +
  geom_histogram(
    aes(
      x = vri_ratio
    ),
    bins = 50
  )
