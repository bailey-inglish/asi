library(tidyverse)
setwd("electoral_studies_paper")

ovr_vri <- read_csv("final_data/vri_ratio_2008-2022.csv")

vdi <- read_csv("final_data/vdi_1994-2022.csv")

current_vdi <- filter(vdi, !is.na(more_dem_votes))
for (gvar in unique(vdi$grouping_var)) {
  sub <- filter(current_vdi_f, grouping_var == gvar)
  mod <- lm(more_dem_votes ~ pres_incumb + midterm + vdi, data = sub)
  print(paste("GROUPING VAR:", gvar, "YEARS: ALL 2004-2022", "—————————————————————"))
  print((mod))
  print("——————————————————————————————————————————————————————————————")
}

current_vdi <- filter(vdi, !is.na(more_dem_votes), is.element(year, 2004:2012))
for (gvar in unique(vdi$grouping_var)) {
  sub <- filter(current_vdi, grouping_var == gvar)
  mod <- lm(more_dem_votes ~ year + has_sdr + midterm + pres_incumb + vdi, data = sub)
  print(paste("GROUPING VAR:", gvar, "YEARS: 2004-2012", "—————————————————————"))
  print(anova(mod))
  print("——————————————————————————————————————————————————————————————")
}

current_vdi <- filter(vdi, !is.na(more_dem_votes), is.element(year, 2014:2022))
for (gvar in unique(vdi$grouping_var)) {
  sub <- filter(current_vdi, grouping_var == gvar)
  mod <- lm(more_dem_votes ~ year + has_sdr + midterm + pres_incumb + vdi, data = sub)
  print(paste("GROUPING VAR:", gvar, "YEARS: 2014-2022", "—————————————————————"))
  print(anova(mod))
  print("——————————————————————————————————————————————————————————————")
}

gvar <- "is_college_educ"
loc <- "United States"
sub <- filter(vdi, locality != loc & grouping_var == gvar)
mod <- lm(more_dem_votes ~ vdi_ratio + locality + has_sdr, data = sub)
anova(mod)

ggplot(sub) +
  geom_boxplot(
    aes(
      fill = more_dem_votes,
      col = more_dem_votes,
      y = vdi_ratio,
      x = locality
    ),
    alpha = 0.2
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(
    title = "Boxplot of VDI by electoral outcomes and state across years"
  ) +
  geom_hline(yintercept = mean(filter(sub, more_dem_votes == TRUE)$vdi_ratio), col = "blue") +
  geom_hline(yintercept = mean(filter(sub, more_dem_votes == FALSE)$vdi_ratio), col = "red")

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
