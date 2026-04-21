# Testing hypotheses relating to Anthony et al. (2023)

# Libraries
library(tidyverse)

# Data
setwd("rcv-partisanship")
cmps20 <- read_csv("data/cmps20.csv")

# Select just the columns I need :)
cmps20_c <- cmps20 %>%
  filter(
    q199split == "Split A"
  ) %>%
  mutate(
    race = S2_Race_Prime,
    rcv_scale = as.numeric(gsub("[^0-9]", "", Q198)),
    rcv_choice = Q199,
    party = Q21,
    weight = weight,
    .keep = "none"
  )

# Cool graph time
plot_df <- cmps20_c %>%
  filter(rcv_choice %in% c("Single vote", "Ranked vote")) %>%
  group_by(rcv_scale, party, rcv_choice) %>%
  summarize(w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(rcv_scale, party) %>%
  mutate(
    pct = w / sum(w),
    label = as.character(round(w))
  ) %>%
  ungroup()

ggplot(
  plot_df,
  aes(x = factor(rcv_scale), y = pct, fill = rcv_choice)
) +
  geom_col(width = 0.7, color = "white") +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  facet_wrap(~party) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Single vote" = "#B0BEC5",
      "Ranked vote" = "#455A64"
    )
  ) +
  labs(
    title = "Satisfaction with RCV by Party Group",
    x = "Satistfaction with RCV (0-10)",
    y = "Percent of Party Group",
    subtitle = "Data Source: CMPS 2020, Split A (Q199)",
    caption = "Note: Split A includes respondents who were randomly assigned to the control condition for RCV framing",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    legend.position = "bottom"
  )

  # Plot showing counts instead of percentages
  count_df <- cmps20_c %>%
    filter(rcv_choice %in% c("Single vote", "Ranked vote")) %>%
    group_by(rcv_scale, party, rcv_choice) %>%
    summarize(w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
    ungroup()

  ggplot(
    count_df,
    aes(x = factor(rcv_scale), y = w, fill = rcv_choice)
  ) +
    geom_col(width = 0.7, color = "white") +
    facet_wrap(~party) +
    scale_fill_manual(
      values = c(
        "Single vote" = "#B0BEC5",
        "Ranked vote" = "#455A64"
      )
    ) +
    labs(
      title = "RCV Preference Distribution by Party Group",
      x = "Satisfaction with RCV (0-10)",
      y = "Weighted Count",
      subtitle = "Data Source: CMPS 2020, Split A (Q199)",
      caption = "Note: Split A includes respondents who were randomly assigned to the control condition for RCV framing",
      fill = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      axis.title = element_text(size = 10),
      legend.position = "bottom"
    )

    # Plot showing proportions within each party group
    prop_df <- cmps20_c %>%
      filter(rcv_choice %in% c("Single vote", "Ranked vote")) %>%
      group_by(rcv_scale, party, rcv_choice) %>%
      summarize(w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
      group_by(party, rcv_choice) %>%
      mutate(
        prop = w / sum(w),
        label = scales::percent(prop, accuracy = 1)
      ) %>%
      ungroup()

    ggplot(
      prop_df,
      aes(x = factor(rcv_scale), y = prop, fill = rcv_choice)
    ) +
      geom_col(width = 0.7, color = "white") +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        size = 3
      ) +
      facet_wrap(~party) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        values = c(
          "Single vote" = "#B0BEC5",
          "Ranked vote" = "#455A64"
        )
      ) +
      labs(
        title = "RCV Satisfaction Distribution by Party Group",
        x = "Satisfaction with RCV (0-10)",
        y = "Proportion within Party Group",
        subtitle = "Data Source: CMPS 2020, Split A (Q199)",
        caption = "Note: Each panel shows the distribution as a share of party group members",
        fill = NULL
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 12),
        axis.title = element_text(size = 10),
        legend.position = "bottom"
      )

eugenia_table <- cmps20_c %>%
  filter(party != "Other party") %>%
  group_by(party, rcv_choice, rcv_scale) %>%
  summarize(w = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    cmps20_c %>%
      group_by(party, rcv_choice) %>%
      summarize(total_w = sum(weight, na.rm = TRUE), .groups = "drop"),
    by = c("party", "rcv_choice")
  ) %>%
  mutate(
    prop = w / total_w,
    partyrcv = paste(party, rcv_choice, sep = " - "),
    rcv_scale = rcv_scale,
    .keep = "none"
  ) %>%
  pivot_wider(
    names_from = partyrcv,
    values_from = prop
  ) %>%
  arrange(rcv_scale) %>%
  knitr::kable(
    digits = 3,
    caption = "RCV Satisfaction by Party and Choice",
    col.names = c("RCV Scale", gsub("_", " ", colnames(.)[-1]))
  ) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# figure 1 redo
mean_df <- cmps20_c %>%
  filter(rcv_choice %in% c("Single vote", "Ranked vote")) %>%
  group_by(race, party, rcv_choice) %>%
  summarize(mean_sat = weighted.mean(rcv_scale, weight, na.rm = TRUE), .groups = "drop")

ggplot(
  mean_df,
  aes(x = factor(race), y = mean_sat, fill = rcv_choice)
) +
  geom_col(width = 0.7, color = "white", position = "dodge") +
  geom_text(
    aes(label = round(mean_sat, 1)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    angle = 45,
    hjust = 0,
    size = 3
  ) +
  facet_wrap(~party) +
  scale_fill_manual(
    values = c(
      "Single vote" = "#455A64",
      "Ranked vote" = "#B0BEC5"
    )
  ) +
  labs(
    title = "Mean RCV Satisfaction by Race and Party",
    x = "Race",
    y = "Mean Satisfaction with RCV",
    subtitle = "Data Source: CMPS 2020, Split A (Q199)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  ) +
  scale_y_continuous(limits = c(0, 10))
