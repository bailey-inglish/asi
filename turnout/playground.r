library(tidyverse)
library(kableExtra)
library(forcats)
library(ipumsr)

cps <- read_ipums_ddi("C:/Users/baile/Desktop/asi/cps_cleaning/raw_data/cps_00011.xml") %>% read_ipums_micro()

# Hispanic simple recoding
cps$is_hispanic <- cps$HISPAN != 0
cps$is_hispanic[cps$HISPAN > 900] <- NA

# Race simple recoding
cps$race_cluster <- rep("multiracial", nrow(cps))
cps$race_cluster[cps$RACE == 100] <- "white"
cps$race_cluster[cps$RACE == 200] <- "black"
cps$race_cluster[cps$RACE == 300] <- "native american"
cps$race_cluster[cps$RACE == 651] <- "asian"

cps <- mutate(cps, is_registered = VOTED == 2 | VOREG == 2)

income_conv <- tribble(
  ~FAMINC, ~income_range,
  100, "$5,000 or less",
  110, "Under $1,000",
  120, "$1,000 - 1,999",
  130, "$2,000 - 2,999",
  140, "$3,000 - 3,999",
  150, "$4,000 - 4,999",
  200, "$5,000 - 7,999",
  210, "$5,000 - 5,999",
  220, "$5,000 - 5,999",
  231, "$6,000 - 7,499",
  300, "$7,500 - 9,999",
  430, "$10,000 - 12,499",
  440, "$10,000 - 11,999",
  460, "$12,500 - 14,999",
  470, "$12,500 - 14,999",
  500, "$15,000 - 19,999",
  540, "$15,000 - 17,499",
  550, "$17,500 - 19,999",
  600, "$20,000 - 24,999",
  700, "$25,000 - 49,999",
  710, "$25,000 - 29,999",
  720, "$30,000 - 34,999",
  730, "$35,000 - 39,999",
  740, "$40,000 - 49,999",
  800, "$50,000 and over",
  810, "$50,000 - 74,999",
  820, "$50,000 - 59,999",
  830, "$60,000 - 74,999",
  840, "$75,000 and over",
  841, "$75,000 - 99,999",
  842, "$100,000 - 149,999",
  843, "$150,000 and over",
  996, "Refused",
  997, "Don't know",
  999, "Blank"
)

for (tx in c(FALSE, TRUE)) {
  for (y in 1982 + (0:20 * 2)) {
    cps_c <- filter(cps, YEAR == y)
    if (tx) {
      cps_c <- filter(cps_c, STATEFIP == 48)
    }
    if (y >= 1994) {
      income_strength <-  cps_c %>%
        group_by(FAMINC) %>%
        summarize(
          count = round(sum(VOSUPPWT * (AGE >= 18 & CITIZEN < 5)), 1),
          voted = round(sum(VOSUPPWT * (VOTED == 2)), 1),
          pop_pct = 100 * round(count / sum(filter(cps_c, AGE >= 18 & CITIZEN < 5)$VOSUPPWT), 3),
          voter_pct = 100 * round(voted / sum(cps_c$VOSUPPWT * (cps_c$VOTED == 2)), 3),
          pct_diff = 100 * round((voter_pct - pop_pct) / pop_pct, 3) # (Obs - True) / True
        )
    } else {
      income_strength <-  cps_c %>%
        group_by(FAMINC) %>%
        summarize(
          count = round(sum(VOSUPPWT * (AGE >= 18)), 1),
          voted = round(sum(VOSUPPWT * (VOTED == 2)), 1),
          pop_pct = 100 * round(count / sum(filter(cps_c, AGE >= 18)$VOSUPPWT), 3),
          voter_pct = 100 * round(voted / sum(cps_c$VOSUPPWT * (cps_c$VOTED == 2)), 3),
          pct_diff = 100 * round((voter_pct - pop_pct) / pop_pct, 3) # (Obs - True) / True
        )
    }

    income_strength <- left_join(income_strength, income_conv, by = "FAMINC") %>%
      filter(
        FAMINC < 900
      )

    p <- ggplot(
      income_strength,
      aes(
        x = fct_reorder(
          income_range,
          FAMINC
        )
      )
    ) +
      geom_col(
        aes(
          y = pct_diff
        ),
        width = 0.25
      ) +
      labs(
        title = "Percent difference in voting power of income groups",
        subtitle = paste("In", as.character(y)),
        x = "Household Income Range",
        y = "Represenation in the Electorate vs. Population (% difference)",
        caption = paste("TEXAS ONLY:", as.character(tx))
      ) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          vjust = 1,
          hjust = 1
        )
      ) +
      scale_y_continuous(
        limits = c(-100, 100),
        breaks = (-5:5 * 20)
      ) +
      geom_hline(
        yintercept = 0,
        col = "black",
        linewidth = 0.5
      )
    print(p)
  }
}

age_power_cont <- filter(cps, AGE >= 18 & (is.na(CITIZEN) | CITIZEN < 5)) %>%
  group_by(AGE, YEAR) %>%
  reframe(
    count = round(sum(VOSUPPWT), 1),
    voted = round(sum(VOSUPPWT * (!is.na(VOTED) & VOTED == 2)), 1)
  ) %>%
  left_join(
    group_by(cps, YEAR) %>%
      summarize(
        total_count = round(sum(VOSUPPWT * ((is.na(CITIZEN) | CITIZEN < 5) & AGE >= 18)), 1),
        total_voted = round(sum(VOSUPPWT * (!is.na(VOTED) & VOTED == 2)), 1)
      ),
    by = "YEAR") %>%
  mutate(
    pop_pct = 100 * round(count / total_count, 3),
    voter_pct = 100 * round(voted / total_voted, 3),
    pct_diff = 100 * round((voter_pct - pop_pct) / pop_pct, 3) # (True - Obs) / True
  )

age_power_cont_tx <- filter(cps, AGE >= 18 & (is.na(CITIZEN) | CITIZEN < 5) & STATEFIP == 48) %>%
  group_by(AGE, YEAR) %>%
  reframe(
    count = round(sum(VOSUPPWT), 1),
    voted = round(sum(VOSUPPWT * (!is.na(VOTED) & VOTED == 2)), 1)
  ) %>%
  left_join(
    group_by(filter(cps, STATEFIP == 48), YEAR) %>%
      summarize(
        total_count = round(sum(VOSUPPWT * ((is.na(CITIZEN) | CITIZEN < 5) & AGE >= 18)), 1),
        total_voted = round(sum(VOSUPPWT * (!is.na(VOTED) & VOTED == 2)), 1)
      ),
    by = "YEAR") %>%
  mutate(
    pop_pct = 100 * round(count / total_count, 3),
    voter_pct = 100 * round(voted / total_voted, 3),
    pct_diff = 100 * round((voter_pct - pop_pct) / pop_pct, 3) # (True - Obs) / True
  )


for (tx in c(FALSE, TRUE)) {
  for (y in 1976 + (0:23 * 2)) {
    age_power_cont_c <- filter(age_power_cont, YEAR == y)
    cap <- "United States"
    if (tx) {
      age_power_cont_c <- filter(age_power_cont_tx, YEAR == y)
      cap <- "Texas Only"
    }
    p <- ggplot(age_power_cont_c) +
      geom_line(
        aes(
          x = AGE,
          y = voter_pct,
          col = "Electorate"
        ),
        alpha = 0.75
      ) +
      geom_line(
        aes(
          x = AGE,
          y = pop_pct,
          col = "Voting Eligible\nPopulation"
        ),
        alpha = 0.75
      ) +
      labs(
        title = "Relative voting strength of young vs. old voters",
        subtitle = paste("For the November", y, "general election"),
        x = "Age",
        y = "% of Population vs. % of Electorate",
        col = "Group",
        caption = str_c("Note: 80-84 topcoded at 80; 85+ topcoded at 85\n", cap)
      ) +
      scale_x_continuous(
        limits = c(18, 85),
        breaks = c(18, 10 * 3:8,  85)
      ) +
      scale_y_continuous(
        limits = c(0, 5),
        breaks = c(0.5 * 0:10)
      ) +
      scale_color_manual(
        values = c(
          "Voting Eligible\nPopulation" = "blue",
          "Electorate" = "red"
        )
      )
    print(p)
  }
}

for (tx in c(FALSE, TRUE)) {
  for (y in 1976 + (0:23 * 2)) {
    age_power_cont_c <- filter(age_power_cont, YEAR == y)
    cap <- "United States"
    if (tx) {
      age_power_cont_c <- filter(age_power_cont_tx, YEAR == y)
      cap <- "Texas Only"
    }
    p <- ggplot(age_power_cont_c) +
      geom_smooth(
        aes(
          x = AGE,
          y = voter_pct,
          col = "Electorate"
        ),
        alpha = 0.75
      ) +
      geom_smooth(
        aes(
          x = AGE,
          y = pop_pct,
          col = "Voting Eligible\nPopulation"
        ),
        alpha = 0.75
      ) +
      labs(
        title = "Relative voting strength of young vs. old voters",
        subtitle = paste("For the November", y, "general election (with smoothing)"),
        x = "Age",
        y = "% of Population vs. % of Electorate",
        col = "Group",
        caption = str_c("Note: 80-84 topcoded at 80; 85+ topcoded at 85\n", cap)
      ) +
      scale_x_continuous(
        limits = c(18, 85),
        breaks = c(18, 10 * 3:8,  85)
      ) +
      scale_y_continuous(
        limits = c(0, 5),
        breaks = c(0.5 * 0:10)
      ) +
      scale_color_manual(
        values = c(
          "Voting Eligible\nPopulation" = "blue",
          "Electorate" = "red"
        )
      )
    print(p)
  }
}
