# name: plots.r
# purpose: generate plots for the disengement brief
# authors: bailey inglish, eugenia quintanilla

# libraries
library(tidyverse)
library(gtsummary)
library(haven)

# data
setwd("briefs/whyvote25")
anes <- read_csv("raw_data/anes.csv")
ces <- read_dta("raw_data/ces.dta")

# theme
ut_orange <- "#BF5700"
ut_blue <- "#005F86"
ut_teal <- "#00A9B7"
ut_gray <- "#666666"

theme_asi <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = ut_orange),
      plot.subtitle = element_text(size = 10, color = ut_gray, margin = margin(b = 10)),
      plot.caption = element_text(size = 8, color = ut_gray, hjust = 0),
      axis.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      strip.text = element_text(face = "bold", size = 10, color = ut_blue),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA)
    )
}

theme_set(theme_asi())

# data cleaning
anes_c <- anes %>%
  mutate(
    # filtering
    year = VCF0004,
    # outcome
    voted = case_when(
      # [NON-validated/self-reported voter turnout]
      VCF0703 == 3 ~ TRUE, # voted
      VCF0703 %in% c(1, 2) ~ FALSE, # did not vote or not registered
      VCF0703 == 0 ~ NA
    ),
    # covariates
    efficacy = case_when(
      # 'I don’t think public officials care much what people like me think.'
      VCF0609 == 1 ~ 0, # 'Agree' = low efficacy
      VCF0609 == 2 ~ 0.5, # 'Neither agree nor disagree' (1988 and later only) = neutral
      VCF0609 == 3 ~ 1, # 'Disagree' = high efficacy
      VCF0609 %in% c(0, 9) ~ NA_real_
    ),
    trust = case_when(
      # 'Would you say the government is pretty much run by a few big interests
      # looking out for themselves or that it is run for the benefit of all
      # the people?'
      VCF0605 == 1 ~ 0, # 'Few big interests' = low trust
      VCF0605 == 2 ~ 1, # 'Benefit of all' = high trust
      VCF0605 %in% c(0, 9) ~ NA_real_
    ),
    dem_pres_rating = case_when(
      # 'After I read the name of a presidential candidate, please rate them on a scale from 0 to 10, where 0 means you
      # strongly dislike that candidate and 10 means that you strongly like that candidate. If I come to a presidential candidate
      # you haven’t heard of or you feel you do not know enough about, just say so'
      VCF9207 < 0 | VCF9207 >= 97 ~ NA_real_,
      TRUE ~ VCF9207 # ratings from 0 = 'Strongly dislike' <-> 10 = 'Strongly like'
    ),
    rep_pres_rating = case_when(
      # 'After I read the name of a presidential candidate, please rate them on a scale from 0 to 10, where 0 means you
      # strongly dislike that candidate and 10 means that you strongly like that candidate. If I come to a presidential candidate
      # you haven’t heard of or you feel you do not know enough about, just say so'
      VCF9208 < 0 | VCF9208 >= 97 ~ NA_real_,
      TRUE ~ VCF9208 # ratings from 0 = 'Strongly dislike' <-> 10 = 'Strongly like'
    ),
    party3 = case_when(
      # 'Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?'
      VCF0303 == 1 ~ "Democrat", # Democrat or leaning Democrat
      VCF0303 == 2 ~ "Independent", # Independent only
      VCF0303 == 3 ~ "Republican", # Republican or leaning Republican
      VCF0303 == 0 ~ NA_character_
    ),
    partisan_strength = case_when(
      # [strength of partisanship]
      VCF0305 == 1 ~ 0, # Independent
      VCF0305 %in% c(2, 3) ~ 1, # Leaning independent or weak partisan
      VCF0305 == 4 ~ 2, # Strong partisan
      VCF0305 == 0 ~ NA_real_
    ),
    election_interest = case_when(
      # 'Some people don’t pay much attention to political campaigns. How
      # about you, would you say that you have been/were very much interested,
      # somewhat interested, or not much interested in (1952-1998: following)
      # the political campaigns (so far) this year?'
      VCF0310 == 1 ~ 0, # 'Not much interested' = low interest
      VCF0310 == 2 ~ 0.5, # 'Somewhat interested' = medium interest
      VCF0310 == 3 ~ 1, # 'Very much interested' = high interest
      VCF0310 %in% c(0, 9) ~ NA_real_
    ),
    # controls
    is_white_nh = case_when(
      # 'What racial or ethnic group or groups best describes you?'
      VCF0105b == 1 ~ TRUE, # 'White non-Hispanic'
      VCF0105b %in% c(0, 9) ~ NA,
      TRUE ~ FALSE, # [all other valid race/ethnicity responses]
    ),
    is_hispanic = case_when(
      # 'Are you Spanish, Hispanic, or Latino?'
      VCF0108 == 1 ~ TRUE, # 'Yes' = Hispanic
      VCF0108 == 2 ~ FALSE, # 'No' = Not Hispanic
      TRUE ~ NA # 'Don't know/Refused'
    ),
    gender = case_when(
      # [respondent gender]
      VCF0104 == 1 ~ "Male",
      VCF0104 == 2 ~ "Female",
      VCF0104 == 3 ~ "Other",
      VCF0104 == 0 ~ NA_character_
    ),
    age = case_when(
      # 'What is the month, day and year of your birth?' -> [age in years at time of survey]
      VCF0101 <= 0 ~ NA_real_,
      TRUE ~ VCF0101 # [age in years at time of survey]
    ),
    income_percentile = case_when(
      # 'Please mark the answer that includes the income of all members of your family
      # living here in [year] before taxes.' -> [inflation-adjusted income percentile]
      VCF0114 == 1 ~ "0-16",
      VCF0114 == 2 ~ "17-33",
      VCF0114 == 3 ~ "34-67",
      VCF0114 == 4 ~ "68-95",
      VCF0114 == 5 ~ "96-100",
      VCF0114 == 0 ~ NA_character_
    ),
    education = case_when(
      # 'What is the highest level of school you have completed or the
      # highest degree you have received?'
      VCF0110 == 1 ~ "Grade school or less",
      VCF0110 == 2 ~ "Any high school",
      VCF0110 == 3 ~ "Some college",
      VCF0110 == 4 ~ "College or advanced degree",
      VCF0110 == 0 ~ NA_character_
    ),
    # recodes
    dislike_own_party = case_when(
      # [dislike of own party's presidential candidate] - negative values = likes own party canidate -> 0 (does not dislike)
      party3 == "Democrat" ~ if_else(5 - dem_pres_rating < 0, 0, 5 - dem_pres_rating), # reverse code so higher values = more dislike
      party3 == "Republican" ~ if_else(5 - rep_pres_rating < 0, 0, 5 - rep_pres_rating), # reverse code so higher values = more dislike
      TRUE ~ NA_real_ # [Independent or missing party ID]
    ),
    dislike_other_party = case_when(
      # [dislike of other party's presidential candidate] - negative values = likes other party canidate -> 0 (does not dislike)
      party3 == "Democrat" ~ if_else(5 - rep_pres_rating < 0, 0, 5 - rep_pres_rating), # reverse code so higher values = more dislike
      party3 == "Republican" ~ if_else(5 - dem_pres_rating < 0, 0, 5 - dem_pres_rating), # reverse code so higher values = more dislike
      TRUE ~ NA_real_ # [Independent or missing party ID]
    ),
    dislike_both_party = dislike_other_party > 0 & dislike_own_party > 0 # [dislike of both parties' presidential candidates]
  )
