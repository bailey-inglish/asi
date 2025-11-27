# File name: analysis.r
# Project:   midnight_sun
# Purpose:   Do some fun tricks with AK CVR data. Not especially organized.
# Author:    Bailey Inglish
# Source(s): general_elections_2024.csv <- Alaska Elections Division

## Setup
# Libaries
library(tidyverse)

# Directory
setwd("midnight_sun")

# Files
ge <- read_csv("general_election_2024.csv")

## Scripts
# Crossover voting
ge_c <- filter(ge, is.element(Pres1Id, c(542, 543)), !is.na(BM2Name)) %>%
  mutate(crossover = (Pres1Id == 542 & BM2Name == "YES") | (Pres1Id == 543 & BM2Name == "NO"))

tot_cross_prop <- sum(ge_c$crossover) / nrow(ge_c)
blank_prop <- sum(ge_c$BM2Name == "[Blank]") / nrow(ge_c)
dem_prop <- sum(ge_c$Pres1Id == 542 & ge_c$crossover == TRUE) / sum(ge_c$Pres1Id == 542)
rep_prop <- sum(ge_c$Pres1Id == 543 & ge_c$crossover == TRUE) / sum(ge_c$Pres1Id == 543)

ge_tp <- filter(ge, is.element(Pres1Id, c(554, 540, 547, 545, 546, 541)), !is.na(BM2Name))
third_party_prop <- sum(ge_tp$BM2Name == "NO") / nrow(ge_tp)

# Intra-party support for rcv
rep_rcv_maintain <- nrow(filter(ge, Pres1Id == 543, BM2Name == "NO")) / nrow(filter(ge, Pres1Id == 543))
dem_rcv_maintain <- nrow(filter(ge, Pres1Id == 542, BM2Name == "NO")) / nrow(filter(ge, Pres1Id == 542))

# House/senate district review
sen <- filter(
  ge,
  !is.na(BM2Name),
  !is.na(SDContestName),
  SD1Name != "[Blank]"
) %>%
  group_by(SDContestName) %>%
  summarize(pct_support_rcv = sum(BM2Name == "NO") / sum(BM2Name != "[Blank]")) %>%
  arrange(desc(pct_support_rcv))

house <- filter(
  ge,
  !is.na(BM2Name),
  !is.na(HDContestName),
  HD1Name != "[Blank]"
) %>%
  group_by(HDContestName) %>%
  summarize(pct_support_rcv = sum(BM2Name == "NO") / sum(BM2Name != "[Blank]")) %>%
  arrange(desc(pct_support_rcv))

# House/senate district review vs. Trump
sen <- filter(
  ge,
  !is.na(BM2Name),
  !is.na(SDContestName),
  !is.na(Pres1Id)
) %>%
  group_by(SDContestName) %>%
  summarize(
    support_rcv = sum(BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    support_trump = sum(Pres1Id == 543) / sum(Pres1Name != "[Blank]"),
    support_harris = sum(Pres1Id == 542) / sum(Pres1Name != "[Blank]"),
    support_rfk = sum(Pres1Id == 554) / sum(Pres1Name != "[Blank]"),
    support_other = sum(!is.element(Pres1Id, c(543, 542, 554))) / sum(Pres1Name != "[Blank]"),
    trump_and_rcv = sum(Pres1Id == 543 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    harris_and_rcv = sum(Pres1Id == 542 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    rfk_and_rcv = sum(Pres1Id == 554 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    trump_no_rcv = sum(Pres1Id == 543 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    harris_no_rcv = sum(Pres1Id == 542 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    rfk_no_rcv = sum(Pres1Id == 554 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    other_and_rcv = sum(!is.element(Pres1Id, c(542, 543, 554)) & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    other_no_rcv = sum(!is.element(Pres1Id, c(542, 543, 554)) & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    pres_ballots_counted = sum(Pres1Name != "[Blank]"),
    rcv_ballots_counted = sum(BM2Name != "[Blank]")
  )

house <- filter(
  ge,
  !is.na(BM2Name),
  !is.na(HDContestName),
  !is.na(Pres1Id)
) %>%
  group_by(HDContestName) %>%
  summarize(
    support_rcv = sum(BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    support_trump = sum(Pres1Id == 543) / sum(Pres1Name != "[Blank]"),
    support_harris = sum(Pres1Id == 542) / sum(Pres1Name != "[Blank]"),
    support_rfk = sum(Pres1Id == 554) / sum(Pres1Name != "[Blank]"),
    support_other = sum(!is.element(Pres1Id, c(543, 542, 554))) / sum(Pres1Name != "[Blank]"),
    trump_and_rcv = sum(Pres1Id == 543 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    harris_and_rcv = sum(Pres1Id == 542 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    rfk_and_rcv = sum(Pres1Id == 554 & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    trump_no_rcv = sum(Pres1Id == 543 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    harris_no_rcv = sum(Pres1Id == 542 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    rfk_no_rcv = sum(Pres1Id == 554 & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    other_and_rcv = sum(!is.element(Pres1Id, c(542, 543, 554)) & BM2Name == "NO") / sum(BM2Name != "[Blank]"),
    other_no_rcv = sum(!is.element(Pres1Id, c(542, 543, 554)) & BM2Name == "YES") / sum(BM2Name != "[Blank]"),
    pres_ballots_counted = sum(Pres1Name != "[Blank]"),
    rcv_ballots_counted = sum(BM2Name != "[Blank]")
  )

for (i in seq_len(nrow(house))) {
  if (nchar(house$HDContestName[i]) == 16) {
    house$HDContestName[i] <- str_replace(house$HDContestName[i], "House District ", "House District 0")
  }
}

house <- arrange(house, HDContestName)

## Graphs
ge_safe <- filter(ge, !is.na(Pres1Name), !is.na(BM2Name))

# Overall prefs
group_by(
  ge_safe,
  Pres1Name,
  BM2Name
) %>%
  reframe(
    vote_pct = 100 * n() / nrow(ge_safe)
  ) %>%
  ggplot() +
  geom_col(
    aes(
      x = fct_reorder(
        Pres1Name,
        desc(vote_pct)
      ),
      y = vote_pct,
      fill = BM2Name
    ),
    width = 0.5
  ) +
  scale_y_continuous(
    breaks = 0:11 * 5
  ) +
  labs(
    title = "Attitudes Toward RCV by Presidential Preference",
    x = "First Choice Presidential Candidate",
    y = "% of Votes Cast",
    fill = "Prop. 2:\nEliminate RCV?"
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

group_by(
  ge_safe,
  Pres1Name,
  BM2Name
) %>%
  reframe(
    vote_pct = 100 * n() / nrow(ge_safe)
  ) %>%
  ggplot() +
  geom_col(
    aes(
      x = fct_reorder(
        Pres1Name,
        desc(vote_pct)
      ),
      y = vote_pct,
      fill = BM2Name
    ),
    width = 0.5,
    position = "dodge"
  ) +
  scale_y_continuous(
    breaks = 0:11 * 5
  ) +
  labs(
    title = "Attitudes Toward RCV by Presidential Preference",
    x = "First Choice Presidential Candidate",
    y = "% of Votes Cast",
    fill = "Prop. 2:\nEliminate RCV?"
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Share of RCV cause
group_by(
  ge_safe,
  Pres1Name,
  BM2Name
) %>%
  reframe(
    vote_pct = 100 * n() / nrow(ge_safe)
  ) %>%
  ggplot() +
  geom_col(
    aes(
      x = fct_reorder(
        BM2Name,
        desc(vote_pct)
      ),
      y = vote_pct,
      fill = fct_reorder(
        Pres1Name,
        desc(vote_pct)
      )
    ),
    width = 0.5
  ) +
  scale_y_continuous(
    breaks = 0:11 * 5
  ) +
  labs(
    title = "Presidential Preference by Attitudes Toward RCV",
    x = "Prop. 2: Eliminate RCV?",
    y = "% of Votes Cast",
    fill = "First Choice\nPresidential Candidate"
  )

group_by(
  ge_safe,
  Pres1Name,
  BM2Name
) %>%
  reframe(
    vote_pct = 100 * n() / nrow(ge_safe)
  ) %>%
  ggplot() +
  geom_col(
    aes(
      x = fct_reorder(
        BM2Name,
        desc(vote_pct)
      ),
      y = vote_pct,
      fill = fct_reorder(
        Pres1Name,
        desc(vote_pct)
      )
    ),
    width = 0.5,
    position = "dodge"
  ) +
  scale_y_continuous(
    breaks = 0:11 * 5
  ) +
  labs(
    title = "Presidential Preference by Attitudes Toward RCV",
    x = "Prop. 2: Eliminate RCV?",
    y = "% of Votes Cast",
    fill = "First Choice\nPresidential Candidate"
  )

## Hypotheticals
# People who didn't vote for pres and just voted RCV didn't vote
ge_nb <- filter(
  ge,
  !is.na(Pres1Name),
  !is.na(BM2Name),
  Pres1Name != "[Blank]",
  BM2Name != "[Blank]"
)
group_by(ge_nb, BM2Name) %>%
summarize(n = n(), pct = n() / nrow(ge_nb)) # 173 votes in favor of retaining RCV after blank pres elimination

precincts <- ge %>%
  mutate(
    location = VotingLocationName,
    precinct = PrecinctExternalId,
    district = HDContestName,
    district_number = as.integer(str_remove(HDContestName, "House District ")),
    pres = Pres1Name,
    remove_rcv = BM2Name
  ) %>%
  filter(
    is.element(district_number, c(40, 39, 38, 37, 18))
  ) %>%
  group_by(precinct) %>%
  summarize(
    district = first(district),
    total_votes = n(),
    cross_no = sum(pres == "Trump/Vance" & remove_rcv == "NO"),
    cross_yes = sum(pres != "Trump/Vance" & remove_rcv == "YES"),
    pct_cross = sum(cross_no, cross_yes) / n(),
    pres_trump = sum(pres == "Trump/Vance"),
    pres_harris = sum(pres == "Harris/Walz"),
    pres_other = sum(!is.element(pres, c("Trump/Vance", "Harris/Walz", "[Blank]"))),
    rcv_remove = sum(remove_rcv == "YES"),
    rcv_retain = sum(remove_rcv == "NO")
  ) %>%
  arrange(desc(pct_cross))
