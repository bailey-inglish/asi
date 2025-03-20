# File name: ballots.r
# Purpose:   Converts a massive trove of JSON files to CSV files, while also
#            filtering for just variables of interest and tidying up.
# Author:    Bailey Inglish

# Libaries
library(tidyverse)
library(rjson)

# Directory
setwd("midnight_sun")

## Manifests
# bc: Blank Check - ensures empty lists are counted as NA rather than lst()
bc <- function(value) {
  if (is.list(value)) {
    return(NA)
  }
  return(value)
}

# Candidate Manifest
CandidateManifest <- fromJSON(file = "manifests/CandidateManifest.json")[[2]]
candidates <- tibble(
  candidate_name = character(),
  candidate_id = integer(),
  contest_id = integer(),
  party_id = integer()
)

for (i in seq_len(length(CandidateManifest))) {
  cand_info <- CandidateManifest[[i]]
  cand_info_tib <- tibble(
    candidate_name = cand_info$Description[1],
    candidate_id = cand_info$Id[1],
    contest_id = cand_info$ContestId[1],
    party_id = bc(cand_info$Affiliations[1])
  )
  candidates <- rows_append(candidates, cand_info_tib)
}
