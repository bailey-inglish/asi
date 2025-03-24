# File name: ballots.r
# Purpose:   Converts a massive trove of JSON files to CSV files, while also
#            tidying up!
# Author:    Bailey Inglish

# Libaries
library(tidyverse)
library(rjson)

# Directory
setwd("midnight_sun")

## Manifests
# bc: Blank Check - ensures empty lists are counted as "None" rather than lst()
bc <- function(value) {
  if (is.list(value)) {
    return("None")
  }
  return(value)
}


# BallotType Manifest
BallotTypeManifestRaw <- fromJSON(file = "manifests/BallotTypeManifest.json")[[2]]
BallotTypeM2 <- tibble(
  BallotTypeName = character(),
  BallotTypeId = integer()
)

for (i in seq_len(length(BallotTypeManifestRaw))) {
  info <- BallotTypeManifestRaw[[i]]
  info_tib <- tibble(
    BallotTypeName = info$Description[1],
    BallotTypeId = info$Id[1],
  )
  BallotTypeM2 <- rows_append(BallotTypeM2, info_tib)
}

# Candidate Manifest
CandidateManifestRaw <- fromJSON(file = "manifests/CandidateManifest.json")[[2]]
CandidateM2 <- tibble(
  CandidateName = character(),
  CandidateId = integer(),
  ContestId = integer(),
  PartyId = integer()
)

for (i in seq_len(length(CandidateManifestRaw))) {
  info <- CandidateManifestRaw[[i]]
  info_tib <- tibble(
    CandidateName = info$Description[1],
    CandidateId = info$Id[1],
    ContestId = info$ContestId[1],
    PartyId = bc(info$Affiliations[1])
  )
  CandidateM2 <- rows_append(CandidateM2, info_tib)
}


# Contest Manifest
ContestManifestRaw <- fromJSON(file = "manifests/ContestManifest.json")[[2]]
ContestM2 <- tibble(
  ContestName = character(),
  ContestId = integer(),
  DistrictId = integer(), # won't need to use this
  VoteFor = integer(),
  NumOfRanks = integer(),
  NumOfWriteins = integer(),
  ContestType = character()
)

for (i in seq_len(length(ContestManifestRaw))) {
  info <- ContestManifestRaw[[i]]
  info_tib <- tibble(
    ContestName = info$Description[1],
    ContestId = info$Id[1],
    DistrictId = info$DistrictId[1], # won't need to use this
    VoteFor = info$VoteFor[1],
    NumOfRanks = info$NumOfRanks[1],
    NumOfWriteins = info$NumOfWriteins[1],
    ContestType = info$Type[1]
  )
  ContestM2 <- rows_append(ContestM2, info_tib)
}

# CountingGroup Manifest
CountingGroupM2 <- tribble(
  ~CountingGroupId, ~CountingGroupName,
  1, "Election Day",
  2, "Absentee",
  3, "Early Voting",
  4, "Question",
  5, "Remote"
)

# DistrictPrecinctPortion Manifest
DistrictPrecinctPortionManifestRaw <- fromJSON(file = "manifests/DistrictPrecinctPortionManifest.json")[[2]]
DistrictPrecinctPortionM2 <- tibble(
  DistrictId = integer(),
  PrecinctPortionId = integer()
)

for (i in seq_len(length(DistrictPrecinctPortionManifestRaw))) {
  info <- DistrictPrecinctPortionManifestRaw[[i]]
  info_tib <- tibble(
    DistrictId = info$DistrictId[1],
    PrecinctPortionId = info$PrecinctPortionId[1]
  )
  DistrictPrecinctPortionM2 <- rows_append(DistrictPrecinctPortionM2, info_tib)
}

# District Manifest
DistrictManifestRaw <- fromJSON(file = "manifests/DistrictManifest.json")[[2]]
DistrictM2 <- tibble(
  DistrictName = character(),
  DistrictId = integer(),
  DistrictTypeId = character()
)

for (i in seq_len(length(DistrictManifestRaw))) {
  info <- DistrictManifestRaw[[i]]
  info_tib <- tibble(
    DistrictName = info$Description[1],
    DistrictId = info$Id[1],
    DistrictTypeId = info$DistrictTypeId[1]
  )
  DistrictM2 <- rows_append(DistrictM2, info_tib)
}

# DistrictType Manifest
DistrictTypeManifestRaw <- fromJSON(file = "manifests/DistrictTypeManifest.json")[[2]]
DistrictTypeM2 <- tibble(
  DistrictTypeName = character(),
  DistrictTypeId = character()
)

for (i in seq_len(length(DistrictTypeManifestRaw))) {
  info <- DistrictTypeManifestRaw[[i]]
  info_tib <- tibble(
    DistrictTypeName = info$Description[1],
    DistrictTypeId = info$Id[1]
  )
  DistrictTypeM2 <- rows_append(DistrictTypeM2, info_tib)
}

# OutstackCondition Manifest
OutstackConditionManifestRaw <- fromJSON(file = "manifests/OutstackConditionManifest.json")[[2]]
OutstackConditionM2 <- tibble(
  OutstackConditionName = character(),
  OutstackConditionId = integer()
)

for (i in seq_len(length(OutstackConditionManifestRaw))) {
  info <- OutstackConditionManifestRaw[[i]]
  info_tib <- tibble(
    OutstackConditionName = info$Description[1],
    OutstackConditionId = info$Id[1]
  )
  OutstackConditionM2 <- rows_append(OutstackConditionM2, info_tib)
}

# Party Manifest
PartyManifestRaw <- fromJSON(file = "manifests/PartyManifest.json")[[2]]
PartyM2 <- tibble(
  PartyName = character(),
  PartyId = integer()
)

for (i in seq_len(length(PartyManifestRaw))) {
  info <- PartyManifestRaw[[i]]
  info_tib <- tibble(
    PartyName = info$Description[1],
    PartyId = info$Id[1]
  )
  PartyM2 <- rows_append(PartyM2, info_tib)
}

# Precinct Manifest
PrecinctManifestRaw <- fromJSON(file = "manifests/PrecinctManifest.json")[[2]]
PrecinctM2 <- tibble(
  PrecinctName = character(),
  PrecinctId = integer(),
  PrecinctExternalId = character()
)

for (i in seq_len(length(PrecinctManifestRaw))) {
  info <- PrecinctManifestRaw[[i]]
  info_tib <- tibble(
    PrecinctName = info$Description[1],
    PrecinctId = info$Id[1],
    PrecinctExternalId = info$ExternalId[1]
  )
  PrecinctM2 <- rows_append(PrecinctM2, info_tib)
}

# PrecinctPortion Manifest
PrecinctPortionManifestRaw <- fromJSON(file = "manifests/PrecinctPortionManifest.json")[[2]]
PrecinctPortionM2 <- tibble(
  PrecinctPortionName = character(),
  PrecinctPortionId = integer(),
  PrecinctExternalId = character(),
  PrecinctId = integer()
)

for (i in seq_len(length(PrecinctPortionManifestRaw))) {
  info <- PrecinctPortionManifestRaw[[i]]
  info_tib <- tibble(
    PrecinctPortionName = info$Description[1],
    PrecinctPortionId = info$Id[1],
    PrecinctExternalId = info$ExternalId[1],
    PrecinctId = info$PrecinctId[1]
  )
  PrecinctPortionM2 <- rows_append(PrecinctPortionM2, info_tib)
}

# Tabulator Manifest
TabulatorManifestRaw <- fromJSON(file = "manifests/TabulatorManifest.json")[[2]]
TabulatorM2 <- tibble(
  TabulatorName = character(),
  TabulatorId = integer(),
  VotingLocationNumber = integer(),
  VotingLocationName = character(),
  TabulatorType = character(),
  TabulatorThresholdMin = integer(),
  TabulatorThresholdMax = integer(),
  TabulatorWriteInThresholdMin = integer(),
  TabulatorWriteInThresholdMax = integer()
)

for (i in seq_len(length(TabulatorManifestRaw))) {
  info <- TabulatorManifestRaw[[i]]
  info_tib <- tibble(
    TabulatorName = info$Description[1],
    TabulatorId = info$Id[1],
    VotingLocationNumber = info$VotingLocationNumber[1],
    VotingLocationName = info$VotingLocationName[1],
    TabulatorType = info$Type[1],
    TabulatorThresholdMin = info$ThresholdMin[1],
    TabulatorThresholdMax = info$ThresholdMax[1],
    TabulatorWriteInThresholdMin = info$WriteThresholdMin[1],
    TabulatorWriteInThresholdMax = info$WriteThresholdMax[1]
  )
  TabulatorM2 <- rows_append(TabulatorM2, info_tib)
}

## Reframing data from JSON files using the manifests
cvr <- tibble(
  TabulatorId = integer(),
  TabulatorName = character(),#>TabulatorM2
  VotingLocationNumber = integer(),
  VotingLocationName = character(),
  TabulatorType = character(),
  TabulatorThresholdMin = integer(),
  TabulatorThresholdMax = integer(),
  TabulatorWriteInThresholdMin = integer(),
  TabulatorWriteInThresholdMax = integer(),#^
  BatchId = integer(),
  RecordId = integer(),
  CountingGroupId = integer(),
  CountingGroupName = character(),#CountingGroupM2
  ImageMask = character(),
  SessionType = character(),
  VotingSessionIdentifier = character(),
  UniqueVotingIdentifier = character(),
  PrecinctPortionId = integer(),
  PrecinctPortionName = character(),#>PrecinctPortionM2
  PrecinctExternalId = character(),
  PrecinctId = integer(),#^
  PrecinctName = character(),
  BallotTypeId = integer(),
  BallotTypeName = character(),#BallotTypeM2
  IsCurrent = logical(), #<<<<< end of bl data
  Pres1Id = integer(),
  Pres1Name = character(),
  Pres1PartyId = integer(),
  Pres1PartyName = integer(),
  Pres2Id = integer(),
  Pres2Name = character(),
  Pres2PartyId = integer(),
  Pres2PartyName = integer(),
  Pres3Id = integer(),
  Pres3Name = character(),
  Pres3PartyId = integer(),
  Pres3PartyName = integer(),
  Pres4Id = integer(),
  Pres4Name = character(),
  Pres4PartyId = integer(),
  Pres4PartyName = integer(),
  Pres5Id = integer(),
  Pres5Name = character(),
  Pres5PartyId = integer(),
  Pres5PartyName = integer(),
  Pres6Id = integer(),
  Pres6Name = character(),
  Pres6PartyId = integer(),
  Pres6PartyName = integer(),
  Pres7Id = integer(),
  Pres7Name = character(),
  Pres7PartyId = integer(),
  Pres7PartyName = integer(),
  Pres8Id = integer(),
  Pres8Name = character(),
  Pres8PartyId = integer(),
  Pres8PartyName = integer(),
  Rep1Id = integer(),
  Rep1Name = character(),
  Rep1PartyId = integer(),
  Rep1PartyName = integer(),
  Rep2Id = integer(),
  Rep2Name = character(),
  Rep2PartyId = integer(),
  Rep2PartyName = integer(),
  Rep3Id = integer(),
  Rep3Name = character(),
  Rep3PartyId = integer(),
  Rep3PartyName = integer(),
  Rep4Id = integer(),
  Rep4Name = character(),
  Rep4PartyId = integer(),
  Rep4PartyName = integer(),
  Rep5Id = integer(),
  Rep5Name = character(),
  Rep5PartyId = integer(),
  Rep5PartyName = integer(),
  SDContestId = integer(),
  SDContestName = character(),
  SD1Id = integer(),
  SD1Name = character(),
  SD1PartyId = integer(),
  SD1PartyName = integer(),
  SD2Id = integer(),
  SD2Name = character(),
  SD2PartyId = integer(),
  SD2PartyName = integer(),
  SD3Id = integer(),
  SD3Name = character(),
  SD3PartyId = integer(),
  SD3PartyName = integer(),
  SD4Id = integer(),
  SD4Name = character(),
  SD4PartyId = integer(),
  SD4PartyName = integer(),
  HDContestId = integer(),
  HDContestName = character(),
  HD1Id = integer(),
  HD1Name = character(),
  HD1PartyId = integer(),
  HD1PartyName = integer(),
  HD2Id = integer(),
  HD2Name = character(),
  HD2PartyId = integer(),
  HD2PartyName = integer(),
  HD3Id = integer(),
  HD3Name = character(),
  HD3PartyId = integer(),
  HD3PartyName = integer(),
  HD4Id = integer(),
  HD4Name = character(),
  HD4PartyId = integer(),
  HD4PartyName = integer(),
  HD5Id = integer(),
  HD5Name = character(),
  HD5PartyId = integer(),
  HD5PartyName = integer(),
  BM1Id = integer(),
  BM1Name = character(),
  BM2Id = integer(),
  BBM2Name = character(),
  SC1Id = integer(),
  SC1Name = character(),
  SC2Id = integer(),
  SC2Name = character(),
  COA1Id = integer(),
  COA1Name = character(),
  COA2Id = integer(),
  COA2Name = character(),
  DIST_JD1Id = integer(),
  DIST_JD1Name = character(),
  SUP_JD3_1Id = integer(),
  SUP_JD3_1Name = character(),
  SUP_JD3_2Id = integer(),
  SUP_JD3_2Name = character(),
  SUP_JD3_3Id = integer(),
  SUP_JD3_3Name = character(),
  SUP_JD3_4Id = integer(),
  SUP_JD3_4Name = character(),
  DIST_JD3_1Id = integer(),
  DIST_JD3_1Name = character(),
  DIST_JD3_2Id = integer(),
  DIST_JD3_2Name = character(),
  DIST_JD3_3Id = integer(),
  DIST_JD3_3Name = character(),
  DIST_JD3_4Id = integer(),
  DIST_JD3_4Name = character(),
  DIST_JD3_5Id = integer(),
  DIST_JD3_5Name = character(),
  DIST_JD3_6Id = integer(),
  DIST_JD3_6Name = character(),
  DIST_JD3_7Id = integer(),
  DIST_JD3_7Name = character(),
  SUP_JD4Id = integer(),
  SUP_JD4Name = character(),
  DIST_JD4_1Id = integer(),
  DIST_JD4_1Name = character(),
  DIST_JD4_2Id = integer(),
  DIST_JD4_2Name = character()
)

conv <- function(M2, new_col_name, known_col_name, known_col_val) {
  return(M2[M2[, known_col_name] == known_col_val, new_col_name][[1]])
}

cvr <- tibble(
  TabulatorId = integer(),
  TabulatorName = character(),#>TabulatorM2
  VotingLocationNumber = integer(),
  VotingLocationName = character(),
  TabulatorType = character(),
  TabulatorThresholdMin = integer(),
  TabulatorThresholdMax = integer(),
  TabulatorWriteInThresholdMin = integer(),
  TabulatorWriteInThresholdMax = integer(),#^
  BatchId = integer(),
  RecordId = integer(),
  CountingGroupId = integer(),
  CountingGroupName = character(),#CountingGroupM2
  ImageMask = character(),
  SessionType = character(),
  VotingSessionIdentifier = character(),
  UniqueVotingIdentifier = character(),
  PrecinctPortionId = integer(),
  PrecinctPortionName = character(),#>PrecinctPortionM2
  PrecinctExternalId = character(),
  PrecinctId = integer(),#^
  PrecinctName = character(),
  BallotTypeId = integer(),
  BallotTypeName = character(),#BallotTypeM2
  IsCurrent = logical()
)
for (i in 18:2046) {
  current_batch <- fromJSON(file = str_c("cvr/CvrExport_", i, ".json"))[[3]]
  for (j in seq_along(current_batch)) {
    current_session <- current_batch[[j]]
    # General data
    tab_id <- current_session$TabulatorId
    cg_id <- current_session$CountingGroupId
    pp_id <- current_session$Original$PrecinctPortionId
    p_id <- conv(PrecinctPortionM2, "PrecinctId", "PrecinctPortionId", pp_id)
    bt_id <- current_session$Original$BallotTypeId
    ballot <- tibble(
      TabulatorId = tab_id,
      TabulatorName = conv(TabulatorM2, "TabulatorName", "TabulatorId", tab_id),
      VotingLocationNumber = conv(TabulatorM2, "VotingLocationNumber", "TabulatorId", tab_id),
      VotingLocationName = conv(TabulatorM2, "VotingLocationName", "TabulatorId", tab_id),
      TabulatorType = conv(TabulatorM2, "TabulatorType", "TabulatorId", tab_id),
      TabulatorThresholdMin = conv(TabulatorM2, "TabulatorThresholdMin", "TabulatorId", tab_id),
      TabulatorThresholdMax = conv(TabulatorM2, "TabulatorThresholdMax", "TabulatorId", tab_id),
      TabulatorWriteInThresholdMin = conv(TabulatorM2, "TabulatorWriteInThresholdMin", "TabulatorId", tab_id),
      TabulatorWriteInThresholdMax = conv(TabulatorM2, "TabulatorWriteInThresholdMax", "TabulatorId", tab_id),
      BatchId = current_session$BatchId,
      RecordId = current_session$RecordId,
      CountingGroupId = cg_id,
      CountingGroupName = conv(CountingGroupM2, "CountingGroupName", "CountingGroupId", cg_id),
      ImageMask = current_session$ImageMask,
      SessionType = current_session$SessionType,
      VotingSessionIdentifier = current_session$VotingSessionIdentifier,
      UniqueVotingIdentifier = current_session$UniqueVotingIdentifier,
      PrecinctPortionId = pp_id,
      PrecinctPortionName = conv(PrecinctPortionM2, "PrecinctPortionName", "PrecinctPortionId", pp_id),
      PrecinctId = p_id,
      PrecinctName = conv(PrecinctM2, "PrecinctName", "PrecinctId", p_id),
      PrecinctExternalId = conv(PrecinctM2, "PrecinctExternalId", "PrecinctId", p_id),
      BallotTypeId = bt_id,
      BallotTypeName = conv(BallotTypeM2, "BallotTypeName", "BallotTypeId", bt_id),
      IsCurrent = current_session$Original$IsCurrent
    )
    cvr <- rows_append(cvr, ballot)
  }
  print(paste(i, "is done"))
}
