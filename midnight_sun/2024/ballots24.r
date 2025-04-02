# File name: ballots24.r
# Project:   midnight_sun
# Purpose:   Converts a massive trove of 2024 JSON files to CSV files, while
#            also tidying up! Takes forever to run though :(
# Author:    Bailey Inglish
# Source(s): Alaska Elections Division

## Setup
# Libaries
library(tidyverse)
library(rjson)

# Directory
setwd("midnight_sun/2024")

## Manifests
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

# bc: Blank Check - ensures empty lists are counted as 1 (Nonpartisan) rather than lst()
bc <- function(value) {
  if (is.list(value)) {
    return(1)
  }
  return(value)
}

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
cvr_template <- tibble(
  TabulatorId = integer(),
  TabulatorName = character(),
  VotingLocationNumber = integer(),
  VotingLocationName = character(),
  TabulatorType = character(),
  TabulatorThresholdMin = integer(),
  TabulatorThresholdMax = integer(),
  TabulatorWriteInThresholdMin = integer(),
  TabulatorWriteInThresholdMax = integer(),
  BatchId = integer(),
  RecordId = integer(),
  CountingGroupId = integer(),
  CountingGroupName = character(),
  ImageMask = character(),
  SessionType = character(),
  VotingSessionIdentifier = character(),
  UniqueVotingIdentifier = character(),
  PrecinctPortionId = integer(),
  PrecinctPortionName = character(),
  PrecinctExternalId = character(),
  PrecinctId = integer(),
  PrecinctName = character(),
  BallotTypeId = integer(),
  BallotTypeName = character(),
  IsCurrent = logical(),
  Pres1Id = integer(),
  Pres1Name = character(),
  Pres1PartyId = integer(),
  Pres1PartyName = character(),
  Pres2Id = integer(),
  Pres2Name = character(),
  Pres2PartyId = integer(),
  Pres2PartyName = character(),
  Pres3Id = integer(),
  Pres3Name = character(),
  Pres3PartyId = integer(),
  Pres3PartyName = character(),
  Pres4Id = integer(),
  Pres4Name = character(),
  Pres4PartyId = integer(),
  Pres4PartyName = character(),
  Pres5Id = integer(),
  Pres5Name = character(),
  Pres5PartyId = integer(),
  Pres5PartyName = character(),
  Pres6Id = integer(),
  Pres6Name = character(),
  Pres6PartyId = integer(),
  Pres6PartyName = character(),
  Pres7Id = integer(),
  Pres7Name = character(),
  Pres7PartyId = integer(),
  Pres7PartyName = character(),
  Pres8Id = integer(),
  Pres8Name = character(),
  Pres8PartyId = integer(),
  Pres8PartyName = character(),
  Rep1Id = integer(),
  Rep1Name = character(),
  Rep1PartyId = integer(),
  Rep1PartyName = character(),
  Rep2Id = integer(),
  Rep2Name = character(),
  Rep2PartyId = integer(),
  Rep2PartyName = character(),
  Rep3Id = integer(),
  Rep3Name = character(),
  Rep3PartyId = integer(),
  Rep3PartyName = character(),
  Rep4Id = integer(),
  Rep4Name = character(),
  Rep4PartyId = integer(),
  Rep4PartyName = character(),
  Rep5Id = integer(),
  Rep5Name = character(),
  Rep5PartyId = integer(),
  Rep5PartyName = character(),
  SDContestId = integer(),
  SDContestName = character(),
  SD1Id = integer(),
  SD1Name = character(),
  SD1PartyId = integer(),
  SD1PartyName = character(),
  SD2Id = integer(),
  SD2Name = character(),
  SD2PartyId = integer(),
  SD2PartyName = character(),
  SD3Id = integer(),
  SD3Name = character(),
  SD3PartyId = integer(),
  SD3PartyName = character(),
  SD4Id = integer(),
  SD4Name = character(),
  SD4PartyId = integer(),
  SD4PartyName = character(),
  HDContestId = integer(),
  HDContestName = character(),
  HD1Id = integer(),
  HD1Name = character(),
  HD1PartyId = integer(),
  HD1PartyName = character(),
  HD2Id = integer(),
  HD2Name = character(),
  HD2PartyId = integer(),
  HD2PartyName = character(),
  HD3Id = integer(),
  HD3Name = character(),
  HD3PartyId = integer(),
  HD3PartyName = character(),
  HD4Id = integer(),
  HD4Name = character(),
  HD4PartyId = integer(),
  HD4PartyName = character(),
  HD5Id = integer(),
  HD5Name = character(),
  HD5PartyId = integer(),
  HD5PartyName = character(),
  BM1Id = integer(),
  BM1Name = character(),
  BM2Id = integer(),
  BM2Name = character(),
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

measure_vs_contest <- tribble(
  ~MeasureColName, ~ContestId,
  "BM1", 68,
  "BM2", 69,
  "SC1", 71,
  "SC2", 72,
  "COA1", 76,
  "COA2", 77,
  "DIST_JD1", 85,
  "SUP_JD3_1", 90,
  "SUP_JD3_2", 91,
  "SUP_JD3_3", 92,
  "SUP_JD3_4", 93,
  "DIST_JD3_1", 116,
  "DIST_JD3_2", 117,
  "DIST_JD3_3", 118,
  "DIST_JD3_4", 119,
  "DIST_JD3_5", 120,
  "DIST_JD3_6", 121,
  "DIST_JD3_7", 122,
  "SUP_JD4", 129,
  "DIST_JD4_1", 137,
  "DIST_JD4_2", 138
)

conv <- function(M2, new_col_name, known_col_name, known_col_val) {
  return(M2[M2[, known_col_name] == known_col_val, new_col_name][[1]])
}

check_val <- function(n, Marks) {
  if (n <= length(Marks)) {
    if (Marks[[n]]$IsVote == TRUE) {
      return(TRUE)
    }
  }
  return(FALSE)
}

batches <- list(18:20) #list(18:100, 101:200, 201:300, 301:400, 401:580, 582:600, 601:700, 701:800, 801:900, 901:1000, 1001:1100, 1101: 1200, 1201:1300, 1301:1400, 1401:1500, 1501:1600, 1601:1700, 1701:1800, 1801:1900, 1901:2046)

for (b in batches) {
  cvr <- cvr_template
  for (i in b) {
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
        IsCurrent = current_session$Original$IsCurrent,
        Pres1Id = NA,
        Pres1Name = NA,
        Pres1PartyId = NA,
        Pres1PartyName = NA,
        Pres2Id = NA,
        Pres2Name = NA,
        Pres2PartyId = NA,
        Pres2PartyName = NA,
        Pres3Id = NA,
        Pres3Name = NA,
        Pres3PartyId = NA,
        Pres3PartyName = NA,
        Pres4Id = NA,
        Pres4Name = NA,
        Pres4PartyId = NA,
        Pres4PartyName = NA,
        Pres5Id = NA,
        Pres5Name = NA,
        Pres5PartyId = NA,
        Pres5PartyName = NA,
        Pres6Id = NA,
        Pres6Name = NA,
        Pres6PartyId = NA,
        Pres6PartyName = NA,
        Pres7Id = NA,
        Pres7Name = NA,
        Pres7PartyId = NA,
        Pres7PartyName = NA,
        Pres8Id = NA,
        Pres8Name = NA,
        Pres8PartyId = NA,
        Pres8PartyName = NA,
        Rep1Id = NA,
        Rep1Name = NA,
        Rep1PartyId = NA,
        Rep1PartyName = NA,
        Rep2Id = NA,
        Rep2Name = NA,
        Rep2PartyId = NA,
        Rep2PartyName = NA,
        Rep3Id = NA,
        Rep3Name = NA,
        Rep3PartyId = NA,
        Rep3PartyName = NA,
        Rep4Id = NA,
        Rep4Name = NA,
        Rep4PartyId = NA,
        Rep4PartyName = NA,
        Rep5Id = NA,
        Rep5Name = NA,
        Rep5PartyId = NA,
        Rep5PartyName = NA,
        SDContestId = NA,
        SDContestName = NA,
        SD1Id = NA,
        SD1Name = NA,
        SD1PartyId = NA,
        SD1PartyName = NA,
        SD2Id = NA,
        SD2Name = NA,
        SD2PartyId = NA,
        SD2PartyName = NA,
        SD3Id = NA,
        SD3Name = NA,
        SD3PartyId = NA,
        SD3PartyName = NA,
        SD4Id = NA,
        SD4Name = NA,
        SD4PartyId = NA,
        SD4PartyName = NA,
        HDContestId = NA,
        HDContestName = NA,
        HD1Id = NA,
        HD1Name = NA,
        HD1PartyId = NA,
        HD1PartyName = NA,
        HD2Id = NA,
        HD2Name = NA,
        HD2PartyId = NA,
        HD2PartyName = NA,
        HD3Id = NA,
        HD3Name = NA,
        HD3PartyId = NA,
        HD3PartyName = NA,
        HD4Id = NA,
        HD4Name = NA,
        HD4PartyId = NA,
        HD4PartyName = NA,
        HD5Id = NA,
        HD5Name = NA,
        HD5PartyId = NA,
        HD5PartyName = NA,
        BM1Id = NA,
        BM1Name = NA,
        BM2Id = NA,
        BM2Name = NA,
        SC1Id = NA,
        SC1Name = NA,
        SC2Id = NA,
        SC2Name = NA,
        COA1Id = NA,
        COA1Name = NA,
        COA2Id = NA,
        COA2Name = NA,
        DIST_JD1Id = NA,
        DIST_JD1Name = NA,
        SUP_JD3_1Id = NA,
        SUP_JD3_1Name = NA,
        SUP_JD3_2Id = NA,
        SUP_JD3_2Name = NA,
        SUP_JD3_3Id = NA,
        SUP_JD3_3Name = NA,
        SUP_JD3_4Id = NA,
        SUP_JD3_4Name = NA,
        DIST_JD3_1Id = NA,
        DIST_JD3_1Name = NA,
        DIST_JD3_2Id = NA,
        DIST_JD3_2Name = NA,
        DIST_JD3_3Id = NA,
        DIST_JD3_3Name = NA,
        DIST_JD3_4Id = NA,
        DIST_JD3_4Name = NA,
        DIST_JD3_5Id = NA,
        DIST_JD3_5Name = NA,
        DIST_JD3_6Id = NA,
        DIST_JD3_6Name = NA,
        DIST_JD3_7Id = NA,
        DIST_JD3_7Name = NA,
        SUP_JD4Id = NA,
        SUP_JD4Name = NA,
        DIST_JD4_1Id = NA,
        DIST_JD4_1Name = NA,
        DIST_JD4_2Id = NA,
        DIST_JD4_2Name = NA
      )
      # Voting data
      for (c in current_session$Original$Cards[[1]]$Contests) {
        con_id <- c$Id
        if (con_id == 4) {
          for (n in seq_len(conv(ContestM2, "NumOfRanks", "ContestId", con_id))) {
            if (check_val(n, c$Marks)) {
              cand_id <- c$Marks[[n]]$CandidateId
              party_id <- c(conv(CandidateM2, "PartyId", "CandidateId", cand_id))
              ballot[, str_c("Pres", n, "Id")] <- c(cand_id)
              ballot[, str_c("Pres", n, "Name")] <- c(conv(CandidateM2, "CandidateName", "CandidateId", cand_id))
              ballot[, str_c("Pres", n, "PartyId")] <- c(party_id)
              ballot[, str_c("Pres", n, "PartyName")] <- c(conv(PartyM2, "PartyName", "PartyId", party_id))
            } else {
              ballot[, str_c("Pres", n, "Id")] <- c(NA)
              ballot[, str_c("Pres", n, "Name")] <- c("[Blank]")
              ballot[, str_c("Pres", n, "PartyId")] <- c(NA)
              ballot[, str_c("Pres", n, "PartyName")] <- c("[Blank]")
            }
          }
        } else if (con_id == 7) {
          for (n in seq_len(conv(ContestM2, "NumOfRanks", "ContestId", con_id))) {
            if (check_val(n, c$Marks)) {
              cand_id <- c$Marks[[n]]$CandidateId
              party_id <- c(conv(CandidateM2, "PartyId", "CandidateId", cand_id))
              ballot[, str_c("Rep", n, "Id")] <- c(cand_id)
              ballot[, str_c("Rep", n, "Name")] <- c(conv(CandidateM2, "CandidateName", "CandidateId", cand_id))
              ballot[, str_c("Rep", n, "PartyId")] <- c(party_id)
              ballot[, str_c("Rep", n, "PartyName")] <- c(conv(PartyM2, "PartyName", "PartyId", party_id))
            } else {
              ballot[, str_c("Rep", n, "Id")] <- c(NA)
              ballot[, str_c("Rep", n, "Name")] <- c("[Blank]")
              ballot[, str_c("Rep", n, "PartyId")] <- c(NA)
              ballot[, str_c("Rep", n, "PartyName")] <- c("[Blank]")
            }
          }
        } else if (is.element(con_id, 9:27)) {
          c_name <- conv(ContestM2, "ContestName", "ContestId", con_id)
          ballot[, "SDContestId"] <- c(con_id)
          ballot[, "SDContestName"] <- c(c_name)
          for (n in seq_len(conv(ContestM2, "NumOfRanks", "ContestId", con_id))) {
            if (check_val(n, c$Marks)) {
              cand_id <- c$Marks[[n]]$CandidateId
              party_id <- c(conv(CandidateM2, "PartyId", "CandidateId", cand_id))
              ballot[, str_c("SD", n, "Id")] <- c(cand_id)
              ballot[, str_c("SD", n, "Name")] <- c(conv(CandidateM2, "CandidateName", "CandidateId", cand_id))
              ballot[, str_c("SD", n, "PartyId")] <- c(party_id)
              ballot[, str_c("SD", n, "PartyName")] <- c(conv(PartyM2, "PartyName", "PartyId", party_id))
            } else {
              ballot[, str_c("SD", n, "Id")] <- c(NA)
              ballot[, str_c("SD", n, "Name")] <- c("[Blank]")
              ballot[, str_c("SD", n, "PartyId")] <- c(NA)
              ballot[, str_c("SD", n, "PartyName")] <- c("[Blank]")
            }
          }
        } else if (is.element(con_id, 28:67)) { #hd
          c_name <- conv(ContestM2, "ContestName", "ContestId", con_id)
          ballot[, "HDContestId"] <- c(con_id)
          ballot[, "HDContestName"] <- c(c_name)
          for (n in seq_len(conv(ContestM2, "NumOfRanks", "ContestId", con_id))) {
            if (check_val(n, c$Marks)) {
              cand_id <- c$Marks[[n]]$CandidateId
              party_id <- c(conv(CandidateM2, "PartyId", "CandidateId", cand_id))
              ballot[, str_c("HD", n, "Id")] <- c(cand_id)
              ballot[, str_c("HD", n, "Name")] <- c(conv(CandidateM2, "CandidateName", "CandidateId", cand_id))
              ballot[, str_c("HD", n, "PartyId")] <- c(party_id)
              ballot[, str_c("HD", n, "PartyName")] <- c(conv(PartyM2, "PartyName", "PartyId", party_id))
            } else {
              ballot[, str_c("HD", n, "Id")] <- c(NA)
              ballot[, str_c("HD", n, "Name")] <- c("[Blank]")
              ballot[, str_c("HD", n, "PartyId")] <- c(NA)
              ballot[, str_c("HD", n, "PartyName")] <- c("[Blank]")
            }
          }
        } else {
          col_header <- conv(measure_vs_contest, "MeasureColName", "ContestId", con_id)
          if (length(c$Marks) != 1) {
            ballot[, str_c(col_header, "Id")] <- c(NA)
            ballot[, str_c(col_header, "Name")] <- c("[Blank]")
          } else {
            ballot[, str_c(col_header, "Id")] <- c(c$Marks[[1]]$CandidateId)
            ballot[, str_c(col_header, "Name")] <- c(conv(CandidateM2, "CandidateName", "CandidateId", c$Marks[[1]]$CandidateId))
          }
        }
      }
      cvr <- rows_append(cvr, ballot)
    }
    print(paste(i, "is done"))
  }
  write_csv(cvr, str_c("products/general_election_2024-", b[1], ".csv"))
  #rm(cvr)
}

cvr <- cvr_template
for (b in batches) {
  chunk <- read_csv(str_c("products/general_election_2024-", b[1], ".csv"))
  cvr <- rows_append(cvr, chunk)
}

write_csv(cvr, str_c("general_election_2024.csv"))
