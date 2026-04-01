# ============================================================================
# TX Voter Data: Analysis
# Load both Parquet files + start writing dplyr queries and ggplots
# ============================================================================

library(arrow)
library(dplyr)
library(ggplot2)

# Simple timing wrapper for long lazy queries.
run_timed <- function(label, expr) {
  cat("\n[START]", label, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  t0 <- Sys.time()
  out <- eval.parent(substitute(expr))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  cat("[DONE ]", label, sprintf("(%.1f sec)", elapsed), "\n")
  out
}

# Optional DuckDB helper for very long SQL jobs with built-in progress bar.
run_duckdb_sql <- function(sql, db_path = "voterfile/voter_work.duckdb") {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("duckdb", quietly = TRUE)) {
    stop("Install DBI and duckdb to use run_duckdb_sql().")
  }
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "PRAGMA enable_progress_bar")
  DBI::dbGetQuery(con, sql)
}

# Load both voter datasets (lazy—no data in RAM yet)
voters_2024 <- arrow::open_dataset("voterfile/2024_TX.parquet")
voters_source <- arrow::open_dataset("voterfile/TX_source.parquet")

# ============================================================================
# EXAMPLES AND SAMPLES
# ============================================================================

# Example 1: Active voters by county (top 15)
# Uncomment and modify to start:
# active_by_county <- voters_2024 %>%
#   filter(voter_status == "Active") %>%
#   group_by(county_name) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(desc(n)) %>%
#   slice_head(n = 15) %>%
#   collect()
# 
# ggplot(active_by_county, aes(x = reorder(county_name, n), y = n)) +
#   geom_col(fill = "steelblue") +
#   coord_flip() +
#   labs(title = "Top 15 Counties by Active Voters",
#        x = "County", y = "Number of Active Voters") +
#   theme_minimal()

# Example 2: Gender distribution pie chart
# Uncomment and modify to start:
# gender_dist <- voters_2024 %>%
#   filter(gender %in% c("M", "F")) %>%
#   group_by(gender) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   mutate(pct = 100 * n / sum(n)) %>%
#   collect()
# 
# ggplot(gender_dist, aes(x = "", y = n, fill = gender)) +
#   geom_col() +
#   coord_polar("y", start = 0) +
#   labs(title = "Voter Gender Distribution", fill = "Gender") +
#   theme_minimal() +
#   theme(axis.title = element_blank(), axis.text = element_blank())

# Example 3: Registration trends over time
# Uncomment and modify to start:
# reg_by_year <- voters_2024 %>%
#   mutate(reg_year = substr(as.character(registration_date), 1, 4)) %>%
#   filter(registration_date > 0, reg_year >= "2010") %>%
#   group_by(reg_year) %>%
#   summarise(n = n(), .groups = "drop") %>%
#   arrange(reg_year) %>%
#   collect()
# 
# ggplot(reg_by_year, aes(x = reg_year, y = n, group = 1)) +
#   geom_line(color = "steelblue", size = 1) +
#   geom_point(size = 2) +
#   labs(title = "New Voter Registrations by Year",
#        x = "Year", y = "Number of New Registrations") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================================
# Useful reference: Common column selections
# ============================================================================

# voters_2024 columns (55 total):
# Names: first_name, last_name, middle_name, suffix
# Demographics: gender, birth_date, race, ethnicity, hispanic_surname_flag
# Registration: registration_date, voter_status, voter_status_reason, party_name
# Location: county_name, precinct_name, precinct_id, residential_city, 
#           residential_zip, mailing_address*
# Other: sos_voter_id, is_deceased, is_felon, is_confidential

# voters_source columns (189 total, includes voting history):
# All above + voting history columns (vote_general_2012 through vote_primary_2026)
# + voter scores (score_support_gen_dem, score_turnout_presidential, etc.)
# + geographic coordinates (addr_latitude_residential, addr_longitude_residential)

# Quick filter examples:
# - Active voters: filter(voter_status == "Active")
# - Specific county: filter(county_name == "Travis")
# - Registration period: filter(registration_date >= 20200101, registration_date <= 20201231)
# - Valid gender: filter(gender %in% c("M", "F"))
# - Voters who died: filter(is_deceased == TRUE)

active_by_county <- voters_2024 %>%
  filter(voter_status == "Active") %>%
  group_by(county_name) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 15) %>%
  collect()

deceased_counts <- voters_2024 %>%
  count(is_deceased) %>%
  collect()

# ==========================================================================
# ID AUDIT: Are voter IDs 1:1 between 2024 and source files?
# ==========================================================================

# Normalize join keys with Arrow-supported operations.
ids_2024 <- voters_2024 %>%
  transmute(sos_voter_id = sos_voter_id) %>%
  filter(!is.na(sos_voter_id), sos_voter_id != "")

ids_source <- voters_source %>%
  transmute(sos_voter_id = id_sos_civitech) %>%
  filter(!is.na(sos_voter_id), sos_voter_id != "")

summary_2024 <- ids_2024 %>%
  summarise(file = "2024", rows_with_id = n(), distinct_ids = n_distinct(sos_voter_id)) %>%
  collect()

summary_source <- ids_source %>%
  summarise(file = "source", rows_with_id = n(), distinct_ids = n_distinct(sos_voter_id)) %>%
  collect()

id_summary <- bind_rows(summary_2024, summary_source) %>%
  mutate(duplicate_rows = rows_with_id - distinct_ids)

ids_2024_distinct <- ids_2024 %>% distinct(sos_voter_id)
ids_source_distinct <- ids_source %>% distinct(sos_voter_id)

ids_only_2024 <- ids_2024_distinct %>%
  anti_join(ids_source_distinct, by = "sos_voter_id")

ids_only_source <- ids_source_distinct %>%
  anti_join(ids_2024_distinct, by = "sos_voter_id")

ids_in_both_n <- ids_2024_distinct %>%
  inner_join(ids_source_distinct, by = "sos_voter_id") %>%
  summarise(n = n()) %>%
  collect()

ids_only_2024_n <- ids_only_2024 %>%
  summarise(n = n()) %>%
  collect()

ids_only_source_n <- ids_only_source %>%
  summarise(n = n()) %>%
  collect()

only_2024_examples <- ids_only_2024 %>%
  slice_head(n = 20) %>%
  collect()

only_source_examples <- ids_only_source %>%
  slice_head(n = 20) %>%
  collect()

cat("\nID audit summary:\n")
print(id_summary)
cat("\nDistinct IDs in both files:", format(ids_in_both_n$n, big.mark = ","), "\n")
cat("Distinct IDs only in 2024:", format(ids_only_2024_n$n, big.mark = ","), "\n")
cat("Distinct IDs only in source:", format(ids_only_source_n$n, big.mark = ","), "\n")

# ==========================================================================
# MISMATCH DIAGNOSTICS: Why might IDs appear in only one file?
# ==========================================================================

# 2024-only: status and potential explanatory factors.
status_only_2024 <- run_timed("2024-only status counts", {
  voters_2024 %>%
    semi_join(ids_only_2024, by = "sos_voter_id") %>%
    count(voter_status, sort = TRUE) %>%
    collect()
})

status_reason_only_2024 <- run_timed("2024-only status reason counts", {
  voters_2024 %>%
    semi_join(ids_only_2024, by = "sos_voter_id") %>%
    count(voter_status, voter_status_reason, sort = TRUE) %>%
    slice_head(n = 30) %>%
    collect()
})

flags_only_2024 <- run_timed("2024-only deceased/felon/confidential flags", {
  voters_2024 %>%
    semi_join(ids_only_2024, by = "sos_voter_id") %>%
    count(is_deceased, is_felon, is_confidential, sort = TRUE) %>%
    slice_head(n = 20) %>%
    collect()
})

county_only_2024 <- run_timed("2024-only top counties", {
  voters_2024 %>%
    semi_join(ids_only_2024, by = "sos_voter_id") %>%
    count(county_name, sort = TRUE) %>%
    slice_head(n = 20) %>%
    collect()
})

reg_year_only_2024 <- run_timed("2024-only registration year", {
  voters_2024 %>%
    semi_join(ids_only_2024, by = "sos_voter_id") %>%
    mutate(reg_year = substr(as.character(registration_date), 1, 4)) %>%
    count(reg_year, sort = TRUE) %>%
    filter(reg_year >= "1900", reg_year <= "2030") %>%
    collect()
})

# source-only: statuses and potential explanatory factors.
status_only_source <- run_timed("source-only status counts", {
  voters_source %>%
    semi_join(ids_only_source, by = c("id_sos_civitech" = "sos_voter_id")) %>%
    count(civitech_voter_status, sos_voter_status, sort = TRUE) %>%
    slice_head(n = 30) %>%
    collect()
})

party_only_source <- run_timed("source-only party id", {
  voters_source %>%
    semi_join(ids_only_source, by = c("id_sos_civitech" = "sos_voter_id")) %>%
    count(demo_party_id, sort = TRUE) %>%
    slice_head(n = 20) %>%
    collect()
})

county_only_source <- run_timed("source-only top counties", {
  voters_source %>%
    semi_join(ids_only_source, by = c("id_sos_civitech" = "sos_voter_id")) %>%
    count(dist_county, sort = TRUE) %>%
    slice_head(n = 20) %>%
    collect()
})

reg_year_only_source <- run_timed("source-only registration year", {
  voters_source %>%
    semi_join(ids_only_source, by = c("id_sos_civitech" = "sos_voter_id")) %>%
    mutate(reg_year = substr(as.character(dt_registration), 1, 4)) %>%
    count(reg_year, sort = TRUE) %>%
    filter(reg_year >= "1900", reg_year <= "2030") %>%
    collect()
})

# Duplicate-ID diagnostic in 2024 file (source had 0 duplicate rows by ID).
dupe_ids_2024 <- run_timed("2024 duplicate ID check", {
  voters_2024 %>%
    transmute(sos_voter_id = sos_voter_id) %>%
    filter(!is.na(sos_voter_id), sos_voter_id != "") %>%
    count(sos_voter_id, sort = TRUE) %>%
    filter(n > 1) %>%
    slice_head(n = 20) %>%
    collect()
})

cat("\n2024-only voter_status (top):\n")
print(status_only_2024)
cat("\n2024-only status_reason (top 30):\n")
print(status_reason_only_2024)
cat("\n2024-only deceased/felon/confidential combos (top):\n")
print(flags_only_2024)
cat("\n2024-only top counties:\n")
print(county_only_2024)
cat("\n2024-only registration year distribution:\n")
print(reg_year_only_2024)

cat("\nsource-only status combos (top 30):\n")
print(status_only_source)
cat("\nsource-only party id (top):\n")
print(party_only_source)
cat("\nsource-only top counties:\n")
print(county_only_source)
cat("\nsource-only registration year distribution:\n")
print(reg_year_only_source)

cat("\nTop duplicate IDs in 2024 (if any):\n")
print(dupe_ids_2024)

# Optional: run this only after checking cardinality in id_summary.
# If source has duplicate IDs, deduplicate first to avoid row explosion.
#
# source_race_for_2024 <- voters_source %>%
#   transmute(
#     sos_voter_id = id_sos_civitech,
#     race_source = demo_race
#   ) %>%
#   filter(!is.na(sos_voter_id), sos_voter_id != "") %>%
#   semi_join(ids_2024_distinct, by = "sos_voter_id") %>%
#   distinct(sos_voter_id, race_source)
#
# voters_2024_merge <- voters_2024 %>%
#   left_join(source_race_for_2024, by = "sos_voter_id") %>%
#   mutate(race = coalesce(race, race_source)) %>%
#   select(-race_source)
#
# merged_race_counts <- voters_2024_merge %>%
#   count(race) %>%
#   collect()
#
# race_by_status <- voters_2024_merge %>%
#   count(race, voter_status) %>%
#   collect()

me <- voters_source %>%
  filter(email_1 == "kc46982@my.utexas.edu") %>%
  collect()

old <- voters_source %>%
  filter(!is.na(vote_general_2012)) %>%
  count(vote_general_2012) %>%
  collect()