library(DBI)
library(duckdb)

con <- dbConnect(duckdb::duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, "PRAGMA enable_progress_bar")
dbExecute(con, "PRAGMA threads=8")

run_query <- function(title, sql) {
  cat("\n=== ", title, " ===\n", sep = "")
  start <- Sys.time()
  out <- dbGetQuery(con, sql)
  elapsed <- difftime(Sys.time(), start, units = "secs")
  print(out)
  cat("Elapsed:", round(as.numeric(elapsed), 2), "sec\n")
  invisible(out)
}

cat("Starting mismatch diagnostics. DuckDB progress bar is enabled for long queries.\n")

# Build temporary ID sets once to avoid repeating DISTINCT scans.
dbExecute(con, "
CREATE OR REPLACE TEMP TABLE ids_2024 AS
SELECT DISTINCT sos_voter_id
FROM read_parquet('voterfile/2024_TX.parquet')
WHERE sos_voter_id IS NOT NULL AND sos_voter_id <> ''
")

dbExecute(con, "
CREATE OR REPLACE TEMP TABLE ids_source AS
SELECT DISTINCT id_sos_civitech AS sos_voter_id
FROM read_parquet('voterfile/TX_source.parquet')
WHERE id_sos_civitech IS NOT NULL AND id_sos_civitech <> ''
")

dbExecute(con, "
CREATE OR REPLACE TEMP TABLE ids_only_2024 AS
SELECT i.sos_voter_id
FROM ids_2024 i
LEFT JOIN ids_source s USING (sos_voter_id)
WHERE s.sos_voter_id IS NULL
")

dbExecute(con, "
CREATE OR REPLACE TEMP TABLE ids_only_source AS
SELECT s.sos_voter_id
FROM ids_source s
LEFT JOIN ids_2024 i USING (sos_voter_id)
WHERE i.sos_voter_id IS NULL
")

run_query("ID OVERLAP SUMMARY", "
SELECT
  (SELECT COUNT(*) FROM ids_2024) AS ids_2024,
  (SELECT COUNT(*) FROM ids_source) AS ids_source,
  (SELECT COUNT(*) FROM ids_2024 i INNER JOIN ids_source s USING (sos_voter_id)) AS ids_in_both,
  (SELECT COUNT(*) FROM ids_only_2024) AS ids_only_2024,
  (SELECT COUNT(*) FROM ids_only_source) AS ids_only_source
")

run_query("2024-ONLY: VOTER STATUS", "
SELECT v.voter_status, COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet') v
INNER JOIN ids_only_2024 m USING (sos_voter_id)
GROUP BY 1
ORDER BY n DESC
")

run_query("2024-ONLY: FLAGS (DECEASED/FELON/CONFIDENTIAL)", "
SELECT
  is_deceased,
  is_felon,
  is_confidential,
  COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet') v
INNER JOIN ids_only_2024 m USING (sos_voter_id)
GROUP BY 1,2,3
ORDER BY n DESC
LIMIT 15
")

run_query("2024-ONLY: TOP COUNTIES", "
SELECT county_name, COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet') v
INNER JOIN ids_only_2024 m USING (sos_voter_id)
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("2024-ONLY: REGISTRATION YEAR", "
SELECT
  SUBSTR(CAST(registration_date AS VARCHAR), 1, 4) AS reg_year,
  COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet') v
INNER JOIN ids_only_2024 m USING (sos_voter_id)
WHERE registration_date IS NOT NULL
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("2024-ONLY: RACE", "
SELECT race, COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet') v
INNER JOIN ids_only_2024 m USING (sos_voter_id)
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("SOURCE-ONLY: STATUS COMBO", "
SELECT
  s.civitech_voter_status,
  s.sos_voter_status,
  COUNT(*) AS n
FROM read_parquet('voterfile/TX_source.parquet') s
INNER JOIN ids_only_source m ON s.id_sos_civitech = m.sos_voter_id
GROUP BY 1,2
ORDER BY n DESC
LIMIT 20
")

run_query("SOURCE-ONLY: TOP DIST COUNTIES", "
SELECT dist_county, COUNT(*) AS n
FROM read_parquet('voterfile/TX_source.parquet') s
INNER JOIN ids_only_source m ON s.id_sos_civitech = m.sos_voter_id
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("SOURCE-ONLY: REGISTRATION YEAR", "
SELECT
  SUBSTR(CAST(dt_registration AS VARCHAR), 1, 4) AS reg_year,
  COUNT(*) AS n
FROM read_parquet('voterfile/TX_source.parquet') s
INNER JOIN ids_only_source m ON s.id_sos_civitech = m.sos_voter_id
WHERE dt_registration IS NOT NULL
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("SOURCE-ONLY: RACE", "
SELECT demo_race, COUNT(*) AS n
FROM read_parquet('voterfile/TX_source.parquet') s
INNER JOIN ids_only_source m ON s.id_sos_civitech = m.sos_voter_id
GROUP BY 1
ORDER BY n DESC
LIMIT 20
")

run_query("DUPLICATE IDS IN 2024 (TOP 20)", "
SELECT sos_voter_id, COUNT(*) AS n
FROM read_parquet('voterfile/2024_TX.parquet')
WHERE sos_voter_id IS NOT NULL AND sos_voter_id <> ''
GROUP BY 1
HAVING COUNT(*) > 1
ORDER BY n DESC, sos_voter_id
LIMIT 20
")

cat("\nDiagnostics complete.\n")
