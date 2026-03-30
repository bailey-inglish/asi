# Minimal schema inspection using DuckDB
# Safe way to peek at large CSVs without loading them

if (!require("duckdb", quietly = TRUE)) {
  install.packages("duckdb")
}
if (!require("DBI", quietly = TRUE)) {
  install.packages("DBI")
}

library(duckdb)
library(DBI)

con <- dbConnect(duckdb::duckdb())

# Peek at first CSV
cat("===== 2024_TX.csv =====\n")
schema1 <- dbGetQuery(con, "
  SELECT * 
  FROM read_csv_auto('voterfile/2024_TX.csv')
  LIMIT 0
")
print(colnames(schema1))
print(str(schema1))

# Peek at second CSV
cat("\n===== TX_source.csv =====\n")
schema2 <- dbGetQuery(con, "
  SELECT * 
  FROM read_csv_auto('voterfile/TX_source.csv')
  LIMIT 0
")
print(colnames(schema2))
print(str(schema2))

# Quick row count for 2024
cat("\n===== Row counts =====\n")
count1 <- dbGetQuery(con, "SELECT COUNT(*) as n FROM read_csv_auto('voterfile/2024_TX.csv')")
count2 <- dbGetQuery(con, "SELECT COUNT(*) as n FROM read_csv_auto('voterfile/TX_source.csv')")
cat("2024_TX.csv:", count1$n, "rows\n")
cat("TX_source.csv:", count2$n, "rows\n")

dbDisconnect(con)
