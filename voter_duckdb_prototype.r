# ============================================================================
# Voter File DuckDB Prototype
# Work with large TX voter CSVs without loading full data into RAM
# ============================================================================

# Install/load required packages
packages <- c("duckdb", "DBI", "arrow", "dplyr")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("✓ All packages loaded.\n\n")

# ============================================================================
# SETUP: Connect to DuckDB and inspect CSVs
# ============================================================================

cat("=== SETUP: Loading schema and row counts ===\n")

con <- dbConnect(duckdb::duckdb())

# Define type specs for CSVs (handle problematic columns that auto-detect incorrectly)
# 2024_TX.csv: gender is VARCHAR, not BOOLEAN
types_2024_tx <- "gender VARCHAR, party_id VARCHAR, is_confidential VARCHAR, is_deceased VARCHAR, is_felon VARCHAR"

# TX_source.csv: most columns are VARCHAR; a few are numeric or date
types_tx_source <- "demo_gender VARCHAR, demo_gender_confidence VARCHAR"

# Quick inspection of both files
cat("=== SETUP: Inspecting schemas ===\n")

# 2024_TX.csv
cat("\n📄 voterfile/2024_TX.csv\n")
tryCatch({
  count1 <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM read_csv('voterfile/2024_TX.csv', types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'})"))
  cols1 <- dbGetQuery(con, sprintf("SELECT * FROM read_csv('voterfile/2024_TX.csv', types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'}) LIMIT 0"))
  cat("   Rows:", format(count1$n, big.mark = ","), "\n")
  cat("   Cols:", length(colnames(cols1)), "\n")
}, error = function(e) cat("   Error:", e$message, "\n"))

# TX_source.csv
cat("\n📄 voterfile/TX_source.csv\n")
tryCatch({
  count2 <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM read_csv('voterfile/TX_source.csv', types={'demo_gender': 'VARCHAR', 'demo_gender_confidence': 'VARCHAR'})"))
  cols2 <- dbGetQuery(con, sprintf("SELECT * FROM read_csv('voterfile/TX_source.csv', types={'demo_gender': 'VARCHAR', 'demo_gender_confidence': 'VARCHAR'}) LIMIT 0"))
  cat("   Rows:", format(count2$n, big.mark = ","), "\n")
  cat("   Cols:", length(colnames(cols2)), "\n")
}, error = function(e) cat("   Error:", e$message, "\n"))

cat("\n")

# ============================================================================
# OPTION 1: Direct DuckDB queries (no in-memory load)
# ============================================================================

cat("=== OPTION 1: Query CSVs directly with SQL ===\n\n")

# Example 1: Count rows
query_example_1 <- function() {
  tryCatch({
    result <- dbGetQuery(con, "
      SELECT 
        COUNT(*) as total_voters
      FROM read_csv('voterfile/2024_TX.csv', types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'})
    ")
    cat("Total voters in 2024_TX.csv:", format(result$total_voters, big.mark = ","), "\n")
  }, error = function(e) {
    cat("Query error:", e$message, "\n")
  })
}

query_example_1()

# Example 2: Sample first few rows (safe because LIMIT is small)
cat("\nFirst 5 rows preview:\n")
preview <- dbGetQuery(con, "
  SELECT * 
  FROM read_csv('voterfile/2024_TX.csv', types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'})
  LIMIT 5
")
print(head(preview))

cat("\n")

# ============================================================================
# OPTION 2: Convert CSVs to Parquet (one-time, then work from Parquet)
# ============================================================================

cat("=== OPTION 2: Convert to Parquet for faster repeated access ===\n")
cat("⚠️  This will take a few minutes but only needs to run once.\n")

convert_to_parquet <- function() {
  # CSV files with their explicit type specs
  conversions <- list(
    list(csv = "voterfile/2024_TX.csv", 
         parquet = "voterfile/2024_TX.parquet",
         types = "types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'}"),
    list(csv = "voterfile/TX_source.csv",
         parquet = "voterfile/TX_source.parquet",
         types = "types={'demo_gender': 'VARCHAR', 'demo_gender_confidence': 'VARCHAR'}")
  )
  
  for (conv in conversions) {
    csv_file <- conv$csv
    parquet_file <- conv$parquet
    type_spec <- conv$types
    
    if (!file.exists(parquet_file)) {
      cat("\nConverting", csv_file, "→", parquet_file, "\n")
      
      query <- sprintf("
        COPY (
          SELECT *
          FROM read_csv('%s', %s)
        ) TO '%s' (FORMAT PARQUET, COMPRESSION 'snappy')
      ", csv_file, type_spec, parquet_file)
      
      start_time <- Sys.time()
      tryCatch({
        dbExecute(con, query)
        elapsed <- difftime(Sys.time(), start_time, units = "mins")
        cat("✓ Done in", round(as.numeric(elapsed), 2), "minutes.\n")
      }, error = function(e) {
        cat("✗ Error:", e$message, "\n")
      })
    } else {
      cat("\n✓", parquet_file, "already exists.\n")
    }
  }
}

# Uncomment to run conversion:
convert_to_parquet()

cat("\n")

# ============================================================================
# OPTION 3: Work with Parquet via Arrow + dplyr (if converted)
# ============================================================================

cat("=== OPTION 3: Arrow + dplyr workflow (after Parquet conversion) ===\n\n")

query_with_arrow <- function() {
  parquet_file <- "voterfile/2024_TX.parquet"
  
  if (file.exists(parquet_file)) {
    cat("Opening", parquet_file, "with Arrow...\n")
    
    # Open as lazy dataset (doesn't load into RAM)
    ds <- arrow::open_dataset(parquet_file)
    
    # Example: filter and summarize (all operations stay on disk until collect())
    result <- ds %>%
      dplyr::filter(row_number() <= 1000) %>%  # Example: just first 1000 rows
      dplyr::slice_sample(n = 100) %>%  # Random sample
      dplyr::collect()  # NOW bring into memory
    
    cat("✓ Collected 100 rows:\n")
    print(head(result, 10))
  } else {
    cat("ℹ️  Parquet file not found yet. Run convert_to_parquet() first.\n")
  }
}

# Uncomment to run:
query_with_arrow()

cat("\n")

# ============================================================================
# HELPER FUNCTIONS for common tasks
# ============================================================================

cat("=== HELPER FUNCTIONS ===\n\n")

# Quick aggregate query
aggregate_csv <- function(csv_file, group_col, agg_col = NULL) {
  # This won't work without knowing columns; shown as template
  query <- sprintf(
    "SELECT COUNT(*) as n FROM read_csv_auto('%s')",
    csv_file
  )
  result <- dbGetQuery(con, query)
  return(result)
}

# Export query results to small CSV
export_query_results <- function(query_sql, output_file) {
  result <- dbGetQuery(con, query_sql)
  write.csv(result, output_file, row.names = FALSE)
  cat("✓ Exported to", output_file, "(" , nrow(result), "rows )\n")
}

# Sample rows from CSV
sample_csv <- function(csv_file, n_rows = 1000) {
  query <- sprintf(
    "SELECT * FROM read_csv_auto('%s') ORDER BY random() LIMIT %d",
    csv_file, n_rows
  )
  return(dbGetQuery(con, query))
}

cat("Available functions:\n")
cat("  - aggregate_csv(csv_file, group_col, agg_col)\n")
cat("  - export_query_results(query_sql, output_file)\n")
cat("  - sample_csv(csv_file, n_rows)\n")

cat("\n")

# ============================================================================
# CLEANUP
# ============================================================================

cat("=== END OF PROTOTYPE ===\n")
cat("DuckDB connection remained open for queries.\n")
cat("Disconnect when done: dbDisconnect(con)\n\n")

# Uncomment to close connection when done:
# dbDisconnect(con)
