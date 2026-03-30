# ============================================================================
# TX Voter CSV to Parquet Conversion
# Convert large voter files (20M+ rows) to efficient Parquet format for analysis
# ============================================================================

# SETUP: Install required packages
packages <- c("duckdb", "DBI")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, quiet = TRUE)
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

cat("✓ Packages loaded.\n\n")

# ============================================================================
# STEP 1: Connect to DuckDB
# ============================================================================

con <- dbConnect(duckdb::duckdb())

cat("=== STEP 1: Inspect CSV schemas ===\n\n")

# Define explicit column type specs to avoid auto-detection errors
# (e.g., gender column has mixed TRUE/FALSE and M/F values)
types_2024_tx <- "types={'gender': 'VARCHAR', 'party_id': 'VARCHAR', 'is_confidential': 'VARCHAR', 'is_deceased': 'VARCHAR', 'is_felon': 'VARCHAR'}"
types_tx_source <- "types={'demo_gender': 'VARCHAR', 'demo_gender_confidence': 'VARCHAR'}"

# Quick row/column count for each file
files <- list(
  list(name = "2024_TX.csv", types = types_2024_tx),
  list(name = "TX_source.csv", types = types_tx_source)
)

for (f in files) {
  csv_file <- paste0("voterfile/", f$name)
  count_query <- sprintf("SELECT COUNT(*) as n FROM read_csv('%s', %s)", csv_file, f$types)
  
  result <- dbGetQuery(con, count_query)
  cat("📄", f$name, "\n")
  cat("   Rows:", format(result$n, big.mark = ","), "\n")
  cat("   Input path: voterfile/", f$name, "\n\n")
}

# ============================================================================
# STEP 2: Convert CSVs to Parquet (one-time conversion)
# ============================================================================

cat("=== STEP 2: Convert to Parquet ===\n\n")

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
  
  # Skip if Parquet already exists
  if (file.exists(parquet_file)) {
    cat("✓", parquet_file, "already exists—skipping.\n")
    next
  }
  
  # Convert CSV → Parquet with snappy compression
  cat("Converting", csv_file, "→", parquet_file, "\n")
  
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
    cat("✓ Conversion complete in", round(as.numeric(elapsed), 2), "minutes.\n\n")
  }, error = function(e) {
    cat("✗ Error:", e$message, "\n\n")
  })
}

# ============================================================================
# STEP 3: Verify Parquet files
# ============================================================================

cat("=== STEP 3: Verify conversions ===\n\n")

parquet_files <- c("voterfile/2024_TX.parquet", "voterfile/TX_source.parquet")

for (pf in parquet_files) {
  if (file.exists(pf)) {
    size_bytes <- file.size(pf)
    size_gb <- round(size_bytes / 1e9, 2)
    count <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM read_parquet('%s')", pf))
    
    cat("✓", pf, "\n")
    cat("   File size:", size_gb, "GB\n")
    cat("   Rows:", format(count$n, big.mark = ","), "\n\n")
  }
}

cat("✓ Conversion complete. Both Parquet files ready for analysis.\n")
cat("  Next: source('analysis_starter.r') to begin queries.\n")

dbDisconnect(con)
