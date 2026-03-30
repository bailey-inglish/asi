# ============================================================================
# TX Voter Data: Analysis Starter Script
# Load both Parquet files + start writing dplyr queries and ggplots
# ============================================================================

library(arrow)
library(dplyr)
library(ggplot2)

# Load both voter datasets (lazy—no data in RAM yet)
voters_2024 <- arrow::open_dataset("voterfile/2024_TX.parquet")
voters_source <- arrow::open_dataset("voterfile/TX_source.parquet")

cat("✓ Datasets loaded:\n")
cat("  - voters_2024:   19.9M rows, 55 columns\n")
cat("  - voters_source: 18.7M rows, 189 columns\n")
cat("  Ready for dplyr + ggplot queries!\n\n")

# ============================================================================
# YOUR ANALYSIS STARTS HERE
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
