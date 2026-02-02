# Third Place Index (TPI)
## National Census Tract Dataset

**Version**: 2025 v1.0
**File**: `third_place_index_1.0.csv`
**Records**: 83,531 U.S. Census Tracts
**Author**: Evan O'Neil / Data Studio of Evan O'Neil
**Contact**: evan@evanoneil.studio


---

## Overview

The Third Place Index measures the availability of "third places" - community gathering spaces beyond home (first place) and work (second place) - at the census tract level across the United States. The concept originates from sociologist Ray Oldenburg's 1989 book *The Great Good Place*.

Third places include cafes, restaurants, libraries, parks, community centers, fitness centers, and other spaces where people gather for social interaction and community building.

### Key Features

- **National coverage**: All 50 states plus Washington, D.C.
- **Tract-level precision**: 83,531 census tracts
- **Percentile-based scoring**: Tracts ranked relative to all U.S. tracts
- **Detailed breakdowns**: Counts by category and individual place type

---

## Methodology

### Place Data Collection

Third place locations were collected from OpenStreetMap (OSM) via the Overpass API. Locations were identified across the following categories:

Cafes, restaurants, bars, pubs, barber shops, salons, bookstores, convenience stores, marketplaces, libraries, community centers, places of worship, parks, gardens, plazas, theaters, cinemas, arts centers, fitness centers, coworking spaces, bowling alleys, arcades, escape rooms, and malls.

### Index Calculation

The Third Place Index uses a **percentile-based methodology** :

1. Census tracts are ranked by total third place count
2. Rankings are converted to percentile scores (0.00 to 1.00)
3. A score of **0.75** means the tract has more third places than 75% of all U.S. tracts

**Interpretation**:
- 0.75 - 1.00: High availability
- 0.50 - 0.75: Above average
- 0.25 - 0.50: Below average
- 0.00 - 0.25: Limited availability ("third place desert")

---

## Data Dictionary

### Identifiers

| Column | Type | Description |
|--------|------|-------------|
| `geoid` | String | 11-digit Census tract identifier |
| `state_fips` | String | 2-digit state FIPS code |
| `urban_rural_class` | String | "Urbanized Area" (50,000+ pop), "Urban Cluster" (2,500-50,000), or "Rural" |

### Index Score

| Column | Type | Range | Description |
|--------|------|-------|-------------|
| `third_place_percentile` | Float | 0-1 | Percentile rank based on total third places compared to all U.S. tracts |

### Demographics

| Column | Type | Description |
|--------|------|-------------|
| `total_population` | Integer | Total population (ACS 2022 5-year) |
| `median_income` | Float | Median household income in dollars |
| `pct_bachelors` | Float | Percent with bachelor's degree or higher |
| `pop_under_18` | Integer | Population under 18 years old |
| `pct_under_18` | Float | Percent under 18 years old |
| `area_sq_miles` | Float | Land area in square miles |

### Place Counts - Categories

| Column | Description |
|--------|-------------|
| `total_places` | Total count of all third places |
| `food_beverage_count` | Restaurants, cafes, bars, pubs, coffee shops |
| `personal_service_count` | Hairdressers, beauty salons |
| `traditional_retail_count` | Convenience stores, bookstores, marketplaces |
| `civic_educational_count` | Places of worship, libraries, community centres |
| `public_spaces_count` | Parks, gardens, squares |
| `cultural_arts_count` | Theatres, cinemas, arts centres |
| `commercial_retail_count` | Department stores, malls |
| `fitness_wellness_count` | Fitness centres, sports centres |
| `work_creation_count` | Coworking spaces |
| `entertainment_gaming_count` | Bowling alleys, arcades, escape games |

### Place Counts - Individual Types

**Food & Beverage**: `restaurant_count`, `cafe_count`, `bar_count`, `pub_count`, `ice_cream_count`, `coffee_count`, `food_court_count`, `biergarten_count`

**Personal Services**: `hairdresser_count`, `beauty_count`

**Retail**: `convenience_count`, `books_count`, `marketplace_count`

**Civic & Educational**: `place_of_worship_count`, `library_count`, `social_facility_count`, `community_centre_count`

**Public Spaces**: `park_count`, `garden_count`, `square_count`, `village_green_count`

**Cultural & Arts**: `theatre_count`, `cinema_count`, `arts_centre_count`

**Commercial**: `department_store_count`, `mall_count`

**Fitness**: `fitness_centre_count`, `sports_centre_count`

**Entertainment**: `bowling_alley_count`, `amusement_arcade_count`, `escape_game_count`

**Work**: `coworking_space_count`

### Density

| Column | Type | Description |
|--------|------|-------------|
| `total_places_per_sqmi` | Float | Third places per square mile |

---

## Data Sources

- **Place Data**: OpenStreetMap contributors (2025) via Overpass API
- **Demographics**: U.S. Census Bureau, American Community Survey 2022 5-Year Estimates
- **Geography**: U.S. Census Bureau, TIGER/Line Shapefiles 2020

---

## Limitations

- **OSM Coverage**: OpenStreetMap completeness varies by region; urban areas are generally better mapped than rural areas
- **Relative Measure**: The percentile score is relative, not absolute - it compares tracts to each other, not to an ideal standard
- **Presence vs. Quality**: The index measures presence of places, not their quality, accessibility, affordability, or actual usage
- **Point-in-Time**: Data represents a snapshot; third places open and close over time

---

## License

- **Place Data**: OpenStreetMap data is available under the Open Database License (ODbL)
- **Census Data**: U.S. Census Bureau data is in the public domain


---

## Contact

Evan O'Neil
evan@evanoneil.studio
