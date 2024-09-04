# Setup & import
library(tidyverse)
library(labelled)
setwd("cps_cleaning")

fips_conv <- read_csv("fips_name_abbr.csv")
fips_conv$fips <- as.numeric(fips_conv$fips)

cps20 <- read_csv("nov20pub.csv")
cps20$GESTFIPS <- cps20$gestfips # for some reason this was lowercase :(
cps22 <- read_csv("nov22pub.csv")

table(cps20$HEFAMINC)

# Variables renamed to be human readable
clean_vars <- function(cps_data) {
  spec_cps_data <- cps_data %>%
    mutate(
      year = HRYEAR4,
      final_weight = PWSSWGT,
      family_income_range = HEFAMINC,
      geo_region = GEREG,
      state = GESTFIPS,
      principal_city_balance_status = GTCBSAST,
      is_metro = GTMETSTA,
      metro_size = GTCBSASZ,
      age = PRTAGE,
      is_age_top_coded = PRTFAGE,
      marital_status = PEMARITL,
      sex = PESEX,
      highest_edu = PEEDUCA,
      race = PTDTRACE,
      armed_forces_marital_status = PRMARSTA,
      citizen_status = PRCITSHP,
      immigrant_year_of_entry = PRINUYER,
      voted = PES1,
      registered_voter = PES2,
      main_reason_not_registered = PES3,
      main_reason_not_voted = PES4,
      voted_in_person_or_mail = PES5,
      voted_on_election_day = PES6,
      where_registered_to_vote = PES7,
      lived_at_address_range = PRS8,
      .keep = "none"
    )
  return(spec_cps_data)
}

cps <- rows_append(clean_vars(cps20), clean_vars(cps22))

conv_table <- c(
  "family_income_range" = tribble(
    ~key, ~value,
    -1, NA,
    1, "LESS THAN $5,000",
    2, "$5,000 TO $7,499",
    3, "7,500 TO 9,999",
    4, "10,000 TO 12,499",
    5, "12,500 TO 14,999",
    6, "15,000 TO 19,999",
    7, "20,000 TO 24,999",
    8, "25,000 TO 29,999",
    9, "30,000 TO 34,999",
    10, "35,000 TO 39,999",
    11, "40,000 TO 49,999",
    12, "50,000 TO 59,999",
    13, "60,000 TO 74,999",
    14, "75,000 TO 99,999",
    15, "100,000 TO 149,999",
    16, "150,000 OR MORE"
  ),
  "geo_region" = tribble(
    ~key, ~value,
    1, "NORTHEAST",
    2, "MIDWEST",
    3, "SOUTH",
    4, "WEST"
  ),
  "state" = tibble(
    key = fips_conv$fips,
    value = fips_conv$name
  ),
  "principal_city_balance_status" = tribble(
    ~key, ~value,
    1, "PRINCIPAL CITY",
    2, "BALANCE",
    3, "NONMETROPOLITAN",
    4, NA
  ),
  "is_metro" = tribble(
    ~key, ~value,
    1, "METROPOLITAN",
    2, "NONMETROPOLITAN",
    3, NA
  ),
  "metro_size" = tribble(
    ~key, ~value,
    0, NA,
    2, "100,000 - 249,999",
    3, "250,000 - 499,999",
    4, "500,000 - 999,999",
    5, "1,000,000 - 2,499,999",
    6, "2,500,000 - 4,999,999",
    7, "5,000,000+"
  ),
  "age" = tibble(
    key = c(0:79, 80, 85),
    value = c(0:79, "80-84 Years Old", "85+ Years Old")
  ),
  "is_age_top_coded" = tribble(
    ~key, ~value,
    0, FALSE,
    1, TRUE
  ),
  "marital_status" = tribble(
    ~key, ~value,
    -1, NA,
    1, "MARRIED - SPOUSE PRESENT",
    2, "MARRIED - SPOUSE ABSENT",
    3, "WIDOWED",
    4, "DIVORCED",
    5, "SEPARATED",
    6, "NEVER MARRIED"
  ),
  "sex" = tribble(
    ~key, ~value,
    -1, NA,
    1, "MALE",
    2, "FEMALE"
  ),
  "highest_edu" = tribble(
    ~key, ~value,
    -1, NA,
    31, "LESS THAN 1ST GRADE",
    32, "1ST, 2ND, 3RD OR 4TH GRADE",
    33, "5TH OR 6TH GRADE",
    34, "7TH OR 8TH GRADE",
    35, "9TH GRADE",
    36, "10TH GRADE",
    37, "11TH GRADE",
    38, "12TH GRADE NO DIPLOMA",
    39, "HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)",
    40, "SOME COLLEGE BUT NO DEGREE",
    41, "ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL",
    42, "ASSOCIATE DEGREE-ACADEMIC PROGRAM",
    43, "BACHELOR'S DEGREE (EX: BA, AB, BS)",
    44, "MASTER'S DEGREE (EX: MA, MS, MEng, MEd, MSW)",
    45, "PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)",
    46, "DOCTORATE DEGREE (EX: PhD, EdD)"
  ),
  "race" = tribble(
    ~key, ~value,
    -1, NA,
    1, "White Only",
    2, "Black Only",
    3, "American Indian, Alaskan Native Only",
    4, "Asian Only",
    5, "Hawaiian/Pacific Islander Only",
    6, "White-Black",
    7, "White-AI",
    8, "White-Asian",
    9, "White-HP",
    10, "Black-AI",
    11, "Black-Asian",
    12, "Black-HP",
    13, "AI-Asian",
    14, "AI-HP",
    15, "Asian-HP",
    16, "W-B-AI",
    17, "W-B-A",
    18, "W-B-HP",
    19, "W-AI-A",
    20, "W-AI-HP",
    21, "W-A-HP",
    22, "B-AI-A",
    23, "W-B-AI-A",
    24, "W-AI-A-HP",
    25, "Other 3 Race Combinations",
    26, "Other 4 and 5 Race Combination"
  ),
  "armed_forces_marital_status" = tribble(
    ~key, ~value,
    -1, NA,
    1, "MARRIED, CIVILIAN SPOUSE PRESENT",
    2, "MARRIED, ARMED FORCES SPOUSE PRESENT",
    3, "MARRIED, SPOUSE ABSENT (EXC. SEPARATED)",
    4, "WIDOWED",
    5, "DIVORCED",
    6, "SEPARATED",
    7, "NEVER MARRIED",
  ),
  "citizen_status" = tribble(
    ~key, ~value,
    -1, NA,
    1, "NATIVE, BORN IN THE UNITED STATES",
    2, "NATIVE, BORN IN PUERTO RICO OR OTHER U.S. ISLAND AREAS",
    3, "NATIVE, BORN ABROAD OF AMERICAN PARENT OR PARENTS",
    4, "FOREIGN BORN, U.S. CITIZEN BY NATURALIZATION",
    5, "FOREIGN BORN, NOT A CITIZEN OF THE UNITED STATES"
  ),
  "voted" = tribble(
    ~key, ~value,
    1, "Yes",
    2, "No",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "registered_voter" =  tribble(
    ~key, ~value,
    1, "Yes",
    2, "No",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "main_reason_not_registered" = tribble(
    ~key, ~value,
    1, "Did not meet registration deadlines",
    2, "Did now know where or how to register",
    3, "Did not meet residency requirements/did not live here long enough",
    4, "Permanent illness or disability",
    5, "Difficulty with English",
    6, "Not interested in the election or not involved in politics",
    7, "My vote would not make a difference",
    8, "Not eligible to vote",
    9, "Other reason",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "main_reason_not_voted" = tribble(
    ~key, ~value,
    1, "Illness or disability (own or family's)",
    2, "Out of town or away from home",
    3, "Forgot to vote (or send in absentee ballot)",
    4, "Not interested, felt my vote wouldn't make a difference",
    5, "Too busy, conflicting work or school schedule",
    6, "Transportation problems",
    7, "Didn't like candidates or campaign issues",
    8, "Registration problems (i.e. didn't receive absentee ballot, not registered in current location)",
    9, "Bad weather conditions",
    10, "Inconvenient hours, polling place or hours or lines too long",
    11, "Other",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "voted_in_person_or_mail" = tribble(
    ~key, ~value,
    1, "In person",
    2, "By mail",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "voted_on_election_day" = tribble(
    ~key, ~value,
    1, "On election day",
    2, "Before election day",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "where_registered_to_vote" = tribble(
    ~key, ~value,
    1, "At a department of motor vehicles (for example, when obtaining a driver's license or other identification card)",
    2, "At a public assistance agency (for example, a Medicaid, AFDC, or Food Stamps office, an office serving disabled persons, or an unemployment office)",
    3, "Registered by mail",
    4, "Registered using the internet or online",
    5, "At a school, hospital, or on campus",
    6, "Went to a town hall or county/government registration office",
    7, "Filled out form at a registration drive (library, post office, or someone came to your door)",
    8, "Registered at polling place (on election or primary day)",
    9, "Other",
    -1, NA,
    -2, "Don't Know",
    -3, "Refused",
    -9, "No Response"
  ),
  "lived_at_address_range" = tribble(
    ~key, ~value,
    -1, NA,
    1, "Less than 1 year",
    2, "1-2 years",
    3, "3-4 years",
    4, "5 years or longer",
  )
)

# come back to immigrant year of entry - cross-walking issues with 20-22 recodes