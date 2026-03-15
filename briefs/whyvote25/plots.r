# name: plots.r
# purpose: generate plots for the disengagement brief
# authors: bailey inglish, eugenia quintanilla

# libraries
library(tidyverse)
library(gtsummary)
library(haven)
library(ggtext)

# data
#setwd("briefs/whyvote25")
anes <- read_csv("raw_data/anes.csv")
#ces <- read_dta("raw_data/ces.dta")

# theme
ut_orange <- "#BF5700"
ut_blue <- "#005F86"
ut_teal <- "#00A9B7"
ut_gray <- "#666666"

theme_asi <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 13, color = ut_orange),
      plot.subtitle = element_textbox_simple(size = 10, color = ut_gray, margin = margin(b = 10)),
      plot.caption = element_textbox_simple(size = 8, color = ut_gray, hjust = 0, margin = margin(t = 5)),
      axis.title = element_text(size = 10, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = 9),
      strip.text = element_text(face = "bold", size = 10, color = ut_blue),
      strip.background = element_rect(fill = "gray95", color = NA),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray80", fill = NA)
    )
}

theme_set(theme_asi())

# data cleaning
anes_c <- anes %>%
  filter(VCF0004 %in% c(1972, 1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020, 2024)) %>% # filtering to years with presidential elections for consistency
  mutate(
    # filtering
    year = VCF0004,
    state_abv = VCF0901b,
    # outcome
    voted = case_when(
      # [NON-validated/self-reported voter turnout]
      VCF0703 == 3 ~ TRUE, # voted
      VCF0703 %in% c(1, 2) ~ FALSE, # did not vote or not registered
      VCF0703 == 0 ~ NA
    ),
    # covariates
    efficacy = case_when(
      # 'I don’t think public officials care much what people like me think.'
      VCF0609 == 1 ~ 0, # 'Agree' = low efficacy
      VCF0609 == 2 ~ 0.5, # 'Neither agree nor disagree' (1988 and later only) = neutral
      VCF0609 == 3 ~ 1, # 'Disagree' = high efficacy
      VCF0609 %in% c(0, 9) ~ NA_real_
    ),
    trust = case_when(
      # 'Would you say the government is pretty much run by a few big interests
      # looking out for themselves or that it is run for the benefit of all
      # the people?'
      VCF0605 == 1 ~ 0, # 'Few big interests' = low trust
      VCF0605 == 2 ~ 1, # 'Benefit of all' = high trust
      VCF0605 %in% c(0, 9) ~ NA_real_
    ),
    dem_pres_rating = case_when(
      # 'After I read the name of a presidential candidate, please rate them on a scale from 0 to 10, where 0 means you
      # strongly dislike that candidate and 10 means that you strongly like that candidate. If I come to a presidential candidate
      # you haven’t heard of or you feel you do not know enough about, just say so'
      VCF9207 < 0 | VCF9207 >= 97 ~ NA_real_,
      TRUE ~ VCF9207 # ratings from 0 = 'Strongly dislike' <-> 10 = 'Strongly like'
    ),
    rep_pres_rating = case_when(
      # 'After I read the name of a presidential candidate, please rate them on a scale from 0 to 10, where 0 means you
      # strongly dislike that candidate and 10 means that you strongly like that candidate. If I come to a presidential candidate
      # you haven’t heard of or you feel you do not know enough about, just say so'
      VCF9208 < 0 | VCF9208 >= 97 ~ NA_real_,
      TRUE ~ VCF9208 # ratings from 0 = 'Strongly dislike' <-> 10 = 'Strongly like'
    ),
    party3 = case_when(
      # 'Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?'
      VCF0303 == 1 ~ "Democrat", # Democrat or leaning Democrat
      VCF0303 == 2 ~ "Independent", # Independent only
      VCF0303 == 3 ~ "Republican", # Republican or leaning Republican
      VCF0303 == 0 ~ NA_character_
    ),
    partisan_strength = case_when(
      # [strength of partisanship]
      VCF0305 == 1 ~ 0, # Independent
      VCF0305 %in% c(2, 3) ~ 1, # Leaning independent or weak partisan
      VCF0305 == 4 ~ 2, # Strong partisan
      VCF0305 == 0 ~ NA_real_
    ),
    election_interest = case_when(
      # 'Some people don’t pay much attention to political campaigns. How
      # about you, would you say that you have been/were very much interested,
      # somewhat interested, or not much interested in (1952-1998: following)
      # the political campaigns (so far) this year?'
      VCF0310 == 1 ~ 0, # 'Not much interested' = low interest
      VCF0310 == 2 ~ 0.5, # 'Somewhat interested' = medium interest
      VCF0310 == 3 ~ 1, # 'Very much interested' = high interest
      VCF0310 %in% c(0, 9) ~ NA_real_
    ),
    # controls
    is_white_nh = case_when(
      # 'What racial or ethnic group or groups best describes you?'
      VCF0105b == 1 ~ TRUE, # 'White non-Hispanic'
      VCF0105b %in% c(0, 9) ~ NA,
      TRUE ~ FALSE, # [all other valid race/ethnicity responses]
    ),
    is_hispanic = case_when(
      # 'Are you Spanish, Hispanic, or Latino?'
      VCF0108 == 1 ~ TRUE, # 'Yes' = Hispanic
      VCF0108 == 2 ~ FALSE, # 'No' = Not Hispanic
      TRUE ~ NA # 'Don't know/Refused'
    ),
    gender = case_when(
      # [respondent gender]
      VCF0104 == 1 ~ "Male",
      VCF0104 == 2 ~ "Female",
      VCF0104 %in% c(0, 3) ~ NA_character_ # 'Other' option was only available for one survey year
    ),
    age = case_when(
      # 'What is the month, day and year of your birth?' -> [age in years at time of survey]
      VCF0101 <= 0 ~ NA_real_,
      TRUE ~ VCF0101 # [age in years at time of survey]
    ),
    income_percentile = case_when(
      # 'Please mark the answer that includes the income of all members of your family
      # living here in [year] before taxes.' -> [inflation-adjusted income percentile]
      VCF0114 == 1 ~ "0-16",
      VCF0114 == 2 ~ "17-33",
      VCF0114 == 3 ~ "34-67",
      VCF0114 == 4 ~ "68-95",
      VCF0114 == 5 ~ "96-100",
      VCF0114 == 0 ~ NA_character_
    ),
    education = case_when(
      # 'What is the highest level of school you have completed or the
      # highest degree you have received?'
      VCF0110 == 1 ~ "Grade school or less",
      VCF0110 == 2 ~ "Any high school",
      VCF0110 == 3 ~ "Some college",
      VCF0110 == 4 ~ "College or advanced degree",
      VCF0110 == 0 ~ NA_character_
    ),
    # recodes
    dislike_own_party = case_when(
      # [dislike of own party's presidential candidate] - negative values = likes own party canidate -> 0 (does not dislike)
      party3 == "Democrat" ~ if_else(5 - dem_pres_rating < 0, 0, 5 - dem_pres_rating), # reverse code so higher values = more dislike
      party3 == "Republican" ~ if_else(5 - rep_pres_rating < 0, 0, 5 - rep_pres_rating), # reverse code so higher values = more dislike
      TRUE ~ NA_real_ # [Independent or missing party ID]
    ),
    dislike_other_party = case_when(
      # [dislike of other party's presidential candidate] - negative values = likes other party canidate -> 0 (does not dislike)
      party3 == "Democrat" ~ if_else(5 - rep_pres_rating < 0, 0, 5 - rep_pres_rating), # reverse code so higher values = more dislike
      party3 == "Republican" ~ if_else(5 - dem_pres_rating < 0, 0, 5 - dem_pres_rating), # reverse code so higher values = more dislike
      TRUE ~ NA_real_ # [Independent or missing party ID]
    ),
    dislike_both_party = dislike_other_party > 0 & dislike_own_party > 0, # [dislike of both parties' presidential candidates]
    weight = VCF0009z, # [survey weight]
    .keep = "none"
  ) %>%
  filter(year > 1970) %>%
  mutate(
    party3 = factor(party3, levels = c("Independent", "Democrat", "Republican")),
    gender = factor(gender, levels = c("Female", "Male", "Other")),
    income_percentile = factor(
      income_percentile,
      levels = c("34-67", "0-16", "17-33", "68-95", "96-100")
    ),
    education = factor(
      education,
      levels = c("Any high school", "Grade school or less", "Some college", "College or advanced degree")
    )
  )

# test model
test_model <- glm(
  voted ~ election_interest +
    efficacy + trust +
    party3 + partisan_strength +
    dislike_own_party + dislike_other_party +
    is_white_nh + is_hispanic + gender + age +
    income_percentile + education + year,
  data = anes_c,
  weights = weight,
  family = quasibinomial
)


# ── plot utilities ────────────────────────────────────────────────────────────

# Internal: convert a snake_case variable name to a Title Case display label
.to_title <- function(x) {
  tools::toTitleCase(gsub("_", " ", x))
}

# Internal: UT brand palette, cycled to the required length
.ut_palette <- function(n) {
  rep_len(c(ut_gray, ut_teal, ut_orange, ut_blue), n)
}

# Internal: apply human-readable labels to the group_var column of an
# aggregated data frame, mapping sorted raw levels → color_labels in order.
.apply_labels <- function(agg, group_var, grp_vals, color_labels) {
  label_map <- setNames(color_labels, as.character(grp_vals))
  agg[[group_var]] <- label_map[as.character(agg[[group_var]])]
  agg
}

# Internal: shared data-prep used by both plot_turnout() and export_turnout_data().
#   - Applies exact-match filters, drops NAs on key columns, aggregates to
#     weighted rates, and computes per-x_var n for the caption.
.prep_turnout_data <- function(
    data, outcome, group_var, x_var, weight_var,
    filters, drop_na_vars
) {
  df <- data

  # Exact-match subsetting (each filter value treated as %in%)
  if (length(filters) > 0) {
    for (nm in names(filters)) {
      if (!nm %in% names(df))
        stop(sprintf("Filter variable '%s' not found in data.", nm))
      df <- df[df[[nm]] %in% filters[[nm]], ]
    }
  }

  # Drop rows with NA in outcome, group_var, and any user-supplied extras
  if (is.null(drop_na_vars)) drop_na_vars <- group_var
  vars_to_check <- intersect(unique(c(outcome, group_var, drop_na_vars)), names(df))
  df <- df[complete.cases(df[, vars_to_check, drop = FALSE]), ]

  if (nrow(df) == 0)
    stop("No data remaining after applying filters and dropping NAs.")

  # N per x_var unit (used in the auto-caption)
  n_by_x <- df %>%
    group_by(across(all_of(x_var))) %>%
    summarize(n = n(), .groups = "drop") %>%
    pull(n)

  # Aggregate outcome to weighted rate; keep cell-level n for export
  agg <- df %>%
    group_by(across(all_of(c(x_var, group_var)))) %>%
    summarize(
      rate = weighted.mean(.data[[outcome]], .data[[weight_var]], na.rm = TRUE),
      n    = n(),
      .groups = "drop"
    )

  list(agg = agg, n_min = min(n_by_x), n_max = max(n_by_x), n_total = nrow(df))
}

# Internal: resolve the file stem (no extension) for saving plot + data.
# Creates the output directory if it doesn't exist.
.resolve_file_stem <- function(file) {
  dir <- dirname(file)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  tools::file_path_sans_ext(file)
}

#' Plot weighted outcome rate over time, colored by a grouping variable.
#'
#' Parameters are identical to export_turnout_data() — the same arguments can
#' be passed to either function to visualize or extract the underlying data.
#'
#' @param data          Data frame to use (e.g. anes_c, or any filtered subset).
#' @param outcome       Name of the binary 0/1 outcome column. Default "voted".
#' @param group_var     Column used to color lines. Default "election_interest".
#' @param x_var         X-axis (time) column name. Default "year".
#' @param weight_var    Survey weight column name. Default "weight".
#' @param filters       Named list for exact-match subsetting before aggregation,
#'                      e.g. list(state = "TX", party3 = c("Democrat","Republican")).
#' @param drop_na_vars  Character vector of extra columns that must be non-NA.
#'                      Defaults to group_var only.
#' @param title         Plot title. NULL autogenerates "{outcome} by {group_var}".
#' @param subtitle      Subtitle string, or NULL to omit.
#' @param caption_notes Extra text appended to the auto-built caption.
#' @param source        Data source attribution shown in the caption.
#' @param color_values  Character vector of hex colors for the group levels.
#'                      Defaults to cycling UT brand colors (gray → teal → orange → blue).
#' @param color_labels  Character vector of legend display labels for each level.
#'                      Defaults to the raw level values from the data.
#' @param color_name    Legend title. NULL autogenerates from group_var.
#' @param y_label       Y-axis label. Default "Turnout Rate (%)".
#' @param x_label       X-axis label. Default "Year".
#' @param y_limits      Numeric c(min, max) for y-axis, or NULL to let ggplot scale freely.
#' @param file          Base file path (no extension) under plot_data/. When supplied,
#'                      saves <file>.png (plot image) and <file>.csv (labeled data).
#'                      E.g. "final_data/plot_data/turnout_interest".
plot_turnout <- function(
    data,
    outcome       = "voted",
    group_var     = "election_interest",
    x_var         = "year",
    weight_var    = "weight",
    filters       = list(),
    drop_na_vars  = NULL,
    title         = NULL,
    subtitle      = NULL,
    caption_notes = NULL,
    source        = "American National Election Studies (ANES) 1972-2024. Turnout was self-reported.",
    color_values  = NULL,
    color_labels  = NULL,
    color_name    = NULL,
    y_label       = "Turnout Rate (%)",
    x_label       = "Year",
    y_limits      = c(0, 100),
    plot_type     = "line",
    file          = NULL
) {
  prep  <- .prep_turnout_data(data, outcome, group_var, x_var, weight_var,
                               filters, drop_na_vars)
  agg   <- prep$agg
  n_min <- prep$n_min
  n_max <- prep$n_max

  # Resolve group levels in natural sort order
  grp_vals <- sort(unique(agg[[group_var]]))
  n_levels <- length(grp_vals)

  if (is.null(color_values)) color_values <- .ut_palette(n_levels)
  if (is.null(color_labels)) color_labels <- as.character(grp_vals)
  if (is.null(color_name))   color_name   <- .to_title(group_var)

  # Autogenerate title
  plot_title <- if (!is.null(title)) title else {
    paste0(.to_title(outcome), " by ", .to_title(group_var))
  }

  # Build filter note for caption
  filter_str <- if (length(filters) > 0) {
    pairs <- mapply(
      function(k, v)
        paste0(.to_title(k), " \u2208 {", paste(v, collapse = ", "), "}"),
      names(filters), filters, SIMPLIFY = TRUE
    )
    paste0(" Filtered to: ", paste(pairs, collapse = "; "), ".")
  } else ""

  # Split "Grouped by: ..." out of subtitle into the caption
  grouped_by_note <- NULL
  if (!is.null(subtitle) && grepl(" Grouped by: ", subtitle, fixed = TRUE)) {
    parts           <- strsplit(subtitle, " Grouped by: ", fixed = TRUE)[[1]]
    subtitle        <- parts[1]
    grouped_by_note <- paste0("Grouped by: ", parts[2])
  }

  n_str   <- if (n_min == n_max) as.character(n_min) else paste0(n_min, "\u2013", n_max)
  caption <- paste0(
    source,
    filter_str,
    " (n\u2009=\u2009", n_str, " per ", x_var, ")",
    if (!is.null(grouped_by_note)) paste0(" ", grouped_by_note) else "",
    if (!is.null(caption_notes)) paste0(" ", caption_notes) else ""
  )

  # Enforce factor order so color scale matches legend order
  agg[[group_var]] <- factor(agg[[group_var]], levels = grp_vals)

  # Determine if we should scale outcome by 100 (for percentages) based on y_limits
  scale_by_100 <- !is.null(y_limits) && y_limits[2] > 1

  if (plot_type == "bar") {
    p <- agg %>%
      ggplot(aes(
        x    = .data[[x_var]],
        y    = if (scale_by_100) rate * 100 else rate,
        fill = .data[[group_var]]
      )) +
      geom_col(position = "dodge") +
      scale_fill_manual(
        values = color_values,
        labels = color_labels,
        name   = color_name
      ) +
      labs(
        title    = plot_title,
        subtitle = subtitle,
        x        = x_label,
        y        = y_label,
        caption  = caption
      )
  } else {
    p <- agg %>%
      ggplot(aes(
        x     = .data[[x_var]],
        y     = if (scale_by_100) rate * 100 else rate,
        color = .data[[group_var]]
      )) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_color_manual(
        values = color_values,
        labels = color_labels,
        name   = color_name
      ) +
      labs(
        title    = plot_title,
        subtitle = subtitle,
        x        = x_label,
        y        = y_label,
        caption  = caption
      )
  }

  if (!is.null(y_limits)) p <- p + ylim(y_limits[1], y_limits[2])

  # Save image + labeled CSV when file is supplied
  if (!is.null(file)) {
    plot_stem <- .resolve_file_stem(sub("^plot_data/", "plot_data/plots/", file))
    data_stem <- .resolve_file_stem(sub("^plot_data/", "plot_data/data/",  file))
    # Save PNG
    ggsave(paste0(plot_stem, ".png"), plot = p, width = 9, height = 5.5, dpi = 150)
    # Save CSV with human-readable group labels
    out_labeled <- .apply_labels(agg, group_var, grp_vals, color_labels)
    out_labeled[[group_var]] <- as.character(out_labeled[[group_var]])
    out_labeled$rate <- round(if (scale_by_100) out_labeled$rate * 100 else out_labeled$rate, 2)
    col_name <- if (scale_by_100) paste0(outcome, "_rate_pct") else paste0(outcome, "_score")
    names(out_labeled)[names(out_labeled) == "rate"] <- col_name
    write.csv(out_labeled, paste0(data_stem, ".csv"), row.names = FALSE)
    message(sprintf("Saved plot → %s.png | data → %s.csv", plot_stem, data_stem))
  }

  p
}

#' Export the aggregated data underlying a plot_turnout() call.
#'
#' Every parameter mirrors plot_turnout(). Returns a tidy data frame with
#' human-readable group labels. If file is supplied, writes to disk.
#'
#' @param file  Output file path (with or without extension). ".rds" → saveRDS(),
#'              anything else → write.csv(). NULL (default) returns without writing.
#' @inheritParams plot_turnout
export_turnout_data <- function(
    data,
    outcome      = "voted",
    group_var    = "election_interest",
    x_var        = "year",
    weight_var   = "weight",
    filters      = list(),
    drop_na_vars = NULL,
    color_labels = NULL,
    file         = NULL
) {
  prep     <- .prep_turnout_data(data, outcome, group_var, x_var, weight_var,
                                  filters, drop_na_vars)
  agg      <- prep$agg
  grp_vals <- sort(unique(agg[[group_var]]))

  if (is.null(color_labels)) color_labels <- as.character(grp_vals)

  # Apply human-readable labels and clean up rate column
  out <- .apply_labels(agg, group_var, grp_vals, color_labels)
  out$rate <- round(out$rate * 100, 2)
  names(out)[names(out) == "rate"] <- paste0(outcome, "_rate_pct")

  if (!is.null(file)) {
    stem <- .resolve_file_stem(sub("^plot_data/", "plot_data/data/", file))
    ext  <- tolower(tools::file_ext(file))
    if (ext == "rds") saveRDS(out, paste0(stem, ".rds")) else write.csv(out, paste0(stem, ".csv"), row.names = FALSE)
    message(sprintf("Exported %d rows to %s.csv", nrow(out), stem))
  }

  invisible(out)
}

# ── generating plots ──────────────────────────────────────────────────────────

# 1 - election interest and turnout
plot_turnout(
  anes_c,
  title         = "Voter Turnout by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = paste(
    "Would you say that you have been very much interested,",
    "somewhat interested, or not much interested in the political campaigns this year?"
  ),
  file          = "plot_data/all/1_turnout_election_interest"
)

# 2 - party identification and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Party Identification",
  group_var    = "party3",
  color_values = c(ut_gray, ut_blue, ut_orange),
  color_labels = c("Independent", "Democrat", "Republican"),
  subtitle     = paste(
    "Generally speaking, do you usually think of yourself as a Republican,",
    "a Democrat, an Independent, or what?"
  ),
  file         = "plot_data/all/2_turnout_party3"
)

# 3 - partisan strength and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Strength of Partisanship",
  group_var    = "partisan_strength",
  color_values = c(ut_gray, ut_teal, ut_orange),
  color_labels = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle     = "How strong is your attachment to your party?",
  file         = "plot_data/all/3_turnout_partisan_strength"
)

# 4 - gender and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Gender",
  group_var    = "gender",
  color_values = c(ut_orange, ut_blue, ut_teal),
  color_labels = c("Female", "Male", "Other"),
  subtitle     = "Respondent gender",
  file         = "plot_data/all/4_turnout_gender"
)

# 5 - income percentile and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Household Income Percentile",
  group_var    = "income_percentile",
  color_values = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                   "68–95th pctile", "96–100th pctile"),
  subtitle     = paste(
    "Please mark the answer that includes the income of all members of",
    "your family living here before taxes. (Inflation-adjusted percentile)"
  ),
  file         = "plot_data/all/5_turnout_income_percentile"
)

# 6 - education and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Educational Attainment",
  group_var    = "education",
  color_values = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels = c("Grade school or less", "Any high school",
                   "Some college", "College or advanced degree"),
  subtitle     = paste(
    "What is the highest level of school you have completed",
    "or the highest degree you have received?"
  ),
  file         = "plot_data/all/6_turnout_education"
)

# 7 - internal political efficacy and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Internal Political Efficacy",
  group_var    = "efficacy",
  color_values = c(ut_orange, ut_teal, ut_blue),
  color_labels = c("Low (agree officials don't care)",
                   "Neutral",
                   "High (disagree officials don't care)"),
  subtitle     = "\"I don't think public officials care much what people like me think.\"",
  file         = "plot_data/all/7_turnout_efficacy"
)

# 8 - institutional trust and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Trust in Government",
  group_var    = "trust",
  color_values = c(ut_orange, ut_blue),
  color_labels = c("Low trust (run for few big interests)",
                   "High trust (run for benefit of all)"),
  subtitle     = paste(
    "Would you say the government is pretty much run by a few big interests",
    "looking out for themselves, or that it is run for the benefit of all the people?"
  ),
  file         = "plot_data/all/8_turnout_trust"
)

# 9 - race (white non-hispanic) and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Race (White Non-Hispanic)",
  group_var    = "is_white_nh",
  color_values = c(ut_teal, ut_orange),
  color_labels = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle     = "What racial or ethnic group or groups best describes you?",
  file         = "plot_data/all/9_turnout_is_white_nh"
)

# 10 - hispanic ethnicity and turnout
plot_turnout(
  anes_c,
  title        = "Voter Turnout by Hispanic Ethnicity",
  group_var    = "is_hispanic",
  color_values = c(ut_gray, ut_orange),
  color_labels = c("Not Hispanic", "Hispanic"),
  subtitle     = "Are you Spanish, Hispanic, or Latino?",
  file         = "plot_data/all/10_turnout_is_hispanic"
)

# ── young people (age < 30) ───────────────────────────────────────────────────

# Y1 - election interest and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title         = "Youth Voter Turnout by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = paste(
    "Would you say that you have been very much interested,",
    "somewhat interested, or not much interested in the political campaigns this year?"
  ),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y1_turnout_election_interest"
)

# Y2 - party identification and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Party Identification",
  group_var    = "party3",
  color_values = c(ut_gray, ut_blue, ut_orange),
  color_labels = c("Independent", "Democrat", "Republican"),
  subtitle     = paste(
    "Generally speaking, do you usually think of yourself as a Republican,",
    "a Democrat, an Independent, or what?"
  ),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y2_turnout_party3"
)

# Y3 - partisan strength and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Strength of Partisanship",
  group_var    = "partisan_strength",
  color_values = c(ut_gray, ut_teal, ut_orange),
  color_labels = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle     = "How strong is your attachment to your party?",
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y3_turnout_partisan_strength"
)

# Y4 - gender and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Gender",
  group_var    = "gender",
  color_values = c(ut_orange, ut_blue, ut_teal),
  color_labels = c("Female", "Male", "Other"),
  subtitle     = "Respondent gender",
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y4_turnout_gender"
)

# Y5 - income percentile and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Household Income Percentile",
  group_var    = "income_percentile",
  color_values = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                   "68–95th pctile", "96–100th pctile"),
  subtitle     = paste(
    "Please mark the answer that includes the income of all members of",
    "your family living here before taxes. (Inflation-adjusted percentile)"
  ),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y5_turnout_income_percentile"
)

# Y6 - education and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Educational Attainment",
  group_var    = "education",
  color_values = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels = c("Grade school or less", "Any high school",
                   "Some college", "College or advanced degree"),
  subtitle     = paste(
    "What is the highest level of school you have completed",
    "or the highest degree you have received?"
  ),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y6_turnout_education"
)

# Y7 - internal political efficacy and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Internal Political Efficacy",
  group_var    = "efficacy",
  color_values = c(ut_orange, ut_teal, ut_blue),
  color_labels = c("Low (agree officials don't care)",
                   "Neutral",
                   "High (disagree officials don't care)"),
  subtitle     = "\"I don't think public officials care much what people like me think.\"",
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y7_turnout_efficacy"
)

# Y8 - institutional trust and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Trust in Government",
  group_var    = "trust",
  color_values = c(ut_orange, ut_blue),
  color_labels = c("Low trust (run for few big interests)",
                   "High trust (run for benefit of all)"),
  subtitle     = paste(
    "Would you say the government is pretty much run by a few big interests",
    "looking out for themselves, or that it is run for the benefit of all the people?"
  ),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y8_turnout_trust"
)

# Y9 - race (white non-hispanic) and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Race (White Non-Hispanic)",
  group_var    = "is_white_nh",
  color_values = c(ut_teal, ut_orange),
  color_labels = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle     = "What racial or ethnic group or groups best describes you?",
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y9_turnout_is_white_nh"
)

# Y10 - hispanic ethnicity and turnout (youth)
plot_turnout(
  filter(anes_c, age < 30),
  title        = "Youth Voter Turnout by Hispanic Ethnicity",
  group_var    = "is_hispanic",
  color_values = c(ut_gray, ut_orange),
  color_labels = c("Not Hispanic", "Hispanic"),
  subtitle     = "Are you Spanish, Hispanic, or Latino?",
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/Y10_turnout_is_hispanic"
)

# ── texas ─────────────────────────────────────────────────────────────────────

# T1 - election interest and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title         = "Texas Voter Turnout by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = paste(
    "Would you say that you have been very much interested,",
    "somewhat interested, or not much interested in the political campaigns this year?"
  ),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T1_turnout_election_interest"
)

# T2 - party identification and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Party Identification",
  group_var    = "party3",
  color_values = c(ut_gray, ut_blue, ut_orange),
  color_labels = c("Independent", "Democrat", "Republican"),
  subtitle     = paste(
    "Generally speaking, do you usually think of yourself as a Republican,",
    "a Democrat, an Independent, or what?"
  ),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T2_turnout_party3"
)

# T3 - partisan strength and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Strength of Partisanship",
  group_var    = "partisan_strength",
  color_values = c(ut_gray, ut_teal, ut_orange),
  color_labels = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle     = "How strong is your attachment to your party?",
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T3_turnout_partisan_strength"
)

# T4 - gender and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Gender",
  group_var    = "gender",
  color_values = c(ut_orange, ut_blue, ut_teal),
  color_labels = c("Female", "Male", "Other"),
  subtitle     = "Respondent gender",
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T4_turnout_gender"
)

# T5 - income percentile and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Household Income Percentile",
  group_var    = "income_percentile",
  color_values = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                   "68–95th pctile", "96–100th pctile"),
  subtitle     = paste(
    "Please mark the answer that includes the income of all members of",
    "your family living here before taxes. (Inflation-adjusted percentile)"
  ),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T5_turnout_income_percentile"
)

# T6 - education and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Educational Attainment",
  group_var    = "education",
  color_values = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels = c("Grade school or less", "Any high school",
                   "Some college", "College or advanced degree"),
  subtitle     = paste(
    "What is the highest level of school you have completed",
    "or the highest degree you have received?"
  ),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T6_turnout_education"
)

# T7 - internal political efficacy and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Internal Political Efficacy",
  group_var    = "efficacy",
  color_values = c(ut_orange, ut_teal, ut_blue),
  color_labels = c("Low (agree officials don't care)",
                   "Neutral",
                   "High (disagree officials don't care)"),
  subtitle     = "\"I don't think public officials care much what people like me think.\"",
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T7_turnout_efficacy"
)

# T8 - institutional trust and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Trust in Government",
  group_var    = "trust",
  color_values = c(ut_orange, ut_blue),
  color_labels = c("Low trust (run for few big interests)",
                   "High trust (run for benefit of all)"),
  subtitle     = paste(
    "Would you say the government is pretty much run by a few big interests",
    "looking out for themselves, or that it is run for the benefit of all the people?"
  ),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T8_turnout_trust"
)

# T9 - race (white non-hispanic) and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Race (White Non-Hispanic)",
  group_var    = "is_white_nh",
  color_values = c(ut_teal, ut_orange),
  color_labels = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle     = "What racial or ethnic group or groups best describes you?",
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T9_turnout_is_white_nh"
)

# T10 - hispanic ethnicity and turnout (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  title        = "Texas Voter Turnout by Hispanic Ethnicity",
  group_var    = "is_hispanic",
  color_values = c(ut_gray, ut_orange),
  color_labels = c("Not Hispanic", "Hispanic"),
  subtitle     = "Are you Spanish, Hispanic, or Latino?",
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/T10_turnout_is_hispanic"
)

# ── efficacy outcome ──────────────────────────────────────────────────────────
# Outcome: efficacy (0 = low, 0.5 = neutral, 1 = high)
# Group vars: all categorical predictors except voted

# E1 - election interest and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Campaign Interest",
  group_var    = "election_interest",
  color_labels = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E1_efficacy_election_interest"
)

# E2 - party identification and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Party Identification",
  group_var    = "party3",
  color_values = c(ut_gray, ut_blue, ut_orange),
  color_labels = c("Independent", "Democrat", "Republican"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E2_efficacy_party3"
)

# E3 - partisan strength and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Strength of Partisanship",
  group_var    = "partisan_strength",
  color_values = c(ut_gray, ut_teal, ut_orange),
  color_labels = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Strength of partisan identification.",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E3_efficacy_partisan_strength"
)

# E4 - gender and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Gender",
  group_var    = "gender",
  color_values = c(ut_orange, ut_blue, ut_teal),
  color_labels = c("Female", "Male", "Other"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Respondent gender.",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E4_efficacy_gender"
)

# E5 - income percentile and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Household Income Percentile",
  group_var    = "income_percentile",
  color_values = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                   "68–95th pctile", "96–100th pctile"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Inflation-adjusted household income percentile.",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E5_efficacy_income_percentile"
)

# E6 - education and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Educational Attainment",
  group_var    = "education",
  color_values = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels = c("Grade school or less", "Any high school",
                   "Some college", "College or advanced degree"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Highest level of school completed or degree received.",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E6_efficacy_education"
)

# E7 - trust and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Trust in Government",
  group_var    = "trust",
  color_values = c(ut_orange, ut_blue),
  color_labels = c("Low trust (run for few big interests)",
                   "High trust (run for benefit of all)"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say the government is pretty much run by a few big interests looking out for themselves, or that it is run for the benefit of all the people? (0 = Few big interests; 1 = Benefit of all)",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E7_efficacy_trust"
)

# E8 - race (white non-hispanic) and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Race (White Non-Hispanic)",
  group_var    = "is_white_nh",
  color_values = c(ut_teal, ut_orange),
  color_labels = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: What racial or ethnic group or groups best describes you?",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E8_efficacy_is_white_nh"
)

# E9 - hispanic ethnicity and efficacy
plot_turnout(
  anes_c,
  outcome      = "efficacy",
  title        = "Political Efficacy by Hispanic Ethnicity",
  group_var    = "is_hispanic",
  color_values = c(ut_gray, ut_orange),
  color_labels = c("Not Hispanic", "Hispanic"),
  subtitle     = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label      = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits     = c(0, 1),
  file         = "plot_data/all/E9_efficacy_is_hispanic"
)

# ── trust in government outcome ───────────────────────────────────────────────
# Outcome: trust (0 = low trust, 1 = high trust)
# Group vars: all categorical predictors except voted

# TR1 - election interest and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Campaign Interest",
  group_var    = "election_interest",
  color_labels = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR1_trust_election_interest"
)

# TR2 - party identification and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Party Identification",
  group_var    = "party3",
  color_values = c(ut_gray, ut_blue, ut_orange),
  color_labels = c("Independent", "Democrat", "Republican"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR2_trust_party3"
)

# TR3 - partisan strength and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Strength of Partisanship",
  group_var    = "partisan_strength",
  color_values = c(ut_gray, ut_teal, ut_orange),
  color_labels = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Strength of partisan identification.",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR3_trust_partisan_strength"
)

# TR4 - gender and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Gender",
  group_var    = "gender",
  color_values = c(ut_orange, ut_blue, ut_teal),
  color_labels = c("Female", "Male", "Other"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Respondent gender.",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR4_trust_gender"
)

# TR5 - income percentile and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Household Income Percentile",
  group_var    = "income_percentile",
  color_values = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                   "68–95th pctile", "96–100th pctile"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Inflation-adjusted household income percentile.",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR5_trust_income_percentile"
)

# TR6 - education and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Educational Attainment",
  group_var    = "education",
  color_values = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels = c("Grade school or less", "Any high school",
                   "Some college", "College or advanced degree"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Highest level of school completed or degree received.",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR6_trust_education"
)

# TR7 - efficacy and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Internal Political Efficacy",
  group_var    = "efficacy",
  color_values = c(ut_orange, ut_teal, ut_blue),
  color_labels = c("Low (agree officials don't care)",
                   "Neutral",
                   "High (disagree officials don't care)"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: \"I don't think public officials care much what people like me think.\" (0 = Agree/Low efficacy; 0.5 = Neutral; 1 = Disagree/High efficacy)",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR7_trust_efficacy"
)

# TR8 - race (white non-hispanic) and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Race (White Non-Hispanic)",
  group_var    = "is_white_nh",
  color_values = c(ut_teal, ut_orange),
  color_labels = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: What racial or ethnic group or groups best describes you?",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR8_trust_is_white_nh"
)

# TR9 - hispanic ethnicity and trust
plot_turnout(
  anes_c,
  outcome      = "trust",
  title        = "Trust in Government by Hispanic Ethnicity",
  group_var    = "is_hispanic",
  color_values = c(ut_gray, ut_orange),
  color_labels = c("Not Hispanic", "Hispanic"),
  subtitle     = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label      = "Share Saying Gov't Benefits All (%)",
  y_limits     = c(0, 100),
  file         = "plot_data/all/TR9_trust_is_hispanic"
)

# ── young people (age < 30) — efficacy outcome ────────────────────────────────

# E_Y1 - election interest and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y1_efficacy_election_interest"
)

# E_Y2 - party identification and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Party Identification",
  group_var     = "party3",
  color_values  = c(ut_gray, ut_blue, ut_orange),
  color_labels  = c("Independent", "Democrat", "Republican"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y2_efficacy_party3"
)

# E_Y3 - partisan strength and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Strength of Partisanship",
  group_var     = "partisan_strength",
  color_values  = c(ut_gray, ut_teal, ut_orange),
  color_labels  = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Strength of partisan identification.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y3_efficacy_partisan_strength"
)

# E_Y4 - gender and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Gender",
  group_var     = "gender",
  color_values  = c(ut_orange, ut_blue, ut_teal),
  color_labels  = c("Female", "Male", "Other"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Respondent gender.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y4_efficacy_gender"
)

# E_Y5 - income percentile and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Household Income Percentile",
  group_var     = "income_percentile",
  color_values  = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels  = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                    "68–95th pctile", "96–100th pctile"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Inflation-adjusted household income percentile.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y5_efficacy_income_percentile"
)

# E_Y6 - education and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Educational Attainment",
  group_var     = "education",
  color_values  = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels  = c("Grade school or less", "Any high school",
                    "Some college", "College or advanced degree"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Highest level of school completed or degree received.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y6_efficacy_education"
)

# E_Y7 - trust and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Trust in Government",
  group_var     = "trust",
  color_values  = c(ut_orange, ut_blue),
  color_labels  = c("Low trust (run for few big interests)",
                    "High trust (run for benefit of all)"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say the government is run by a few big interests [0] or for the benefit of all [1]?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y7_efficacy_trust"
)

# E_Y8 - race (white non-hispanic) and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Race (White Non-Hispanic)",
  group_var     = "is_white_nh",
  color_values  = c(ut_teal, ut_orange),
  color_labels  = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: What racial or ethnic group or groups best describes you?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y8_efficacy_is_white_nh"
)

# E_Y9 - hispanic ethnicity and efficacy (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "efficacy",
  title         = "Youth Political Efficacy by Hispanic Ethnicity",
  group_var     = "is_hispanic",
  color_values  = c(ut_gray, ut_orange),
  color_labels  = c("Not Hispanic", "Hispanic"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/E_Y9_efficacy_is_hispanic"
)

# ── texas — efficacy outcome ──────────────────────────────────────────────────

# E_T1 - election interest and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T1_efficacy_election_interest"
)

# E_T2 - party identification and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Party Identification",
  group_var     = "party3",
  color_values  = c(ut_gray, ut_blue, ut_orange),
  color_labels  = c("Independent", "Democrat", "Republican"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T2_efficacy_party3"
)

# E_T3 - partisan strength and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Strength of Partisanship",
  group_var     = "partisan_strength",
  color_values  = c(ut_gray, ut_teal, ut_orange),
  color_labels  = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Strength of partisan identification.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T3_efficacy_partisan_strength"
)

# E_T4 - gender and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Gender",
  group_var     = "gender",
  color_values  = c(ut_orange, ut_blue, ut_teal),
  color_labels  = c("Female", "Male", "Other"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Respondent gender.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T4_efficacy_gender"
)

# E_T5 - income percentile and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Household Income Percentile",
  group_var     = "income_percentile",
  color_values  = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels  = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                    "68–95th pctile", "96–100th pctile"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Inflation-adjusted household income percentile.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T5_efficacy_income_percentile"
)

# E_T6 - education and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Educational Attainment",
  group_var     = "education",
  color_values  = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels  = c("Grade school or less", "Any high school",
                    "Some college", "College or advanced degree"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Highest level of school completed or degree received.",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T6_efficacy_education"
)

# E_T7 - trust and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Trust in Government",
  group_var     = "trust",
  color_values  = c(ut_orange, ut_blue),
  color_labels  = c("Low trust (run for few big interests)",
                    "High trust (run for benefit of all)"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Would you say the government is run by a few big interests [0] or for the benefit of all [1]?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T7_efficacy_trust"
)

# E_T8 - race (white non-hispanic) and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Race (White Non-Hispanic)",
  group_var     = "is_white_nh",
  color_values  = c(ut_teal, ut_orange),
  color_labels  = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: What racial or ethnic group or groups best describes you?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T8_efficacy_is_white_nh"
)

# E_T9 - hispanic ethnicity and efficacy (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "efficacy",
  title         = "Texas Political Efficacy by Hispanic Ethnicity",
  group_var     = "is_hispanic",
  color_values  = c(ut_gray, ut_orange),
  color_labels  = c("Not Hispanic", "Hispanic"),
  subtitle      = "\"I don't think public officials care much what people like me think.\" (Agree = Low efficacy [0]; Disagree = High efficacy [1]). Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label       = "Mean Efficacy Score (0 = Low, 1 = High)",
  y_limits      = c(0, 1),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/E_T9_efficacy_is_hispanic"
)

# ── young people (age < 30) — trust outcome ───────────────────────────────────

# TR_Y1 - election interest and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y1_trust_election_interest"
)

# TR_Y2 - party identification and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Party Identification",
  group_var     = "party3",
  color_values  = c(ut_gray, ut_blue, ut_orange),
  color_labels  = c("Independent", "Democrat", "Republican"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y2_trust_party3"
)

# TR_Y3 - partisan strength and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Strength of Partisanship",
  group_var     = "partisan_strength",
  color_values  = c(ut_gray, ut_teal, ut_orange),
  color_labels  = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Strength of partisan identification.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y3_trust_partisan_strength"
)

# TR_Y4 - gender and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Gender",
  group_var     = "gender",
  color_values  = c(ut_orange, ut_blue, ut_teal),
  color_labels  = c("Female", "Male", "Other"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Respondent gender.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y4_trust_gender"
)

# TR_Y5 - income percentile and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Household Income Percentile",
  group_var     = "income_percentile",
  color_values  = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels  = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                    "68–95th pctile", "96–100th pctile"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Inflation-adjusted household income percentile.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y5_trust_income_percentile"
)

# TR_Y6 - education and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Educational Attainment",
  group_var     = "education",
  color_values  = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels  = c("Grade school or less", "Any high school",
                    "Some college", "College or advanced degree"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Highest level of school completed or degree received.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y6_trust_education"
)

# TR_Y7 - efficacy and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Internal Political Efficacy",
  group_var     = "efficacy",
  color_values  = c(ut_orange, ut_teal, ut_blue),
  color_labels  = c("Low (agree officials don't care)",
                    "Neutral",
                    "High (disagree officials don't care)"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: \"I don't think public officials care much what people like me think.\" (0 = Agree/Low efficacy; 0.5 = Neutral; 1 = Disagree/High efficacy)",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y7_trust_efficacy"
)

# TR_Y8 - race (white non-hispanic) and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Race (White Non-Hispanic)",
  group_var     = "is_white_nh",
  color_values  = c(ut_teal, ut_orange),
  color_labels  = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: What racial or ethnic group or groups best describes you?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y8_trust_is_white_nh"
)

# TR_Y9 - hispanic ethnicity and trust (youth)
plot_turnout(
  filter(anes_c, age < 30),
  outcome       = "trust",
  title         = "Youth Trust in Government by Hispanic Ethnicity",
  group_var     = "is_hispanic",
  color_values  = c(ut_gray, ut_orange),
  color_labels  = c("Not Hispanic", "Hispanic"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to respondents under age 30.",
  file          = "plot_data/youth/TR_Y9_trust_is_hispanic"
)

# ── texas — trust outcome ─────────────────────────────────────────────────────

# TR_T1 - election interest and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Campaign Interest",
  group_var     = "election_interest",
  color_labels  = c("Not much interested", "Somewhat interested", "Very much interested"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Would you say that you have been very much interested, somewhat interested, or not much interested in the political campaigns this year?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T1_trust_election_interest"
)

# TR_T2 - party identification and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Party Identification",
  group_var     = "party3",
  color_values  = c(ut_gray, ut_blue, ut_orange),
  color_labels  = c("Independent", "Democrat", "Republican"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Generally speaking, do you usually think of yourself as a Republican, a Democrat, an Independent, or what?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T2_trust_party3"
)

# TR_T3 - partisan strength and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Strength of Partisanship",
  group_var     = "partisan_strength",
  color_values  = c(ut_gray, ut_teal, ut_orange),
  color_labels  = c("Independent", "Leaning/Weak partisan", "Strong partisan"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Strength of partisan identification.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T3_trust_partisan_strength"
)

# TR_T4 - gender and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Gender",
  group_var     = "gender",
  color_values  = c(ut_orange, ut_blue, ut_teal),
  color_labels  = c("Female", "Male", "Other"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Respondent gender.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T4_trust_gender"
)

# TR_T5 - income percentile and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Household Income Percentile",
  group_var     = "income_percentile",
  color_values  = c(ut_gray, ut_teal, ut_blue, ut_orange, "#4B0082"),
  color_labels  = c("0–16th pctile", "17–33rd pctile", "34–67th pctile",
                    "68–95th pctile", "96–100th pctile"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Inflation-adjusted household income percentile.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T5_trust_income_percentile"
)

# TR_T6 - education and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Educational Attainment",
  group_var     = "education",
  color_values  = c(ut_teal, ut_gray, ut_orange, ut_blue),
  color_labels  = c("Grade school or less", "Any high school",
                    "Some college", "College or advanced degree"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Highest level of school completed or degree received.",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T6_trust_education"
)

# TR_T7 - efficacy and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Internal Political Efficacy",
  group_var     = "efficacy",
  color_values  = c(ut_orange, ut_teal, ut_blue),
  color_labels  = c("Low (agree officials don't care)",
                    "Neutral",
                    "High (disagree officials don't care)"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: \"I don't think public officials care much what people like me think.\" (0 = Agree/Low efficacy; 0.5 = Neutral; 1 = Disagree/High efficacy)",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T7_trust_efficacy"
)

# TR_T8 - race (white non-hispanic) and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Race (White Non-Hispanic)",
  group_var     = "is_white_nh",
  color_values  = c(ut_teal, ut_orange),
  color_labels  = c("Non-white or Hispanic", "White non-Hispanic"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: What racial or ethnic group or groups best describes you?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T8_trust_is_white_nh"
)

# TR_T9 - hispanic ethnicity and trust (texas)
plot_turnout(
  filter(anes_c, state_abv == "TX"),
  outcome       = "trust",
  title         = "Texas Trust in Government by Hispanic Ethnicity",
  group_var     = "is_hispanic",
  color_values  = c(ut_gray, ut_orange),
  color_labels  = c("Not Hispanic", "Hispanic"),
  subtitle      = "'Is the government run by a few big interests [0] or for the benefit of all [1]?' Grouped by: Are you Spanish, Hispanic, or Latino?",
  y_label       = "Share Saying Gov't Benefits All (%)",
  y_limits      = c(0, 100),
  caption_notes = "Restricted to Texas respondents.",
  file          = "plot_data/texas/TR_T9_trust_is_hispanic"
)
