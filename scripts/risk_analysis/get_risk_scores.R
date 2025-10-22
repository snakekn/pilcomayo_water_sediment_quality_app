# get the base functions loaded
source(here::here("scripts/risk_analysis/get_risk_scores.R"))

# Load libraries
library(dplyr)
library(purrr)
library(rlang)
library(tidyr)

get_scores <- function(sample_data, loc_col = NULL, year_col = NULL) {
  
  # --- 1) core column checks ---
  req <- c("parameter","media","concentration","unit")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) abort(paste0("sample_data missing: ", paste(miss, collapse = ", ")))
  
  # If route is absent, add it as NA (so CR becomes NA but HQ still computes)
  if (!"cr_route" %in% names(sample_data)) {
    sample_data <- mutate(sample_data, cr_route = NA_character_)
  }
  
  # --- 2) figure out grouping columns we can actually use ---
  df <- sample_data
  
  # If year grouping requested but column not present, derive from 'date' if available
  if (!is.null(year_col) && !(year_col %in% names(df)) && ("date" %in% names(df))) {
    # require lubridate for year()
    if (!requireNamespace("lubridate", quietly = TRUE)) {
      abort("year_col requested but not found; install 'lubridate' or provide a year column.")
    }
    df <- df %>% mutate(!!year_col := lubridate::year(.data$date))
  }
  
  group_vars <- character(0)
  if (!is.null(loc_col)  && loc_col  %in% names(df)) group_vars <- c(group_vars, loc_col)
  if (!is.null(year_col) && year_col %in% names(df)) group_vars <- c(group_vars, year_col)
  
  # --- 3) run calculate_location_score per group (or once if no groups) ---
  if (length(group_vars) == 0) {
    # Single score (no grouping)
    res <- calculate_location_score(df)
    res_data <- res$scored_data

    return(
      tibble(
        env_score = res$env_score,
        hazard_index = res_data$hazard_index,
        total_CR_cases_10k = res_data$total_CR_cases_10k,
        by_parameter = list(res_data$by_parameter)
      )
    )
  }
  
  # Grouped scoring (location, year, or both)
  df %>%
    group_by(across(all_of(group_vars))) %>%
    group_map(~{
      res <- calculate_location_score(.x)
      res_data <- res$scored_data
      
      # emit one row per group, keeping the group keys from .y
      tibble(
        !!!.y,
        env_score = res$env_score,
        hazard_index = res_data$hazard_index,
        total_CR_cases_10k = res_data$total_CR_cases_10k,
        by_parameter = list(res_data$by_parameter)
      )
    }) %>%
    list_rbind() %>%
    arrange(across(all_of(group_vars)))
}


split_locations <- function(sample_data,
                            loc_col  = "station",
                            year_col = "year") {
  
  # required columns for CLS + grouping
  req <- c("parameter","media","concentration","unit","cr_route", loc_col, year_col)
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) abort(paste0("sample_data missing: ", paste(miss, collapse = ", ")))
  
  gvars <- c(loc_col, year_col)
  
  sample_data %>%
    group_by(across(all_of(gvars))) %>%
    group_map(~{
      res <- calculate_location_score(.x)   # your existing CLS
      
      # derive optional rollups from the by-parameter table if present
      byp <- res$scored_data
      hazard_index <- if ("hazard_component" %in% names(byp))
        sum(byp$hazard_component, na.rm = TRUE) else NA_real_
      total_CR_cases_10k <- if ("CR_cases_10k" %in% names(byp))
        sum(byp$CR_cases_10k, na.rm = TRUE) else NA_real_
      
      tibble(
        !!loc_col  := .y[[loc_col]],
        !!year_col := .y[[year_col]],
        env_score  = res$env_score,
        hazard_index = hazard_index,
        total_CR_cases_10k = total_CR_cases_10k,
        by_parameter = list(byp)     # keep details for explanations later
      )
    }) %>%
    list_rbind() %>%
    arrange(!!sym(loc_col), !!sym(year_col))
}


# example for running it:
results <- split_locations(all_samples_df, loc_col = "site_id", year_col = "year")