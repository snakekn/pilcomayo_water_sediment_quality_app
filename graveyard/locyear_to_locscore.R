#### Take in locyear data, rescore to return a time-weighted score ####

# Note: The resulting df is quite large (water_locyear became 89MB vs 19KB w/o it)
# Note: This likely means we won't want to actually load it
# Note: If we need it, we can load it from elsewhere
# --> made a flag to allow us to remove the detailed datapoints 
weigh_inverse_time <- function(locyear, details = FALSE) {
  
  df <- locyear |>
    group_by(station, year) |>  # Group by station and year
    summarize(
      # Keep ID columns (take first value since they should be the same within group)
      lat = first(lat),
      lon = first(lon),
      
      # Calculate time-weighted scores
      recent_year = max(year, na.rm = TRUE),
      time_dist = recent_year - year + 1,
      
      # Aggregate scores (sum or mean depending on your needs)
      total_hq = sum(hazard_index, na.rm = TRUE),
      total_cr = sum(total_CR_cases_10k, na.rm = TRUE),
      total_wl = sum(wl_index, na.rm = TRUE),
      
      # Time-weighted scores
      hq_t = sum(hazard_index / time_dist, na.rm = TRUE),
      cr_t = sum(total_CR_cases_10k / time_dist, na.rm = TRUE),
      wl_t = sum(wl_index / time_dist, na.rm = TRUE),
      
      # Count of observations
      n_obs = n(),
      
      detail_rows = ifelse(details, list(first(detail_rows)), list(NULL)),
      by_parameter = ifelse(details, list(first(by_parameter)), list(NULL)),      
      
      .groups = "drop"
    ) |>
    # Now group by station only to get station-level aggregates
    group_by(station) |>
    summarize(
      # Keep ID columns
      lat = first(lat),
      lon = first(lon),
      
      # Station-level aggregated scores
      loc_hq = sum(hq_t, na.rm = TRUE),
      loc_cr = sum(cr_t, na.rm = TRUE),
      loc_wl = sum(wl_t, na.rm = TRUE),
      
      # Total observations across all years
      total_obs = sum(n_obs),
      
      # Year range
      first_year = min(year),
      last_year = max(year),
      n_years = n_distinct(year),
      
      # keep df's inside for traceability
      detail_rows = list(detail_rows),
      by_parameter = list(by_parameter),
      
      .groups = "drop"
    )
  
  # simplify the detail_rows list if there are no details
  if (!details) {
    df$detail_rows = NA # NULL removes the column *shrug*
    df$by_parameter = NA
  }
  return(df)
}
