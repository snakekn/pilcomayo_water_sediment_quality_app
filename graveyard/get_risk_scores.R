#### Split scored df by station & year, merge into a single set of scores per location-year ####
# potentially good for showing all on a map. Note: can take awhile to load, so we want to sse this (or show a loading bar) early if possible
score_to_loc_year <- function(scored_data, loc_col = "station", year_col = "year", lat_col = "latitude_decimal", lon_col = "longitude_decimal") {
  
  req <- c("station", "year", "parameter","media","concentration","unit") # regular check
  miss <- setdiff(req, names(scored_data))
  
  if (length(miss)) {
    message(paste0("scored_data missing: ", paste(miss, collapse = ", ")))
    return(NULL)
  } 
  
  # If route is absent, add it as NA (so CR becomes NA but HQWL still compute)
  if (!"cr_route" %in% names(scored_data)) {
    scored_data <- mutate(scored_data, cr_route = NA_character_)
  }
  
  # If year grouping requested but column not present, derive from 'date' if available
  if (!is.null(year_col) && !(year_col %in% names(scored_data)) && ("date" %in% names(scored_data))) {
    # require lubridate for year()
    if (!requireNamespace("lubridate", quietly = TRUE)) {
      message("year_col requested but not found; install 'lubridate' or provide a year column.")
    }
    scored_data <- scored_data %>% mutate(!!year_col := lubridate::year(.data$date))
  }
  
  group_vars <- character(0)
  if (!is.null(loc_col)  && loc_col  %in% names(scored_data)) group_vars <- c(group_vars, loc_col)
  if (!is.null(year_col) && year_col %in% names(scored_data)) group_vars <- c(group_vars, year_col)
  print(paste0("[score_to_loc_year] Group vars: ", group_vars))
  
  # --- 3) run per group (or once if no groups) ---
  if (length(group_vars) == 0) {
    # Single score (no grouping)
    message("no group_vars!")
    return(scored_data)
  }
  
  # Grouping & calculating (location, year, or both)
  res <- scored_data %>%
    group_by(across(all_of(group_vars))) %>%
    group_map(~{
      # grab lat/lon values (prefer .x values, fallback to .y if lat/lon were grouping keys)
      lat_val <- if (!is.null(lat_col) && lat_col %in% names(.x)) {
        .x[[lat_col]][1]    # first value in the group's rows
      } else if (!is.null(lat_col) && lat_col %in% names(.y)) {
        .y[[lat_col]]       # value comes from the group keys
      } else NA_real_
      
      lon_val <- if (!is.null(lon_col) && lon_col %in% names(.x)) {
        .x[[lon_col]][1]
      } else if (!is.null(lon_col) && lon_col %in% names(.y)) {
        .y[[lon_col]]
      } else NA_real_
      
      s = .x # just to make it easy lol
      
      # defensive extraction in case some elements are missing
      hazard_index <- if (!is.null(s$HQ)) sum(s$HQ[s$HQ>1], na.rm=TRUE) else NA_real_
      total_CR <- if (!is.null(s$CR)) sum(s$CR, na.rm=TRUE) else NA_real_
      wl_index <- if (!is.null(s$WL)) sum(s$WL[s$WL>1], na.rm=TRUE) else NA_real_
      
      by_param = .x |> 
        group_by(parameter, unit) |>
        summarise(
          parameter = first(parameter, default = NA_character_),
          media = first(media),
          HQ_max    = if (all(is.na(HQ))) NA_real_ else max(HQ, na.rm = TRUE),
          HQ_median = if (all(is.na(HQ))) NA_real_ else median(HQ, na.rm = TRUE),
          HQ_n      = sum(HQ[HQ>1]),
          
          CR_max    = if (all(is.na(CR))) NA_real_ else max(CR, na.rm = TRUE),
          CR_median = if (all(is.na(CR))) NA_real_ else median(CR, na.rm = TRUE),
          CR_n      = sum(!is.na(CR)),
          
          WL_max    = if (all(is.na(WL))) NA_real_ else max(WL, na.rm = TRUE),
          WL_median = if (all(is.na(WL))) NA_real_ else median(WL, na.rm = TRUE),
          WL_n      = sum(WL[WL>1]),
          .groups = "drop"
        )
      
      tibble(
        !!!.y,                           
        lat = lat_val,
        lon = lon_val,
        hazard_index   = hazard_index,
        total_CR_cases_10k = total_CR*1e4,
        wl_index       = wl_index,
        detail_rows = list(.x), # all the rows for this group
        by_parameter = list(by_param)
      )
    }) %>%
    list_rbind() %>%
    arrange(across(all_of(group_vars)))
  
  return(res)
}

### Put together an easy-to-load standards list
# Load csv's & prepare for standards & weights. STDs include Cancer Risk
# make_key = function(parameter, media, std_type) paste0(parameter, "||", media, "||", std_type)

#stds_location = here::here("data/standards/strict_standards.csv")
# stds_location = here::here("data/standards/strict_standards_1_7_2026.csv") # need to review both for inconsistencies

# stds = readr::read_csv(stds_location) |>
#   mutate(.key = make_key(parameter, media, hqcr)) |>
#   filter(!is.na(value)) # skip any values that we don't have data for, HQ/CR/WL
# std_map <- split(stds, stds$.key)

# create a standardized set of exposure factors for simple CR calculations
EXPOSURE_FACTORS <- list(
  IR = list(
    oral_L_per_day   = 2.0,   # adults drinking water, L/day
    soil_mg_per_day  = 100.0  # incidental soil ingestion, mg/day
  ),
  EF = 350,     # Exposure frequency, days/year
  ED = 30,      # Exposure duration, years
  BW = 70,      # Body weight, kg
  EL = 365*70  # Expected lifespan, days (70 yrs * 365)
)


### calculate WL - closely related to grabbing HQ
std = get_std(param, "wl", med) # fetch WL standard

# confirm we got the standard, otherwise stop trying to calculate based on this parameter-media
if(!is.null(std)) {
  # check the units are the same and abort if they're not
  unit_check_wl = compare_units(unit, std$unit) # in helpers_server.R. Gives helpful responses
  if(!unit_check_wl$convertible) { # can't convert
    # message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_wl$message, " Received sample units ", unit_check_wl$sample_parsed$raw, " and standard ", unit_check_wl$standard_parsed$raw, ". Leaving as NA with a note.")) 
  } else {
    val_norm = val / unit_check_wl$conversion_factor
    wl = val_norm/std$value
    
    std_info[["WL"]] = list(std_reg=std$regulator, std_val=std$value, std_unit=std$unit)
    
  }
} 


#### calculate_hqcr removed sections ####
{
  ## calculate CR - not as simple :))
  
  # get_std using oral only -- no air samples now :( will just need a dictionary to check for it
  std = get_std(param, "cr", "oral")
  
  # we don't always have CR data -- will leave as NA if so
  if (!is.null(std$value) && !is.na(std$value) && !is.null(std$unit) && !is.na(std$unit)) {
    unit_check_cr = compare_units(unit, std$unit)
    if(!unit_check_cr$convertible) { # can't convert
      message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_cr$message, " Received sample units ", unit_check_cr$sample_parsed$raw, " and standard ", unit_check_cr$standard_parsed$raw, ". Leaving as NA with a note.")) 
    } else {
      val_norm = val / unit_check_cr$conversion_factor
      
      ep <- EXPOSURE_FACTORS
      sf <- std$value
      
      cr = switch(route,
                  "inhalation" = val_norm * sf,  # val in µg/m^3; sf is unit risk (µg/m^3)^-1
                  "oral" = {
                    dose <- (val_norm * ep$IR$oral_L_per_day * ep$EF * ep$ED) / (ep$BW * ep$EL)
                    dose * sf
                  },
                  "soil_oral" = {
                    CF <- 1e-6
                    dose <- (val_norm * ep$IR$soil_mg_per_day * CF * ep$EF * ep$ED) / (ep$BW * ep$EL)
                    dose * sf
                  },
                  NA_real_
      )
    }
    
    std_info[["CR"]] = list(std_reg = std$regulator, std_val = std$value, std_unit = std$unit)
  }
}