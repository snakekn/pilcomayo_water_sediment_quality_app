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
make_key = function(parameter, media, std_type) paste0(parameter, "||", media, "||", std_type)

#stds_location = here::here("data/standards/strict_standards.csv")
stds_location = here::here("data/standards/strict_standards_1_7_2026.csv") # need to review both for inconsistencies

stds = readr::read_csv(stds_location) |>
  mutate(.key = make_key(parameter, media, hqcr)) |>
  filter(!is.na(value)) # skip any values that we don't have data for, HQ/CR/WL
std_map <- split(stds, stds$.key)

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


# Add this helper function before score_data
convert_suspended_concentrations <- function(sample_data) {
  # Check if we have the necessary columns
  if (!all(c("parameter", "unit", "concentration") %in% names(sample_data))) {
    return(sample_data)
  }
  
  # Check if media column exists
  if (!"media" %in% names(sample_data)) {
    message("Warning: 'media' column not found. Skipping conversion.")
    return(sample_data)
  }
  
  # Identify grouping columns 
  group_cols <- intersect(names(sample_data), 
                          c("station", "date"))
  
  if (length(group_cols) == 0) {
    message("Warning: No grouping columns found for suspended solids conversion")
    return(sample_data)
  }
  
  # Extract TSS (Total Suspended Solids) data
  tss_data <- sample_data %>%
    filter(parameter == "TSS") %>%
    filter(unit == "mg/l" | unit == "mg/L") %>%
    filter(fraction == "Suspended") |>
    select(all_of(group_cols), tss_mg_L = concentration)
  
  # Join TSS back to main data and convert mg/kg to mg/L
  sample_data <- sample_data %>%
    left_join(tss_data, by = group_cols) %>%
    mutate(
      # Convert mg/kg to mg/L using TSS - only for water media
      # mg/L = (mg/kg) × (mg/L TSS) × (1 kg / 1e6 mg)
      concentration_converted = if_else(
        unit == "mg/kg" & !is.na(tss_mg_L) & media %in% c("water", "drinking water"),
        concentration * tss_mg_L / 1e6,  # Convert TSS from mg/L to kg/L
        concentration
      ),
      unit_converted = if_else(
        unit == "mg/kg" & !is.na(tss_mg_L) & media %in% c("water", "drinking water"),
        "mg/L",
        unit
      ),
      # Track which values were converted
      converted_from_mg_kg = unit == "mg/kg" & !is.na(tss_mg_L) & media %in% c("water", "drinking water")
    ) %>%
    # Use converted values
    mutate(
      concentration = concentration_converted,
      unit = unit_converted
    ) %>%
    # Clean up temporary columns
    select(-concentration_converted, -unit_converted, -tss_mg_L)
  
  # Report conversion stats
  n_converted <- sum(sample_data$converted_from_mg_kg, na.rm = TRUE)
  n_failed <- sum(sample_data$unit == "mg/kg" & 
                    sample_data$parameter != "solids" &
                    sample_data$media %in% c("water", "drinking water"), na.rm = TRUE)
  n_skipped_non_water <- sum(sample_data$unit == "mg/kg" & 
                               sample_data$parameter != "solids" &
                               !sample_data$media %in% c("water", "drinking water"), na.rm = TRUE)
  
  if (n_converted > 0) {
    message(paste0("Converted ", n_converted, " mg/kg measurements to mg/L using TSS"))
  }
  if (n_failed > 0) {
    message(paste0("Warning: ", n_failed, 
                   " mg/kg measurements could not be converted (no TSS data for those samples)"))
  }
  if (n_skipped_non_water > 0) {
    message(paste0(n_skipped_non_water, 
                   " mg/kg measurements skipped (not water media)"))
  }
  
  return(sample_data)
}

# to get HQ, CR, and calculated scores for each data point. Note that data points with more data will have higher HQs, rather than being normalized
score_data <- function(sample_data) {
  # need: parameter, media, concentration, unit, cr_route
  req <- c("parameter","media","concentration","unit","cr_route")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) rlang::abort(paste0("sample_data missing: ", paste(miss, collapse=", ")))
  
  # Convert suspended sediment concentrations BEFORE scoring
  sample_data <- convert_suspended_concentrations(sample_data)
  
  # Calculate hqcr for each row
  scored <- sample_data |>
    dplyr::mutate(
      hqcr = purrr::pmap(
        list(parameter, media, concentration, unit, cr_route),
        calculate_hqcr
      )
    )

  # Unnest the hqcr list-column to extract HQ, CR, WL, and std_info
  scored <- scored |>
    tidyr::unnest_wider(hqcr)
  
  # Now add the derived columns after unnesting
  scored <- scored |>
    dplyr::mutate(
      CR_cases_10k = dplyr::if_else(is.na(CR), NA_real_, CR * 1e4),
      has_HQ = !is.na(HQ),
      has_CR = !is.na(CR),
      has_WL = !is.na(WL),
      has_standard = has_HQ | has_CR | has_WL  # Add this column
    )
  
  return(scored)
}

# for quickly retrieving standards 
get_std <- function(parameter, std_type, media) {
  
  key = make_key(parameter, media, std_type)
  std <- std_map[[key]]

  # if there's no standard, send nothing back
  if (is.null(std)) return(NULL)
  return(std)
}

# For an individual parameter: get & prep the parameter standard, then compare with the found value
calculate_hqcr = function(param, med, val, unit, route=NULL) { # tibble should have: param, med, val, unit
  ### set HQ/CR/WL to NA in case we don't have the data, then calculate each separately
  hq = NA_real_
  cr = NA_real_
  wl = NA_real_

  std_info <- list(
    HQ = list(std_reg = NA, std_val = NA, std_unit = NA),
    CR = list(std_reg = NA, std_val = NA, std_unit = NA),
    WL = list(std_reg = NA, std_val = NA, std_unit = NA)
  )
  ### Manage special cases (pH, Oxygen Saturation)
  
  # Edge Case: Oxygen Saturation - inverse relationship (lower is worse)
  if (grepl("oxygen.*saturation", param, ignore.case = TRUE)) {
    std = get_std(parameter=param, std_type="hq", media=med)
    
    if (is.null(std) || (is.data.frame(std) && nrow(std) == 0)) {
      return(list(HQ=hq, CR=cr, WL=wl, std_info = std_info))
    }
    
    unit_check_hq = compare_units(unit, std$unit)
    
    if(!unit_check_hq$convertible) {
      # Can't convert units
    } else {
      val = val / unit_check_hq$conversion_factor
      # INVERSE: HQ = standard / measured (lower values = higher hazard)
      if (val == 0) {
        hq <- Inf
      } else {
        hq = std$value / val
      }
      std_info[["HQ"]] = list(std_reg=std$regulator, std_val=std$value, std_unit=std$unit)
    }
    
    # Oxygen Saturation has no CR or WL; return early
    return(list(HQ = hq, CR = NA_real_, WL = NA_real_, std_info = std_info))
  } # end Oxygen Saturation special-case
  
  # Edge Case: pH - acceptable range with midpoint-based calculation
  if (grepl("^pH\\b", param, ignore.case = TRUE)) {
    if(is.na(unit)) { unit = "pH unit" } # edge case where pH in lab is empty
    if(str_detect(unit, "mV")) { # skip the pH (mV) measure
      return(list(HQ=hq, CR=cr, WL=wl, std_info = std_info))
    }
    
    # Get both lower and upper standards
    std_low = get_std(parameter="pH low", std_type="hq", media=med)
    std_high = get_std(parameter="pH high", std_type="hq", media=med)
    
    # Stop if we don't have both standards
    if (is.null(std_low) || is.null(std_high) || 
        (is.data.frame(std_low) && nrow(std_low) == 0) ||
        (is.data.frame(std_high) && nrow(std_high) == 0)) {
      return(list(HQ=hq, CR=cr, WL=wl, std_info = std_info))
    }
    
    lower <- std_low$value
    upper <- std_high$value
    midpoint <- (lower + upper) / 2
    
    # Calculate HQ based on which side of midpoint
    if (is.na(val)) {
      hq <- NA_real_
    } else if (val >= lower && val <= upper) {
      # Within acceptable range
      hq <- 0
    } else if (val < midpoint) {
      # Below midpoint: use standard/measured (low pH is bad)
      if (val == 0) {
        hq <- Inf
      } else {
        hq <- lower / val
      }
    } else {
      # Above midpoint: use measured/standard (high pH is bad)
      if (upper == 0) {
        hq <- Inf
      } else {
        hq <- val / upper
      }
    }
    
    # Store both standards in std_info
    std_info[["HQ"]] = list(
      std_reg = paste(std_low$regulator, std_high$regulator, sep="/"),
      std_val = paste(lower, upper, sep="-"),
      std_unit = std_low$unit
    )
    
    # pH has no CR or WL; return early
    return(list(HQ = hq, CR = NA_real_, WL = NA_real_, std_info = std_info))
  } # end pH special-case
  
  ### Calculate HQ
  
  std = get_std(param, "hq", med) # fetch HQ standard
  # print(std)
  # confirm we got the standard, otherwise stop trying to calculate based on this parameter-media
  if(!is.null(std)) {
    # check the units are the same and abort if they're not
    unit_check_hq = compare_units(unit, std$unit) # in helpers_server.R. Gives helpful responses
    
    if(!unit_check_hq$convertible) { # can't convert
      # message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_hq$message, " Received sample units ", unit_check_hq$sample_parsed$raw, " and standard ", unit_check_hq$standard_parsed$raw, ". Leaving as NA with a note.")) 
    } else {
      val_norm = val / unit_check_hq$conversion_factor
      hq = val_norm/std$value
      
      std_info[["HQ"]] = list(std_reg=std$regulator, std_val=std$value, std_unit=std$unit)
    }
    
  }
  
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
  
  if (is.null(std) || (is.data.frame(std) && nrow(std) == 0)) {
    return(list(HQ=hq, CR = cr, WL=wl, std_info = std_info))
  } else return(list(HQ=hq, CR = cr, WL=wl, std_info = std_info)) # redundant?
}

