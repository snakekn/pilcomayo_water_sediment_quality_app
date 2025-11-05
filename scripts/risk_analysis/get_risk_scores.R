
#### Split scored df by station & year, merge into a single set of scores per location-year ####
# potentially good for showing all on a map. Note: can take awhile to load, so we want to parse this (or show a loading bar) early if possible
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
          HQ_n      = sum(!is.na(HQ)),
          
          CR_max    = if (all(is.na(CR))) NA_real_ else max(CR, na.rm = TRUE),
          CR_median = if (all(is.na(CR))) NA_real_ else median(CR, na.rm = TRUE),
          CR_n      = sum(!is.na(CR)),
          
          WL_max    = if (all(is.na(WL))) NA_real_ else max(WL, na.rm = TRUE),
          WL_median = if (all(is.na(WL))) NA_real_ else median(WL, na.rm = TRUE),
          WL_n      = sum(!is.na(WL)),
          .groups = "drop"
        )
      
      tibble(
        !!!.y,                           # keeps the group key columns and names
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

stds = readr::read_csv(here::here("data/standards/strict_standards.csv")) |>
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

# to get HQ, CR, and calculated scores for each data point. Note that data points with more data will have higher HQs, rather than being normalized
# Nadav's Note: may want to only calculate ones above 1?? 
score_data <- function(sample_data) {
  # need: parameter, media, concentration, unit, cr_route
  req <- c("parameter","media","concentration","unit","cr_route")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) rlang::abort(paste0("sample_data missing: ", paste(miss, collapse=", ")))
  
  # print(nrow(sample_data)) # many rows! 33,470 for water data
  
  scored <- sample_data |>
    dplyr::mutate(
      hqcr = purrr::pmap(
        list(parameter, media, concentration, unit, cr_route),
        calculate_hqcr
      )
    ) |>
    tidyr::unnest_wider(hqcr) |>
    dplyr::mutate(
      CR_cases_10k = ifelse(is.na(CR), NA_real_, CR * 1e4),
      has_HQ = !is.na(HQ),
      has_CR = !is.na(CR),
      has_WL = !is.na(WL)
    )
}

# for quickly retrieving standards 
get_std <- function(parameter, std_type, media) {
  
  key = make_key(parameter, media, std_type)
  #print(paste0("make_key", key))
  std <- std_map[[key]]
  #print(std)
  
  # if there's no standard, send nothing back
  if (is.null(std)) return(NULL)
  return(std)
}

# For an individual parameter: get & prep the parameter standard, then compare with the found value
calculate_hqcr = function(param, med, val, unit, route=NULL) { # tibble should have: param, med, val, unit
  ### set HQ/CR/WL to NA in case we don't have the data, then calculate each separately
  # print(paste0(param, med, val, unit, route)) # for sanity :)
  hq = NA_real_
  cr = NA_real_
  wl = NA_real_
  # print(list(param, med,val, unit, route))
  
  std_info <- list(
    HQ = list(std_reg = NA, std_val = NA, std_unit = NA),
    CR = list(std_reg = NA, std_val = NA, std_unit = NA),
    WL = list(std_reg = NA, std_val = NA, std_unit = NA)
  )
  ### Manage special cases (pH)
  # Edge Case: pH. Only calculate HQ
  if (grepl("^pH\\b", param, ignore.case = TRUE)) {
    if(is.na(unit)) { unit = "pH unit" } # edge case where pH in lab is empty
    if(str_detect(unit, "mV")) { # skip the pH (mV) measure
      return(list(HQ=hq, CR=cr, WL=wl, std_info = std_info))
    }
    
    std = get_std(parameter=param, std_type="hq", media=med) # get the std
    
    # stop computing if the standard isn't there
    if (is.null(std) || (is.data.frame(std) && nrow(std) == 0)) {
      return(list(HQ=hq, CR=cr, WL=wl, std_info = std_info))
    }
    
    # helper to pick numeric named fields
    try_numeric_field <- function(df, names_vec) {
      for (nm in names_vec) {
        if (!is.null(df[[nm]]) && is.numeric(df[[nm]]) && !all(is.na(df[[nm]]))) {
          v <- as.numeric(df[[nm]])
          v <- v[!is.na(v)]
          if (length(v)) return(range(v, na.rm = TRUE))
        }
      }
      NULL
    }
    
    # 1) explicit lower/upper/min/max
    rng <- try_numeric_field(std, c("lower", "upper", "min", "max"))
    # if explicit fields returned a 2-value range, use directly
    if (!is.null(rng) && length(rng) == 2) {
      lower <- rng[1]; upper <- rng[2]
    } else {
      # 2) try "Class A/B/C/D" style numeric fields or any numeric columns
      numeric_cols <- vapply(std, is.numeric, logical(1))
      if (any(numeric_cols)) {
        vals <- unlist(std[ , numeric_cols, drop = FALSE], use.names = FALSE)
        vals <- as.numeric(vals[!is.na(vals)])
        if (length(vals) >= 2) {
          lower <- min(vals, na.rm = TRUE)
          upper <- max(vals, na.rm = TRUE)
        } else {
          lower <- upper <- NA_real_
        }
      } else {
        lower <- upper <- NA_real_
      }
    }
    
    # If still NA, give a friendly error
    if (is.na(lower) || is.na(upper)) {
      message(paste0("Could not determine pH acceptable range from std for '", param,
                     "'. Std object needs numeric lower/upper or numeric class thresholds."))
      return(list(HQ = hq, CR = cr, WL = wl, std_info = std_info))
    }
    
    # ensure proper ordering
    if (lower > upper) {
      tmp <- lower; lower <- upper; upper <- tmp
    }
    
    # compute HQ:
    # - inside range -> HQ = 0
    # - below lower  -> HQ = lower / val (simple ratio)
    # - above upper  -> HQ = val / upper
    if (is.na(val)) {
      hq <- NA_real_
    } else if (val >= lower && val <= upper) {
      hq <- 0
    } else if (val < lower) {
      # avoid divide-by-zero
      if (val == 0) {
        hq <- Inf
      } else {
        hq <- lower / val
      }
    } else { # val > upper
      if (upper == 0) {
        hq <- Inf
      } else {
        hq <- val / upper
      }
    }
    
    # pH has no CR; return early
    std_info[["HQ"]] = list(std_reg = std$regulator, std_val = std$value, std_unit = std$unit)
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
      val = val * unit_check_hq$conversion_factor
      hq = val/std$value
      
      std_info[["HQ"]] = list(std_reg=std$regulator, std_val=std$value, std_unit=std$unit)
    }
  } # ELSE: leave HQ as NA_real_ 
  
  ### calculate WL - closely related to grabbing HQ
  std = get_std(param, "wl", med) # fetch WL standard
  # print(std)
  # confirm we got the standard, otherwise stop trying to calculate based on this parameter-media
  if(!is.null(std)) {
    # check the units are the same and abort if they're not
    unit_check_wl = compare_units(unit, std$unit) # in helpers_server.R. Gives helpful responses
    if(!unit_check_wl$convertible) { # can't convert
      # message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_wl$message, " Received sample units ", unit_check_wl$sample_parsed$raw, " and standard ", unit_check_wl$standard_parsed$raw, ". Leaving as NA with a note.")) 
    } else {
      val = val * unit_check_wl$conversion_factor
      wl = val/std$value
      
      std_info[["WL"]] = list(std_reg=std$regulator, std_val=std$value, std_unit=std$unit)
      
    }
  } # ELSE: leave WL as NA_real_ 
  
  ## calculate CR - not as simple :))
  
  # get_std using oral only -- no air samples now :( will just need a dictionary to check for it
  std = get_std(param, "cr", "oral")
  
  # we don't always have CR data -- will leave as NA if so
  if (!is.null(std$value) && !is.na(std$value) && !is.null(std$unit) && !is.na(std$unit)) {
    unit_check_cr = compare_units(unit, std$unit)
    if(!unit_check_cr$convertible) { # can't convert
      # message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_cr$message, " Received sample units ", unit_check_cr$sample_parsed$raw, " and standard ", unit_check_cr$standard_parsed$raw, ". Leaving as NA with a note.")) 
    } else {
      val = val * unit_check_cr$conversion_factor
      
      ep <- EXPOSURE_FACTORS
      sf <- std$value
      
      cr = switch(route,
                  "inhalation" = val * sf,  # val in µg/m^3; sf is unit risk (µg/m^3)^-1
                  "oral" = {
                    dose <- (val * ep$IR$oral_L_per_day * ep$EF * ep$ED) / (ep$BW * ep$EL)
                    dose * sf
                  },
                  "soil_oral" = {
                    CF <- 1e-6
                    dose <- (val * ep$IR$soil_mg_per_day * CF * ep$EF * ep$ED) / (ep$BW * ep$EL)
                    dose * sf
                  },
                  NA_real_
      )
    }
    
    std_info[["CR"]] = list(std_reg = std$regulator, std_val = std$value, std_unit = std$unit)
  }
  # print(std) # had an issue with !is.null(std) failing when the std wasn't null (received properly)
  if (is.null(std) || (is.data.frame(std) && nrow(std) == 0)) {
    return(list(HQ=hq, CR = cr, WL=wl, std_info = std_info))
  } else return(list(HQ=hq, CR = cr, WL=wl, std_info = std_info)) # redundant?
}

