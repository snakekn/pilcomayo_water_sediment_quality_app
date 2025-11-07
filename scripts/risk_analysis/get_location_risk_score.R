# Purpose: iteratively calculate score for station given a set of concentrations data
### It's called by get_risk_scores.R to get per-station data. GRS.R will send in data split by year-location
library(tidyverse)

# Load csv's & prepare for standards & weights. STDs include Cancer Risk
make_key = function(parameter, media) paste0(parameter, "||", media)

stds = readr::read_csv(here::here("data/standards/strict_standards.csv")) |>
  mutate(.key = make_key(parameter, media)) |>
  filter(!is.na(value)) # skip any values that we don't have data for, HQ or CR
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

# to get the total environmental score for a set of data at a station
calculate_location_score = function(sample_data) {
  
  ### pseudocode:
  # 1. Check all columns are present, abort otherwise
  # 2. Calculate the HQ+CR for each parameter
  # 3. Weigh each parameter based on priority
  # 4. Add all scores
  # 5. Return Env Score & underlying data
  
  # ensure all required columns are present
  req <- c("parameter", "media", "concentration", "unit")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) {
    rlang::abort(as.character(glue::glue("sample_data missing columns: {paste(miss, collapse=', ')}")))
  }
  
  # calcuate HQ, CR, get weights, then calculate a score for each item
  scored_data = score_data(sample_data)
  
  # calculate summed score 
  hq_score = scored_data$hazard_index
  cr_score = scored_data$total_CR_cases_10k

  # return both the final score & the data behind it for future viewing
  list(
    hq_score = hq_score,
    cr_score = cr_score,
    scored_data = scored_data
  )
}

# to get HQ, CR, and calculated scores for each data point. Note that data points with more data will have higher HQs, rather than being normalized
# Nadav's Note: may want to only calculate ones above 1?? 
score_data <- function(sample_data) {
  # need: parameter, media, concentration, unit, cr_route
  req <- c("parameter","media","concentration","unit","cr_route")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) rlang::abort(paste0("sample_data missing: ", paste(miss, collapse=", ")))
  
  print(nrow(sample_data))
  
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
      has_CR = !is.na(CR)
    )
  
  # --- HQ branch: collapse to one row per parameter-media using worst HQ ---
  hq_by_param <- scored |>
    filter(has_HQ) |>
    dplyr::group_by(parameter, media) |>
    dplyr::summarize(
      HQ_max       = max(HQ, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(hazard_quotient = HQ_max)
  
  hazard_index <- sum(hq_by_param$hazard_quotient, na.rm = TRUE)
  
  # --- CR branch: sum cases per 10k across all rows (no parameter weights) ---
  # If you prefer to sum per-parameter first, then total, do a group_by + summarize.
  cr_by_param <- scored |>
    filter(has_CR) |>
    dplyr::group_by(parameter, media) |>
    dplyr::summarize(
      CR_cases_10k = sum(CR_cases_10k, na.rm = TRUE),
      .groups = "drop"
    )
  
  total_CR_cases_10k <- sum(cr_by_param$CR_cases_10k, na.rm = TRUE)
  
  # --- merge HQ & CR per-parameter views for explainability ---
  by_parameter <- hq_by_param |>
    dplyr::left_join(cr_by_param, by = c("parameter","media")) |>
    # dplyr::select(
    #   parameter, media,
    #   HQ_max,
    #   weight_param, weight_norm, hazard_quotient,
    #   CR_cases_10k
    # ) |>
    dplyr::arrange(dplyr::desc(hazard_quotient), dplyr::desc(CR_cases_10k))
  
  list( # sent to calculate_location_score
    hazard_index        = hazard_index,        # dimensionless index
    total_CR_cases_10k  = total_CR_cases_10k,  # expected excess cases per 10,000 (sum across params)
    by_parameter        = by_parameter,        # tidy table per parameter-media
    detail_rows         = scored               # every row with HQ, CR, CR_cases_10k, raw weight
  )
}

# for quickly retrieving standards 
get_std <- function(parameter, hqcr, media) {
  
  key = make_key(parameter, media)
  std <- std_map[[key]]
  
  # if there's no standard, send nothing back
  if (is.null(std)) return(NULL)
  return(std)
}

# For an individual parameter: get & prep the parameter standard, then compare with the found value
calculate_hqcr = function(param, med, val, unit, route=NULL) { # tibble should have: param, med, val, unit
  # set HQ & CR to NA in case we don't have the data, then calculate each separately
  # print(paste0(param, med, val, unit, route)) # for sanity :)
  hq = NA_real_
  cr = NA_real_
  
  ## Get the standards data
  std = get_std(param, "hq", med) # fetch HQ standard
  # print(std)
  # confirm we got the standard, otherwise stop trying to calculate based on this parameter-media
  if(is.null(std)) return(list(HQ=0,CR=0))
  
  # Edge Case: pH. Skip CR
  if (grepl("^pH\\b", param, ignore.case = TRUE)) {
    # attempt to extract lower / upper bounds from std in a robust way
    
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
      rlang::abort(paste0("Could not determine pH acceptable range from std for '", param,
                          "'. Std object needs numeric lower/upper or numeric class thresholds."))
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
    return(list(HQ = hq, CR = NA_real_, std_reg = std$regulator, std_val = std$value, std_unit = std$unit))
  } # end pH special-case
  
  ## Calculate HQ
  # check the units are the same and abort if they're not
  unit_check_hq = compare_units(unit, std$unit) # in helpers_server.R. Gives helpful responses
  if(!unit_check_hq$convertible) { # can't convert
    # message(paste0("[pivot_pilcomayo_data: compare_units()] ", param, ": ", unit_check_hq$message, " Received sample units ", unit_check_hq$sample_parsed$raw, " and standard ", unit_check_hq$standard_parsed$raw, ". Leaving as NA with a note.")) 
  } else {
    val = val & unit_check_hq$conversion_factor
    hq = val/std$value
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
      val = val & unit_check_cr$conversion_factor
      
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
                  0
      )
    }
  }
  # print(std) # had an issue with !is.null(std) failing when the std wasn't null (received properly)
  if (!is.null(std) && !is.na(std)) { # ensure they're not null
    return(list(HQ=hq, CR = cr, std_reg = std$regulator, std_val = std$value, std_unit = std$unit))
  } else {return(list(HQ=hq, CR = cr, std_reg = NA, std_val = NA, std_unit = NA))}
}

