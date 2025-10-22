# Purpose: iteratively calculate score for station given a set of concentrations data
### It's called by get_risk_scores.R to get per-station data. GRS.R will send in data split by year-location
library(tidyverse)

# Load csv's & prepare for standards & weights. STDs include Cancer Risk
make_key = function(parameter, media) paste0(parameter, "||", media)

stds = readr::read_csv(here::here("data/standards/strict_standards.csv")) |>
  mutate(.key = make_key(parameter, media))
std_map <- split(stds, stds$.key)

wts = readr::read_csv(here::here("data/risk_weights.csv")) |>
  janitor::clean_names() |> # turn headers into easy variable names
  mutate(.key = make_key(parameter, media))

# create a standardized set of exposure factors for simple CR calculations
EXPOSURE_FACTORS <- list(
  IR = list(
    oral_L_per_day   = 2.0,   # adults drinking water, L/day
    soil_mg_per_day  = 100.0  # incidental soil ingestion, mg/day
  ),
  EF = 350,     # Exposure frequency, days/year
  ED = 30,      # Exposure duration, years
  BW = 70,      # Body weight, kg
  EL = 365*70,  # Expected lifespan, days (70 yrs * 365)
  cr_weight = 0.3
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
  
  # reset weights if either component is missing. Static value for cr_weight at the top
  cr_weight = NA_real_
  if(cr_score == 0) {
    cr_weight = 0
  } else if (hq_score == 0) {
    cr_weight = 1
  } else { cr_weight = EXPOSURE_FACTORS$cr_weight }
  
  env_score = hq_score*(1-cr_weight) + cr_score*cr_weight
  
  # return both the final score & the data behind it for future viewing
  list(
    env_score = env_score,
    scored_data = scored_data
  )
}

# to get HQ, CR, weights, and calculated scores for each data point
score_data <- function(sample_data) {
  # need: parameter, media, concentration, unit, cr_route
  req <- c("parameter","media","concentration","unit","cr_route")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) rlang::abort(paste0("sample_data missing: ", paste(miss, collapse=", ")))
  
  scored <- sample_data |>
    dplyr::mutate(
      hqcr = purrr::pmap(
        list(parameter, media, concentration, unit, cr_route),
        calculate_hqcr
      )
    ) |>
    tidyr::unnest_wider(hqcr) |>
    dplyr::mutate(
      weight_param = purrr::map2_dbl(parameter, media, get_weight),
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
      weight_param = dplyr::first(weight_param),   # your scheme is per param/media
      .groups = "drop"
    )
  
  # Normalize weights among the params that actually contributed HQ
  total_w <- sum(hq_by_param$weight_param, na.rm = TRUE)
  if (nrow(hq_by_param) > 0) {
    if (total_w > 0) {
      hq_by_param <- hq_by_param |>
        dplyr::mutate(weight_norm = weight_param / total_w)
    } else {
      hq_by_param <- hq_by_param |>
        dplyr::mutate(weight_norm = 1 / dplyr::n())
    }
  } else {
    # no HQ data available
    hq_by_param$weight_norm <- numeric(0)
  }
  
  hq_by_param = hq_by_param |>
    mutate(hazard_quotient = weight_norm * HQ_max)
  
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
get_std <- function(parameter, media) {
  
  key = make_key(parameter, media)
  std <- std_map[[key]]
  if (is.null(std)) return(NULL)
  return(std)
}

get_weight = function(parameter, media, lookup=wts) {
  # easy lookups with a key
  key = make_key(parameter, media)
  # try getting the right weight
  w = lookup |> filter(key == !!key)
  # if the perfect param/media doesn't exist, **just use any weight for the parameter itself**
  if (nrow(w)==0) w = lookup |> filter(parameter == !!parameter) 
  # if no weight found, assign 0 to this parameter
  if(nrow(w)==0) return(0)
  
  return(w$weight[[1]])
}

# For an individual parameter: get & prep the parameter standard, then compare with the found value
calculate_hqcr = function(param, med, val, unit, route=NULL) { # tibble should have: param, med, val, unit
  # set HQ & CR to NA in case we don't have the data, then calculate each separately
  hq = NA_real_
  cr = NA_real_
  
  ## Get the standards data
  std = get_std(param, med) # fetch standard
  
  ## Calculate HQ
  # check the units are the same and abort if they're not
  if(unit != std$unit) rlang::abort(paste0("Unit mismatch for ", param, " in ", med, "! Received ", unit, " but standard is in", std$unit)) 
  
  hq = val/std$concentration
  
  ## calculate CR - not as simple :))
  
  # we don't always have CR data -- can just set to 0 if so
  if (!is.null(std$cr_slope) && !is.na(std$cr_slope) && !is.null(std$cr_route) && !is.na(std$cr_route)) {
    
    # optional CR unit guard: only check if provided
    cr_unit <- std$cr_unit[[1]]
    if (!is.na(cr_unit) && isTRUE(unit != cr_unit)) {
      rlang::abort(paste0("CR unit mismatch for ", param, " in ", med,
                          ": got '", unit, "', expected '", cr_unit, "'"))
    }
    
    ep <- EXPOSURE_FACTORS
    sf <- std$cr_slope[[1]]
    
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
  return(list(HQ=hq, CR = cr))
}

