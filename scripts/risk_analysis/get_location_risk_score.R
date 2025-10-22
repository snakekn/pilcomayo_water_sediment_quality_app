# Purpose: iteratively calculate score for station given a set of concentrations data
library(rlang)

# Load csv's & prepare for standards & weights. STDs include Cancer Risk
make_key = function(parameter, media) paste0(parameter, "||", media)

stds = readr::read_csv(here("data/standards/strict_standards.csv")) |>
  mutate(.key = make_key(parameter, media))
std_map <- split(stds, stds$.key)

wts = readr::read_csv(here("data/risk_weights.csv")) |>
  clean_names() |> # turn headers into easy variable names
  mutate(.key = make_key(parameter, media))

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
  env_score = sum(scored_data$score, na.rm=TRUE)
  
  # return both the final score & the data behind it for future viewing
  list(
    env_score = env_score,
    scored_data = scored_data
  )
}

# to get HQ, CR, weights, and calculated scores for each data point
score_data = function(sample_data) {
  scored_data = sample_data |>
    mutate(
      hqcr   = purrr::pmap_dbl(list(parameter, media, concentration, unit), calculate_hq),
      weight = purrr::map2_dbl(parameter, media, get_weight),
      score  = hqcr * weight
    )
  
  # ensure we only have one parameter-media combo in the data set, and take the worst score
  by_parameter <- scored_data %>%
    group_by(parameter, media) %>%
    slice_max(order_by = score, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(desc(score))
  
  return(by_parameter)
}

# for quickly retrieving standards 
get_std <- function(parameter, media) {
  
  key = make_key(parameter, media)
  std <- std_map[[key]]
  if (is.null(std)) return(NULL)
  list(
    standard_unit = std$unit[[1]],
    concentration = std$concentration[[1]]
  )
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
calculate_hq = function(param, med, val, unit) { # tibble should have: param, med, val, unit
  # print(paste0(param, med, val, unit))
  std = get_std(param, med) # fetch standard
  # check the units are the same and abort if they're not
  if(unit != std$standard_unit) rlang::abort(paste0("Unit mismatch for ", param, " in ", med, "! Received ", unit, " but standard is in", std$standard_unit)) 
  
  # calculate HQ + Cancer Risk!
  hq = val/std$concentration
  
  ## need to calculate CR! will be harder :)
  cr = 0
  # get Hazard Quotient (HQ) & send it back
  return (hq+cr)
}
