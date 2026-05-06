# Add this helper function before score_data
convert_suspended_concentrations <- function(sample_data) {
  message("START: convert_suspended_concentrations")
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

# to get HQ and calculated scores for each data point
score_data <- function(sample_data) {
  req <- c("parameter", "media", "concentration", "unit")
  miss <- setdiff(req, names(sample_data))
  if (length(miss)) {
    rlang::abort(paste0("sample_data missing: ", paste(miss, collapse = ", ")))
  }
  
  sample_data <- convert_suspended_concentrations(sample_data)
  
  safe_calc <- purrr::safely(function(parameter, media, concentration, unit) {
    calculate_hq(parameter, media, concentration, unit)
  })
  
  hq_results <- purrr::pmap(
    list(
      parameter = sample_data$parameter,
      media = sample_data$media,
      concentration = sample_data$concentration,
      unit = sample_data$unit
    ),
    safe_calc
  )
  
  failed_idx <- which(!purrr::map_lgl(hq_results, ~ is.null(.x$error)))
  
  if (length(failed_idx) > 0) {
    message("[score_data] first failing row index: ", failed_idx[1])
    print(sample_data[failed_idx[1], c("parameter", "media", "concentration", "unit"), drop = FALSE])
    message("[score_data] error message: ", conditionMessage(hq_results[[failed_idx[1]]]$error))
    stop("[score_data] calculate_hq failed; see console for first failing row")
  }
  
  sample_data$hqcr <- purrr::map(hq_results, "result")
  
  scored <- sample_data %>%
    tidyr::unnest_wider(hqcr) %>%
    dplyr::mutate(
      has_HQ = !is.na(HQ),
      has_standard = has_HQ
    )
  
  n_total <- nrow(scored)
  n_scored <- sum(scored$has_HQ, na.rm = TRUE)
  n_unscored <- n_total - n_scored
  
  message(
    sprintf(
      "[score_data] total rows: %d | HQ scored: %d | HQ missing: %d",
      n_total, n_scored, n_unscored
    )
  )
  
  scored
}

# for quickly retrieving standards
get_std <- function(parameter, std_type, media) {
  key <- make_key(parameter, media, std_type)
  std <- std_map[[key]]

  if (is.null(std)) return(NULL)

  if (!is.data.frame(std)) {
    warning(paste0("[get_std] expected data.frame for key '", key,
                   "', got ", class(std)[1], ". Returning NULL."))
    return(NULL)
  }

  if (nrow(std) == 0) return(NULL)

  needed_cols <- c("value", "unit", "regulator")
  missing_cols <- setdiff(needed_cols, names(std))
  if (length(missing_cols) > 0) {
    warning(paste0("[get_std] missing required columns for key '", key,
                   "': ", paste(missing_cols, collapse = ", "), ". Returning NULL."))
    return(NULL)
  }

  # When multiple standards exist for the same key, use the most stringent (lowest value)
  if (nrow(std) > 1) std <- std[which.min(std$value), , drop = FALSE]

  std
}

# confirms the std is valid to refer to
has_valid_std <- function(std) {
  !is.null(std) &&
    is.data.frame(std) &&
    nrow(std) > 0 &&
    all(c("value", "unit") %in% names(std)) &&
    !is.na(std$value[1]) &&
    !is.na(std$unit[1]) &&
    nzchar(trimws(std$unit[1]))
}

# For an individual parameter: get & prep the parameter standard, then compare with the found value

calculate_hq <- function(param, med, val, unit) {
  hq <- NA_real_
  
  std_info <- list(
    HQ = list(std_reg = NA, std_val = NA, std_unit = NA)
  )
  
  # Edge case: Oxygen Saturation (lower is worse)
  if (grepl("oxygen.*saturation", param, ignore.case = TRUE)) {
    std <- get_std(parameter = param, std_type = "hq", media = med)
    
    if (!has_valid_std(std)) {
      return(list(HQ = hq, std_info = std_info))
    }
    
    unit_check_hq <- compare_units(unit, std$unit[1])
    
    if (isTRUE(unit_check_hq$convertible)) {
      val_norm <- val / unit_check_hq$conversion_factor
      
      if (is.na(val_norm)) {
        hq <- NA_real_
      } else if (val_norm == 0) {
        hq <- Inf
      } else {
        hq <- std$value[1] / val_norm
      }
      
      std_info[["HQ"]] <- list(
        std_reg = std$regulator[1],
        std_val = std$value[1],
        std_unit = std$unit[1]
      )
    }
    
    return(list(HQ = hq, std_info = std_info))
  }
  
  # Edge case: pH (acceptable range)
  if (grepl("^pH\\b", param, ignore.case = TRUE)) {
    if (is.na(unit) || !nzchar(trimws(unit))) {
      unit <- "pH unit"
    }
    
    if (stringr::str_detect(unit, "mV")) {
      return(list(HQ = hq, std_info = std_info))
    }
    
    std_low <- get_std(parameter = "pH low", std_type = "hq", media = med)
    std_high <- get_std(parameter = "pH high", std_type = "hq", media = med)
    
    if (!has_valid_std(std_low) || !has_valid_std(std_high)) {
      return(list(HQ = hq, std_info = std_info))
    }
    
    lower <- std_low$value[1]
    upper <- std_high$value[1]
    midpoint <- (lower + upper) / 2
    
    if (is.na(val)) {
      hq <- NA_real_
    } else if (val >= lower && val <= upper) {
      hq <- 0
    } else if (val < midpoint) {
      if (val == 0) {
        hq <- Inf
      } else {
        hq <- lower / val
      }
    } else {
      if (upper == 0) {
        hq <- Inf
      } else {
        hq <- val / upper
      }
    }
    
    std_info[["HQ"]] <- list(
      std_reg = paste(std_low$regulator[1], std_high$regulator[1], sep = "/"),
      std_val = paste(lower, upper, sep = "-"),
      std_unit = std_low$unit[1]
    )
    
    return(list(HQ = hq, std_info = std_info))
  }
  
  # General case
  std <- get_std(param, "hq", med)
  
  if (!has_valid_std(std)) {
    return(list(HQ = hq, std_info = std_info))
  }
  
  unit_check_hq <- compare_units(unit, std$unit[1])
  
  if (isTRUE(unit_check_hq$convertible)) {
    val_norm <- val / unit_check_hq$conversion_factor
    
    if (is.na(val_norm)) {
      hq <- NA_real_
    } else {
      hq <- val_norm / std$value[1]
    }
    
    std_info[["HQ"]] <- list(
      std_reg = std$regulator[1],
      std_val = std$value[1],
      std_unit = std$unit[1]
    )
  }
  
  return(list(HQ = hq, std_info = std_info))
}