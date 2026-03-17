#### parse a unit string into structured components ####
parse_unit <- function(u) {
  u0 <- as.character(u %||% "")
  u0 <- tolower(u0)
  # normalize micro symbols
  u0 <- str_replace_all(u0, "\u00B5|µ|μ", "u")
  u0 <- str_squish(u0)
  if (u0 == "") return(list(raw = u0, left = NA_character_, right = NA_character_, left_kind = NA_character_, prefix = NA_character_, gram_factor_g = NA_real_, denom_type = NA_character_, denom_scale = NA_real_))
  
  # normalize " per " into "/"
  u0 <- str_replace_all(u0, "\\s+per\\s+", "/")
  
  # if we have a slash, split on the first slash
  if (str_detect(u0, "/")) {
    parts <- str_split_fixed(u0, "/", n = 2)
    left_raw <- str_trim(parts[1])
    right_raw <- str_trim(parts[2])
  } else {
    # no slash: maybe token like "mg" or "ntu" or "ph unit" or "cfu/100 ml" baked into parentheses earlier
    left_raw <- str_trim(u0)
    right_raw <- ""
  }
  
  # identify left kind and prefix
  # counts / indicators
  if (str_detect(left_raw, "\\b(cfu|mpn|coliform|mesophilic|cfu\\)|mpn\\)|coliform)")) {
    left_kind <- "count"
    gram_factor_g <- NA_real_
    prefix <- NA_character_
  } else {
    # attempt to detect prefix (pg, ng, ug, mg, g, kg) at start of token
    prefix_match <- str_match(left_raw, "^(pg|ng|u?g|mg|g|kg)")[,1]
    # normalize µg patterns "ug" already
    prefix_match <- ifelse(prefix_match == "ug" | prefix_match == "ug", "ug", prefix_match)
    if (!is.na(prefix_match) && nzchar(prefix_match)) {
      # canonicalize "ug" vs "µg"
      prefix <- prefix_match
      # convert u?g -> ug
      prefix <- str_replace(prefix, "^ug$", "ug")
      # gram factor (grams per left unit)
      gram_factor_g <- .prefix_to_g[[prefix]]
      left_kind <- "mass"
    } else {
      # left didn't include a mass prefix - it might be "ph", "ntu", "ohm.cm" or other textual unit like "mg/l Zn" (we'll treat it as mass if "mg" appears anywhere)
      if (str_detect(left_raw, "mg|ug|ng|g|kg")) {
        # find first matching prefix anywhere
        prefix_match <- str_extract(left_raw, "pg|ng|ug|mg|g|kg")
        prefix <- prefix_match
        gram_factor_g <- .prefix_to_g[[prefix]]
        left_kind <- "mass"
      } else {
        # it's probably a non-mass token (NTU, mV, pH, etc) or a count token without explicit CFU
        prefix <- NA_character_
        gram_factor_g <- NA_real_
        # check common non-mass
        if (str_detect(left_raw, "ntu|ohm|mv|ph|sal|t ref|m3/s|m3 s")) {
          left_kind <- "other"
        } else {
          left_kind <- "other"
        }
      }
    }
  }
  
  # parse right (denominator) -> detect numeric multiplier (like "100 ml")
  denom_num <- suppressWarnings(as.numeric(str_extract(right_raw, "\\d+")))
  if (is.na(denom_num)) denom_num <- 1
  
  # detect denom unit type and compute denom_scale:
  # denom_scale is the numeric size of the denominator in base units:
  #  - for volume denominators we report denom_scale in L (so "100 ml" -> 0.1)
  #  - for mass denominators we report denom_scale in kg (so "kg" -> 1)
  denom_type <- NA_character_
  denom_scale <- NA_real_
  
  if (str_detect(right_raw, "ml")) {
    denom_type <- "L"
    denom_scale <- denom_num * 0.001
  } else if (str_detect(right_raw, "\\bdl\\b|deciliter|dL")) {
    denom_type <- "L"
    denom_scale <- denom_num * 0.1
  } else if (str_detect(right_raw, "\\bl\\b|liter|litre")) {
    denom_type <- "L"
    denom_scale <- denom_num * 1
  } else if (str_detect(right_raw, "m3|m\\^3")) {
    denom_type <- "L"
    denom_scale <- denom_num * 1000  # 1 m3 = 1000 L
  } else if (str_detect(right_raw, "kg|kilogram")) {
    denom_type <- "kg"
    denom_scale <- denom_num * 1
  } else if (str_trim(right_raw) == "") {
    denom_type <- NA_character_
    denom_scale <- NA_real_
  } else {
    # fallback: look for "100 ml" or 'per 100 ml' already normalized -> handled by ml, but keep fallback
    if (str_detect(right_raw, "100\\s*ml")) {
      denom_type <- "L"; denom_scale <- 100 * 0.001
    } else {
      denom_type <- "other"; denom_scale <- NA_real_
    }
  }
  
  # if left kind is count, treat gram_factor_g as NA but useful for computations we set gram_factor_g = 1 (counts are unit-less in mass sense)
  if (left_kind == "count") {
    gram_factor_g <- NA_real_
  }
  
  list(
    raw = u0,
    left_raw = left_raw,
    right_raw = right_raw,
    left_kind = left_kind,        # "mass", "count", "other"
    prefix = prefix,
    gram_factor_g = gram_factor_g, # grams per left-unit (if left_kind == "mass")
    denom_type = denom_type,      # "L", "kg", "other", NA
    denom_scale = denom_scale     # numeric scale in base (L or kg) or NA
  )
}

#### compares sample_unit -> standard_unit ####
# returns a list: convertible (T/F), conversion_factor (multiply sample value by this -> standard unit), message, parsed_sample, parsed_standard
compare_units <- function(sample_unit, standard_unit) {
  s <- parse_unit(sample_unit)
  t <- parse_unit(standard_unit)
  
  # quick identical after normalization
  if (!is.na(s$raw) && !is.na(t$raw) && s$raw == t$raw) {
    return(list(convertible = TRUE, conversion_factor = 1, message = "Exact match (normalized).", sample_parsed = s, standard_parsed = t))
  }
  
  # If either is NA/empty, can't convert
  if (is.na(s$raw) || s$raw == "" || is.na(t$raw) || t$raw == "") {
    return(list(convertible = FALSE, conversion_factor = NA_real_, message = "One of the units is empty/unparseable.", sample_parsed = s, standard_parsed = t))
  }
  
  # If both are 'other' (like pH, NTU, Ohm.cm), only convertible if exact token match
  if (s$left_kind == "other" || t$left_kind == "other") {
    # allow exact normalized equality only
    if (s$raw == t$raw) {
      return(list(convertible = TRUE, conversion_factor = 1, message = "Other-type exact match.", sample_parsed = s, standard_parsed = t))
    } else {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Non-mass/non-count units differ and are not convertible.", sample_parsed = s, standard_parsed = t))
    }
  }
  
  # both counts (e.g., CFU/100 ml, MPN/100 ml) -> treat like count/volume
  if (s$left_kind == "count" && t$left_kind == "count") {
    # need denom_type both L-like
    if (is.na(s$denom_scale) || is.na(t$denom_scale) || s$denom_type != "L" || t$denom_type != "L") {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Count units but denominator not volume or unparseable.", sample_parsed = s, standard_parsed = t))
    }
    # conversion factor is ratio of denom scales (e.g., sample per 100ml -> denom_scale 0.1 L, standard per 1 L -> denom_scale 1; factor = denom_std/denom_sample)
    factor <- t$denom_scale / s$denom_scale
    return(list(convertible = TRUE, conversion_factor = factor, message = "Count-per-volume convertible (denominator scale applied).", sample_parsed = s, standard_parsed = t))
  }
  
  # both mass-ish
  if (s$left_kind == "mass" && t$left_kind == "mass") {
    # denom must be same family: both L (mass/volume) or both kg (mass/mass)
    if (is.na(s$denom_type) || is.na(t$denom_type)) {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "One of denominators unparseable.", sample_parsed = s, standard_parsed = t))
    }
    if (s$denom_type != t$denom_type) {
      # cannot convert mass/volume <-> mass/mass without density
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Different denominator families (e.g. L vs kg) — requires density; not convertible.", sample_parsed = s, standard_parsed = t))
    }
    # both same denom family (e.g., both L)
    if (is.na(s$gram_factor_g) || is.na(t$gram_factor_g) || is.na(s$denom_scale) || is.na(t$denom_scale)) {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Missing prefix/denominator info; can't compute factor.", sample_parsed = s, standard_parsed = t))
    }
    # conversion factor formula derived so that:
    # sample_value * factor = value expressed in standard units.
    # Let sample "left_s / denom_s", standard "left_t / denom_t"
    # gram_factor_g = grams per left-unit.
    # denom_scale is numeric in base (L or kg), i.e. denom of sample = denom_scale_s (liters or kg).
    # The factor simplifies to:
    # factor = (gram_factor_s / denom_scale_s) / (gram_factor_t / denom_scale_t)
    factor <- (t$gram_factor_g / t$denom_scale) / (s$gram_factor_g / s$denom_scale)
    
    # debugging
    # cat("\n[calculate_hqcr]: HQ values:\nInitial val: ",val," (",unit,")\nStd val: ",
    #     std$value, " (",std$unit,")\nHQ: ", hq, "\nConversion Factor: ", unit_check_hq$conversion_factor)
    # 
    
    res = list(convertible = TRUE, conversion_factor = as.numeric(factor), message = "Mass units convertible via prefix/denom scaling.", sample_parsed = s, standard_parsed = t)
    # str(res, max.level=3, give.attr=FALSE, strict.width="cut")
    
    return(res)
  }
  
  # otherwise fallback: not convertible by metric multipliers only
  cat("[compare_units] Fallback.")
  list(convertible = FALSE, conversion_factor = NA_real_, message = "Units not both mass or count-per-volume families; cannot auto-convert.", sample_parsed = s, standard_parsed = t)
}

#### wrapper for comparing units ####
compare_units_summary <- function(sample_unit, standard_unit) {
  res <- compare_units(sample_unit, standard_unit)
  if (res$convertible) {
    sprintf("Convertible: TRUE — multiply sample values by %g to get %s (message: %s)",
            res$conversion_factor, standard_unit, res$message)
  } else {
    sprintf("Convertible: FALSE — %s (sample='%s', standard='%s')",
            res$message, sample_unit, standard_unit)
  }
}