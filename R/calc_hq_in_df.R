## Note: this was deprecated for the other score_data() function

calc_hq_in_df <- function(data, media, param, plot_type) {
  df <- data
  
  # Retrieve standard for this parameter-media combination
  param_stds <- strict_std |>
    filter(.data$media == media,
           str_detect(.data$parameter, fixed(param)))
  
  # Check if standard exists
  has_standard <- nrow(param_stds) > 0
  
  if (has_standard) {
    # Check if this is a range-based parameter (like pH)
    has_low <- any(str_detect(param_stds$parameter, "low"))
    has_high <- any(str_detect(param_stds$parameter, "high"))
    is_range_param <- has_low && has_high
    
    if (is_range_param) {
      # Handle range-based standards (pH)
      low_std <- param_stds |> filter(str_detect(parameter, "low"))
      high_std <- param_stds |> filter(str_detect(parameter, "high"))
      
      param_std_low <- low_std$value[1]
      param_std_high <- high_std$value[1]
      std_unit <- low_std$unit[1]
      data_unit <- df$unit[1]
      std_source <- low_std$regulator[1]
      
      # Unit conversion function
      convert_units <- function(value, from_unit, to_unit) {
        # Normalize units (remove spaces, make lowercase)
        from <- tolower(gsub("\\s+", "", from_unit))
        to <- tolower(gsub("\\s+", "", to_unit))
        
        # If units are the same, no conversion needed
        if (from == to) return(value)
        
        # Conversion factors (to base unit)
        conversions <- list(
          # Mass conversions (to grams)
          "kg" = 1000, "g" = 1, "mg" = 0.001, "ug" = 0.000001, "µg" = 0.000001,
          # Concentration conversions
          "mg/kg" = 1, "ug/kg" = 0.001, "µg/kg" = 0.001,
          "mg/l" = 1, "ug/l" = 0.001, "µg/l" = 0.001,
          "ppm" = 1, "ppb" = 0.001
        )
        
        # Get conversion factors
        from_factor <- conversions[[from]]
        to_factor <- conversions[[to]]
        
        # Check if both units are recognized
        if (is.null(from_factor) || is.null(to_factor)) {
          warning(paste("Cannot convert from", from_unit, "to", to_unit, "- using original values"))
          return(value)
        }
        
        # Convert: value * (from_factor / to_factor)
        converted <- value * (from_factor / to_factor)
        return(converted)
      }
      
      # Handle unit conversion - special case for pH which is dimensionless
      if (param == "pH") {
        display_unit <- "pH units"
        # Don't convert - pH is dimensionless
      } else if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
        param_std_low <- convert_units(param_std_low, std_unit, data_unit)
        param_std_high <- convert_units(param_std_high, std_unit, data_unit)
        message(paste("Converted standard from", std_unit, "to", data_unit))
        display_unit <- data_unit
      } else {
        display_unit <- std_unit
      }
      
      # Calculate HQ based on distance from acceptable range
      # HQ >= 1 means outside acceptable range
      df <- df |>
        mutate(
          # Distance below low threshold or above high threshold
          hq = case_when(
            concentration < ((param_std_low + param_std_high) / 2) ~ param_std_low / concentration, # Below middle of range
            concentration > ((param_std_low + param_std_high) / 2) ~ concentration / param_std_high, # Above middle of range
            TRUE ~ 0.5  # Middle of range
          ),
          has_standard = TRUE,
          std_source = std_source,
          param_std_low = param_std_low,    
          param_std_high = param_std_high,    
          display_unit = display_unit
        )
      
      # Add map-specific columns if needed
      if (plot_type == "map") {
        df <- df |>
          mutate(
            marker_radius = ifelse(concentration < param_std_low | concentration > param_std_high, 9, 5),
            stroke_color = ifelse(concentration < param_std_low | concentration > param_std_high, "yellow", "black"),
            stroke_weight = ifelse(concentration < param_std_low | concentration > param_std_high, 3, 1.5)
          )
      }
      
    } else {
      # Handle single-threshold standards (all other parameters)
      param_std <- param_stds$value[1]
      std_unit <- param_stds$unit[1]
      data_unit <- df$unit[1]
      std_source <- param_stds$regulator[1]
      
      # Unit conversion function (same as above)
      convert_units <- function(value, from_unit, to_unit) {
        from <- tolower(gsub("\\s+", "", from_unit))
        to <- tolower(gsub("\\s+", "", to_unit))
        if (from == to) return(value)
        
        conversions <- list(
          "kg" = 1000, "g" = 1, "mg" = 0.001, "ug" = 0.000001, "µg" = 0.000001,
          "mg/kg" = 1, "ug/kg" = 0.001, "µg/kg" = 0.001,
          "mg/l" = 1, "ug/l" = 0.001, "µg/l" = 0.001,
          "ppm" = 1, "ppb" = 0.001
        )
        
        from_factor <- conversions[[from]]
        to_factor <- conversions[[to]]
        
        if (is.null(from_factor) || is.null(to_factor)) {
          warning(paste("Cannot convert from", from_unit, "to", to_unit, "- using original values"))
          return(value)
        }
        
        converted <- value * (from_factor / to_factor)
        return(converted)
      }
      
      # Handle unit conversion
      if (param == "pH") {
        display_unit <- "pH units"
      } else if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
        param_std <- convert_units(param_std, std_unit, data_unit)
        message(paste("Converted standard from", std_unit, "to", data_unit))
        display_unit <- data_unit
      } else {
        display_unit <- std_unit
      }
      
      # Calculate HQ for single threshold
      df <- df |>
        mutate(
          hq = concentration / param_std,
          has_standard = TRUE,
          std_source = std_source,
          param_std = param_std,
          display_unit = display_unit
        )
      
      # Add map-specific columns if needed
      if (plot_type == "map") {
        df <- df |>
          mutate(
            marker_radius = ifelse(hq >= 1, 9, 5),
            stroke_color = ifelse(hq >= 1, "yellow", "black"),
            stroke_weight = ifelse(hq >= 1, 3, 1.5)
          )
      }
    }
    
  } else {
    # If no standard exists, set all to default
    df <- df |>
      mutate(
        hq = NA_real_,
        has_standard = FALSE,
        std_source = NA_character_
      )
    
    # Add map-specific columns if needed
    if (plot_type == "map") {
      df <- df |>
        mutate(
          marker_radius = 6,
          stroke_color = "black",
          stroke_weight = 1.5
        )
    }
  }
  
  df <- df |>
    mutate(is_range_param = is_range_param)
  View(df |> mutate(test=1))
  return(df)
}
