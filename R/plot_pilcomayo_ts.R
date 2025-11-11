# where is this called??
plot_pilcomayo_ts <- function(data, media, param, station, fraction = "Total") {
  # req(master_data)
  # print("[plot_pilcomayo_ts]")
  # print(names(master_data)) # for sanity
  # 
  # # filter data...
  # if (media == "water") df = master_data$water_scored else df = master_data$sed_scored
  # 
  # filter data for selected media, parameter and station
  df <- data |>
    filter(.data$media == .env$media,
           .data$parameter == .env$param,
           .data$station == .env$station)
  
  # Only apply fraction filter for parameters that actually have fractions
  # Skip for pH and other field parameters
  # Only do this step for water data (sediment is not broken into fractions for any parameters)
  if (media == "water") {
    if (param != "pH" && any(data$fraction == fraction)) {
      df <- df |>
        filter(fraction == !!fraction)
    }
  }
  
  
  # Special handling for pH - filter to pH units only
  if (param == "pH") {
    df <- df |>
      filter(unit == "u")
  }
  
  # retrieve standard for this parameter-media combination
  ### FIGURE OUT HOW TO HANDLE CHROMIUM (HEX VS TRI) ###
  param_stds <- strict_std |>
    filter(.data$media == .env$media,
           str_detect(.data$parameter, .env$param))
  
  # Check if standard exists
  has_standard <- nrow(param_stds) > 0
  
  if (has_standard) {
    param_std <- param_stds$value
    std_unit <- param_stds$unit
    data_unit <- first(df$unit)
    std_source <- param_stds$regulator
    
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
    
    # Convert standard to match data units if needed
    if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
      param_std <- convert_units(param_std, std_unit, data_unit)
      message(paste("Converted standard from", std_unit, "to", data_unit))
      display_unit <- data_unit
    } else {
      display_unit <- std_unit
    }
  }
  
  # create Y-axis label for later use
  y_lab <- paste(first(df$parameter), " (", first(df$unit), ")", sep = "")
  
  # Calculate daily averages for the line
  df_avg <- df |>
    group_by(date) |>
    summarise(
      avg_concentration = mean(concentration, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate range of data for use in offsetting labels
  y_range <- max(df$concentration, na.rm = TRUE) - min(df$concentration, na.rm = TRUE)
  y_offset_amount <- y_range * 0.15
  
  x_range <- max(df$date, na.rm = TRUE) - min(df$date, na.rm = TRUE)
  x_offset_amount <- x_range * 0.15
  
  
  if (!!media == "sediment") {
    # Normalize distance_from_bank to 0-1 scale for alpha
    # Closer to bank (smaller distance) = higher alpha (more opaque)
    # Further from bank (larger distance) = lower alpha (more transparent)
    if (all(!is.na(df$distance_from_bank))) {
      min_dist <- min(df$distance_from_bank, na.rm = TRUE)
      max_dist <- max(df$distance_from_bank, na.rm = TRUE)
      
      # Invert so closer = more opaque
      df <- df |>
        mutate(alpha_value = 1 - ((distance_from_bank - min_dist) / (max_dist - min_dist)))
      
      # Ensure alpha stays in reasonable range (0.3 to 1.0)
      df <- df |>
        mutate(alpha_value = scales::rescale(alpha_value, to = c(0.3, 1.0)))
    } else {
      # If distance_from_bank has NAs or doesn't exist, set all to 1
      df <- df |>
        mutate(alpha_value = 1.0)
    }
  } else {
    # If distance_from_bank has NAs or doesn't exist, set all to 1
    df <- df |>
      mutate(alpha_value = 1.0)
  }
  
  
  df <- df |>
    mutate(
      hover_text = paste0(
        "Station: ", station, "<br>",
        "Date: ", format(date, "%Y-%m-%d"), "<br>",
        str_to_title(parameter), ": ", round(concentration, 3), " ", unit, "<br>",
        if (has_standard) {
          paste0("HQ: ", round(concentration/param_std, 3), "<br>")
        } else {
          ""
        },
        if (!!media == "sediment") {
          paste0(
            "Sieve size: ", ifelse(is.na(sieve_size), "N/A", sieve_size), "<br>",
            "Distance from bank: ", ifelse(is.na(distance_from_bank), "N/A", distance_from_bank)
          )
        } else {
          ""
        }
        
      )
    )
  
  
  # Create base plot
  p <- ggplot() +
    # Line showing daily averages
    geom_line(data = df_avg, aes(x = date, y = avg_concentration, group = 1), 
              color = "black", linewidth = 0.8) +
    # Points showing all individual observations with alpha based on distance from bank
    geom_point(data = df, aes(x = date, y = concentration, alpha = alpha_value, text = hover_text), 
               color = "black", size = 2) +
    scale_alpha_identity() +
    labs(x = "Date",
         y = y_lab,
         title = paste("Time Series of ", param, " at ", station, " (", str_to_title(media), ")", sep = "")) +
    theme_minimal()
  
  # Add standard line with hover text if standard exists
  if (has_standard) {
    # Create a data frame for the standard line that spans the x-axis range
    std_df <- data.frame(
      date = seq(min(df$date), max(df$date), length.out = 100),
      std_value = param_std,
      hover_text = paste("Standard = ", round(param_std, 3), " ", display_unit, " (", std_source, ")", sep = "")
    )
    
    p <- p +
      geom_line(data = std_df, 
                aes(x = date, y = std_value, text = hover_text),
                color = "red", linetype = "dashed", linewidth = 0.8)
  }
  
  # Convert to plotly
  ply <- ggplotly(p, tooltip = "text")
  
  return(ply)
}
