plot_pilcomayo_ts <- function(data, media, param, station, fraction = "Total") {
  
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
  
  # Score data if not already scored (same as plot_pilcomayo_map)
  if (!"HQ" %in% names(df)) {
    print("scoring data")
    df <- score_data(df)
  }
  
  print("data scored")
  
  # Check if has_standard column exists, if not create it (same as plot_pilcomayo_map)
  if (!"has_standard" %in% names(df)) {
    message("has_standard column missing, checking std_info...")
    # Try to determine if standards exist from std_info
    if ("std_info" %in% names(df)) {
      df <- df |>
        mutate(has_standard = !is.na(HQ) | !is.na(CR) | !is.na(WL))
    } else {
      # Default to FALSE if we can't determine
      df <- df |>
        mutate(has_standard = FALSE)
    }
  }
  
  # Check if standard exists (using the same approach as plot_pilcomayo_map)
  has_standard <- if (nrow(df) > 0 && !is.null(df$has_standard)) {
    first(df$has_standard, default = FALSE)
  } else {
    FALSE
  }
  
  # Extract standard info from std_info column if standard exists
  if (has_standard && "std_info" %in% names(df) && nrow(df) > 0) {
    std_info <- first(df$std_info)
    
    # Check if this is a range parameter
    if ("is_range_param" %in% names(df) && first(df$is_range_param, default = FALSE)) {
      # Extract range values
      if ("param_std_low" %in% names(df) && "param_std_high" %in% names(df)) {
        param_std_low <- first(df$param_std_low)
        param_std_high <- first(df$param_std_high)
        display_unit <- first(df$unit)
        std_source <- "Standard"
      }
    } else if (!is.null(std_info) && length(std_info) > 0) {
      # Try to extract from HQ field in std_info
      if (!is.null(std_info$HQ) && !is.na(std_info$HQ$std_val)) {
        param_std <- std_info$HQ$std_val
        display_unit <- std_info$HQ$std_unit
        std_source <- std_info$HQ$std_reg
      }
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
  
  
  # Build hover text
  if (has_standard) {
    df <- df |>
      mutate(
        hover_text = paste0(
          "Station: ", station, "<br>",
          "Date: ", format(date, "%Y-%m-%d"), "<br>",
          str_to_title(parameter), ": ", round(concentration, 3), " ", unit, "<br>",
          ifelse(!is.na(HQ), paste0("HQ: ", round(HQ, 3), "<br>"), ""),
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
  } else {
    df <- df |>
      mutate(
        hover_text = paste0(
          "Station: ", station, "<br>",
          "Date: ", format(date, "%Y-%m-%d"), "<br>",
          str_to_title(parameter), ": ", round(concentration, 3), " ", unit, "<br>",
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
  }
  
  
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
  if (has_standard && exists("param_std") && !is.na(param_std)) {
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