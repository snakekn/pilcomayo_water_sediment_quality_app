# Shiny: will be used as station_scores_plot

plot_top_hq_stations <- function(data, media, param, fraction = "any", method = "max") {
  ## Validate selected inputs
  req(param) # to prevent param coming in as "" before the selector is initialized
  cat("\n[plot_top_hq_stations]: Values: ", media, " - ", param, " - ", fraction, " - ", method, "\n")
  
  # Validate method parameter
  if (!method %in% c("max", "mean", "average")) {
    message("method must be either 'max', 'mean', or 'average'. Relying on max.")
    method = "max"
  }
  # Standardize method name
  if (method == "average") method <- "mean"
  
  # Validate fraction parameter
  if (!fraction %in% c("Total", "Dissolved", "Suspended", "any")) {
    message("method must be one of: 'Total', 'Dissolved', 'Suspended', or 'any'. Relying on any")
    fraction = "any"
  }
  
  # Unit conversion helper function (defined once at top level)
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
  
  # Filter for media and parameter
  if (media != "all") { # only filter on media if we're not using them all
    data = data |> filter(media == !!media) # update data
    cat("\n\nafter filtering for media, nrow(df) = ", nrow(df), "\n")
  }
  df <- data |> # then filter by param no matter what. Move data into df
    filter(parameter == param)
  cat("\n\nafter filtering for parameter, nrow(df) = ", nrow(df), "\n")
  
  # Only apply fraction filter for parameters that actually have fractions
  # Skip for pH and other field parameters
  # Only do this step for water data (sediment is not broken into fractions for any parameters)
  
  if (media == "water" && param != "pH" && fraction != "any") {
    df <- df |>
      filter(fraction == !!fraction)
    cat("\n\nafter filtering for fraction, nrow(df) = ", nrow(df), "\n")
  }
  if(nrow(df) == 0) stop(paste("No data found using the current filters. Please update your filters."))
  
  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media == "water" && param != "pH" && any(df$fraction == fraction))
  
  # Special handling for pH - filter to pH units only
  if (param == "pH") {
    df <- df |>
      filter(unit == "u")
  }
  
  # Retrieve standard for this parameter-media combination
  param_stds <- stds |>
    filter(str_detect(.data$parameter, !!param))
  
  if (media != "all") { # filter by media if needed
    param_stds = param_stds |>
      filter(.data$media == !!media)
  }
  
  # Check if standard exists
  has_standard <- nrow(param_stds) > 0
  
  if (!has_standard) {
    message(paste("No standard found for", param, "in", media, "- cannot calculate hazard quotients"))
  }
  
  # Check if this is a range-based parameter (like pH)
  has_low <- any(str_detect(param_stds$parameter, "low"))
  has_high <- any(str_detect(param_stds$parameter, "high"))
  is_range_param <- has_low && has_high
  cat("\n[plot_top_hq_stations]: is_range_param = ", is_range_param, "\n")
  
  if (is_range_param) {
    # Handle range-based standards (pH)
    low_std <- param_stds |> filter(str_detect(parameter, "low"))
    high_std <- param_stds |> filter(str_detect(parameter, "high"))
    
    param_std_low <- low_std$value[1]
    param_std_high <- high_std$value[1]
    std_unit <- low_std$unit[1]
    data_unit <- df$unit[1]
    std_source <- low_std$regulator[1]
    
    # Handle unit conversion - special case for pH which is dimensionless
    if (param == "pH") {
      display_unit <- "pH units"
      # Don't convert - pH is dimensionless
    } else if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
      param_std_low <- convert_units(param_std_low, std_unit, data_unit)
      param_std_high <- convert_units(param_std_high, std_unit, data_unit)
      display_unit <- data_unit
    } else {
      display_unit <- std_unit
    }
    
    # Calculate HQ based on distance from acceptable range
    # For range-based parameters: HQ > 1 when outside range
    # CRITICAL: Use !! to force evaluation of param_std_low and param_std_high
    std_low_val <- param_std_low
    std_high_val <- param_std_high
    
    df <- df |>
      mutate(
        hq = case_when(
          concentration < std_low_val ~ std_low_val / concentration,
          concentration > std_high_val ~ concentration / std_high_val,
          TRUE ~ NA_real_
        )
      )
    
    std_text <- paste0(round(param_std_low, 3), " - ", round(param_std_high, 3), " ", display_unit)
    
  } 
  else {
    # Handle single-threshold standards
    param_std <- param_stds$value[1]
    std_unit <- param_stds$unit[1]
    data_unit <- df$unit[1]
    std_source <- param_stds$regulator[1]
    
    # Handle unit conversion
    if (param == "pH") {
      display_unit <- "pH units"
    } else if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
      param_std <- convert_units(param_std, std_unit, data_unit)
      display_unit <- data_unit
    } else {
      display_unit <- std_unit
    }
    
    # Calculate HQ for single threshold
    std_val <- param_std
    df <- df |>
      mutate(hq = concentration / std_val)
    
    std_text <- paste0(round(param_std, 3), " ", display_unit)
  }
  
  # Filter to only exceedances
  cat("\n\before filtering for hq, nrow(df) = ", nrow(df), "\n")
  df_exceedances <- df |>
    filter(!is.na(hq), hq > 1) # remove where HQ is acceptable
  
  # Check if there are any exceedances
  if (nrow(df_exceedances) == 0) {
    stop(paste("No exceedances found for", param, "in", media, "- all measurements are within acceptable range"))
  }
  
  # Aggregate HQ by station using specified method
  # Aggregate HQ by station using specified method
  if (method == "max") {
    station_hq <- df_exceedances |>
      group_by(station) |>
      summarise(
        hq = max(hq, na.rm = TRUE),
        max_date = date[which.max(hq)],
        n_measurements = n(),
        .groups = "drop"
      )
    method_label <- "Maximum"
  } else {
    station_hq <- df_exceedances |>
      group_by(station) |>
      summarise(
        hq = mean(hq, na.rm = TRUE),
        max_date = date[which.max(hq)],
        n_measurements = n(),
        .groups = "drop"
      )
    method_label <- "Average"
  }
  
  
  
  # Get top 10 stations by HQ
  top_stations <- station_hq |>
    arrange(desc(hq)) |>
    slice_head(n = 10) |>
    mutate(
      station_label = paste0(station, " (n=", n_measurements, ")"),
      station_label = factor(station_label, levels = rev(station_label)),
      exceeds_standard = hq >= 1,
      hover_text = paste0(
        "Station: ", station, "<br>",
        if (method == "max") paste0("Date: ", max_date, "<br>"),
        if (method == "mean") {paste0(str_to_title(method), " ")},
        "HQ: ", round(hq, 3), "<br>",
        "Total # observations: ", n_measurements, "<br>"
      )
    )
  
  # Create bar chart with hover text
  p <- ggplot(top_stations, aes(x = station_label, y = hq, fill = exceeds_standard, text = hover_text)) +
    geom_col() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
    scale_fill_manual(
      values = c("TRUE" = "darkorange", "FALSE" = "steelblue"),
      labels = c("TRUE" = "Exceeds Standard", "FALSE" = "Below Standard"),
      name = NULL
    ) +
    coord_flip() +
    labs(
      title = paste(
        "Top 10 Stations by", method_label, "Hazard Quotient:",
        if (fraction_applied) paste(param, " (", fraction, ")", sep = "") else param
      ),
      subtitle = paste0("Media: ", media, "\nStandard: ", std_text, " (", std_source, ")"),
      x = "Station",
      y = paste(method_label, "Hazard Quotient (HQ)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray30"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # Convert to plotly for interactive hover
  ply <- ggplotly(p, tooltip = "text")
  
  ply <- ply |>
    layout(
      title = list(
        text = paste0(
          "Top 10 Stations by ", method_label, " Hazard Quotient: ",
          if (fraction_applied) paste(param, " (", fraction, ")", sep = "") else param,
          "<br><sup>",  # `<sup>` renders smaller subtitle text
          "Media: ", media, " — Standard: ", std_text, " (", std_source, ")",
          "</sup>"
        )
      ),
      legend = list(
        orientation = "h",
        x = 0.5,
        xanchor = "center",
        y = -0.15,
        yanchor = "top"
      )
    )
  
  
  # Fix legend labels - find and replace any TRUE/FALSE with proper labels
  for (i in seq_along(ply$x$data)) {
    current_name <- ply$x$data[[i]]$name
    
    # Check if name contains patterns we want to replace
    if (!is.null(current_name)) {
      # Replace variations of TRUE with "Exceeds Standard"
      if (grepl("TRUE|Exceeds Standard", current_name, ignore.case = FALSE)) {
        ply$x$data[[i]]$name <- "Exceeds Standard"
      }
      # Replace variations of FALSE with "Below Standard"
      else if (grepl("FALSE|Below Standard", current_name, ignore.case = FALSE)) {
        ply$x$data[[i]]$name <- "Below Standard"
      }
    }
  }
  
  
  return(ply)
}
