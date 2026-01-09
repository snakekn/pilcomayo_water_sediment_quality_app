plot_top_hq_params <- function(data, media, fraction = "Total", 
                               temporal_aggregation = "max",
                               spatial_aggregation = "max",
                               decay_per_day = NULL) {   # NEW: for weighted temporal aggregation
  
  # Validate temporal_aggregation parameter
  valid_temporal_aggregations <- c("recent", "mean", "average", "max", "weighted")
  if (!temporal_aggregation %in% valid_temporal_aggregations) {
    stop(paste("Invalid temporal_aggregation. Choose from:", paste(valid_temporal_aggregations, collapse = ", ")))
  }
  
  # Standardize method names
  if (temporal_aggregation == "average") temporal_aggregation <- "mean"
  
  # Validate spatial_aggregation parameter
  valid_spatial_aggregations <- c("mean", "average", "median", "max")
  if (!spatial_aggregation %in% valid_spatial_aggregations) {
    stop(paste("Invalid spatial_aggregation. Choose from:", paste(valid_spatial_aggregations, collapse = ", ")))
  }
  
  # Standardize method names
  if (spatial_aggregation == "average") spatial_aggregation <- "mean"
  
  # Unit conversion helper function
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
  
  # Filter for media
  df <- data |>
    filter(media == !!media)
  
  # Only apply fraction filter for drinking water and non-pH parameters
  if (media == "water") {
    # Get unique parameters that have the specified fraction
    params_with_fraction <- df |>
      filter(fraction == !!fraction) |>
      pull(parameter) |>
      unique()
    
    # Apply fraction filter only to those parameters
    df <- df |>
      filter(
        (parameter %in% params_with_fraction & fraction == !!fraction) |
          (!parameter %in% params_with_fraction)
      )
  }
  
  # Get unique parameters in the filtered data
  params <- unique(df$parameter)
  
  # Calculate HQ for each parameter
  param_hq_list <- list()
  
  for (param in params) {
    param_df <- df |> filter(parameter == param)
    
    # Special handling for pH - filter to pH units only
    if (param == "pH") {
      param_df <- param_df |> filter(unit == "u")
    }
    
    # Retrieve standard for this parameter-media combination
    param_stds <- strict_std |>
      filter(.data$media == !!media,
             str_detect(.data$parameter, !!param))
    
    # Skip if no standard exists
    if (nrow(param_stds) == 0) next
    
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
      data_unit <- param_df$unit[1]
      std_source <- low_std$regulator[1]
      
      # Handle unit conversion
      if (param == "pH") {
        display_unit <- "pH units"
      } else if (!is.na(std_unit) && !is.na(data_unit) && std_unit != data_unit) {
        param_std_low <- convert_units(param_std_low, std_unit, data_unit)
        param_std_high <- convert_units(param_std_high, std_unit, data_unit)
        display_unit <- data_unit
      } else {
        display_unit <- std_unit
      }
      
      # Calculate HQ based on distance from acceptable range
      std_low_val <- param_std_low
      std_high_val <- param_std_high
      
      param_df <- param_df |>
        mutate(
          hq = case_when(
            concentration < std_low_val ~ std_low_val / concentration,
            concentration > std_high_val ~ concentration / std_high_val,
            TRUE ~ NA_real_
          )
        )
      
      std_text <- paste0(round(param_std_low, 3), " - ", round(param_std_high, 3), " ", display_unit)
      
    } else {
      # Handle single-threshold standards
      param_std <- param_stds$value[1]
      std_unit <- param_stds$unit[1]
      data_unit <- param_df$unit[1]
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
      param_df <- param_df |>
        mutate(hq = concentration / std_val)
      
      std_text <- paste0(round(param_std, 3), " ", display_unit)
    }
    
    # Filter to only exceedances
    param_df_exceedances <- param_df |>
      filter(!is.na(hq))
    
    # Skip if no exceedances
    if (nrow(param_df_exceedances) == 0) next
    
    # STEP 1: TEMPORAL AGGREGATION by station
    message(paste("Processing", param, "- Temporal aggregation:", temporal_aggregation))
    
    if (temporal_aggregation == "recent") {
      # Get most recent measurement for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        arrange(desc(date)) |>
        slice(1) |>
        ungroup() |>
        select(station, hq, date)
      
      temporal_label <- "Most Recent"
      
    } else if (temporal_aggregation == "weighted") {
      # Weighted average with more recent observations weighted higher
      target_date <- max(param_df_exceedances$date, na.rm = TRUE)
      
      if (is.null(decay_per_day)) {
        decay_per_day <- 0.001
      }
      
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        mutate(
          days_ago = as.numeric(target_date - date),
          weight = exp(-decay_per_day * days_ago)
        ) |>
        summarise(
          hq = weighted.mean(hq, w = weight, na.rm = TRUE),
          date = date[which.max(hq)],
          .groups = "drop"
        )
      
      temporal_label <- "Weighted Average"
      
    } else if (temporal_aggregation == "max") {
      # Maximum HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(hq)],
          hq = max(hq, na.rm = TRUE),
          .groups = "drop"
        )
      
      temporal_label <- "Maximum"
      
    } else {  # temporal_aggregation == "mean"
      # Mean HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(hq)],
          hq = mean(hq, na.rm = TRUE),
          .groups = "drop"
        )
      
      temporal_label <- "Average"
    }
    
    # STEP 2: SPATIAL AGGREGATION across all stations
    if (spatial_aggregation == "max") {
      param_summary <- station_temporal |>
        summarise(
          max_station = station[which.max(hq)],  # Track which station has max
          max_date = date[which.max(hq)],  # Track date of max HQ
          hq = max(hq, na.rm = TRUE),
          n_stations = n(),
          .groups = "drop"
        )
      
      spatial_label <- "Maximum"
      
    } else if (spatial_aggregation == "median") {
      param_summary <- station_temporal |>
        summarise(
          hq = median(hq, na.rm = TRUE),
          n_stations = n(),
          max_station = NA_character_,  # Not applicable for median
          max_date = as.Date(NA),  # Not applicable for median
          .groups = "drop"
        )
      
      spatial_label <- "Median"
      
    } else {  # spatial_aggregation == "mean"
      param_summary <- station_temporal |>
        summarise(
          hq = mean(hq, na.rm = TRUE),
          n_stations = n(),
          max_station = NA_character_,  # Not applicable for mean
          max_date = as.Date(NA),  # Not applicable for mean
          .groups = "drop"
        )
      
      spatial_label <- "Average"
    }
    
    # Add parameter info
    param_summary <- param_summary |>
      mutate(
        parameter = param,
        standard = std_text,
        std_source = std_source,
        n_measurements = nrow(param_df_exceedances)
      )
    
    param_hq_list[[param]] <- param_summary
  }
  
  # Combine all parameter summaries
  if (length(param_hq_list) == 0) {
    stop(paste("No exceedances found for any parameters in", media))
  }
  
  all_params <- bind_rows(param_hq_list)
  
  # Get top 10 parameters by HQ
  top_params <- all_params |>
    arrange(desc(hq)) |>
    slice_head(n = 10) |>
    mutate(
      param_label = paste0(parameter, " (n=", n_measurements, ")"),
      param_label = factor(param_label, levels = rev(param_label)),
      exceeds_standard = hq >= 1,
      hover_text = paste0(
        "Parameter: ", parameter, "<br>",
        # Show temporal aggregation method only if actual aggregation occurred
        if (temporal_aggregation != "recent") {
          paste0("Temporal aggregation: ", temporal_label, "<br>")
        } else {
          ""
        },
        # Show date for recent or max temporal aggregation
        if (temporal_aggregation %in% c("recent", "max")) {
          ifelse(!is.na(max_date),
                 paste0("Date: ", max_date, "<br>"),
                 "")
        } else {
          ""
        },
        # Always show spatial aggregation
        "Spatial aggregation: ", spatial_label, "<br>",
        # Show station if spatial aggregation is max
        ifelse(!is.na(max_station), 
               paste0("Station: ", max_station, "<br>"),
               ""),
        # HQ label - use "Aggregated HQ" if any aggregation occurred
        if (temporal_aggregation != "recent") {
          paste0("Aggregated HQ: ", round(hq, 3), "<br>")
        } else {
          paste0("HQ: ", round(hq, 3), "<br>")
        },
        "Standard: ", standard, " (", std_source, ")<br>",
        "# Stations: ", n_stations, "<br>",
        "# Measurements: ", n_measurements
      )
    )
  
  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media == "water" && any(data$fraction == fraction))
  
  # Create title based on aggregation methods used
  title_text <- paste(
    "Top 10 Parameters by", temporal_label, "+", spatial_label, "Hazard Quotient"
  )
  
  # Create bar chart with hover text
  p <- ggplot(top_params, aes(x = param_label, y = hq, fill = exceeds_standard, text = hover_text)) +
    geom_col() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
    scale_fill_manual(
      values = c("TRUE" = "darkorange", "FALSE" = "steelblue"),
      labels = c("TRUE" = "Exceeds Standard", "FALSE" = "Below Standard"),
      name = NULL
    ) +
    coord_flip() +
    labs(
      title = title_text,
      subtitle = paste0("Media: ", media, if (fraction_applied) paste0(" (", fraction, ")")),
      x = "Parameter",
      y = paste(temporal_label, "+", spatial_label, "Hazard Quotient (HQ)")
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
          "Top 10 Parameters by ", temporal_label, " + ", spatial_label, " Hazard Quotient",
          "<br><sup>",
          "Media: ", media, if (fraction_applied) paste0(" (", fraction, ")"),
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
  
  # Fix legend labels
  for (i in seq_along(ply$x$data)) {
    current_name <- ply$x$data[[i]]$name
    
    if (!is.null(current_name)) {
      if (grepl("TRUE|Exceeds Standard", current_name, ignore.case = FALSE)) {
        ply$x$data[[i]]$name <- "Exceeds Standard"
      } else if (grepl("FALSE|Below Standard", current_name, ignore.case = FALSE)) {
        ply$x$data[[i]]$name <- "Below Standard"
      }
    }
  }
  
  return(ply)
}
