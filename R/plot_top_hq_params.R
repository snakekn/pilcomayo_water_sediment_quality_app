plot_top_hq_params <- function(data, media, fraction = "Total", method = "max") {
  
  # Validate method parameter
  if (!method %in% c("max", "mean", "average", "median")) {
    stop("method must be either 'max', 'mean', 'average', or 'median'")
  }
  
  # Standardize method name
  if (method == "average") method <- "mean"
  
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
  if (media == "drinking water") {
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
    
    # Aggregate HQ by parameter using specified method
    if (method == "max") {
      param_summary <- param_df_exceedances |>
        summarise(
          hq = max(hq, na.rm = TRUE),
          n_measurements = n(),
          n_stations = n_distinct(station),
          .groups = "drop"
        ) |>
        mutate(
          parameter = param,
          standard = std_text,
          std_source = std_source
        )
    } else if (method == "median") {
      param_summary <- param_df_exceedances |>
        summarise(
          hq = median(hq, na.rm = TRUE),
          n_measurements = n(),
          n_stations = n_distinct(station),
          .groups = "drop"
        ) |>
        mutate(
          parameter = param,
          standard = std_text,
          std_source = std_source
        )
    } else {
      param_summary <- param_df_exceedances |>
        summarise(
          hq = mean(hq, na.rm = TRUE),
          n_measurements = n(),
          n_stations = n_distinct(station),
          .groups = "drop"
        ) |>
        mutate(
          parameter = param,
          standard = std_text,
          std_source = std_source
        )
    }
    
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
        if (method == "mean") paste0(str_to_title(method), " "),
        if (method == "median") paste0(str_to_title(method), " "),
        "HQ: ", round(hq, 3), "<br>",
        "Standard: ", standard, " (", std_source, ")<br>",
        "# Stations: ", n_stations, "<br>",
        "# Measurements: ", n_measurements
      )
    )
  
  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media == "drinking water" && any(data$fraction == fraction))
  
  method_label <- if (method == "max") {
    "Maximum"
  } else if (method == "median") {
    "Median"
  } else {
    "Average"
  }
  
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
      title = paste(
        "Top 10 Parameters by", method_label, "Hazard Quotient",
        if (fraction_applied) paste0(" (", fraction, ")")
      ),
      subtitle = paste("Media:", media),
      x = "Parameter",
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
          "Top 10 Parameters by ", method_label, " Hazard Quotient",
          if (fraction_applied) paste0(" (", fraction, ")"),
          "<br><sup>",
          "Media: ", media,
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