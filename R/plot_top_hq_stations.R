# example: plot_top_hq_stations(all_media_scored, media = "water", param = "Arsenic", temporal_aggregation = "recent", param_aggregation = "pct95")
plot_top_hq_stations <- function(data, media, param, fraction = "Total", 
                                 temporal_aggregation = "max",  # RENAMED from method
                                 param_aggregation = NULL,
                                 decay_per_day = NULL,   # NEW: for weighted temporal aggregation 
                                 all_stations = FALSE,
                                 return_data_only = FALSE,
                                 recent_range = 0) {
  cat("\n[plot_top_hq_stations]: Values: ", media, " - ", param, " - ", fraction, " - ", temporal_aggregation, " - ", param_aggregation, "\n")
  
  # Validate temporal_aggregation parameter
  valid_temporal_aggregations <- c("recent", "mean", "average", "max", "weighted", "nemerow")
  if (!temporal_aggregation %in% valid_temporal_aggregations) {
    stop(paste("Invalid temporal_aggregation. Choose from:", paste(valid_temporal_aggregations, collapse = ", ")))
  }
  
  # Standardize method names
  if (temporal_aggregation == "average") temporal_aggregation <- "mean"
  
  # Validate param_aggregation if provided
  if (!is.null(param_aggregation)) {
    valid_param_aggregations <- c("mean", "median", "max", "sum", "pct95")
    if (!param_aggregation %in% valid_param_aggregations) {
      stop(paste("Invalid param_aggregation. Choose from:", paste(valid_param_aggregations, collapse = ", ")))
    }
  }
  
  # Handle "all" parameter - use all parameters in dataset
  if (length(param) == 1 && tolower(param) == "all") {
    message("Using ALL parameters in dataset...")
  } else {
    message("Filtering for parameters: ")
    message(param)
    message("Total rows pre-param filtering: ", nrow(data))
    data = data |>
      filter(parameter %in% param)
    message("Total rows post-param filtering: ", nrow(data))
  }
  
    # Filter for media only
    if(media != "all") {
      df <- data |>
        filter(media == !!media)
      message("Filtered for media: ", media)
    } else {
      df = data
    }
    
    # Get list of unique parameters for reporting
    all_parameters <- unique(df$parameter)
    message(paste("Found", length(all_parameters), "unique parameters in dataset"))

    
    all_parameters <- param
    param_display <- if (param == "all") "All Parameters" else if (length(param) > 1) paste(length(param), "Parameters") else param 
    

  # Filter for media and parameter
  if (media != "all") { # only filter on media if we're not using them all
    data = data |> filter(media == !!media) # update data
    cat("\n\nafter filtering for media, nrow(df) = ", nrow(df), "\n")
  }
    
  # Only apply fraction filter for parameters that actually have fractions
  # Skip for pH and other field parameters
  # Only do this step for water data (sediment is not broken into fractions for any parameters)
  if (media == "water") {
    # Skip fraction filtering if using "all" or if param is pH
    if (!(length(param) == 1 && tolower(param) == "all") && 
        !(length(param) == 1 && param == "pH") && 
        any(data$fraction == fraction)) {
      df <- df |>
        filter(is.na(fraction) | fraction == !!fraction)
    }
  }
  if(nrow(df) == 0) stop(paste("No data found using the current filters. Please update your filters."))
  
  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media == "water" && 
                         !(length(param) == 1 && tolower(param) == "all") &&
                         !(length(param) == 1 && param == "pH") && 
                         any(data$fraction == fraction))

  # Special handling for pH - filter to pH units only
  if (length(param) == 1 && param == "pH") {
    df <- df |>
      filter(unit == "u")
  }
  
  # Retrieve standard for this parameter-media combination (for display purposes)
  # Skip standard lookup if using "all" parameters
  if (length(param) == 1 && tolower(param) == "all") {
    has_standard <- FALSE
    std_text <- "Various"
    std_source <- "Multiple"
  } else {
    param_stds <- strict_std |>
      filter(.data$media == !!media,
             str_detect(.data$parameter, !!param[1]))  # Use first param for standard lookup
    
    # Check if standard exists
    has_standard <- nrow(param_stds) > 0
    
    if (!has_standard) {
      warning(paste("No standard found for", param[1], "in", media, "- proceeding with pre-calculated HQ values"))
      std_text <- "Pre-calculated"
      std_source <- "Various"
    } else {
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
        std_source <- low_std$regulator[1]
        
        # Handle unit display
        if (length(param) == 1 && param == "pH") {
          display_unit <- "pH units"
        } else {
          display_unit <- std_unit
        }
        
        std_text <- paste0(round(param_std_low, 3), " - ", round(param_std_high, 3), " ", display_unit)
        
      } else {
        # Handle single-threshold standards
        param_std <- param_stds$value[1]
        std_unit <- param_stds$unit[1]
        std_source <- param_stds$regulator[1]
        
        # Handle unit display
        if (length(param) == 1 && param == "pH") {
          display_unit <- "pH units"
        } else {
          display_unit <- std_unit
        }
        
        std_text <- paste0(round(param_std, 3), " ", display_unit)
      }
    }
  }
  
  # Filter to only exceedances
  cat("\nBefore filtering for hq, nrow(df) = ", nrow(df), "\n")

  df_exceedances <- df |>
    filter(!is.na(HQ), HQ > 1) # remove where HQ is acceptable

  cat("\nAfter filtering for hq>1, nrow(df) = ", nrow(df), "\n")
  
  # View(df_exceedances)
  
  # Check if there are any exceedances
  if (nrow(df_exceedances) == 0) {
    stop(paste("No exceedances found for", 
               ifelse(length(param) == 1 && tolower(param) == "all", 
                      "all parameters", 
                      paste(param, collapse = ", ")), 
               "in", media, "- all HQ values are NA or zero"))
  }
  
  # TEMPORAL AGGREGATION by station using specified method
  message(paste("Temporal aggregation method:", temporal_aggregation))
  
  if (temporal_aggregation == "recent") {
    # Get most recent measurement for each station
    message("Getting most recent measurement for each station...")
    
    if(recent_range != 0) {
      station_hq = df_exceedances |>
        group_by(station) |>
        summarize(last_year = max(year, na.rm=TRUE),
                  min_year = last_year - recent_range,
                  max_date = max(date, na.rm=TRUE),
                  n_measurements=n(),
                  .groups = "drop") |>
        right_join(df_exceedances, by="station") |>
        filter(year > min_year) |>
        select(station, parameter, HQ, date, max_date, n_measurements)
        
      method_label = sprintf("Last %d year%s", recent_range, if_else(recent_range>1, "s", ""))
      
    } else { # do it like we did before
      station_hq <- df_exceedances |>
        group_by(station) |>
        arrange(desc(date)) |>
        slice(1) |>
        ungroup() |>
        mutate(
          max_date = date,
          n_measurements = 1
        ) |>
        select(station, HQ, max_date, n_measurements)
      
      method_label <- "Most Recent"
    }
    
  } else if (temporal_aggregation == "weighted") {
    # Weighted average with more recent observations weighted higher
    message("Calculating weighted average with recency weighting...")
    
    # Get target date (most recent date in dataset)
    target_date <- max(df_exceedances$date, na.rm = TRUE)
    
    # Set default decay if not provided
    if (is.null(decay_per_day)) {
      decay_per_day <- 0.001  # gentle decay
      message("Using default decay: 0.001 per day")
    }
    
    station_hq <- df_exceedances |>
      group_by(station) |>
      mutate(
        days_ago = as.numeric(target_date - date),
        weight = exp(-decay_per_day * days_ago)
      ) |>
      summarise(
        max_date = date[which.max(HQ)],  # Date of maximum HQ for reference
        HQ = weighted.mean(HQ, w = weight, na.rm = TRUE),
        n_measurements = n(),
        effective_n = sum(weight),
        .groups = "drop"
      )
    
    method_label <- "Weighted Average"
    message(paste("Weighted average with decay =", decay_per_day))
    
  } else if (temporal_aggregation == "max") {
    # Maximum HQ across all time points
    station_hq <- df_exceedances |>
      group_by(station) |>
      summarise(
        max_date = date[which.max(HQ)],  # Get date BEFORE HQ is aggregated
        HQ = max(HQ, na.rm = TRUE),
        n_measurements = n(),
        .groups = "drop"
      )
    method_label <- "Maximum"
    
  } else if (temporal_aggregation == "mean") {  # temporal_aggregation == "mean"
    # Mean HQ across all time points
    station_hq <- df_exceedances |>
      group_by(station) |>
      summarise(
        max_date = date[which.max(HQ)],  # Get date of max even for mean method
        HQ = mean(HQ, na.rm = TRUE),
        n_measurements = n(),
        .groups = "drop"
      )
    method_label <- "Average"
  } else if (temporal_aggregation == "nemerow") {
    station_hq <- df_exceedances |>
      group_by(station) |>
      summarise(
        max_date = date[which.max(HQ)],  # Get date of max even for mean method
        HQ = sqrt((max(HQ, na.rm = TRUE)^2+mean(HQ, na.rm = TRUE)^2)/2),
        n_measurements = n(),
        .groups = "drop"
      )
    method_label <- "Pollution Index"
  }
  
  # View(station_hq)
  
  # PARAMETER AGGREGATION (if specified)
  if (!is.null(param_aggregation)) {
    message(paste("Aggregating across parameters using", param_aggregation, "method..."))
    
    station_hq <- station_hq |>
      group_by(station) |>  # Group by station AND date to preserve temporal info
      summarise(
        HQ = switch(param_aggregation,
                    "mean" = mean(HQ, na.rm = TRUE),
                    "median" = median(HQ, na.rm = TRUE),
                    "max" = max(HQ, na.rm = TRUE),
                    "sum" = sum(HQ, na.rm = TRUE),
                    "pct95" = quantile(HQ, probs = 0.95, na.rm=TRUE)),
        
        n_parameters = n(),
        n_measurements = sum(n_measurements),
        parameters = paste(unique(parameter), collapse = ", "),
        .groups = "drop"
      )
    
    message(paste("Aggregated", unique(df_exceedances$n_parameters), "parameters per station-date"))
  }
  
  # Get top 10 stations by HQ
  if(temporal_aggregation != "recent") {
    
    top_stations <- station_hq |>
      arrange(desc(HQ)) |>
      mutate(
        station_label = paste0(station, "(n=", n_measurements, ")"),
        station_label = factor(station_label, levels = rev(station_label)),
        exceeds_standard = HQ >= 1,
        hover_text = paste0(
          "Station: ", station, "<br>",
          if (temporal_aggregation %in% c("max", "recent")) paste0("Date: ", max_date, "<br>"),
          # Show parameter aggregation method if used
          if (!is.null(param_aggregation)) {
            paste0("Parameter aggregation: ", str_to_title(param_aggregation), "<br>")
          },
          # Show temporal aggregation method only if actual aggregation occurred (not "recent")
          if (temporal_aggregation != "recent") {
            paste0("Temporal aggregation: ", method_label, "<br>")
          },
          # HQ label - use "Aggregated HQ" if any aggregation occurred
          if (!is.null(param_aggregation) || temporal_aggregation != "recent") {
            paste0("Aggregated HQ: ", round(HQ, 3), "<br>")
          } else {
            paste0("HQ: ", round(HQ, 3), "<br>")
          }
        )
      )
  } else {
    
    top_stations <- station_hq |>
      arrange(desc(HQ)) |>
      mutate(
        station_label = paste0(station),
        station_label = factor(station_label, levels = rev(unique(station_label))),
        exceeds_standard = HQ >= 1,
        hover_text = paste0(
          "Station: ", station, "<br>",
          if (temporal_aggregation %in% c("max", "recent") && recent_range==0) paste0("Date: ", max_date, "<br>"),
          # Show parameter aggregation method if used
          if (!is.null(param_aggregation)) {
            paste0("Parameter aggregation: ", str_to_title(param_aggregation), "<br>")
          },
          # Show temporal aggregation method only if actual aggregation occurred (not "recent")
          if (temporal_aggregation != "recent") {
            paste0("Temporal aggregation: ", method_label, "<br>")
          },
          # HQ label - use "Aggregated HQ" if any aggregation occurred
          if (!is.null(param_aggregation) || temporal_aggregation != "recent") {
            paste0("Aggregated HQ: ", round(HQ, 3), "<br>")
          } else {
            paste0("HQ: ", round(HQ, 3), "<br>")
          },
          paste0("Total # observations: ", n_measurements, "<br>")
        )
      )
  }
  
  
  if (!all_stations) { # options are all or 10 baby
    top_stations = top_stations |> slice_head(n = 10)
  }
  
  if(return_data_only) {
    return(top_stations)
  }
  
  # debugging
  # View(top_stations)
  
  param_aggregation_title = case_match(param_aggregation,
                                   "pct95" ~ "95th Percentile",
                                   .default = param_aggregation)
  print(param_aggregation_title)
  
  # Create title based on whether param_aggregation was used
  if (!is.null(param_aggregation)) {
    title_text <- paste(
      "All Stations Ranked by", method_label, "Hazard Quotient:",
      paste0(str_to_title(param_aggregation_title), " of ", param_display)
    )
  } else {
    title_text <- paste(
      "All Stations Stations by", method_label, "Hazard Quotient:",
      if (fraction_applied) paste(param_display, " (", fraction, ")", sep = "") else param_display
    )
  }
  
  use_log = (ceiling(log10(max(top_stations$HQ))) - floor(log10(min(top_stations$HQ)))) >= 2
  
  
  # Create bar chart with hover text
  p <- ggplot(top_stations, aes(x = station_label, y = HQ, fill = exceeds_standard, text = hover_text)) +
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
      subtitle = paste0("Media: ", media, "\nStandard: ", std_text, " (", std_source, ")"),
      x = "Station",
      y = paste(if (use_log) {
        paste(method_label, "Hazard Quotient (logarithmic scale)")
      } else {
        paste(method_label, "Hazard Quotient")
      })
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 10, color = "gray30"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  if (use_log) {
    p <- p +
      scale_y_log10(
        breaks = scales::breaks_log(10),
        labels = scales::label_number(accuracy = 1, big.mark = ",")
      )
    
  } else {
    p <- p +
      scale_y_continuous(
        labels = scales::label_number(accuracy = 1, big.mark = ",")
      )
  }
  
  # Convert to plotly for interactive hover
  ply <- ggplotly(p, tooltip = "text")
  
  # Update layout with conditional title
  if (all_stations == TRUE) {
    if (!is.null(param_aggregation)) {
      plotly_title <- paste0(
        "All Stations Ranked by ", method_label, " Hazard Quotient: ",
        str_to_title(param_aggregation_title), " of ", param_display,
        "<br><sup>",
        "Media: ", media, " — Standard: ", std_text, " (", std_source, ")",
        "</sup>"
      )
    } else {
      plotly_title <- paste0(
        "All Stations Ranked by ", method_label, " Hazard Quotient: ",
        if (fraction_applied) paste(param_display, " (", fraction, ")", sep = "") else param_display,
        "<br><sup>",
        "Media: ", media, " — Standard: ", std_text, " (", std_source, ")",
        "</sup>"
      )
    }
  } else {
    if (!is.null(param_aggregation)) {
      plotly_title <- paste0(
        "Top 10 Stations Ranked by ", method_label, " Hazard Quotient: ",
        str_to_title(param_aggregation_title), " of ", param_display,
        "<br><sup>",
        "Media: ", media, " — Standard: ", std_text, " (", std_source, ")",
        "</sup>"
      )
    } else {
      plotly_title <- paste0(
        "Top 10 Stations Ranked by ", method_label, " Hazard Quotient: ",
        if (fraction_applied) paste(param_display, " (", fraction, ")", sep = "") else param_display,
        "<br><sup>",
        "Media: ", media, " — Standard: ", std_text, " (", std_source, ")",
        "</sup>"
      )
    }
  }
  

  ply <- ply |>
    layout(
      title = list(text = plotly_title),
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
