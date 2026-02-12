# example: plot_top_hq_params(all_media_scored, media_type = "all", temporal_aggregation = "recent")
plot_top_hq_params <- function(data, 
                               media_type, 
                               fraction = "all", 
                               station = "all",
                               temporal_aggregation = "max", 
                               spatial_aggregation = "max",
                               decay_per_day = NULL,
                               return_data = FALSE,
                               all_params = FALSE,
                               recent_range = 0
                               ) {
  library(plotly)
  
  cat("[plot_top_hq_params] Params: ")
  params_callout <- list(
    media = media_type,
    fraction = fraction,
    station = station,
    temporal_aggregation = temporal_aggregation,
    spatial_aggregation = spatial_aggregation,
    decay_per_day = decay_per_day
  )
  message(print(params_callout))
  
  # Confirm strict_std exists, or make it
  if(!exists("strict_std")) {
    strict_std = set_strict_stds()
  }
  
  
  # Validate temporal_aggregation parameter
  valid_temporal_aggregations <- c("recent", "mean", "average", "max", "weighted")
  if (!temporal_aggregation %in% valid_temporal_aggregations) {
    stop(paste("Invalid temporal_aggregation. Choose from:", paste(valid_temporal_aggregations, collapse = ", ")))
  }
  
  # Standardize method names
  if (temporal_aggregation == "average") temporal_aggregation <- "mean"
  
  # Validate spatial_aggregation parameter
  valid_spatial_aggregations <- c("mean", "average", "median", "max", "pct95")
  if (!spatial_aggregation %in% valid_spatial_aggregations) {
    stop(paste("Invalid spatial_aggregation. Choose from:", paste(valid_spatial_aggregations, collapse = ", ")))
  }
  
  # Standardize method names
  if (spatial_aggregation == "average") spatial_aggregation <- "mean"
  
  # for easy usage
  df = data
  
  # Filter for station
  if(!station %in% c("All", "all", "All Stations")) {
    cat("\n[plot_top_hq_params] Station filtering\n")
    cat("Pre-filter for station: ", nrow(df))
    df = df |> filter(station == !!station)
    cat("Post-filter for station: ", nrow(df))
  } else {    
    cat("\n[plot_top_hq_params] Not filtering for station. Measurements: ", nrow(df), "\n") 
  }
  
  # Filter for media
  
  ## fix to avoid all media
  if(media_type != "all") {
    cat("\n[plot_top_hq_params] Filtering for media. Pre-filter measurements: ", nrow(df))
    df <- df |>
      filter(media == media_type)

    cat("\n[plot_top_hq_params] Post-filter measurements: ", nrow(df))
  } else {
    cat("\nNot filtering for media.")
  }
  
  # Only apply fraction filter for drinking water and non-pH parameters
  if (media_type != "sediment" && fraction != "all") {
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
    cat("\n[plot_top_hq_params] Filtering for fraction. Post-filter measurements: ", nrow(df))
  } else {
    cat("\nNot considering fraction")
  }

  # check if we filtered everything out
  if(nrow(df) == 0) {
    stop("No more measurements maintained after filtering. Please check your filters")
  } else {
    cat("\nFiltering left some measurements: ", nrow(df))
  }
  
  # View(df) # debugging

  # Get unique parameters in the filtered data
  params <- unique(df$parameter)
  
  # print(length(params))
  
  # Calculate HQ for each parameter
  param_hq_list <<- tibble()
  
  for (param in params) {
    # cat("\n [plot_top_hq_params] On Param: ", param)
    param_df <- df |> filter(parameter == param)
    # cat("\n Total measurements for the parameter: ", length(param_df))
    
    # Special handling for pH - filter to pH units only
    if (param == "pH") {
      param_df <- param_df |> filter(unit == "u")
    }
    
    # Retrieve standard for this parameter-media combination
    if(media_type != "all") {
      df = df |>
        filter(media == media_type)
    }
    
    param_stds <- strict_std |>
      filter(str_detect(.data$parameter, !!param))
    
    # Skip if no standard exists
    if (nrow(param_stds) == 0) {
      message(" -- Skipping because no standard exists")
      next
    } else {
      message(cat("\nStandards found: ", length(param_stds)))
      # message(head(param_stds))
    }
    
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
      
      # View(param_df)
      
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
      # param_df <- param_df |>
      #   mutate(hq = concentration / std_val)
      # 
      std_text <- paste0(round(param_std, 3), " ", display_unit)
      # print(std_text) # debugging
    }
    
    # Filter to only exceedances
    param_df_exceedances <- param_df |>
      filter(!is.na(HQ))
    
    cat("\n[plot_top_hq_params] ", param, " exceedance count: ", nrow(param_df_exceedances))
    
    # View(param_df_exceedances) # debugging
    
    # Skip if no exceedances
    if (nrow(param_df_exceedances) == 0) next
    
    # STEP 1: TEMPORAL AGGREGATION by station
    # message(paste("Processing", param, "- Temporal aggregation:", temporal_aggregation))
    
    if (temporal_aggregation == "recent") {
      # get a range
      if(recent_range != 0) {
        station_temporal = param_df_exceedances |>
          group_by(station) |>
          summarize(last_year = max(year, na.rm=TRUE),
                 min_year = last_year - recent_range,
                 .groups = "drop") |>
          right_join(param_df_exceedances, by="station") |>
          filter(year > min_year) |>
          select(station, HQ, date) |>
          rename(hq = HQ)
        
        temporal_label = sprintf("Last %d years", recent_range)
        
      } else {
        # Get most recent measurement for each station
        station_temporal <- param_df_exceedances |>
          group_by(station) |>
          arrange(desc(date)) |>
          slice(1) |>
          ungroup() |>
          select(station, HQ, date) |>
          rename(hq = HQ)
        
        temporal_label <- "Most Recent Date Sampled at Station"
      }
      
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
          hq = weighted.mean(HQ, w = weight, na.rm = TRUE),
          date = date[which.max(hq)],
          .groups = "drop"
        )
      
      temporal_label <- "Weighted Average Across Years"
      
    } else if (temporal_aggregation == "max") {
      # Maximum HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(HQ)],
          hq = max(HQ, na.rm = TRUE),
          .groups = "drop"
        )
      
      # View(station_temporal) # debugging
      
      temporal_label <- "Maximum Across Years"
      
    } else {  # temporal_aggregation == "mean"

      # Mean HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(HQ)],
          hq = mean(HQ, na.rm = TRUE),
          .groups = "drop"
        )
      
      temporal_label <- "Average Across Years"
    }
    
    # View(station_temporal)
    
    # STEP 2: SPATIAL AGGREGATION across all stations
    if (spatial_aggregation == "max") {
      # View(station_temporal) # debugging
      
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
      
    } else if (spatial_aggregation == "mean") {  # spatial_aggregation == "mean"
      param_summary <- station_temporal |>
        summarise(
          hq = mean(hq, na.rm = TRUE),
          n_stations = n(),
          max_station = NA_character_,  # Not applicable for mean
          max_date = as.Date(NA),  # Not applicable for mean
          .groups = "drop"
        )
      spatial_label <- "Average"
      
    } else if (spatial_aggregation == "pct95") {
      param_summary = station_temporal |>
        summarise(
          hq = quantile(hq, probs = 0.95, na.rm=TRUE),
          n_stations = n(),
          max_station = NA_character_,  # Not applicable for mean
          max_date = as.Date(NA),  # Not applicable for mean
          .groups = "drop"
        )
      spatial_label = "95th Percentile"
    }
    
    spatial_label = paste(spatial_label, "Across Stations")
    
    # Add parameter info
    param_summary <- param_summary |>
      mutate(
        parameter = param,
        standard = std_text,
        std_source = std_source,
        n_measurements = nrow(param_df_exceedances)
      )
    
    # View(param_summary) ## debugging
    
    # param_hq_list[[param]] <<- param_summary
    # message(param_hq_list[[param]])
    # 
    param_hq_list <- bind_rows(
      param_hq_list,
      param_summary |> mutate(parameter = param)
    )
    # print(param)
    # print(param_summary)
  }
  
  # View(param_hq_list)

  # Filter by exceedances
  df = df |>
    filter(HQ > 1)
  
  # Combine all parameter summaries
  if (nrow(df) == 0) {
    stop(paste("No exceedances found for any parameters in", media_type))
  }
  
  # only selects one std max. Is an issue if we want to show all media and the different stds for each type
  std_lookup = df |>
    distinct(parameter, std_info) |>
    mutate(std_reg = map_chr(std_info, ~.x$HQ$std_reg),
           std_val = map_chr(std_info, ~ as.character(.x$HQ$std_val)),
           std_unit = map_chr(std_info, ~.x$HQ$std_unit)) |>
    distinct(parameter, std_reg, std_val, std_unit) |>  # one row per parameter
    group_by(parameter) |>
    slice_min(as.numeric(std_val), n=1)
    
  # param_summary = df |>
  #   group_by(parameter) |>
  #   summarize(
  #     hq = case_when(method == "max" ~ max(HQ, na.rm = TRUE),
  #                    method %in% c("mean", "average") ~ mean(HQ, na.rm = TRUE),
  #                    method == "median" ~ median(HQ, na.rm = TRUE)
  #                    ),
  #     n_measurements = n(),
  #     n_stations = n_distinct(station),
  #     .groups = "drop"
  #   ) |>
  #   left_join(std_lookup, by = "parameter")

  # Debugging: find hq-problematic params
  # problem_tib <- map_lgl(param_hq_list, ~ !"hq" %in% names(.x) || nrow(.x)==0)
  # if(any(problem_tib)) {
  #   cat("Problem params:", names(param_hq_list)[problem_tib], "\n")
  #   print(param_hq_list[problem_tib][[1]])
  # }

  # View(param_hq_list)
  
  top_params <- param_hq_list |>
    arrange(desc(.data$hq)) |>  # .data$hq forces proper scoping
    mutate(param_label = paste0(parameter, " (n=", n_measurements, ")"))
  
  if(!all_params) {
    top_params = top_params |>
      slice_head(n = 10)
  }
  
  if(return_data) {
    message("Returning data only")
    return(top_params)
  }
  
  # 
  # # Get top 10 parameters by HQ
  # top_params <- bind_rows(param_hq_list, .id = "param_name") |>  # Direct bind_rows [file:1]
  #   mutate(hq = coalesce(hq, 0)) |>  # GUARANTEE hq exists
  #   arrange(desc(hq)) |>
  #   slice_head(n = 10) |>
  #   mutate(param_label = paste0(param_name, " (n=", n_measurements, ")"))
  # 
  # View(top_params)
  
  top_params = top_params |>
    mutate(
      param_label = factor(param_label, levels=rev(unique(param_label))),
      #exceeds_standard = hq >= 1,
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
        "Standard: ", trim_zeros(standard), " (", std_source, ")<br>",
        "# Stations: ", n_stations, "<br>",
        "# Measurements: ", n_measurements
      )
    )

  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media_type == "water" && fraction != "all")

  # Create title based on aggregation methods used
  title_text <- paste(
    "Top 10 Parameters (ranked using Hazard Quotient)")
  use_log = (ceiling(log10(max(top_params$hq))) - floor(log10(min(top_params$hq)))) >= 2
  # print(use_log)
  # print(max(top_params$hq))
  # print(min(top_params$hq))
  
  
  # method_label <- if (method == "max") {
  #   "Maximum"
  # } else if (method == "median") {
  #   "Median"
  # } else {
  #   "Average"
  # }

  # y_lab <- paste(
  #   method_label,
  #   if (use_log) "Hazard Quotient (HQ, log10 scale)"
  #   else         "Hazard Quotient (HQ)"
  # )
  
  # main_title <- paste(
  #   "Top 10 Parameters by",
  #   method_label,
  #   if (use_log) "Hazard Quotient (log10)" else "Hazard Quotient"
  # )
  
  ## Nadav's Notes: Would be helpful to trigger log10 if the magnitude difference is high
  # Create bar chart with hover text
  p <- ggplot(top_params, aes(x = param_label, y = hq, text = hover_text)) +
    geom_col() +
    geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
    # scale_fill_manual(
    #   values = c("TRUE" = "darkorange", "FALSE" = "steelblue"),
    #   labels = c("TRUE" = "Exceeds Standard", "FALSE" = "Below Standard"),
    #   name = NULL
    # ) +
    coord_flip() +
    labs(
      title = title_text,
      x = "Parameter",
      y = paste(if (use_log) {
                  "Hazard Quotient (logarithmic scale)"
                  } else {
                    "Hazard Quotient"
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
  
  p = p + theme(
    axis.text.x = element_text(angle = -45, hjust = 0)  # angle HQ ticks
  )

   
  media_label = case_match(media_type,
                          "all" ~ "All Media",
                          "sediment" ~ "Sediment",
                          "water" ~ "Water",
                          .default = "None Selected") 
  # Convert to plotly for interactive hover
  ply <- ggplotly(p, tooltip = "text")
  
  ply <- ply |>
    layout(
      title = list(
        text = paste0(
          "Top 10 Parameters (Ranked using Hazard Quotient)",
          "<br><sup>",
        "Media: ", media_label, if (fraction_applied) paste0(" (", fraction, ")"), ". ",
        temporal_label, " & ", spatial_label,
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
