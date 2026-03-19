# example: plot_top_hq_params(all_media_scored, media_type = "all", temporal_aggregation = "recent")
plot_top_hq_params <- function(data, 
                               media_type, 
                               fraction = "all", 
                               station = "all",
                               temporal_aggregation = "recent", 
                               spatial_aggregation = "pct95",
                               decay_per_day = NULL,
                               return_data = FALSE,
                               all_params = FALSE,
                               recent_range = 0,
                               num_output = 10,
                               ggplot_output = FALSE,
                               graph_type = "boxplot"
                               ) {
  library(plotly) # lazy coding :P
  sys.source(here("R", "set_strict_stds.R"), envir = globalenv()) # set_strict_stds()
  
  
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
  
    #### filter out HQ NAs ####
    # Filter to only exceedances
    param_df_exceedances <- df |>
      filter(!is.na(HQ))
    
    # View(param_df_exceedances) # debugging
    
    # Skip if no exceedances
    if (nrow(param_df_exceedances) == 0) next
    
    #### STEP 1: TEMPORAL filtering ####
    # message(paste("Processing", param, "- Temporal aggregation:", temporal_aggregation))
    
    if (temporal_aggregation == "recent") {
      station_temporal = temporal_filtering(param_df_exceedances, nyears=recent_range) # outsource!
      
      temporal_label = sprintf("Last %d years", recent_range)
      
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
          HQ = weighted.mean(HQ, w = weight, na.rm = TRUE),
          date = date[which.max(HQ)],
          .groups = "drop"
        )
      
      temporal_label <- "Weighted Average Across Years"
    
      } else if (temporal_aggregation == "max") {
      # Maximum HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(HQ)],
          HQ = max(HQ, na.rm = TRUE),
          .groups = "drop"
        )
      
      temporal_label <- "Maximum Across Years"
      
    
      } else {  # temporal_aggregation == "mean"

      # Mean HQ across all time points for each station
      station_temporal <- param_df_exceedances |>
        group_by(station) |>
        summarise(
          date = date[which.max(HQ)],
          HQ = mean(HQ, na.rm = TRUE),
          .groups = "drop"
        )
      
      temporal_label <- "Average Across Years"
    }
    
    #### STEP 2: param aggregation ####
    param_summary = station_temporal |>
      group_by(parameter) |>
      summarize(HQ = case_match(spatial_aggregation,
                                "max" ~ max(HQ, na.rm = TRUE),
                                "median" ~ median(HQ, na.rm=TRUE),
                                "mean" ~ mean(HQ, na.rm=TRUE),
                                "pct95" ~ quantile(HQ, probs=0.95, na.rm=TRUE)),
                max_station = station[which.max(HQ)],
                max_date = date[which.max(HQ)],
                n_samples = n(),
                .groups="drop")
    
    spatial_label = case_match(spatial_aggregation,
                               "max" ~ "Maximum",
                               "median" ~ "Median",
                               "mean" ~ "Mean",
                               "pct95" ~ "95th Percentile",
                               .default = "Not Caught!")
    
    spatial_label = paste(spatial_label, "Across Stations")
    
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
    
  top_params <- param_summary |>
    arrange(desc(HQ)) |>  # .data$hq forces proper scoping
    mutate(param_label = sprintf("%s (n=%d)", parameter, n_samples))
  
  if(!all_params) {
    top_params = top_params |>
      slice_head(n = num_output)
  }
  
  if(return_data) {
    message("Returning data only")
    return(top_params)
  }
  
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
          paste0("Aggregated HQ: ", round(HQ, 3), "<br>")
        } else {
          paste0("HQ: ", round(HQ, 3), "<br>")
        },
        # "Standard: ", trim_zeros(standard), " (", std_source, ")<br>",
        "# Samples: ", n_samples 
        # "# Measurements: ", n_measurements
      )
    )

  # Determine if fraction was applied (for title labeling)
  fraction_applied <- (media_type == "water" && fraction != "all")

  # Create title based on aggregation methods used
  title_text <- sprintf("Top %d Parameters (Data from %s, %s HQ)", nrow(top_params), temporal_label, spatial_label)
  use_log = (ceiling(log10(max(top_params$HQ))) - floor(log10(min(top_params$HQ)))) >= 2
  
  # Final plotting section - REPLACED WITH THIS:
  media_label = case_match(media_type, "all" ~ "All Media", "sediment" ~ "Sediment", "water" ~ "Water", .default = "None Selected") 
  
  if (graph_type == "boxplot") {
    # Boxplot branch - per-parameter station distributions
    params_included <- top_params$parameter
    
    # Use station_temporal (or param_hq_list if you fixed the loop)
    box_data <- station_temporal |>
      filter(parameter %in% params_included) |>
      mutate(parameter = factor(parameter, levels = rev(params_included)))
    
    # Summary points (95th %ile)
    point_data <- top_params |>
      mutate(parameter = factor(parameter, levels = rev(params_included)))
    
    p <- ggplot(box_data, aes(x = parameter, y = HQ)) +
      geom_boxplot(outlier.shape = 16, outlier.size = 1.5, linewidth = 0.7,
                   fatten = 4, outlier.alpha = 0.5, fill = "white") +
      stat_summary(fun = median, geom = "point", color = "darkred", size = 1.5) +
      geom_point(data = point_data, aes(x = parameter, y = HQ),  # Fixed: parameter!
                 shape = 23, size = 3, color = "darkred", stroke = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
      coord_flip() +
      labs(title = title_text, x = "Parameter", 
           y = if(use_log) "Hazard Quotient (log scale)" else "Hazard Quotient") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
    
  } else {
    ## Nadav's Notes: Would be helpful to trigger log10 if the magnitude difference is high
    # Create bar chart with hover text
    p <- ggplot(top_params, aes(x = param_label, y = HQ, text = hover_text)) +
      geom_col() +
      geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
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
  }
  
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
  
  if (ggplot_output) { # easy copy-paste without plotly
    return(p)
  }
  
  # Convert to plotly for interactive hover
  ply <- ggplotly(p, tooltip = "text")
  
  ply <- ply |>
    layout(
      title = list(
        text = paste0(
          sprintf("Top %d Parameters (Ranked using Hazard Quotient)",num_output),
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
