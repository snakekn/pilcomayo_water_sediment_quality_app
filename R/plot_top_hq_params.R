plot_top_hq_params <- function(data, media_type, fraction = "all", method = "max", station = "All") {
  cat("[plot_top_hq_params] Params: ")
  params_callout <- list(
    media = media_type,
    fraction = fraction,
    method = method,
    station = station
  )
  print(params_callout)
  
  # Validate method parameter
  if (!method %in% c("max", "mean", "average", "median")) {
    stop("method must be either 'max', 'mean', 'average', or 'median'")
  }
  
  # Standardize method name
  if (method == "average") method <- "mean"
  
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
  if (media_type != "sed" && fraction != "all") {
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
  
  # Get unique parameters in the filtered data
  params <- unique(df$parameter)
  
  # Filter by exceedances
  df = df |>
    filter(HQ > 1)
  
  # Combine all parameter summaries
  if (nrow(df) == 0) {
    # Nadav's Notes: using media "all" isn't fixed in yet
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
    
  param_summary = df |>
    group_by(parameter) |>
    summarize(
      hq = case_when(method == "max" ~ max(HQ, na.rm = TRUE),
                     method %in% c("mean", "average") ~ mean(HQ, na.rm = TRUE),
                     method == "median" ~ median(HQ, na.rm = TRUE)
                     ),
      n_measurements = n(),
      n_stations = n_distinct(station),
      .groups = "drop"
    ) |>
    left_join(std_lookup, by = "parameter")

  # Get top 10 parameters by HQ
  top_params <- param_summary |>
    arrange(desc(hq)) |>
    slice_head(n = 10) |>
    mutate(
      param_label = paste0(parameter, " (n=", n_measurements, ")")
    )
  
  top_params = top_params |>
    mutate(
      param_label = factor(param_label, levels=rev(unique(param_label))),
      #exceeds_standard = hq >= 1,
      hover_text = paste0(
        "Parameter: ", parameter, "<br>",
        # if (method == "mean") paste0(str_to_title(method), " "),
        # if (method == "median") paste0(str_to_title(method), " "),
        # "HQ: ", round(HQ, 3), "<br>",
        "Standard: ", trim_zeros(std_val), " ", std_unit, " (", std_reg, ")<br>",
        "# Stations: ", n_stations, "<br>",
        "# Measurements: ", n_measurements
      )
    )
  use_log = (log10(max(top_params$hq)) - log10(min(top_params$hq))) >= 2

  method_label <- if (method == "max") {
    "Maximum"
  } else if (method == "median") {
    "Median"
  } else {
    "Average"
  }

  y_lab <- paste(
    method_label,
    if (use_log) "Hazard Quotient (HQ, log10 scale)"
    else         "Hazard Quotient (HQ)"
  )
  
  main_title <- paste(
    "Top 10 Parameters by",
    method_label,
    if (use_log) "Hazard Quotient (log10)" else "Hazard Quotient"
  )
  
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
      title = paste(
        "Top 10 Parameters by", method_label, "Hazard Quotient"
        # if (fraction_applied) paste0(" (", fraction, ")")
      ),
      subtitle = paste0("Media:", media_type, ". Method: ", method_label),
      x = "Parameter",
      y = y_lab
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

    
  # Convert to plotly for interactive hover
  ply <- ggplotly(p, tooltip = "text")
  
  ply <- ply |>
    layout(
      title = list(
        text = paste0(
          "Top 10 Parameters by ", method_label, " Hazard Quotient",
          # if (fraction_applied) paste0(" (", fraction, ")"),
          "<br><sup>",
          "Media: ", media_type, ". Method: ", method_label,
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

trim_zeros <- function(x) {
  s <- format(x, scientific = FALSE, trim = TRUE)  # e.g. "0.010000"
  sub("\\.?0+$", "", s)                            # -> "0.01"
}
