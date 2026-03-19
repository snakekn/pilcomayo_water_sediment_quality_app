# example: plot_top_hq_sieve(all_sed_scored, "Lead", param_aggregation = "mean")
plot_top_hq_sieve = function(data, 
                             param_selection = "all", 
                             param_aggregation = "pct95", 
                             station_selection="all", 
                             temporal_aggregation = "recent", 
                             recent_range = 5,
                             graph_type = "ranking"
                             ) {
  # Demonstrate what we're working with
  cat("\n[plot_top_hq_sieve] Params: ")
  params_callout <- list(
    param_aggregation = param_aggregation,
    station = station_selection,
    param = param_selection
  )
  print(params_callout)  
  
  if(station_selection != "all") {
    cat("\n[plot_top_hq_sieve] Filtering using station \"", station_selection, "\". Measurements: ", nrow(data))
    data = data |> 
      filter(station == station_selection)
    cat("\nAfter filtering: ", nrow(data))
  } else {
    cat("\nNot filtering on station")
  }
  
  if(param_selection != "all") {
    cat("\n[plot_top_hq_sieve] Filtering using parameter \"", param_selection, "\". Measurements: ", nrow(data))
    data = data |> 
      filter(parameter == param_selection)
    cat("\nAfter filtering: ", nrow(data))
  } else {
    cat("\nNot filtering on parameter")
  }

  # check if we have no exceedances
  if(nrow(data) == 0) {
    stop("No measurements remaining to plot. Please check your filters.")
  }
  
  if(temporal_aggregation == "recent") { 
    data = temporal_filtering(data)
  }
  
  sieve_map <- function(sieve_label) {
    case_match(sieve_label,
               # Very Fine Silt (<0.063mm)
               c("0.032 mm - N° 450 (ASTM)", "0.032 - 0.063 mm", "<0.063 mm") ~ "Very Fine (<0.063mm)",
               
               # Fine Silt (0.063-0.125mm)  
               c("0.063 mm - N° 230 (ASTM)", "0.063 - 0.125 mm", "0.063 - <0.125 mm", 
                 "0.032 - 0.125 mm", "<0.125 mm") ~ "Fine Silt (0.063-0.125mm)",
               
               # Medium-Coarse Silt (0.125-0.25mm)
               c("0.125 mm - N° 120 (ASTM)", "0.125 - 0.250 mm", "0.125 - <2.0 mm") ~ "Medium-Coarse Silt (0.125-0.25mm)",
               
               # Fine Sand (0.25-0.5mm)
               "0.250 mm - N° 060 (ASTM)" ~ "Fine Sand (0.25-0.5mm)",
               
               # Medium Sand (0.5-1.0mm)
               c("0.500 mm - N° 035 (ASTM)", "0.5 - 1.0 mm") ~ "Medium Sand (0.5-1.0mm)",
               
               # Coarse Sand (1.0-2.0mm)
               c("1.00 mm - N° 018 (ASTM)", "1.0 - 2.0 mm") ~ "Coarse Sand (1.0-2.0mm)",
               
               # Gravel (>2mm)
               c("2.00 mm - N° 010 (ASTM)", "<=2.00 mm", "0.125 - 4.75 mm", "0.063 - <2.0 mm") ~ ">2mm",
               
               # Broad ranges → conservative (finest end)
               "0.063 - 0.250 mm" ~ "Fine Silt (0.063-0.125mm)",
               
               NA_character_ ~ NA_character_,
               .default = NA_character_  # Catch leftovers
    )
  }
  
  sieve_options = unique(data$sieve_size)
  print(sieve_options)

  sieve_order = c("Very Fine (<0.063mm)", 
                  "Fine Silt (0.063-0.125mm)", 
                  "Medium-Coarse Silt (0.125-0.25mm)", 
                  "Fine Sand (0.25-0.5mm)", 
                  "Medium Sand (0.5-1.0mm)", 
                  "Coarse Sand (1.0-2.0mm)", 
                  ">2mm")
  
  data = data |> 
    mutate(sieve_size = factor(sieve_map(sieve_size), levels=sieve_order)) |>
    filter(!is.na(sieve_size))
  
  plot_df = data |>
    group_by(sieve_size) |>
    summarise(mean_value = mean(HQ, na.rm=TRUE),
              max_value = max(HQ, na.rm=TRUE),
              pct95_value = quantile(HQ, probs=0.95, na.rm=TRUE),
              n_measurements = n(),
              n_stations = n_distinct(station),
              .groups = "drop") |>
    mutate(value = case_when(param_aggregation == "max" ~ max_value,
                             param_aggregation == "avg" ~ mean_value,
                             param_aggregation == "pct95" ~ pct95_value,
                             TRUE ~ pct95_value),
           hover_text = paste0(
             "HQ: ", round(value, 3), "<br>",
             "# Measurements: ", n_measurements, "<br>",
             "# Stations: ", n_stations
           )) 
  
  param_aggregation_label = case_when(param_aggregation == "max" ~ "Max",
                           param_aggregation == "avg" ~ "Mean",
                           param_aggregation == "pct95" ~ "95th Percentile")
  
  use_log = (ceiling(log10(max(data$HQ))) - floor(log10(min(data$HQ)))) >= 2
  
  if(graph_type == "boxplot") {

    data_bp <- data

    title_text <- paste0(
      "Sieve Sizes: HQ Distributions (", param_aggregation_label, " Highlighted)",
      if (param_selection != "all") paste0(" — ", param_selection) else ""
    )
    
    p <- ggplot(data_bp, aes(x = sieve_size, y = HQ)) +
      geom_boxplot(
        outlier.shape = 16,
        outlier.size  = 1.5,
        linewidth     = 0.7,
        fatten        = 4,
        outlier.alpha = 0.5,
        fill          = "white"
      ) +
      stat_summary(
        fun  = median,
        geom = "point",
        color = "darkred",
        size  = 1.5
      ) +
      geom_point(
        data = plot_df,
        aes(x = sieve_size, y = value),
        inherit.aes = FALSE,
        shape = 23,          # diamond
        size  = 3,
        fill  = "tan",
        color = "darkred",
        stroke = 1
      ) +
      geom_hline(
        yintercept = 1,
        linetype   = "dashed",
        color      = "firebrick",
        linewidth  = 1
      ) +
      coord_flip() +
      labs(
        title = title_text,
        x     = "Sieve Size",
        y     = paste0("Hazard Quotient (", param_aggregation_label, ")")
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank()
      )
    
  } else {
    p = plot_df |>
      ggplot(aes(x = reorder(sieve_size, sieve_order), 
                 y = value,
                 text = hover_text)) +
      geom_col(fill = "tan") +
      geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
      coord_flip() + 
      labs(title = paste0("Sieve Sizes Ranked by ", param_aggregation_label, " Value: ", param_selection),
           x = "Sieve Size", y = paste0("Hazard Quotient (", param_aggregation_label, ")")) +
      theme_minimal()
  }
  
  if (use_log) {
    p <- p +
      scale_y_log10(
        breaks = scales::breaks_log(10),
        labels = scales::label_number(accuracy = 1, big.mark = ",")
      )
  }
  
  ply = ggplotly(p, tooltip = "text")
  return(ply)
}
