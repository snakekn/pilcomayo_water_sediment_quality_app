# example: plot_top_hq_sieve(all_sed_scored, "Lead", method = "mean")
plot_top_hq_sieve = function(data, param_selection = "all", method = "max", station_selection="all") {
  # Demonstrate what we're working with
  cat("\n[plot_top_hq_sieve] Params: ")
  params_callout <- list(
    method = method,
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
  
  cat("\n[plot_top_hq_sieve] Filtering for HQ>1: ", nrow(data))
  data = data |> 
    filter(HQ > 1)
  cat("\nAfter filtering: ", nrow(data))

  # check if we have no exceedances
  if(nrow(data) == 0) {
    stop("No measurements remaining to plot. Please check your filters.")
  }
  
  sieve_options = unique(data$sieve_size)
  parse_min_mm <- function(name) {
    # Grab FIRST decimal number anywhere in string (handles <0.125, 0.032 - 0.063, etc.)
    num_match <- str_extract(name, "\\d*\\.?\\d+")
    # if (is.na(num_match)) return(NA)
    as.numeric(num_match)
  }
  # sieve_sizes <- t(sapply(sieve_options, parse_min_mm))
  # sieve_sizes_num <- as.numeric(as.character(sieve_sizes))  # Coerces factors/handles safely
  
  # bins <- cut(sieve_sizes_num, breaks = c(0, 0.063, 0.125, 0.25, 0.5, 1, 2, Inf), 
              # labels = c("Very Fine (<0.063mm)", 
                         # "Fine Silt (0.063-0.125mm)", 
                         # "Medium-Coarse Silt (0.125-0.25mm)", 
                         # "Fine Sand (0.25-0.5mm)", 
                         # "Medium Sand (0.5-1.0mm)", 
                         # "Coarse Sand (1.0-2.0mm)", 
                         # ">2mm"), right = FALSE)
  
  plot_df = data |>
    rename(sieve_name = sieve_size) |>
    mutate(sieve_min = parse_min_mm(sieve_name),
           sieve_size = as.character(cut(sieve_min, 
                            breaks = c(0, 0.063, 0.125, 0.25, 0.5, 1, 2, Inf),
                            labels = c("Very Fine (<0.063mm)", 
                                       "Fine Silt (0.063-0.125mm)", 
                                       "Medium-Coarse Silt (0.125-0.25mm)", 
                                       "Fine Sand (0.25-0.5mm)", 
                                       "Medium Sand (0.5-1.0mm)", 
                                       "Coarse Sand (1.0-2.0mm)", 
                                       ">2mm"), right = FALSE)),
           .keep = "all") |>
    group_by(sieve_size) |>
    summarise(mean_value = mean(HQ, na.rm=TRUE),
              max_value = max(HQ, na.rm=TRUE),
              n_measurements = n(),
              n_stations = n_distinct(station)) |>
    mutate(value = case_when(method == "max" ~ max_value,
                             method == "avg" ~ mean_value),
           min_mm = parse_min_mm(sieve_size)) |>
    mutate(
      hover_text = paste0(
        "HQ: ", round(value, 3), "<br>",
        "# Measurements: ", n_measurements, "<br>",
        "# Stations: ", n_stations
      )
    )
  
  method_label = case_when(method == "max" ~ "Max",
                           method == "avg" ~ "Mean")
  
  p = plot_df |>
    ggplot(aes(x = reorder(sieve_size, min_mm), 
               y = value,
               text = hover_text)) +
    geom_col(fill = "tan") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "firebrick", linewidth = 1) +
    coord_flip() + 
    labs(title = paste0("Sieve Sizes Ranked by ", method_label, " Value: ", param_selection),
         x = "Sieve Size", y = paste0("Hazard Quotient (", method_label, ")")) +
    theme_minimal()
  
  ply = ggplotly(p, tooltip = "text")
  return(ply)
}
