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
  
  plot_df = data |>
    group_by(sieve_size) |>
    summarise(mean_value = mean(HQ, na.rm=TRUE),
              max_value = max(HQ, na.rm=TRUE),
              n_measurements = n(),
              n_stations = n_distinct(station)) |>
    mutate(value = case_when(method == "max" ~ max_value,
                             method == "avg" ~ mean_value)) |>
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
    ggplot(aes(x = reorder(sieve_size, value), 
               y = value,
               text = hover_text)) +
    geom_col(fill = "tan") +
    coord_flip() + 
    labs(title = paste0("Sieve Sizes Ranked by ", method_label, " Value: ", param_selection),
         x = NULL, y = paste0("Hazard Quotient (", method_label, ")")) +
    theme_minimal()
  
  ply = ggplotly(p, tooltip = "text")
  return(ply)
}
