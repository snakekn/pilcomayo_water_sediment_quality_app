facet_plots = function(func, data) {
  params = data |>
    group_by(parameter) |>
    summarize(max_hq = max(hq), .groups = "drop") |>
    arrange(desc(max_hq)) |>
    slice_head(n=10) |>
    select(parameter)
  
  plots = map(
    .x = params,
    .f = func,
    data = data
  )
  
  wrap_plots(plots, ncol=3)
}

# get a set of parameters together
x = facet_plots(plot_top_hq_params, all_water_data)

library(tidyverse)
