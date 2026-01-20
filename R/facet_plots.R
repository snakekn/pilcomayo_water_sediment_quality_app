facet_plots = function(func, data, params) {
  params = data |>
    group_by(parameter) |>
    summarize(max_hq = max(HQ, rm.na=TRUE), .groups = "drop") |>
    arrange(desc(max_hq)) |>
    slice_head(n=30) |>
    select(parameter, max_hq)
  
  plots = purrr::map_dfr(
    .x = params,
    .f = func,
    data = data
  )
  
  wrap_plots(plots, ncol=3)
}

# get a set of parameters together
# x = facet_plots(plot_pilcomayo_ts, all_water_data, params)

# library(tidyverse)

