# plot differences between calculation methods
plot_diffs = function(risk_nem, risk_pct95) {
  diff = risk_nem$HQ / risk_pct95$HQ
  diff_ord = diff[order(diff)]
  n = length(diff)
  
  # deal with labels
  labs <- c("Min", "1st Q", "Median", "Mean", "3rd Q", "Max")
  labs_ord = labs[order(diff)]
  
  tick_at <- floor(c(0, 0.25, 0.5, 0.75, 1) * (n - 1)) + 1
  tick_lab <- c("Min", "1st Q", "Median", "3rd Q", "Max")
  
  
  plot(diff_ord, type="b", xaxt="n", xlab="Summary Statistic", ylab="Diff HQ (Nemerow / Pct_95)", 
       main="Distribution of Differences between Nemerow Pollution Index and 95th Percentile", pch=19)
  axis(1, at=tick_at, labels=tick_lab, las=1)  # las=1 rotates if needed
  grid(nx=NA, ny=NULL)
}

view_stations_ranked = function(data, media, param, summarize_flag = FALSE, year = NULL) {
  if (!is.null(year)) {
    data = data |>
      filter(year == !!year)
  }
  
  data = data |> 
    filter(media == !!media, parameter == param) |>
    group_by(station) |>
    summarize(mean = mean(HQ, na.rm=TRUE),
              .groups = "drop") |>
    arrange(desc(mean)) |>
    mutate(flag = mean > 2)
  
  if(summarize_flag) {
    n_total = nrow(data)
    data = data |>
      summarize(n_over = sum(flag),
                pct_over = n_over/n_total)
  }
  data
}

# example: get_table_5(all_water_scored, "Arsenic")
get_table_5 = function(data, param) {
  table = data |> 
    filter(parameter == param) |>
    mutate(conc_norm = case_match(tolower(unit),
                            "mg/l" ~ concentration * 1000,
                            .default = concentration)) |>
    group_by(year) |>
    summarize(n = n(),
              n_over = sum(HQ > 1, na.rm=TRUE),
              pct_over = round(n_over/n*100,0),
              hq_max = round(max(HQ, na.rm=TRUE),0),
              station_max = station[which.max(HQ)],
              mean_conc = round(mean(conc_norm, na.rm=TRUE),1),
              max_conc = round(max(conc_norm, na.rm=TRUE),1)) |>
    kableExtra::kable() |> kableExtra::kable_styling()
  
  table
}

# example: get_table_8(all_sed_scored, "Arsenic")
get_table_8 = function(data, param) {
  table = data |> 
    filter(parameter == param) |>
    mutate(conc_norm = case_match(tolower(unit),
                                  "mg/l" ~ concentration * 1000,
                                  .default = concentration)) |>
    summarize(timeframe = "all years",
              n = n(),
              n_over = sum(HQ > 1, na.rm=TRUE),
              pct_over = round(n_over/n*100,0),
              hq_max = round(max(HQ, na.rm=TRUE),0),
              station_max = station[which.max(HQ)],
              mean = round(mean(conc_norm, na.rm=TRUE),1),
              unit = "",
              std = unique(std_info))
  
  # print(nrow(table))
  # View(table |> select(std) |> unique())
  
  recent = data |> 
    filter(parameter == param, year == 2024) |>
    mutate(conc_norm = case_match(tolower(unit),
                                  "mg/l" ~ concentration * 1000,
                                  .default = concentration)) |>
    summarize(timeframe = "recent year (2024)", 
              n = n(),
              n_over = sum(HQ > 1, na.rm=TRUE),
              pct_over = round(n_over/n*100,0),
              hq_max = round(max(HQ, na.rm=TRUE),0),
              station_max = station[which.max(HQ)],
              mean = round(mean(conc_norm, na.rm=TRUE),1),
              unit = "",
              std = unique(std_info))

  res = bind_rows(table, recent)
  View(res)
  return(res)
}

# get averages for concentrations, but the concentrations are in diff units
get_stats = function(data, param, goal = "HQ", year = NULL) {
  if (goal == "HQ") {
    if (!is.null(year)) {
      data = data |>
        filter(year == !!year)
    }
    data |> 
      filter(media == "water", 
             parameter == param) |> 
      summarize(mean = mean(HQ, na.rm=TRUE), 
                n_exceed = sum(HQ>1), 
                pct_exceed = n_exceed / n(), 
                n_total = n(), 
                n_under = sum(HQ<=1), .groups = "drop")
  } else if (goal == "conc") {
    total = nrow(data)
    data = data |>
      filter(media == "water",
             parameter == param)
    print(nrow(data))
    if (!is.null(year)) {
      data = data |>
        filter(year == !!year)
    }
    print(nrow(data))
    data |>
      group_by(unit) |>
      summarize(mean = mean(concentration, na.rm=TRUE), 
                n_exceed = sum(HQ>1), 
                pct_exceed = n_exceed / n(), 
                n_total = n(), 
                n_under = sum(HQ<=1),
                na_conc = sum(is.na(concentration)),
                .groups = "drop")
  }
}

# plot stations
plot_top_hq_stations(all_media_scored, 
                     media = "sediment", 
                     param = "Arsenic", 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95", 
                     all_stations = FALSE)


# any contaminants never HQ>1?
all_media_scored |>
  filter(HQ<1) |>
  select(parameter, station, HQ)

prep = all_water_scored  |> 
    group_by(station, parameter) |>
    mutate(conc_norm = case_match(tolower(unit),
                                  "mg/l" ~ concentration * 1000,
                                  .default = concentration)) |>
    summarize(timeframe = "all years",
              n = n(),
              n_over = sum(HQ > 1, na.rm=TRUE),
              pct_over = round(n_over/n*100,0),
              hq_max = round(max(HQ, na.rm=TRUE),0),
              station_max = station[which.max(HQ)],
              mean = round(mean(conc_norm, na.rm=TRUE),1),
              unit = "",
              std = unique(std_info))
by_station = prep |>
  group_by(parameter) |>
  summarize(params_over = sum(pct_over > 0),
            min_pct = min(pct_over, na.rm=TRUE),
            avg_pct = mean(pct_over, na.rm=TRUE),
            max_pct = max(pct_over, na.rm=TRUE))
  
  # print(nrow(table))
  # View(table |> select(std) |> unique())
  
  recent = data |> 
    filter(parameter == param, year == 2024) |>
    mutate(conc_norm = case_match(tolower(unit),
                                  "mg/l" ~ concentration * 1000,
                                  .default = concentration)) |>
    summarize(timeframe = "recent year (2024)", 
              n = n(),
              n_over = sum(HQ > 1, na.rm=TRUE),
              pct_over = round(n_over/n*100,0),
              hq_max = round(max(HQ, na.rm=TRUE),0),
              station_max = station[which.max(HQ)],
              mean = round(mean(conc_norm, na.rm=TRUE),1),
              unit = "",
              std = unique(std_info))
  
  res = bind_rows(table, recent)
  View(res)
  return(res)
}