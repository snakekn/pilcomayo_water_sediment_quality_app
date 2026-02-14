# Jackson ended up finishing this
## check HQs for stations
station_hqs = plot_top_hq_stations(all_media_scored, 
                                   media = "water", 
                                   param = "all", 
                                   temporal_aggregation = "recent", 
                                   param_aggregation = "pct95", return_data = TRUE)

quantile(param_hqs |> select(HQ), probs = seq(0,1,length.out=4), na.rm=TRUE)



# see HQ quantiles across individual data points
bol_water_scored |>
  filter(!is.na(HQ), HQ>1) |>
  select(HQ) |>
  ggplot(aes(x=HQ)) +
  geom_histogram()+
  scale_y_log10()

quantile(bol_water_scored |> filter(HQ>1) |> select(HQ), probs = seq(0,1,length.out=4), na.rm=TRUE)

water_hq_1 = bol_water_scored |> filter(HQ>1, !is.na(HQ)) |> select(HQ)

jenks_obj = classInt::classIntervals(water_hq_1$HQ, n=6, style="jenks")
jenks_obj$brks

bol_water_scored |> 
  filter(!is.na(HQ)) |>
  mutate(
    q_bin = cut(
      concentration,
      breaks = quantile(concentration, probs = seq(0, 1, by = 0.16), na.rm = TRUE),
      include.lowest = TRUE
    )
  ) |>
  attr(q_bin, "breaks")

hq_min <- min(hq, na.rm = TRUE)
hq_max <- max(hq, na.rm = TRUE)

hq_water = bol_water_scored |> 
  filter(!is.na(HQ)) |>
  select(HQ)

above_brks <- quantile(hq_water[HQ>1], probs = seq(0, 1, length.out = 4), na.rm = TRUE)
