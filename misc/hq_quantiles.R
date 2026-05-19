#' We need HQ bins to help classify data for stakeholders:
#' 1. Keep all HQ<1, no need to update these using pct95
#' 2. Use station max dates to calculate date range, but calculate using the date (not year)
#' 3. Bin using aggregated station HQs rather than individual HQs, since we're grading station-level data
 
#### Get final bins ####
## check HQs for station-aggregated values (what we'd end up showing)
station_hqs = plot_top_hq_stations(bol_media_scored, 
                                   media = "water", 
                                   param = "all", 
                                   temporal_aggregation = "recent", 
                                   recent_range = 5,
                                   param_aggregation = "pct95", 
                                   all_stations = TRUE,
                                   return_data = TRUE) |>
  select(HQ) |>
  quantile(probs = seq(0,1,length.out=5), na.rm=TRUE) # c(8,17,30,46,128)

#### Determining method differences ####
# how many stations have HQ data?
bol_water_scored |> # 21
  filter(!is.na(HQ)) |>
  pull(station) |>
  unique() |>
  length()

# get a date range for all stations
bol_water_scored |>
  group_by(station) |>
  summarize(min_date = min(date, na.rm=TRUE),
            max_date = max(date, na.rm=TRUE),
            n_obs = n()) |>
  View()

# view how many samples were taken within the last 5 years at each station
min_date = max(bol_water_scored$date,na.rm=TRUE) - 5*365.25
jackson_method_station_data = bol_water_scored |>
  filter(date > min_date) |>
  group_by(station) |>
  summarize(min_date_last_5_years = min(date, na.rm=TRUE),
            max_date_last_5_years = max(date, na.rm=TRUE),
            n_obs_last_5_years = n()) |>
  right_join(bol_water_scored |> select(station) |> unique(), by="station") |>
  arrange(station)


# view how many samples were taken for the last 5 years of sampling at that location, taking last date sampled into account
nadav_method_station_data = bol_water_scored |>
  group_by(station) |>
  summarize(last_year = max(year, na.rm=TRUE),
            min_year = last_year - 5,
            max_date = max(date, na.rm=TRUE),
            n_measurements=n(),
            .groups = "drop") |>
  right_join(bol_water_scored, by="station") |>
  filter(year > min_year) |>
  group_by(station) |>
  summarize(min_date_5_station_years = min(date, na.rm=TRUE),
            max_date_5_station_years = max(date, na.rm=TRUE),
            n_obs_5_station_years = n()) |>
  right_join(bol_water_scored |> select(station) |> unique(), by="station") |>
  arrange(station)
View(nadav_method_station_data)

compare_year_filtering = cbind(jackson_method_station_data,
                               nadav_method_station_data) |>
  select(-1)|>
  select(station, min_date_last_5_years,min_date_5_station_years, max_date_last_5_years,max_date_5_station_years,n_obs_last_5_years,n_obs_5_station_years)
compare_year_filtering |>  View()

### how many stations do we have after aggregation?
# nadav's method - 21 (expected -- all stations have the last 5 years of their data)
station_hqs |>
  pull(station) |>
  unique() |>
  length()

# jackson's method -- 20 -- just last 5 years, Canutillos 3 has no data
jackson_station_hqs = prepare_water_quality_data(bol_media_scored, 
                                                 params = "all", 
                                                 fraction = NULL, 
                                                 date = max(bol_media_scored$date, na.rm=TRUE))
jackson_station_hqs |>
  pull(station) |>
  unique() |>
  length()
# what are the quantiles here?
quantile(jackson_station_hqs |> select(HQ), probs = seq(0,1,length.out=4), na.rm=TRUE)



### see HQ quantiles across individual data points
bol_water_scored |>
  filter(!is.na(HQ), HQ>1) |>
  select(HQ) |>
  ggplot(aes(x=HQ)) +
  geom_histogram()+
  scale_y_log10()

quantile(bol_water_scored |> filter(HQ>1) |> select(HQ), probs = seq(0,1,length.out=4), na.rm=TRUE)

water_hq_1 = bol_water_scored |> filter(HQ>1, !is.na(HQ)) |> select(HQ)