#### create normalized dataset for media to make it easier to run comparisons ####
# note: it runs rowwise, so it's pretty slow. Be patient - it works!
bol_media_norm = bol_media_scored |> normalize_units_wrapper()

#### review quantiles using station HQs ####
# temporarily save all data (including 2024) to a separate file
# updating base file so I don't have to update other files
bol_media_scored_allyears = bol_media_scored
bol_media_scored = bol_media_scored |>
  filter(year != 2024)

bol_sed_scored_allyears = bol_sed_scored
bol_sed_scored = bol_sed_scored |> 
  filter(year != 2024)

all_station_hq_water = plot_top_hq_stations(bol_media_scored, 
                                            media_type = "water", 
                                            param = "all", 
                                            temporal_aggregation = "recent", 
                                            param_aggregation = "pct95",
                                            ggplot_output = FALSE,
                                            recent_range = 5,
                                            return_data=TRUE,
                                            all_stations=TRUE)

all_station_hq_sed = plot_top_hq_stations(bol_media_scored, 
                                          media_type = "sediment", 
                                          param = "all", 
                                          temporal_aggregation = "recent", 
                                          param_aggregation = "pct95",
                                          ggplot_output = FALSE,
                                          recent_range = 5,
                                          return_data=TRUE,
                                          all_stations=TRUE)


quantile(all_station_hq_water$HQ, probs = seq(0,1,length.out=5), na.rm=TRUE)
quantile(all_station_hq_sed$HQ, probs = seq(0,1,length.out=5), na.rm=TRUE)

#### top lists ####
### Chemicals of highest concern - water
p1 = plot_top_hq_params(bol_media_scored, 
                        media_type = "water", 
                        fraction = "all", 
                        station = "all",
                        temporal_aggregation = "recent", 
                        spatial_aggregation = "pct95",
                        decay_per_day = NULL,
                        return_data = FALSE,
                        all_params = FALSE,
                        recent_range = 5,
                        graph_type = "boxplot",
                        ggplot_output=TRUE)
p1

### Chemicals of highest concern - sediment
p2 = plot_top_hq_params(bol_media_scored, 
                        media_type = "sediment", 
                        fraction = "all", 
                        station = "all",
                        temporal_aggregation = "recent", 
                        spatial_aggregation = "pct95",
                        decay_per_day = NULL,
                        return_data = FALSE,
                        all_params = FALSE,
                        recent_range = 5,
                        num_output = 7,
                        graph_type = "boxplot",
                        ggplot_output=TRUE)
p2

#### individual samples above 1000x HQ
params_1000 = bol_media_scored |> 
  filter(HQ>1000) |>
  select(parameter) |>
  unique()


####silver####
### station boxplot for silver
silver_stations_water_bp = plot_top_hq_stations(bol_media_scored, 
                                                media_type = "water", 
                                                param = "Silver", 
                                                temporal_aggregation = "recent", 
                                                param_aggregation = "pct95",
                                                ggplot_output = FALSE,
                                                recent_range = 5,
                                                graph_type = "boxplot")
silver_stations_water_bp

### Stations with highest silver - water (no sed HQ data here)
silver_stations_water = plot_top_hq_stations(bol_media_scored, 
                                             media_type = "water", 
                                             param = "Silver", 
                                             temporal_aggregation = "recent", 
                                             param_aggregation = "pct95",
                                             ggplot_output = FALSE,
                                             recent_range = 5)

silver_stations_water
####arsenic####
### Stations with highest arsenic - water
arsenic_stations_water = plot_top_hq_stations(bol_media_scored, 
                                              media_type = "water", 
                                              param = "Arsenic", 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              ggplot_output = FALSE,
                                              graph_type = "boxplot",
                                              recent_range = 5)

arsenic_stations_water

### Stations with highest arsenic - sed
arsenic_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                            media_type = "sediment", 
                                            param = "Arsenic", 
                                            temporal_aggregation = "recent", 
                                            param_aggregation = "pct95",
                                            ggplot_output = FALSE,
                                            graph_type = "boxplot",
                                            recent_range = 5)

arsenic_stations_sed

### arsenic sieve sizes
arsenic_sieve = plot_top_hq_sieve(bol_sed_scored, 
                                  param_selection = "Arsenic", 
                                  param_aggregation = "pct95", 
                                  station_selection="all", 
                                  temporal_aggregation = "recent",
                                  graph_type = "boxplot",
                                  recent_range = 5)
arsenic_sieve  


####copper####

### Stations - water
copper_stations_water = plot_top_hq_stations(bol_media_scored, 
                                             media_type = "water", 
                                             param = "Copper", 
                                             temporal_aggregation = "recent", 
                                             param_aggregation = "pct95",
                                             ggplot_output = FALSE,
                                             recent_range = 5)

copper_stations_water

### Stations with highest copper - sed
copper_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                           media_type = "sediment", 
                                           param = "Copper", 
                                           temporal_aggregation = "recent", 
                                           param_aggregation = "pct95",
                                           ggplot_output = FALSE,
                                           recent_range = 5)

copper_stations_sed

### copper sieve sizes
copper_sieve = plot_top_hq_sieve(bol_sed_scored, 
                                 param_selection = "Copper", 
                                 param_aggregation = "pct95", 
                                 station_selection="all", 
                                 temporal_aggregation = "recent", 
                                 recent_range = 5)
copper_sieve  

#### lead! ####
### Stations - water
lead_stations_water = plot_top_hq_stations(bol_media_scored, 
                                           media_type = "water", 
                                           param = "Lead", 
                                           temporal_aggregation = "recent", 
                                           param_aggregation = "pct95",
                                           ggplot_output = FALSE,
                                           recent_range = 5)

lead_stations_water

### Stations with highest arsenic - sed
lead_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                         media_type = "sediment", 
                                         param = "Lead", 
                                         temporal_aggregation = "recent", 
                                         param_aggregation = "pct95",
                                         ggplot_output = FALSE,
                                         recent_range = 5)

lead_stations_sed
lead_stations_sed_data = plot_top_hq_stations(bol_media_scored, 
                                              media_type = "sediment", 
                                              param = "Lead", 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              all_stations=TRUE,
                                              return_data=TRUE,
                                              ggplot_output = FALSE,
                                              recent_range = 5)
lead_stations_sed_data |> select(HQ) |> summary()


### arsenic sieve sizes
lead_sieve = plot_top_hq_sieve(bol_sed_scored, 
                               param_selection = "Lead", 
                               param_aggregation = "pct95", 
                               station_selection="all", 
                               temporal_aggregation = "recent", 
                               recent_range = 5)
lead_sieve 

#### cadmium ####
### Stations - water
cadmium_stations_water = plot_top_hq_stations(bol_media_scored, 
                                              media_type = "water", 
                                              param = "Cadmium", 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              ggplot_output = FALSE,
                                              recent_range = 5)

cadmium_stations_water

### Stations with highest arsenic - sed
cadmium_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                            media_type = "sediment", 
                                            param = "Cadmium", 
                                            temporal_aggregation = "recent", 
                                            param_aggregation = "pct95",
                                            ggplot_output = FALSE,
                                            recent_range = 5)

cadmium_stations_sed
cadmium_stations_sed_data = plot_top_hq_stations(bol_media_scored, 
                                                 media_type = "sediment", 
                                                 param = "Cadmium", 
                                                 temporal_aggregation = "recent", 
                                                 param_aggregation = "pct95",
                                                 all_stations=TRUE,
                                                 return_data=TRUE,
                                                 ggplot_output = FALSE,
                                                 recent_range = 5)
cadmium_stations_sed_data |> select(HQ) |> summary()


### arsenic sieve sizes
cadmium_sieve = plot_top_hq_sieve(bol_sed_scored, 
                                  param_selection = "Lead", 
                                  param_aggregation = "pct95", 
                                  station_selection="all", 
                                  temporal_aggregation = "recent", 
                                  recent_range = 5)
cadmium_sieve 
#### zinc ####
### Stations - water
zinc_stations_water = plot_top_hq_stations(bol_media_scored, 
                                           media_type = "water", 
                                           param = "Zinc", 
                                           temporal_aggregation = "recent", 
                                           param_aggregation = "pct95",
                                           ggplot_output = FALSE,
                                           recent_range = 5)

zinc_stations_water

### Stations with highest arsenic - sed
zinc_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                         media_type = "sediment", 
                                         param = "Zinc", 
                                         temporal_aggregation = "recent", 
                                         param_aggregation = "pct95",
                                         ggplot_output = FALSE,
                                         recent_range = 5)

zinc_stations_sed
zinc_stations_sed_data = plot_top_hq_stations(bol_media_scored, 
                                              media_type = "sediment", 
                                              param = "Zinc", 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              all_stations=TRUE,
                                              return_data=TRUE,
                                              ggplot_output = FALSE,
                                              recent_range = 5)
zinc_stations_sed_data |> select(HQ) |> summary()


### arsenic sieve sizes
zinc_sieve = plot_top_hq_sieve(bol_sed_scored, 
                               param_selection = "Zinc", 
                               param_aggregation = "pct95", 
                               station_selection="all", 
                               temporal_aggregation = "recent", 
                               recent_range = 5)
zinc_sieve 

#### build boxplots for stations on specific params ####
params_water <- c("Silver","Arsenic", "Iron", "Cadmium", "Lead", "Selenium", "Zinc", "Copper", "Mercury")
params_sed = c("Arsenic", "Mercury", "Zinc", "Cadmium", "Lead", "Copper", "Nickel", "Chromium")

plots_water = list()
for (p in params_water) {
  cat("\n---", p, "---\n")
  plt = plot_top_hq_stations(bol_media_scored, 
                             media_type = "water", 
                             param = p, 
                             temporal_aggregation = "recent", 
                             param_aggregation = "pct95",
                             ggplot_output = FALSE,
                             recent_range = 5,
                             graph_type = "boxplot")
  # print(plt)  # important: actually renders the plotly widget
  plots_water[[p]] = plt
}

plots_sed = list()
for (p in params_sed) {
  cat("\n---", p, "---\n")
  plt = plot_top_hq_stations(bol_media_scored, 
                             media_type = "sediment", 
                             param = p, 
                             temporal_aggregation = "recent", 
                             param_aggregation = "pct95",
                             ggplot_output = FALSE,
                             recent_range = 5,
                             graph_type = "boxplot")
  plots_sed[[p]] = plt
  print(plt)  # important: actually renders the plotly widget
}

#### Location-based analysis ####
# water quality, all stations & parameters
plot_top_hq_stations(bol_media_scored, 
                     media_type = "water", 
                     param = "all", 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     all_stations = TRUE,
                     graph_type = "boxplot")

# sed quality, all S&P
plot_top_hq_stations(bol_media_scored, 
                     media_type = "sediment", 
                     param = "all", 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     all_stations = TRUE,
                     graph_type = "boxplot")

#### Get list of standards & sources
std_sources = read_csv(here::here("data/standards/all_standards.csv")) |>
  distinct(regulator, source) |>
  arrange(regulator) |>
  drop_na()
std_sources |> View()

#### to run each analyte at a time ####
parameter = "Silver"
plot_top_hq_stations(bol_media_scored, 
                     media_type = "water", 
                     param = parameter, 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     all_stations = TRUE,
                     graph_type = "boxplot")

# sed quality, all S&P
plot_top_hq_stations(bol_media_scored, 
                     media_type = "sediment", 
                     param = parameter, 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     all_stations = TRUE,
                     graph_type = "boxplot")

plot_top_hq_sieve(bol_sed_scored, 
                  param_selection = parameter, 
                  param_aggregation = "pct95", 
                  station_selection="all", 
                  temporal_aggregation = "recent", 
                  recent_range = 5,
                  graph_type = "boxplot")

#### get tables for samples and years (4.2.1) ####
parameter_year_tables = function(df = bol_media_norm, m, p) {
  d = df |>
    group_by(year, media) |>
    filter(parameter == p, 
           media == m) |>
    summarize(samples = n(),
              exceedance = round(sum(HQ>1) / samples * 100, 0),
              mean = round(mean(concentration_norm, na.rm=TRUE),2),
              median = round(median(concentration_norm, na.rm=TRUE),2),
              max = round(max(concentration_norm, na.rm=TRUE),2),
              max_station = station[which.max(HQ)],
              unit_norm = unique(unit_norm),
              .groups = "drop") |>
    bind_rows(
      df |> 
        filter(parameter == p,
               media == m) |>
        summarize(samples = n(),
                  exceedance = round(sum(HQ>1) / samples * 100, 0),
                  mean = round(mean(concentration_norm, na.rm=TRUE),2),
                  median = round(median(concentration_norm, na.rm=TRUE),2),
                  max = round(max(concentration_norm, na.rm=TRUE),2),
                  max_station = station[which.max(HQ)],
                  unit_norm = unique(unit_norm),
                  .groups = "drop") |>
        mutate(year = NA, media = m)
    )
  return(d)
}

parameter_year_tables(bol_media_norm, p="Lead", m="sediment") |> View()

#### 4.2.1 total percent water exceeding for parameters ####
bol_media_norm |>
  group_by(parameter) |>
  summarize(
    .groups = "drop"
  )

#### 4.2.2 Drinking Water Standards ####
# bol stds
get_frac_bol_class(get_samples_bol_class(bol_media_scored))

# intl stds
get_frac_intl_class(get_samples_intl_class(bol_media_scored))

#### 4.3.1 all station water quality data ####
# stations, water
plot_top_hq_stations(bol_media_scored, 
                     media_type = "water", 
                     param = "all", 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     return_data=FALSE,
                     graph_type = "boxplot",
                     all_stations=TRUE)

# stations, sediment
plot_top_hq_stations(bol_media_scored, 
                     media_type = "sediment", 
                     param = "all", 
                     temporal_aggregation = "recent", 
                     param_aggregation = "pct95",
                     ggplot_output = FALSE,
                     recent_range = 5,
                     return_data=FALSE,
                     graph_type = "boxplot",
                     all_stations=TRUE)
