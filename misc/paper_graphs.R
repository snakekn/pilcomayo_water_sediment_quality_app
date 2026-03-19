#### review quantiles using station HQs ####
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
                         ggplot_output=TRUE)
p1

### Chemicals of highest concern - sediment
p2 = plot_top_hq_params(bol_sed_scored, 
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
                        ggplot_output=TRUE)
p2

#### quick water unit normalization function ####
normalize_water <- function(data, conc_col = "concentration", unit_col = "unit") {
  data |>
    filter(media == "water") |>
    mutate(
      concentration_norm = case_when(
        tolower(.data[[unit_col]]) == tolower("mg/l") ~ .data[[conc_col]] * 1000,
        tolower(.data[[unit_col]]) %in% c("ng/l", "ppt") ~ .data[[conc_col]] / 1e3,
        TRUE ~ .data[[conc_col]]  # Sediment unchanged
      ),
      unit_norm = case_when(
        tolower(.data[[unit_col]]) == tolower("mg/l") ~ "ug/l",
        tolower(.data[[unit_col]]) %in% c("ng/l", "ppt") ~ "ug/l",
        TRUE ~ .data[[unit_col]]  # Sediment unchanged
      ),
    )
}

#### per-year exceedances and maximums ####
parameter_year_tables = function(df, m, p) {
  
  d = df |>
    group_by(year, media, unit) |>
    filter(parameter == p, 
           media == m)
  
  print(sprintf("Parameter Year Table: %g rows remaining", nrow(d)))
  if(nrow(d) == 0) {
    return(NULL)
  }
  
  d = d |>
    summarize(samples = n(),
              exceedance = round(sum(HQ>1) / samples * 100, 0),
              mean = round(mean(concentration, na.rm=TRUE),2),
              median = round(median(concentration, na.rm=TRUE),2),
              max = round(max(concentration, na.rm=TRUE),2),
              max_station = station[which.max(HQ)],
              unit_norm = cat(paste(unique(unit))),
              .groups = "drop") |>
    bind_rows(
      df |> 
        filter(parameter == p,
               media == m) |>
        summarize(samples = n(),
                  exceedance = round(sum(HQ>1) / samples * 100, 0),
                  mean = round(mean(concentration, na.rm=TRUE),2),
                  median = round(median(concentration, na.rm=TRUE),2),
                  max = round(max(concentration, na.rm=TRUE),2),
                  max_station = station[which.max(HQ)],
                  unit_norm = cat(paste(unique(unit))),
                  .groups = "drop") |>
        mutate(year = NA, media = m)
    )
  return(d)
}

analyte_result_section = function(df, p) {
  ### introduce required functions
  sys.source(here("R", "plot_top_hq_stations.R"), envir = globalenv()) # plot_top_hq_stations()
  sys.source(here("R", "plot_top_hq_sieve.R"), envir = globalenv()) # plot_top_hq_sieve()
  
  ### define the outcome
  result = list()
  
  ### filter dataset
  data = df |>
    filter(parameter == p, !is.na(HQ), year!=2024)
  
  ### get yearly tables
  
  ## fix water units
  data_normalized = normalize_water(data) |>
    rename(c_old = concentration, u_old = unit,
            concentration = concentration_norm,
            unit = unit_norm)
  result$table_water = parameter_year_tables(data_normalized, m = "water", p=p)
  result$table_sediment = parameter_year_tables(data, m="sediment", p=p)
  
  ### filter again for most recent years
  data = data |>
    temporal_filtering()
  
  ### get station plots
  result$station_water = tryCatch(plot_top_hq_stations(data, 
                                               media_type = "water", 
                                               param = p, 
                                               temporal_aggregation = "recent", 
                                               param_aggregation = "pct95",
                                               ggplot_output = FALSE,
                                               graph_type = "boxplot",
                                               recent_range = 5),
                                  error = function(e) {
                                    cat(sprintf("Water Station Plot Error: %s", e$message))
                                    e$message
                                  })
  ### get station plots
  result$station_sediment = tryCatch(plot_top_hq_stations(data, 
                                              media_type = "sediment", 
                                              param = p, 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              ggplot_output = FALSE,
                                              graph_type = "boxplot",
                                              recent_range = 5),
                                     error = function(e) {
                                       cat(sprintf("Sediment Station Plot Error: %s", e$message))
                                       e$message
                                     })
  
  ### get sieve plot
  result$sieve = tryCatch(plot_top_hq_sieve(data, 
                    param_selection = p, 
                    param_aggregation = "pct95", 
                    station_selection="all", 
                    temporal_aggregation = "recent",
                    graph_type = "boxplot",
                    recent_range = 5),
                    error = function(e) {
                      cat(sprintf("Sieve Plot Error: %s", e$message))
                      e$message
                    })
  
  return(result)
}


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
  #print(plt)  # important: actually renders the plotly widget
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

#### 4.2.3 Other Contaminants ####
### Get table of each parameter and the exceedance rate for the last 5 years
bol_media_scored_recent = temporal_filtering(bol_media_scored |> filter(year!=2024))

get_exceedances = function(df, m) {
  sys.source(here("R", "set_strict_stds.R"), envir = globalenv()) # set_strict_stds()
  
  d = df |>
    filter(media == m, !is.na(HQ)) |>
    group_by(parameter) |>
    summarize(n_samples = n(),
              n_exceeded = sum(HQ>1, rm.na=TRUE),
              exceedance_rate = round(n_exceeded/n_samples*100,0))
  
  s = set_strict_stds() |> # from set_strict_stds.R
    filter(media == m)

  t = d |>
    left_join(s, by = "parameter") |>
    select(parameter, regulator, value, unit, exceedance_rate, n_samples, n_exceeded)
}

# use: get_exceedances(bol_media_scored_recent, "water")