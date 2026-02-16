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
p1 = plot_top_hq_params(bol_water_scored, 
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
                        ggplot_output=TRUE)
p2
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
                                       recent_range = 5)

arsenic_stations_water

### Stations with highest arsenic - sed
arsenic_stations_sed = plot_top_hq_stations(bol_media_scored, 
                                              media_type = "sediment", 
                                              param = "Arsenic", 
                                              temporal_aggregation = "recent", 
                                              param_aggregation = "pct95",
                                              ggplot_output = FALSE,
                                              recent_range = 5)

arsenic_stations_sed

### arsenic sieve sizes
arsenic_sieve = plot_top_hq_sieve(bol_sed_scored, 
                                  param_selection = "Arsenic", 
                                  param_aggregation = "pct95", 
                                  station_selection="all", 
                                  temporal_aggregation = "recent", 
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