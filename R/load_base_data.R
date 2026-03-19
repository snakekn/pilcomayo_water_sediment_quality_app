load_base_data <- function(load = TRUE, locyear = FALSE, loctime = FALSE, save = TRUE, check_standards = TRUE) {
  
  # Quick library confirmation. Slows things down, but this is a 1-off function anyways
  library(pacman)
  p_load(tidyverse)
  p_load(here)
  
  #### Load source files for functions required only for this function ####
  # Takes a lot to run this place
  dir = "R"
  sys.source(here(dir, "convert_data_format.R"), envir = globalenv()) # pivot_pilcomayo_data()
  sys.source(here(dir, "get_risk_scores.R"), envir = globalenv()) # score_data()
  sys.source(here(dir, "helpers_server.R"), envir = globalenv()) # merge_media_safely(), clip_to_bolivia()
  sys.source(here(dir, "get_risk_scores.R"), envir = globalenv()) # score_to_loc_year() which includes setting stds
  sys.source(here(dir, "locyear_to_locscore.R"), envir = globalenv()) # weigh_inverse_time()
  sys.source(here(dir, "load_water_data.R"), envir = globalenv()) # load_water_data()
  sys.source(here(dir, "clean_water_data.R"), envir = globalenv()) # clean_water_data()
  sys.source(here(dir, "translate_pilco_data.R"), envir = globalenv()) # translate_pilco_data()
  sys.source(here(dir, "param_mapping.R"), envir = globalenv()) # param_mapping()
  sys.source(here(dir, "load_sediment_data.R"), envir = globalenv()) # load_sediment_data()
  sys.source(here(dir, "clean_sediment_data.R"), envir = globalenv()) # clean_sediment_data()
  sys.source(here(dir, "dms_to_decimal.R"), envir = globalenv()) # dms_to_decimal()
  sys.source(here(dir, "convert_data_format.R"), envir = globalenv()) # dms_to_decimal()
  
  # if the user is looking to complete all tasks (not just save the data)
  if(load) {
    print("Loading water data...")
    all_water_data <<- bind_rows(lapply(unlist(list.files("data/water/raw", full.names = TRUE)),
                                        load_water_data, translate_to = "en"))
    print("DONE")
    
    print("Loading sediment data...")
    all_sed_data <<- bind_rows(lapply(unlist(list.files("data/sed/raw", full.names = TRUE)),
                                     load_sediment_data, translate_to = "en"))
    print("DONE")
    
    print("Pivoting water data...")
    all_water_pivot <<- pivot_pilcomayo_data(all_water_data, media_type = "water")
    print("DONE")
    
    print("Pivoting sediment data...")
    all_sed_pivot <<- pivot_pilcomayo_data(all_sed_data, media_type = "sediment")
    print("DONE")
    
    if(check_standards) {
      print("Confirming we have the most up-to-date standards")
      stds <<- readr::read_csv(here::here("data/standards/strict_standards.csv"))
      stds_all <<- readr::read_csv(here::here("data/standards/all_standards.csv"))
    }
    
    print("Scoring water data...")
    all_water_scored <<- score_data(all_water_pivot)
    print("DONE")
    
    print("Scoring sediment data...")
    all_sed_scored <<- score_data(all_sed_pivot)
    print("DONE")
    
    print("Merging scored data...")
    all_media_scored <<- merge_media_safely(all_water_scored, all_sed_scored)
    print("DONE")
    
    if(locyear) {
      print("Turning all_sed_scored into all_sed_locyear")
      all_sed_locyear <<- score_to_loc_year(all_sed_scored)
      print("DONE. Turning all_water_scored into all_water_locyear")
      all_water_locyear <<- score_to_loc_year(all_water_scored)
      print("DONE. Merging into all_media_locyear")
      all_media_locyear <<- merge_media_safely(all_water_locyear, all_sed_locyear)
      print("DONE")
    }
    
    print("All base data loaded, pivoted, scored, and merged as requested.")
    
    print("Loading border shapefiles and clipping scored data to Bolivia...")
    
    bol_border <<- st_read("data/geojson/bol_borders.geojson")
    
    bol_sed_scored <<- all_sed_scored |>
      clip_to_bolivia(lat_col = "latitude_decimal", lon_col = "longitude_decimal", bol_border = bol_border)
    bol_water_scored <<- all_water_scored |>
      clip_to_bolivia(lat_col = "latitude_decimal", lon_col = "longitude_decimal", bol_border = bol_border)
    bol_media_scored <<- all_media_scored |>
      clip_to_bolivia(lat_col = "latitude_decimal", lon_col = "longitude_decimal", bol_border = bol_border)
    
    print("Scored data for water, sediment, and all media clipped to Bolivia.")
  }
  
  if (loctime) {
    print("Creating loctime data")
    all_sed_loctime <<- weigh_inverse_time(sed_locyear)
    all_water_loctime <<- weigh_inverse_time(water_locyear)
    all_media_loctime <<- merge_media_safely(all_water_loctime, all_sed_loctime)
    print("Loctime data created.")
  }
  
  # if the user wants to save everything to master_data
  if(save) {
    print("Adding base data to the master_data file path")
    saveRDS(all_sed_scored, here::here("data/processed/all_sed_scored.rds"))
    saveRDS(all_water_scored, here::here("data/processed/all_water_scored.rds"))
    saveRDS(all_media_scored, here::here("data/processed/all_media_scored.rds"))
    
    if(locyear) {
      saveRDS(all_sed_locyear, here::here("data/processed/all_sed_locyear.rds"))
      saveRDS(all_water_locyear, here::here("data/processed/all_water_locyear.rds"))
      saveRDS(all_media_locyear, here::here("data/processed/all_media_locyear.rds"))
    }
    
    if (loctime) {
      print("Including loctime data in master_data")
      saveRDS(all_sed_loctime, here::here("data/processed/all_sed_loctime.rds"))
      saveRDS(all_water_loctime, here::here("data/processed/all_water_loctime.rds"))
      saveRDS(all_media_loctime, here::here("data/processed/all_media_loctime.rds"))
    }
    
    print("master_data files saved to data/processed/all_* paths")
  }
}

# for easy using. 3 params!
# load_base_data(load = TRUE, loctime=TRUE, save=TRUE)
