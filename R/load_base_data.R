load_base_data <- function(load = TRUE, loctime = FALSE, save = FALSE) {
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
    
    print("Scoring water data...")
    all_water_scored <<- score_data(all_water_pivot)
    print("DONE")
    
    print("Scoring sediment data...")
    all_sed_scored <<- score_data(all_sed_pivot)
    print("DONE")
    
    print("Merging scored data...")
    all_media_scored <<- merge_media_safely(all_water_scored, all_sed_scored)
    print("DONE")
    
    print("Turning all_sed_scored into all_sed_locyear")
    all_sed_locyear <<- score_to_loc_year(all_sed_scored)
    print("DONE. Turning all_water_scored into all_water_locyear")
    all_water_locyear <<- score_to_loc_year(all_water_scored)
    print("DONE. Merging into all_media_locyear")
    all_media_locyear <<- merge_media_safely(all_water_locyear, all_sed_locyear)
    print("DONE")
    
    print("All base data loaded, pivoted, scored, and merged.")
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
    saveRDS(all_sed_locyear, here::here("data/processed/all_sed_locyear.rds"))
    saveRDS(all_water_locyear, here::here("data/processed/all_water_locyear.rds"))
    
    saveRDS(all_media_scored, here::here("data/processed/all_media_scored.rds"))
    saveRDS(all_media_locyear, here::here("data/processed/all_media_locyear.rds"))
    
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
