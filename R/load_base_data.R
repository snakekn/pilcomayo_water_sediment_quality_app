load_base_data <- function() {
  
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
  
  print("All base data loaded, pivoted and scored.")

}
