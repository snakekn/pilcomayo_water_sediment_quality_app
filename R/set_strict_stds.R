### Script Description & Goals
# Author: Nadav Kempinski
# Version: 10/21/25
# ---

### Packages
library(tidyverse)
library(here)
library(janitor)

### Data Import
set_strict_stds = function(FILE_LOCATION = "data/standards/all_standards.csv", SAVE_LOCATION = "data/standards/strict_standards.csv") {
  
  # Note: This file was compilated from existing gDrive files (as of 10/21/2025). You can update this file and re-run to get newest set of lowest params
  std_file = read_csv(here(FILE_LOCATION)) |>
    clean_names()
  
  ### Main Script
  
  # group by parameter & media, get the lowest value, arrange by name of parameter
  strict_std <<- std_file |>
    mutate(is_ph_low = tolower(parameter) == "ph" & grepl("low", parameter, ignore.case = TRUE)) |>
    group_by(parameter, media) |>
    slice_min(
      order_by = ifelse(is_ph_low, -value, value), 
      n = 1, 
      with_ties = FALSE
    ) |>
    arrange(parameter) |>
    ungroup() |>
    select(-is_ph_low)
  
  # save to a file we can use later
  strict_std |>
    write.csv(file=SAVE_LOCATION)
  
  ## TODO:
  # add CR risk as part of the strict calculation
  
  return(strict_std)
}
