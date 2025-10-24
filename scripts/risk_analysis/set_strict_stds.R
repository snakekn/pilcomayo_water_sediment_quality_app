### Script Description & Goals
# Author: Nadav Kempinski
# Version: 10/21/25
# ---

### Packages
library(tidyverse)
library(here)
library(janitor)

### Data Import
FILE_LOCATION = "data/standards/all_standards.csv"
# Note: This file was compilated from existing gDrive files (as of 10/21/2025). You can update this file and re-run to get newest set of lowest params
std_file = read_csv(here(FILE_LOCATION)) |>
  clean_names()

View(std_file)

### Main Script

# group by parameter & media, get the lowest value, arrange by name of parameter
strict_std = std_file |>
  group_by(parameter, media) |>
  slice_min(order_by=value, n=1, with_ties=FALSE) |>
  arrange(parameter) |>
  ungroup()

# save to a file we can use later
strict_std |>
  write.csv(file="data/standards/strict_standards.csv")

## TODO:
# add CR risk as part of the strict calculation