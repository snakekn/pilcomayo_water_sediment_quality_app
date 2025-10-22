### Script Description & Goals
# Author: Nadav Kempinski
# Version: 10/21/25
# ---

### Packages
library(tidyverse)
library(here)
library(janitor)

### Data Import

# Note: This file was compilated from existing gDrive files (as of 10/21/2025). You can update this file and re-run to get newest set of lowest params
std_file = read_csv(here("data/standards/strict_standards.csv")) |>
  clean_names()

### Main Script

# group by parameter & media, get the lowest value, arrange by name of parameter
strict_std = std_file |>
  group_by(parameter, media) |>
  slice_min(order_by=concentration, n=1, with_ties=FALSE) |>
  arrange(parameter) |>
  ungroup()

# save to a file we can use later
strict_std
  write.csv(file="data/standards/strict_standards2.csv")
