# Purpose: allow Jackson's existing code to work with the risk anaylsis functionality
source(here::here("scripts/get_risk_scores.R"))

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)


convert_data_format = function(sample_data, current = NULL) {
  ## pseudocode 
  # 1. check if we know what format we're getting
  # 2. check the df and see what format it's in
  # 3. Change the data from wide to long: 
  # 4. Change the data from long to wide: 
  # 5. return?
  
  # todo: chat to get current format to what we need
}



pivot_lab_results <- function(df,
                              id_cols = c("station_id","sample_date"),
                              media_default = NULL) {
  
  df <- df |> clean_names()
  
  # pivot all parameter columns
  to_pivot <- setdiff(names(df), id_cols[id_cols %in% names(df)])
  print(names(df))
  
  df_long <- df |>
    pivot_longer(
      cols = all_of(to_pivot),
      names_to = "raw_name",
      values_to = "concentration",
      values_drop_na = TRUE
    )
  
  # clean up the raw names to extract parameter and unit
  df_long <- df_long |>
    mutate(
      # remove descriptors like "Total", "Suspended", "Dissolved"
      clean_name = str_remove_all(raw_name, regex("\\b(Total|Suspended|Dissolved)\\b", ignore_case = TRUE)),
      # extract the base parameter (text before parentheses)
      parameter = str_squish(str_trim(str_remove(clean_name, "\\(.*\\)$"))),
      # extract unit and trailing symbol if present, e.g. "(ug/l Zn)" → "ug/l Zn"
      unit = str_extract(clean_name, "(?<=\\().*(?=\\))"),
      # remove any lingering double spaces
      parameter = str_squish(parameter),
      unit = str_squish(unit)
    ) |>
    mutate(
      media = media_default
    ) |>
    select(any_of(id_cols), parameter, media, concentration, unit)
  
  df_long
}
