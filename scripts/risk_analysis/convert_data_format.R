# Purpose: allow Jackson's existing code to work with the risk anaylsis functionality
source(here::here("scripts/get_risk_scores.R"))

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)


convert_sampled_data_types = function(sample_data, type = NULL) {
  ## pseudocode 
  # 1. check if we know what format we're getting
  # 2. check the df and see what format it's in
  # 3. Change the data from wide to long: 
  # 4. Change the data from long to wide: 
  # 5. return?
  
  # todo: chat to get current format to what we need
  
  names(sample_data) <- fix_headers(names(sample_data))
  if(type=="Pilcomayo.net") {
    formatted_data = pivot_pilcomayo_data(sample_data)
  } else if (type=="sediment") {} 
  else { abort("Data format not recognized. Please check your file and try again")
    }
  
}

# intake raw data


# fix to UTF-8
fix_headers <- function(nms) {
  nms <- enc2utf8(nms)                        # ensure UTF-8
  nms <- stringi::stri_trans_general(nms, "Latin-ASCII")  # á->a, µ->u (we'll fix µg below)
  nms <- str_replace_all(nms, "\\s+", " ")    # collapse spaces
  nms <- str_trim(nms)
  nms
}

# when taking in data from clean_style, make sure we call it with following flags
clean_data = read.csv(here::here("data/water_clean_2016.csv"), check.names=FALSE)

pivot_pilcomayo_data <- function(df,
                              id_cols_num = 13, # number of columns we want to keep, since parameters fill the rest of the sheet
                              media_type = NA) {
  
  # sanity: need at least 14 columns to pivot “the rest”
  if (ncol(df) < id_cols_num+1) {
    abort("Expected ≥14 columns: first 13 kept as IDs, remaining pivoted.")
  }
  
  id_cols <- names(df)[1:id_cols_num]
  value_cols <- names(df)[-(1:id_cols_num)]

    df_new = df %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to   = "raw_name",
      values_to  = "concentration",
      values_drop_na = TRUE
    ) %>%
    mutate(
      # remove descriptors not used by your standards
      clean_name = str_remove_all(raw_name, regex("\\b(Total|Suspended|Dissolved)\\b", ignore_case = TRUE)),
      # parameter = text before the parenthesis
      parameter  = str_squish(str_remove(clean_name, "\\(.*\\)$")),
      # unit blob inside parentheses, e.g. "ug/L Zn" or "mg/kg Pb"
      unit_blob  = str_match(clean_name, "\\((.*)\\)")[, 2] %>% str_squish(),
      # unit = the first token that looks like a unit, e.g. "ug/L" or "mg/kg"
      unit       = str_extract(unit_blob %||% "", "^[^ ]+(/[^ ]+)?"),
      media      = media_type,
      # get year from date for later
      Date = lubridate::dmy(Date),
      year       = lubridate::year(Date),
    # post-cleaning for pH and degree symbols
      unit = case_when(
        str_detect(parameter, regex("^pH$", ignore_case = TRUE)) & is.na(unit) ~ "pH units",
        TRUE ~ unit
      ),
      # remove degree symbol if it exists (°C → C)
      unit = str_replace_all(unit, fixed("°"), ""),
      unit = str_trim(unit)
    ) |>
    select(all_of(id_cols), Date, year, parameter, media, concentration, unit) |> clean_names()
}
# 
