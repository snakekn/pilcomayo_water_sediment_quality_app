# Purpose: allow Jackson's existing code to work with the risk anaylsis functionality

library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)


upload_sampled_data = function(sample_data, media = NA, debug_prepped = FALSE, format = NA, src_lang = NA, target_lang = NA) {
  ## pseudocode 
  # 1. check if we know what format we're getting
  # 2. check the df and see what format it's in
  # 3. Change the data from wide to long: 
  # 4. Change the data from long to wide: 
  # 5. return?
  
  # in case we have weird non-UTF-8 characters
  names(sample_data) <- fix_headers(names(sample_data))

  # take raw pilco data and turn it into prepped clean version
  # will only not run if we're sending in pre-cleaned data, which shouldn't happen in production
  if(format=="pilco" && !debug_prepped) {
    sample_data = clean_water_data(sample_data, source=format)
  }
  
  # translate file to appropriate language - should probably make everything english, handle, then revert to es as desired in the front-facing app
  # english for backend work, es/en for front-end
  translated_data = translate_water_data(sample_data, src_lang, target_lang)
  View(translated_data) 
  ## Nadav's Note: Sanity check. Something here isn't translating properly. 
  # After this, getting issue: "Error in UseMethod: no applicable method for 'mutate' applied to an object of class "character"
  
  if(format=="pilco") {
    formatted_data = pivot_pilcomayo_data(translated_data, media_type = media)
  } else if (format=="parameter_long") { formatted_data = translated_data} # we can just start from per-parameter if someone has it
  else { abort("Data format not recognized. Please check your file and try again")
  }
  print("showing notification that upload processed successfully")
  showNotification("Upload processed successfully!", type = "message")
  return(formatted_data)
}

# intake raw data -- JMills


# fix to UTF-8
fix_headers <- function(nms) {
  nms <- enc2utf8(nms)                        # ensure UTF-8
  nms <- stringi::stri_trans_general(nms, "Latin-ASCII")  # á->a, µ->u (we'll fix µg below)
  nms <- str_replace_all(nms, "\\s+", " ")    # collapse spaces
  nms <- str_trim(nms)
  nms
}

# when taking in data from clean_style, make sure we call it with following flags
# clean_data = read.csv(here::here("data/water_clean_2016.csv"), check.names=FALSE)

pivot_pilcomayo_data <- function(df,
                              id_cols_num = 13, # number of columns we want to keep, since parameters fill the rest of the sheet
                              media_type = NA) {
  
  # ---- sanity & logging ----
  if (is.null(df) || !is.data.frame(df)) {
    stop("[pivot_pilcomayo_data] input df is NULL / not a data.frame")
  }
  if (ncol(df) < id_cols_num + 1) {
    stop("[pivot_pilcomayo_data] Expected ≥ ", id_cols_num + 1,
         " cols; got ", ncol(df))
  }
  message("[pivot_pilcomayo_data] nrow=", nrow(df), " ncol=", ncol(df))
  
  
  # sanity: need at least 14 columns to pivot “the rest”
  if (ncol(df) < id_cols_num+1) {
    abort("Expected ≥14 columns: first 13 kept as IDs, remaining pivoted.")
  }
  
  id_cols <- names(df)[1:id_cols_num]
  value_cols <- names(df)[-(1:id_cols_num)]

  print(paste0("value_cols: ", value_cols)) # more sanity
  
    df_new = df %>%
      mutate(across(value_cols, as.double)) |>
    pivot_longer(
      cols = all_of(value_cols),
      names_to   = "raw_name",
      values_to  = "concentration",
      values_drop_na = TRUE
    )
    View(df_new |> mutate(test=1))
    df_new = df_new |>
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
    )
    View(df_new |> mutate(test=2)) # sanity check ugh
    df_new |>
    select(all_of(id_cols), Date, year, parameter, media, concentration, unit) |> clean_names()
}
# 
