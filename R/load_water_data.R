load_water_data <- function(path, is.clean = FALSE, translate_to = NULL) {
  
  file_path <- path
  
  if (str_detect(file_path, ".csv")) data_raw <- read_csv(file_path)

  if (str_detect(file_path, ".xlsx")) data_raw <- read_xlsx(file_path, col_names = FALSE)

  if (!is.clean) {
    data <- clean_water_data(data_raw)
  } else {
    data <- data_raw
  }
  
  if (!is.null(translate_to)) {
    
    target_lang <- translate_to
    source_lang <- ifelse(translate_to == "en", "es", "en")
    
    data <- translate_water_data(data, source_lang = source_lang, target_lang = target_lang)
    
  }
  
  return(data)
}
