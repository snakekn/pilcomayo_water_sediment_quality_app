load_sediment_data <- function(path, is.clean = FALSE, translate_to = NULL) {
  
  file_path <- path
  
  if (str_detect(file_path, ".csv")) data_raw <- suppressMessages(read_csv(file_path, col_names = FALSE)) 
  
  if (str_detect(file_path, ".xls")) data_raw <- suppressMessages(readxl::read_excel(file_path, col_names = FALSE))
  
  if (!is.clean) {
    data <- clean_sediment_data(data_raw)
  } else {
    data <- data_raw
  }
  
  if (!is.null(translate_to)) {
    
    target_lang <- translate_to
    source_lang <- ifelse(translate_to == "en", "es", "en")
    
    data <- translate_pilco_data(data, source_lang = source_lang, target_lang = target_lang, media = "sed")
    
  }
  
  return(data)
}
