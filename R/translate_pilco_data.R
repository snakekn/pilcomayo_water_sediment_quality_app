translate_pilco_data <- function(data, source_lang, target_lang, media) {
  message("[translate_pilco_data] START | media = ", media, 
          " | source = ", source_lang, " | target = ", target_lang)
  
  if (is.null(data) || !is.data.frame(data)) stop("[translate_pilco_data] input data is not a data.frame")
  
  # 1. Validation
  if (is.na(source_lang) || is.na(target_lang)) {
    warning("[translate_pilco_data] Missing language selection. Skipping.")
    return(data)
  }
  
  if (!source_lang %in% c("en", "es") || !target_lang %in% c("en", "es")) {
    stop("[translate_pilco_data] Languages must be 'en' or 'es'")
  }
  
  if (source_lang == target_lang) {
    message("[translate_pilco_data] Source/Target same. Returning data.")
    return(data)
  }
  
  # 2. Select base map
  # Assume param_mapping and param_mapping_sed are available in the environment
  base_map <- if (media == "water") param_mapping else param_mapping_sed
  
  # 3. Create flat, named character vector for translation
  # Spanish -> English: Map is already [ES_Name = EN_Name]
  # English -> Spanish: Map is [EN_Name = ES_Name] (needs reversal)
  if (source_lang == "es" && target_lang == "en") {
    translation_map <- as.character(unlist(base_map))
    names(translation_map) <- names(base_map)
  } else {
    # Reverse mapping
    translation_map <- setNames(names(base_map), as.character(unlist(base_map)))
  }
  
  message("[translate_pilco_data] translation_map length: ", length(translation_map))
  
  # 4. Apply translation
  current_cols <- colnames(data)
  
  new_cols <- sapply(current_cols, function(col) {
    if (col %in% names(translation_map)) {
      return(as.character(translation_map[[col]]))
    } else {
      # Only warn if it's not a common ID column that we expect not to translate
      if (!col %in% c("data_source", "Station", "Date", "Time", "Year")) {
        message("[translate_pilco_data] WARNING: Column '", col, "' not in mapping.")
      }
      return(col)
    }
  }, USE.NAMES = FALSE)
  
  colnames(data) <- new_cols
  
  message("[translate_pilco_data] END | renamed columns: ", paste(new_cols, collapse = ", "))
  return(data)
}