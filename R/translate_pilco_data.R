translate_pilco_data <- function(data, source_lang, target_lang, media) {
  # stop if either is NA
  if(is.na(source_lang) || is.na(target_lang)) {
    warning(paste("Missing language selection. Will skip translations: ", source_lang, target_lang))
    return(data)
  }
  
  # Validate inputs
  if (!source_lang %in% c("en", "es") || !target_lang %in% c("en", "es")) {
    stop("Languages must be 'en' (English) or 'es' (Spanish)")
  }
  
  if (source_lang == target_lang) {
    warning("Source and target languages are the same. Returning data unchanged.")
    return(data)
  }
  
  # Get current column names
  current_cols <- colnames(data)
  
  # Create translation based on direction and media
  if (media == "water") {
    if (source_lang == "es" && target_lang == "en") {
      # Spanish to English: use param_mapping as is
      translation_map <- param_mapping
    } else {
      # English to Spanish: reverse the param_mapping
      translation_map <- setNames(names(param_mapping), unname(unlist(param_mapping)))
    }
  } else if(media == "sed") {
    if (source_lang == "es" && target_lang == "en") {
      # Spanish to English: use param_mapping as is
      translation_map <- param_mapping_sed
    } else {
      # English to Spanish: reverse the param_mapping
      translation_map <- setNames(names(param_mapping_sed), unname(unlist(param_mapping_sed)))
    }
  } 
  
  
  # Translate column names
  new_cols <- sapply(current_cols, function(col) {
    if (col %in% names(translation_map)) {
      return(translation_map[[col]])
    } else {
      warning(paste("Column", col, "not found in mapping. Keeping original name."))
      return(col)
    }
  }, USE.NAMES = FALSE)
  
  # Apply new column names
  colnames(data) <- new_cols
  
  return(data)
}
