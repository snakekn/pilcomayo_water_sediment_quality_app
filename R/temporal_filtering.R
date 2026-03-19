#' note: only set to use recent filtering over 5 years rn. 
#' Can be put into other functions to reduce uncertainty on how we do this
temporal_filtering = function(df, temporal_aggregation = "recent", nyears = 5) {
  message(sprintf("[temporal_filtering] Filtering via method \"%s\" using nyears %d", temporal_aggregation, nyears))
  
  max_date = max(df$date, na.rm=TRUE)
  message(sprintf("Max date found in the dataset: %s", format(max_date, "%B %d, %Y")))
  
  min_date = max_date - nyears*365.25
  message(sprintf("Cutoff date calculated as %s", format(min_date, "%B %d, %Y")))
  
  df |> filter(date >= min_date)
}
