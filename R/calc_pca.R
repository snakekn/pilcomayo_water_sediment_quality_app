calc_pca = function(data, params, media_selection = "all", station_selection = "all", draw_circle = FALSE) {
  if (length(params) < 2) stop("Please select 2 more more variables")
  
  # filter based on user-selected inputs
  if(media_selection != "all") {
    cat("\n[calc_pca] Filtering using media \"", media_selection, "\". Pre-filter Measurements: ", nrow(data))
    data = data |>
      filter(media == media_selection)
    cat("\nAfter filtering: ", nrow(data))
  } else {
    cat("\nNot filtering on media")
  }
  
  # filter based on user-selected inputs
  if(station_selection != "all") {
    cat("\n[calc_pca] Filtering using station \"", station_selection, "\". Pre-filter Measurements: ", nrow(data))
    data = data |>
      filter(station == station_selection)
    cat("\nAfter filtering: ", nrow(data))
  } else {
    cat("\nNot filtering on station")
  }
  
  # filter for the parameter list selected
  df <- data %>%
    filter(parameter %in% params) |>
    select(station, date, media, parameter, concentration) |>
    drop_na()
  # View(df |> mutate(stage = "Pre-pivot"))
  
  # pivot wider
  df = df |>
    group_by(station, date, media, parameter) |>
    summarize(concentration = mean(concentration, na.rm=TRUE), .groups = "drop") |>
    pivot_wider(
      id_cols = c(station, date, media),
      names_from = parameter,
      values_from = concentration
    )
  
  # View(df |> mutate(stage = "Post-pivot"))
  
  # Remove columns that are entirely NA
  df <- df %>% select(where(~ !all(is.na(.))))
  
  cat("\nPCA data dims:", nrow(df), "x", ncol(df), "\n")
  print(names(df))
  
  # Ensure we have enough observations after filtering to attempt a PCA
  if(nrow(df) < 3) {
    stop("Fitering left fewer than 3 observations. Please update your filters.")
  }
  # For grouping later
  df_info = df 
  
  # drop ID columns
  df_num = df |> select(-station, -date, -media)
  
  # If there are no missing values, skip imputation
  if (!any(is.na(df_num))) {
    pca <- FactoMineR::PCA(df_num, graph = FALSE, scale.unit=draw_circle)
  } else {
    # Estimate optimal number of components for imputation
    est <- missMDA::estim_ncpPCA(df_num, method.cv = "Kfold", nbsim = 5)
    
    # Impute missing values
    impute_result <- missMDA::imputePCA(df_num, ncp = est$ncp)
    
    # imputePCA returns a list with $completeObs (matrix/data frame)
    comp <- impute_result$completeObs
    
    # Make sure it's a data.frame
    comp <- as.data.frame(comp)
    
    # Run PCA
    pca <- FactoMineR::PCA(comp, graph = FALSE, scale.unit=draw_circle)
  }
  
  cat("\n[calc_pca] Completed running pca")
  return(list(df = df_info, pca = pca))
}
