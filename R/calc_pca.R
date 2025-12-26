calc_pca = function(data, params) {
  if (length(params) < 2) stop("Please select 2 more more variables")
  
  # filter based on user-selected inputs
  # if(param_selection != "all") {
  #   cat("\n[plot_top_hq_sieve] Filtering using parameter \"", param_selection, "\". Measurements: ", nrow(data))
  #   data = data |> 
  #     filter(parameter == param_selection)
  #   cat("\nAfter filtering: ", nrow(data))
  # } else {
  #   cat("\nNot filtering on parameter")
  # }
  
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
  df = df |> select(-station, -date, -media)
  
  # Estimate optimal number of components for imputation
  est <- estim_ncpPCA(df, method.cv = "Kfold", nbsim = 5)
  
  # Impute missing values
  impute_result <- imputePCA(df, ncp = est$ncp)
  
  # Run PCA
  pca = PCA(impute_result$completeObs, graph = FALSE)
  cat("\n[calc_pca] Completed running pca")
  return(list(df = df_info, pca = pca))
}
