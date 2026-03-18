#### create normalized dataset for media to make it easier to run comparisons ####
# note: it runs rowwise, so it's pretty slow. Be patient - it works!
bol_media_norm = bol_media_scored |> normalize_units_wrapper()

normalize_units_wrapper <- function(data, 
                                    target_units = list(water = "mg/l", 
                                                        sediment = "mg/kg"),
                                    media_col = "media",
                                    unit_col = "unit",
                                    conc_col = "concentration",
                                    parameter_col = "parameter",
                                    drop_original = FALSE) {
  total_rows <- nrow(data)
  cat("Pre-filter count:", total_rows, "\n")
  
  normalized_data <- data %>%
    filter(!is.na(.data[[media_col]]), !is.na(.data[[conc_col]])) %>%
    rowwise() %>%
    mutate(
      target_unit = target_units[[.data[[media_col]]]],
      unit_conv = list({
        if (is.na(.data[[unit_col]]) || .data[[unit_col]] == "" || is.na(target_unit)) {
          list(convertible = FALSE, conversion_factor = 1)
        } else {
          compare_units(.data[[unit_col]], target_unit)
        }
      }),
      concentration_norm = if (unit_conv$convertible) {
        .data[[conc_col]] * unit_conv$conversion_factor
      } else {
        .data[[conc_col]]
      },
      unit_norm = if (unit_conv$convertible) target_unit else .data[[unit_col]]
    ) %>%
    ungroup()
  
  if (drop_original) {
    normalized_data <- normalized_data %>%
      select(-all_of(c(conc_col, unit_col)))
  }
  
  cat(sprintf("Added '%s_norm' and '%s_norm' columns.\n", conc_col, unit_col))
  cat("Rows processed:", nrow(normalized_data), "\n")
  
  return(normalized_data)
}

normalize_units_wrapper <- function(data, 
                                    target_units = list(water = "mg/l", sediment = "mg/kg"),
                                    media_col = "media",
                                    unit_col = "unit",
                                    conc_col = "concentration",
                                    parameter_col = "parameter",
                                    drop_original = FALSE) {
  data_clean <- data %>%
    filter(!is.na(.data[[media_col]]), !is.na(.data[[conc_col]])) %>%
    mutate(target_unit = target_units[.data[[media_col]]],
           unit_valid = !is.na(.data[[unit_col]]) & .data[[unit_col]] != "")
  
  cat("Valid units:", sum(data_clean$unit_valid), "/", nrow(data_clean), "\n")
  
  conv_data <- data_clean %>%
    filter(unit_valid) %>%
    mutate(
      unit_conv = map2(.data[[unit_col]], target_unit, 
                       ~ suppressMessages(compare_units(.x, .y))),
      comparable = map_lgl(unit_conv, "convertible")
    )
  
  cat("Comparable:", sum(conv_data$comparable), "\n")
  
  normalized <- conv_data %>%
    filter(comparable) %>%
    mutate(
      concentration_norm = map2_dbl(.data[[conc_col]], unit_conv, ~ .x * .y$conversion_factor),
      unit_norm = target_unit
    ) %>%
    select(-unit_conv, -comparable) %>%
    ungroup()
  
  data_clean %>%
    left_join(normalized %>% transmute(across(where(is.numeric) | where(is.character), ~ .x)), 
              by = all_vars()) %>%  # Smart join by all cols
    mutate(
      concentration_norm = coalesce(concentration_norm, NA_real_),
      unit_norm = coalesce(unit_norm, .data[[unit_col]])
    ) %>%
    select(-target_unit, -unit_valid)
}

