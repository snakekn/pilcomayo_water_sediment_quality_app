#' Calculate Kd values from water/sediment data
#' 
#' @param data Data frame with `station`, `date`, `parameter`, `media`, `fraction`, 
#'   `concentration`, `unit` columns
#' @param mode "sediment" (water vs sediment) or "water" (dissolved vs suspended)  
#' @param parameter Single parameter name or NULL (all parameters)
#' 
#' @return List: `detailed` (station-level), `summary` (parameter stats)
#' 
#' @examples
#' calculate_kd(bol_media_scored, "sediment")
#' calculate_kd(bol_media_scored, "sediment", "Thallium")
calculate_kd <- function(data, mode = "sediment", parameter = NULL) {
  
  if (mode == "sediment") {
    # water (Suspended) vs sediment
    filt_data <- data |>
      filter((media == "water" & fraction == "Suspended") | media == "sediment")
    pivot_col <- "media"  # values: "water", "sediment"
    
  } else if (mode == "water") {
    # dissolved vs suspended within water
    filt_data <- data |>
      filter(media == "water", fraction %in% c("Dissolved", "Suspended"))
    pivot_col <- "fraction"  # values: "Dissolved", "Suspended"
    
  } else {
    stop("mode must be 'sediment' or 'water'")
  }
  
  if (!is.null(parameter)) {
    filt_data <- filt_data |> filter(parameter == !!parameter)
  }
  
  # standardize data
  filt_data = filt_data |> filter(tolower(unit) %in% c("mg/l", "ug/l", "mg/kg", "ug/kg", "%"), 
                   !(media == "water" & unit == "%")) |>
    mutate(
      conc = case_match(tolower(unit),
                        "%" ~ concentration * 1e4,     
                        "ug/kg" ~ concentration / 1e3, 
                        "ug/l" ~ concentration / 1e3,  
                        .default = concentration),
      units = case_match(tolower(unit),
                         "%" ~ "mg/kg",
                         "ug/kg" ~ "mg/kg", 
                         "ug/l" ~ "mg/l",
                         "mg/l" ~ "mg/l",
                         .default = unit),
      conc = if_else(conc == 0, 1e-12, conc)  # Fix 0 concentrations to build reporting size -- it was sampled but not detected. 
    )
  
  # counts
  counts_data <- filt_data |>
    count(station, date, parameter, !!sym(pivot_col), name = "n_samples")
  
  # means
  means_data <- filt_data |>
    group_by(station, date, parameter, !!sym(pivot_col)) |>
    summarise(
      mean_conc  = mean(conc, na.rm = TRUE),
      unit_check = paste(unique(units), collapse = ", "),
      .groups = "drop"
    )
  
  # join + pivot
  joined <- means_data |>
    left_join(counts_data, by = c("station", "date", "parameter", pivot_col))
  
  kd_data <-
    if (mode == "sediment") {
      joined |>
        pivot_wider(
          names_from  = media,
          values_from = c(mean_conc, n_samples, unit_check),
          # result: mean_conc_water, mean_conc_sediment, n_samples_water, ...
          names_sep = "_",
          values_fill = list(mean_conc = NA, n_samples = 0, unit_check = NA)
        ) |>
        mutate(
          kd = mean_conc_sediment / mean_conc_water
        ) |>
        filter(!is.na(kd), mean_conc_water > 0, mean_conc_sediment > 0)
      
    } else {  # mode == "water"
      joined |>
        pivot_wider(
          names_from  = fraction,
          values_from = c(mean_conc, n_samples, unit_check),
          # e.g. mean_conc_Dissolved, mean_conc_Suspended
          names_sep = "_",
          values_fill = list(mean_conc = NA, n_samples = 0, unit_check = NA)
        ) |>
        mutate(
          kd = mean_conc_Suspended / mean_conc_Dissolved
        ) |>
        filter(!is.na(kd), mean_conc_Dissolved > 0, mean_conc_Suspended > 0)
    }
  
  # summary
  summary_kd <-
    if (mode == "sediment") {
      kd_data |>
        group_by(parameter) |>
        summarise(
          n_water    = sum(n_samples_water,    na.rm = TRUE),
          n_sediment = sum(n_samples_sediment, na.rm = TRUE),
          mean_kd    = mean(kd, na.rm = TRUE),
          median_kd  = median(kd, na.rm = TRUE),
          p25_kd     = quantile(kd, 0.25, na.rm = TRUE),
          p75_kd     = quantile(kd, 0.75, na.rm = TRUE),
          min_kd      = min(kd, na.rm = TRUE),
          max_kd      = max(kd, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      kd_data |>
        group_by(parameter) |>
        summarise(
          n_dissolved = sum(n_samples_Dissolved, na.rm = TRUE),
          n_suspended = sum(n_samples_Suspended, na.rm = TRUE),
          mean_kd     = mean(kd, na.rm = TRUE),
          median_kd   = median(kd, na.rm = TRUE),
          p25_kd      = quantile(kd, 0.25, na.rm = TRUE),
          p75_kd      = quantile(kd, 0.75, na.rm = TRUE),
          min_kd      = min(kd, na.rm = TRUE),
          max_kd      = max(kd, na.rm = TRUE),
          .groups = "drop"
        )
    }
  
  summary_kd = summary_kd |> arrange(desc(mean_kd))
  
  list(detailed = kd_data, summary = summary_kd)
}


##### Basic Workflow #####
# Note: Goal to make a function that takes in a parameter and gives a Kd, with sample size. 
#       - Can select suspended water & sediment, or water dissolved & suspended
#       - Can specify a parameter and get 1 row, or run all and get all parameters

## Start by prepping our data by correcting our units for this analysis
standardized_data <- bol_media_scored |>
  filter(tolower(unit) %in% c("mg/l", "ug/l", "mg/kg", "ug/kg", "%"), 
         !(media == "water" & unit == "%")) |>  # Good unit filters
  filter((media == "water" & fraction == "Suspended") | media == "sediment") |> 
  mutate(
    conc = case_match(tolower(unit),
                      "%" ~ concentration * 1e4,     
                      "ug/kg" ~ concentration / 1e3, 
                      "ug/l" ~ concentration / 1e3,  
                      .default = concentration),
    units = case_match(tolower(unit),
                       "%" ~ "mg/kg",
                       "ug/kg" ~ "mg/kg", 
                       "ug/l" ~ "mg/l",
                       "mg/l" ~ "mg/l",
                       .default = unit),
    conc = if_else(conc == 0, 1e-12, conc)  # Fix 0 concentrations to build reporting size -- it was sampled but not detected. 
  )

# Keep counts for each media
counts_data <- standardized_data |> 
  count(station, date, parameter, media, name = "n_samples")

# Get the mean concentration for each media
means_data <- standardized_data |> 
  group_by(station, date, parameter, media) |> 
  summarise(
    mean_conc = mean(conc, na.rm = TRUE),  # Now standardized mg/L or mg/kg
    unit_check = paste(unique(units), collapse = ", "),  # Standardized units
    .groups = "drop"
  )

# Combine datasets to get Kd for each station-date-parameter set
water_kd_data <- means_data |> 
  left_join(counts_data, by = c("station", "date", "parameter", "media")) |> 
  pivot_wider(
    names_from = media, 
    values_from = c(n_samples, mean_conc, unit_check),
    names_glue = "{media}_{.value}",
    values_fill = list(n_samples = 0, mean_conc = NA, unit_check = NA)
  ) |> 
  mutate(kd = sediment_mean_conc / water_mean_conc) |> 
  filter(!is.na(kd), water_mean_conc > 0, sediment_mean_conc > 0) |> 
  select(station, date, parameter, 
         starts_with("water_"), starts_with("sediment_"), kd)

# get final Kd values for each parameter
summary_kd = water_kd_data |>
  group_by(parameter) |>
  summarize(n_water = sum(water_n_samples, na.rm=TRUE),
            n_sediment = sum(sediment_n_samples, na.rm=TRUE),
            mean_kd = mean(kd, na.rm=TRUE),
            median_kd = median(kd, na.rm=TRUE),
            min_kd = min(kd, na.rm=TRUE),
            max_kd = max(kd, na.rm=TRUE)
  )