#' Calculate Kd values from water/sediment data
#' 
#' @param data Data frame with `station`, `date`, `parameter`, `media`, `fraction`, 
#'   `concentration`, `unit` columns
#' @param mode "sediment" (water vs sediment) or "water" (dissolved vs suspended)  
#' @param parameter Single parameter name or NULL (all parameters)
#' 
#' @return List: `detailed` (station-level), `summary` (parameter stats)
#' 

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
      # conc = if_else(conc == 0, 1e-12, conc)  # Fix 0 concentrations to build reporting size -- it was sampled but not detected.
    ) |>
    filter(conc!=0) # let's try where no 0 data is analyzed to see if our data comes back more normal...
  
  # counts
  counts_data <- filt_data |>
    count(station, date, parameter, !!sym(pivot_col), name = "n_samples")
  
  # grab per-loc/day/param data
  means_data <- filt_data |>
    group_by(station, date, parameter, !!sym(pivot_col)) |>
    summarise(
      mean_conc  = mean(conc, na.rm = TRUE),
      max_conc = max(conc, na.rm=TRUE),
      median_conc = median(conc, na.rm=TRUE),
      unit_check = paste(unique(units), collapse = ", "),
      n=n(),
      .groups = "drop"
    )
  
  # View(means_data)
  
  # join + pivot
  joined <- means_data |>
    left_join(counts_data, by = c("station", "date", "parameter", pivot_col))
  
  kd_data <-
    if (mode == "sediment") {
      joined |>
        pivot_wider(
          id_cols   = c(station, date, parameter),   # <- important
          names_from  = media,
          values_from = c(median_conc, n_samples, unit_check),
          # result: mean_conc_water, mean_conc_sediment, n_samples_water, ...
          names_sep = "_"
        ) |>
        drop_na() |>
        mutate(
          kd = median_conc_sediment / median_conc_water
        ) 
      
      #|>
      #  filter(!is.na(kd))
#        filter(!is.na(kd), median_conc_water > 0, median_conc_sediment > 0)
      
    } else {  # mode == "water"
      joined |>
        pivot_wider(
          id_cols   = c(station, date, parameter),   # <- important
          names_from  = fraction,
          values_from = c(median_conc, n_samples, unit_check),
          # e.g. mean_conc_Dissolved, mean_conc_Suspended
          names_sep = "_"
        ) |>
        drop_na() |>
        mutate(
          kd = median_conc_Suspended / median_conc_Dissolved
        ) 
      }
  
  # summary
  summary_kd <-
    if (mode == "sediment") {
      kd_data |>
        group_by(parameter) |>
        summarise(
          n_pairs=n(),
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
          n_pairs = n(),
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
  
  summary_kd = summary_kd |> arrange(desc(median_kd))
  
  return(list(detailed = kd_data, summary = summary_kd))
}

#### Create boxplots using both methods ####
kd_boxplots = function() {
  ### Get Kd values
  # Suspended (your current mode="sediment")
  kd_water <- calculate_kd(bol_media_scored, mode = "water")
  kd_susp = kd_water$detailed

  # Sediments (River Basin) (assuming mode="sediment" but different data/stations—adjust as needed)
  kd_sed <- calculate_kd(bol_media_scored, mode = "sediment")  # Or filter differently
  kd_basin = kd_sed$detailed
  
  library(broom); library(rstatix)
  wilcox_results <- kd_compare |>
    nest(data = -parameter) |>
    mutate(test = map(data, ~ wilcox.test(kd ~ method, data = .x))) |>
    mutate(test = map(test, broom::tidy)) |>
    unnest(test) |>
    mutate(p.adj = p.adjust(p.value, "BH"),
           signif = case_when(p.adj < 0.001 ~ "***", p.adj < 0.01 ~ "**", 
                              p.adj < 0.05 ~ "*", TRUE ~ "ns"))
  
  
  # Join n_pairs, format labels
  n_labels_df <- full_join(
    kd_water$summary |> transmute(parameter, susp_n = n_pairs),  # Or your n_pairs col
    kd_sed$summary |> transmute(parameter, dep_n = n_pairs),
    by = "parameter"
  ) |>
    left_join(
      wilcox_results |> select(parameter, signif),  # From previous test
      by = "parameter"
    ) |>
    mutate(
      dep_n = replace_na(dep_n, 0),
      susp_n = replace_na(susp_n, 0),
      signif = replace_na(signif, ""),
      signif = if_else(signif == "ns", "", signif),  # Hide ns
      label = sprintf("%s\n(n=%d/%d) %s", parameter, dep_n, susp_n, signif),
      .before = 1
    )
  
  # Add method label
  kd_compare <<- bind_rows(
    kd_susp |> mutate(method = "Suspended Sediment"),
    kd_basin |> mutate(method = "Deposited Sediment")
  ) |>
    filter(!is.na(kd)) |>  # Clean NAs
    mutate(
      parameter = factor(parameter),
      method = factor(method, levels = c("Suspended Sediment", "Deposited Sediment"))  # Order
    )
  
  
  # wilcox_results <- kd_compare |>
  #   group_by(parameter) |>
  #   mutate(
  #     med_diff = median(kd[method == "Suspended Sediment"]) - 
  #       median(kd[method == "Deposited Sediment"])
  #   ) |>
  #   wilcox_test(kd ~ method, detailed = TRUE) |>  # Wilcoxon rank-sum
  #   adjust_pvalue(method = "BH") |>  # Multiple testing correction
  #   add_significance("p.adj") |>   # Stars: ***, **, *, ns
  #   ungroup()
  
  library(ggplot2); library(dplyr); library(patchwork)
  
  # Order params by median kd (suspended)
  param_order <- kd_compare |>
    group_by(parameter) |>
    summarise(med_kd = median(kd, na.rm = TRUE)) |>
    arrange(desc(parameter)) |>
    pull(parameter)
  
  kd_compare <- kd_compare |>
    mutate(parameter = factor(parameter,  levels = param_order))
  
  param_labels <- setNames(n_labels_df$label, n_labels_df$parameter)
  
  p <- ggplot(kd_compare, aes(x = parameter, y = kd, fill = method)) +
    geom_boxplot(position = position_dodge(0.8), 
                 alpha = 0.7, linewidth = 0.7, fatten = 1.2,
                 outlier.shape = 16, outlier.size = 1.2) +
    stat_summary(fun = median, geom = "point", position = position_dodge(0.8),
                 size = 3, shape = 18, color = "darkred") +  # Median diamonds
    coord_flip() +  # Horizontal for params
    scale_y_log10(labels = scales::label_scientific()) +
    scale_fill_manual(values = c("Suspended Sediment" = "#56B4E9", # blue bc water
                                 "Deposited Sediment" = "#E69F00")) + # orange bc soil
    labs(x = "Parameter", y = expression(~K[d]*" (L/kg)"),
         title = expression("Empirical Partitioning Ratio ("~K[d]*") in Pilcomayo Basin"),
         fill = "Method") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(size = 10),
          panel.grid.major.y = element_blank())+
    scale_x_discrete(labels = param_labels)

  # Interactive
  ggplotly(p, tooltip = c("parameter", "method", "y"))
  
  ### 
  
  return(list(p=p,df=kd_compare))
  
}

##### Basic Workflow #####
# Note: Goal to make a function that takes in a parameter and gives a Kd, with sample size. 
#       - Can select suspended water & sediment, or water dissolved & suspended
#       - Can specify a parameter and get 1 row, or run all and get all parameters

# Start by prepping our data by correcting our units for this analysis
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
    names_glue = "{.value}_{media}",
    values_fill = list(n_samples = 0, mean_conc = NA, unit_check = NA)
  ) |>
  mutate(kd = mean_conc_sediment / mean_conc_water) |>
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