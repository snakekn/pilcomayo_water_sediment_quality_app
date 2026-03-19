### Determine days where all indicator contaminants spiked
spike_detection <- bol_media_scored |> 
  filter(media == "water") |>
  filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
  select(station, date, parameter, concentration) |>
  group_by(parameter) |>
  mutate(
    percentile = percent_rank(concentration),
    station_mean = mean(concentration, na.rm = TRUE),
    station_sd = sd(concentration, na.rm = TRUE),
    z_score = (concentration - station_mean) / station_sd
  ) |>
  ungroup() |>
  group_by(station, date) |>
  filter(n_distinct(parameter) == 3) |>
  summarize(
    avg_percentile = mean(percentile),
    min_percentile = min(percentile),
    n_high = sum(percentile > 0.85),  # How many parameters are in top 25%
    all_high = all(percentile > 0.85),  # All 3 are high
    parameters_measured = n(),
    ammonia_conc = concentration[parameter == "Ammonia"],
    bod_conc = concentration[parameter == "BOD"],
    fecal_conc = concentration[parameter == "Fecal coliforms"],
    # Extract individual percentiles
    ammonia_pct = percentile[parameter == "Ammonia"],
    bod_pct = percentile[parameter == "BOD"],
    fecal_pct = percentile[parameter == "Fecal coliforms"],
    .groups = "drop"
  ) |>
  arrange(station, desc(avg_percentile))

# only view when all are high
concurrent_spikes <- spike_detection |>
  filter(all_high == TRUE) |>
  arrange(station, date)

##### what percent of instances sampled surpassed regulatory limits? #####
# these are the BOL Law 1333 Regulations, but they aren't used. 
regulatory_limits_bol <- data.frame(
  parameter = c("Ammonia", "BOD", "Fecal coliforms"),
  class_A = c(0.05, 2, 200),
  class_B = c(1, 5, 1000),
  class_C = c(2, 20, 5000),
  class_D = c(4, 30, 10000)
)

# classify each score based on all 3 parameters
water_class_classification <- bol_media_scored |>
  filter(media == "water") |>
  filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
  group_by(station, date) |>
  filter(n_distinct(parameter) == 3) |>  # Only dates with all 3 measured
  summarize(
    ammonia = concentration[parameter == "Ammonia"],
    bod = concentration[parameter == "BOD"],
    fecal = concentration[parameter == "Fecal coliforms"],
    .groups = "drop"
  ) |>
  mutate(
    # Check if meets each class (all 3 parameters must be within limits)
    meets_A = ammonia <= 0.01 & bod <= 2 & fecal <= 200,
    meets_B = ammonia <= 0.1 & bod <= 5 & fecal <= 1000,
    meets_C = ammonia <= 1.0 & bod <= 20 & fecal <= 5000,
    meets_D = ammonia <= 2.0 & bod <= 30 & fecal <= 10000,
    
    # Assign the best class achieved (most restrictive that passes)
    water_class = case_when(
      meets_A ~ "A",
      meets_B ~ "B",
      meets_C ~ "C",
      meets_D ~ "D",
      TRUE ~ "Beyond D"  # Fails even minimum quality
    )
  ) |>
  arrange(station, date)

# get total counts
station_class_frequency <- water_class_classification |>
  group_by(station, water_class) |>
  summarize(n = n(), .groups = "drop_last") |>
  mutate(
    total = sum(n),
    percentage = round(100 * n / total, 1)
  ) |>
  ungroup() |>
  # Make sure water_class is ordered
  mutate(water_class = factor(water_class, levels = c("A", "B", "C", "D", "Beyond D"))) |>
  arrange(station, water_class) |>
  select(station, water_class, n, percentage)

# get overall quality conditions so it's easier for people to compare
overall_totals <- water_class_classification |>
  count(water_class) |>
  mutate(
    station = "TOTAL",
    total = sum(n),
    percentage = round(100 * n / total, 1)
  ) |>
  select(station, water_class, n, percentage)

# combine into station data
station_class_with_total <- station_class_frequency |>
  select(station, water_class, n, percentage) |>
  bind_rows(overall_totals) |>
  mutate(water_class = factor(water_class,
                              levels = rev(c("A", "B","C","D","Beyond D")))
  )

# help reorder the stations based on water quality
station_order <- station_class_frequency |>
  group_by(station) |>
  summarize(
    quality_score = sum(case_when(
      water_class == "A" ~ percentage,           # Reward %A
      water_class == "B" ~ percentage * 0.5,     # Slight reward
      water_class == "C" ~ percentage * 0.1,     # Penalty
      water_class == "D" ~ percentage * 0.05,     # Heavy penalty  
      water_class == "Beyond D" ~ 0              # Worst
    )),
    .groups = "drop"
  ) |>
  arrange(desc(quality_score)) |>
  pull(station)

# add total to the lineup
station_levels <- rev(c(station_order, "TOTAL"))

# plot!
ggplot(station_class_with_total, 
       aes(x = factor(station, levels=station_levels), 
           y = percentage, 
           fill = water_class)) +
  geom_col(position = "stack") +
  scale_fill_manual(
    values = c("A" = "#2E7D32",
               "B" = "#81C784", 
               "C" = "#FFB74D", 
               "D" = "#E57373",
               "Beyond D" = "#B71C1C"
    ),
    breaks = c("A", "B", "C", "D", "Beyond D")
  ) +
  coord_flip() +
  labs(
    title = "Bolivian Law 1333 Class Distribution by Station",
    subtitle = "Percentage of samples at the station in each quality class",
    x = "Station",
    y = "Percentage of Samples (%)",
    fill = "Water Class"
  ) +
  theme_minimal(base_size=16) +
  theme(legend.position = "bottom",
        axis.text.y = element_text(
          face = ifelse(station_levels == "TOTAL", "bold", "plain")
        ))

#### wastewater indicator analysis: intl ####

regulatory_limits_intl = list(fecal = 0,
                              ammonia = 17, 
                              bod = 30)

wastewater_indication = function(data, limits) {
  result = list()
  ## classify each station-date whether it's over the limit across all parameters
  data_class = data |>
    filter(media == "water") |>
    filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
    group_by(station, date) |>
    filter(n_distinct(parameter) == 3) |>  # Only dates with all 3 measured
    summarize(
      ammonia = concentration[parameter == "Ammonia"],
      bod = concentration[parameter == "BOD"],
      fecal = concentration[parameter == "Fecal coliforms"],
      .groups = "drop"
    ) |>
    mutate(
      within_limits = ammonia <= limits["ammonia"] & bod <= limits["bod"] & fecal <= limits["fecal"],
      class = ifelse(within_limits, "Compliant", "Non-Compliant")
    ) |>
    arrange(station, date)
  
  result$detailed = data_class # for easy return
  
  ## get total counts for each station
  station_frequency = data_class |>
    group_by(station, class) |>
    summarize(n = n(), .groups = "drop_last") |>
    mutate(
      total = sum(n),
      percentage = round(100 * n / total, 1)
    ) |>
    ungroup() |>
    # Make sure water_class is ordered
    mutate(class = factor(class, levels = c("Compliant", "Non-Compliant"))) |>
    arrange(station, class) |>
    select(station, class, n, percentage)
  
  result$station_summary = station_frequency # for easy return
  
  ## get overall quality conditions
  overall_frequency = data_class |>
    count(class) |>
    mutate(
      station = "TOTAL",
      total = sum(n),
      percentage = round(100 * n / total, 1)
    ) |>
    select(station, class, n, percentage)
  
  result$overall_summary = overall_frequency # for easy return :)
  
  ## combine into station data
  station_class_with_total = station_frequency |>
    select(station, class, n, percentage) |>
    bind_rows(overall_frequency) |>
    mutate(class = factor(class,
                          levels = c("Non-Compliant", "Compliant")),
           station_label = ifelse(station == "TOTAL",
                                  paste0("**",station,"**"),
                                  station)
    )
  
  result$class_total = station_class_with_total
  
  ## plot results
  # reorder stations based on water quality
  station_order <- station_frequency |>
    group_by(station) |>
    summarize(
      quality_score = sum(percentage * case_when(
        class == "Compliant" ~ 0,
        class == "Non-Compliant" ~ 1
      )),
      .groups = "drop"
    ) |>
    arrange(quality_score) |>
    pull(station)
  
  # add total to lineup
  station_levels <- c("TOTAL", station_order)
  
  g = ggplot(station_class_with_total, 
             aes(x = factor(station, levels=rev(station_levels)), 
                 y = percentage, 
                 fill = class)) +
    geom_col() +
    scale_fill_manual(
      values = c("Non-Compliant" = "#E57373",
                 "Compliant" = "#2E7D32"),
      guide = "legend"
      #breaks = c("Compliant", "Non-Compliant")
    ) +
    coord_flip() +
    labs(
      title = "WHO & USEPA Wastewater Contamination Guidelines by Station",
      subtitle = "Percentage of samples at the station within recommended limits",
      x = "Station",
      y = "Percentage of Samples (%)",
      fill = "Compliance"
    ) +
    theme_minimal(base_size=16) +
    theme(legend.position = "bottom",
          # axis.text.y = element_text(face = "plain", size = 12),
          axis.text.y = element_text(
            face = ifelse(rev(station_levels) == "TOTAL", "bold", "plain")
          )
    )
  result$g = g # add to result
  
  ## get station compliance rates
  station_compliance = station_frequency |>
    pivot_wider(
      names_from = class,
      values_from = c(n, percentage),
      names_sep = "_",
      values_fill = 0
    ) |>
    mutate(
      total_samples = `n_Non-Compliant` + `n_Compliant`,
      compliance_pct = round(100 * `n_Compliant` / total_samples, 1)
    ) |>
    select(station, 
           compliant_count = `n_Compliant`,
           non_compliant_count = `n_Non-Compliant`,
           compliance_pct,
           total_samples) |>
    arrange(desc(compliance_pct))
  
  result$station_compliance = station_compliance
  
  print(sprintf("Total date-station samples reviewed: %g", sum(station_compliance$total_samples)))
  return(result)
}
