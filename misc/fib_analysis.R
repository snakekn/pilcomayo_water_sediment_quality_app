##### GOALS #####
#' 1. Gather data on 3 parameters of interest
#' 2. Determine which stations have the worst impacts of wastewater

#### OLD CODE #####
### 3 graphs with plotly dates
g = bol_water_scored |> 
  filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
  group_by(station, parameter) |>
  arrange(concentration) |>
  slice(ceiling(0.95 * n())) |>
  summarize(
    pct95 = concentration,
    pct95_date = date,
    n_samples = n(),
    .groups = "drop"
  ) |>
  mutate(parameter = case_when(
    parameter == "Ammonia" ~ "Ammonia (mg/L)",
    parameter == "BOD" ~ "BOD (mg/L)",
    parameter == "Fecal coliforms" ~ "Fecal coliforms (CFU or MPN/100mL)",
    TRUE ~ parameter
  )) |>
  ggplot(aes(x=station, y=pct95, fill=parameter, text = paste0("Station: ", station, "\n",
                                                               "Parameter: ", parameter, "\n",
                                                               "Concentration: ", round(pct95, 2), "\n",
                                                               "Date: ", pct95_date, "\n",
                                                               "Samples: ", n_samples)))+
  geom_col(position="dodge")+
  facet_wrap(~parameter, scales="free_y", ncol=1)+
  labs(title="Wastewater Contamination Indicators per Station",
       x = "Station",
       y="Concentration (95th %ile)",
       fill="Parameter")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
    )+
  scale_y_log10()

ggplotly(g, tooltip = "text")


### initial 3 graphs for each station
# issue: dates aren't factored in
g = bol_water_scored |> 
  filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
  group_by(station, parameter) |>
  arrange(concentration) |>
  slice(ceiling(0.95 * n())) |>
  summarize(
    pct95 = concentration,
    pct95_date = date,
    n_samples = n(),
    .groups = "drop"
  )  
     

ggplotly(g)


### graph for each date shown. biiit too small to make anything out
time_series_data <- bol_water_scored |> 
  filter(parameter %in% c("Ammonia", "BOD", "Fecal coliforms")) |>
  mutate(
    parameter = case_when(
      parameter == "Ammonia" ~ "Ammonia (mg/L)",
      parameter == "BOD" ~ "BOD (mg/L)",
      parameter == "Fecal coliforms" ~ "Fecal coliforms (CFU or MPN/100mL)",
      TRUE ~ parameter
  ))
g <- ggplot(time_series_data, aes(x = date, y = concentration, color = parameter)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~station, ncol = 1, scales = "free_y") +
  labs(
    title = "Water Quality Parameters Over Time by Station",
    x = "Date",
    y = "Concentration",
    color = "Parameter"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11)
  ) +
  scale_y_log10()  # Use log scale if concentrations vary widely

ggplotly(g)

### get correlations for each parameter
spike_detection <- bol_water_scored |> 
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
#set regulatory limits - not used
regulatory_limits <- data.frame(
  parameter = c("Ammonia", "BOD", "Fecal coliforms"),
  class_A = c(0.01, 2, 200),
  class_B = c(0.1, 5, 1000),
  class_C = c(1.0, 20, 5000),
  class_D = c(2.0, 30, 10000)
)

# classify each score based on all 3 parameters
water_class_classification <- bol_water_scored |>
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
                              levels = c("A", "B","C","D","Beyond D"))
  )

# help reorder the stations based on water quality
station_order <- station_class_frequency |>
  group_by(station) |>
  summarize(
    quality_score = sum(percentage * case_when(
      water_class == "A" ~ 1,
      water_class == "B" ~ 2,
      water_class == "C" ~ 3,
      water_class == "D" ~ 4,
      water_class == "Beyond D" ~ 5,
      TRUE ~ 6
    )),
    .groups = "drop"
  ) |>
  arrange(quality_score) |>
  pull(station)

# add total to the lineup
station_levels <- c("TOTAL", station_order)

# plot!
ggplot(station_class_with_total, 
       aes(x = factor(station, levels=station_levels), 
           y = percentage, 
           fill = water_class)) +
  geom_col() +
  scale_fill_manual(
    values = c("A" = "#2E7D32", "B" = "#81C784", 
               "C" = "#FFB74D", "D" = "#E57373", "Beyond D" = "#B71C1C"),
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
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.y = element_text(
          face = ifelse(station_levels == "TOTAL", "bold", "plain")
        ))