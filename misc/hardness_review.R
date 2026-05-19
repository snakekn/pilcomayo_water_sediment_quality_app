### rough: view how hardness has changed over time by station
bol_water_scored |>
  filter(parameter == "hardness") |>
  ggplot(aes(x=date,y=concentration,color=station))+
  geom_line()+
  theme_minimal()+
  labs(title="Water Hardness",
       x=expression(paste("Hardness (mg/L ", CaCO[3], ")")),
       y="Concentration")

### Classify hardness into categories for each sample
hardness_classified <- bol_water_scored |>
  filter(parameter == "hardness") |>
  mutate(
    hardness_category = case_when(
      concentration < 60 ~ "Soft (<60)",
      concentration < 120 ~ "Moderately Hard (60-120)",
      concentration < 180 ~ "Hard (120-180)",
      TRUE ~ "Very Hard (>180)"
    ),
    hardness_category = factor(hardness_category, 
                               levels = c("Soft (<60)", "Moderately Hard (60-120)", 
                                          "Hard (120-180)", "Very Hard (>180)"))
  )

# Calculate frequency distribution by station
hardness_frequency <- hardness_classified |>
  group_by(station, hardness_category) |>
  summarize(n = n(), .groups = "drop_last") |>
  mutate(
    total = sum(n),
    percentage = round(100 * n / total, 1)
  ) |>
  ungroup() |>
  mutate(hardness_category = factor(hardness_category, 
                                    levels = c("Soft (<60)", "Moderately Hard (60-120)", 
                                               "Hard (120-180)", "Very Hard (>180)")))

print(hardness_frequency)

# Create stacked bar chart
ggplot(hardness_frequency, 
       aes(x = reorder(station, desc(station)), y = percentage, fill = reorder(hardness_category, desc(hardness_category)))) +
  geom_col() +
  scale_fill_manual(
    values = c("Soft (<60)" = "#FEE0D2",
               "Moderately Hard (60-120)" = "#FCBBA1",
               "Hard (120-180)" = "#FC9272",
               "Very Hard (>180)" = "#CB181D"),
    breaks = c("Soft (<60)", "Moderately Hard (60-120)", 
               "Hard (120-180)", "Very Hard (>180)"),
    drop = FALSE  # Don't drop unused levels
  ) +
  coord_flip() +
  labs(
    title = expression("Water Hardness Distribution by Station (as"~CaCO[3]*")"),
    #subtitle = "Percentage of samples in each hardness category",
    x = "Station",
    y = "Percentage of Samples (%)",
    fill = expression("Hardness Category (using mg/L"~CaCO[3]*")")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
