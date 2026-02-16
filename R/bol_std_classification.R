# give each sample the resulting class
get_samples_bol_class = function(df) {
  bol_stds_wide = stds |>
    filter(regulator == "Bolivian Law 1333", unit!="u pH") |>
    group_by(parameter, limit, tolower(unit)) |>
    slice_head(n=1)|>
    ungroup()|>
    rename(std_unit = `tolower(unit)`) |>
    select(parameter, limit, value, std_unit) |>
    pivot_wider(names_from = limit, 
                values_from = value,
                names_prefix = "limit_") |>
    janitor::clean_names() |>
    drop_na() |> # get rid of any that only have Class A standards (Oxygen Sat, pH, BOD. BOD5 is complete so no worries)
    mutate(parameter = if_else(parameter == "BOD5", "BOD", parameter))
  
  param_stds = bol_stds_wide |> select(parameter) |> unique() |> pull(parameter)
  
  df_new = df |>
    left_join(bol_stds_wide, by="parameter") |>
    filter(parameter %in% param_stds) |> # only get ones with BOL classification? or just don't color
    filter(parameter != "Color")|> # also, nobody cares about color
    mutate(
      # standardize units
      conversion_info=map2(unit, std_unit, ~compare_units(.x,.y)),
      conversion_factor = map_dbl(conversion_info, ~.x$conversion_factor),
      convertible = map_lgl(conversion_info, ~.x$convertible),
      concentration_std = if_else(convertible,
                                  concentration/conversion_factor,
                                  concentration),
      # get classes
      meets_A = concentration_std <= limit_class_a,
      meets_B = concentration_std <= limit_class_b,
      meets_C = concentration_std <= limit_class_c,
      meets_D = concentration_std <= limit_class_d,
      # Classify to the best class met
      class = case_when(
        !convertible ~ "Incompatible Units",
        meets_A ~ "A",
        meets_B ~ "B", 
        meets_C ~ "C",
        meets_D ~ "D",
        TRUE ~ "Beyond D"
      )
    ) |> 
    select(-c(meets_A, meets_B, meets_C, meets_D, conversion_info, conversion_factor, convertible))
}

# determine the percentage of each classification for the dataset
get_frac_bol_class = function(df) {
  frac_classes = df |>
    count(parameter, class) %>%
    group_by(parameter) %>%
    mutate(n=n, pct = (n / sum(n))) %>%
    ungroup() %>%
    select(parameter, class, n, pct)
  
  # get overall quality conditions so it's easier for people to compare
  total_pct <- df |>
    count(class) |>
    mutate(
      parameter = "TOTAL",
      pct = n / sum(n)
    ) |>
    select(parameter, class, n, pct)
  
  # combine into station data
  parameter_class_with_total <- frac_classes |>
    select(parameter, class, n, pct) |>
    bind_rows(total_pct) |>
    mutate(class = factor(class,
                                levels = c("A", "B","C","D","Beyond D"))
    )
  
  
  # help reorder the stations based on water quality
  param_order <- frac_classes |>
    group_by(parameter) |>
    summarize(
      quality_score = sum(pct * case_when(
        class == "A" ~ 1,
        class == "B" ~ 2,
        class == "C" ~ 3,
        class == "D" ~ 4,
        class == "Beyond D" ~ 5,
        TRUE ~ 6
      )),
      .groups = "drop"
    ) |>
    arrange(desc(quality_score)) |>
    pull(parameter)
  
  # add total to the lineup
  param_levels <- c("TOTAL", param_order)
  
  # get sample size for each parameter
  param_labels <- bol_water_recent_class %>%
    group_by(parameter) %>%
    summarize(n=n(), .groups="drop") |>
    add_row(parameter = "TOTAL", n = nrow(bol_water_recent_class)) |>  # <-- add TOTAL row
    mutate(label = sprintf("%s (n=%d)", parameter, n)) %>%
    select(parameter, label) |>
    deframe()
  
  # plot!
  ggplot(parameter_class_with_total, 
         aes(x = factor(parameter, levels=param_levels), 
             y = pct*100, 
             fill = class)) +
    geom_col(position = position_stack(reverse=TRUE)) +
    scale_x_discrete(labels = param_labels) +  # <-- add this line
    scale_fill_manual(
      values = c("A" = "#2E7D32", "B" = "#81C784", 
                 "C" = "#FFB74D", "D" = "#E57373", "Beyond D" = "#B71C1C"),
      breaks = c("A", "B", "C", "D", "Beyond D")
    ) +
    coord_flip() +
    labs(
      title = "Bolivian Law 1333 Class Distribution by Parameter",
      subtitle = "Percentage of samples for that contaminant in each quality class",
      x = "Parameter",
      y = "Percentage of Samples (%)",
      fill = "Class"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(
            face = ifelse(param_levels == "TOTAL", "bold", "plain")
          ))
}
