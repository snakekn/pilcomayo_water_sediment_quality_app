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

#### use non-bol standards
get_samples_intl_class <- function(df) {
  intl_stds_wide <-water_stds |>
    filter(regulator %in% c("WHO", "EPA"), 
           unit != "u pH", 
           hqcr == "hq", 
           str_detect(media, "water")) |>
    mutate(
      value_mgL = case_when(
        tolower(unit) %in% c("ug/l", "µg/l") ~ value / 1000,
        tolower(unit) == "mg/l" ~ value,
        TRUE ~ value
      ),
      std_unit = tolower(unit)  # Keep original unit for conversion
    ) |>
    group_by(parameter) |>
    slice_min(value_mgL, n = 1, with_ties = FALSE) |>  # Strictest
    ungroup() |>
    select(regulator, parameter, intl_limit = value_mgL, std_unit)
  
  print("Standards shape:")
  print(dim(intl_stds_wide))
  print(names(intl_stds_wide))
  
  param_stds <- intl_stds_wide |> pull(parameter)
  
  # filter out data that we don't care about
  non_chemical <- c("C", "CFU", "MPN", "Ohm.cm", "Sal", "T", "UNT", 
                    "m/s", "m3/s", "mV", "%", "mm")
  
  df_new <- df |>
    left_join(intl_stds_wide, by = "parameter") |>
    filter(parameter %in% param_stds, 
           parameter != "Color",
           !unit %in% non_chemical,
           str_detect(media, "water")) |>
    mutate(
      # VECTORIZED conversion (fast!)
      factor = case_when(
        tolower(unit) == "ug/l" & tolower(std_unit) == "mg/l" ~ 1000,
        tolower(unit) == std_unit ~ 1,
        TRUE ~ NA_real_
      ),
      concentration_std = case_when(
        is.na(factor) ~ NA_real_,
        TRUE ~ concentration / factor
      ),
      passes_intl = concentration_std <= intl_limit,
      class = case_when(
        is.na(concentration_std) ~ "Unit Error",
        passes_intl ~ "Pass",
        TRUE ~ "Fail"
      )
    ) |> 
    select(regulator, parameter, station, date, media, fraction, 
           concentration, unit, class, everything())
  
  print("Classified samples:")
  print(table(df_new$class))
  return(df_new)
}


get_frac_intl_class <- function(df) {
  param_source <- df |> 
    distinct(parameter, regulator) |>
    mutate(reg_label = sprintf(" (%s)", toupper(regulator)))
  
  frac_classes <- df |>
    count(parameter, class) |>
    group_by(parameter) |>
    mutate(pct = n / sum(n),
           param_n = sum(n)) |>
    ungroup()
  
  # Overall
  total_pct <- df |>
    count(class) |>
    mutate(parameter = "TOTAL", 
           pct = n / sum(n),
           param_n = sum(n))
  
  parameter_class_with_total <- bind_rows(frac_classes, total_pct)

  # Order by quality (more Pass = better)
  param_order <- frac_classes |>
    group_by(parameter) |>
    summarize(quality_score = sum(pct[class == "Pass"]), .groups = "drop") |>
    arrange(quality_score) |>
    pull(parameter)
  
  param_levels <- c("TOTAL", param_order)

  param_labels <- frac_classes %>%
    group_by(parameter) %>%
    summarize(n=sum(n), .groups="drop") |>
    left_join(param_source, by="parameter") |>
    mutate(reg_label = coalesce(reg_label, " (?)"),
           label = sprintf("%s%s (n=%d)", parameter, reg_label, n)) |>
    add_row(parameter = "TOTAL", n = sum(frac_classes$n), label = sprintf("%s (n=%d)", parameter, n)) |>  # <-- add TOTAL row
    select(parameter, label) |>
    deframe()
  
  # PLOT: Simplified 2-color
  ggplot(parameter_class_with_total, 
         aes(x = factor(parameter, levels=(param_levels)), 
             y = pct * 100, fill = fct_rev(class))) +
    geom_col(position = position_stack(reverse = TRUE)) +
    scale_x_discrete(labels = param_labels) +
    scale_fill_manual(
      values = c("Pass" = "#2E7D32", "Fail" = "#B71C1C"),   
      name = "Status"
    ) +
    coord_flip() +
    labs(
      title = "Water Quality Compliance by Contaminant Compared Against EPA & WHO Standards",
      x = "", y = "Percentage of Samples (%)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_text(face = ifelse(param_levels == "TOTAL", "bold", "plain"))) # weird thing because it's flipped. pick the farthest one to get total
}

#### get parameters not currently covered by standards
# Parameters in your water data WITHOUT non-Bolivian (WHO/EPA) standards

missing_stds <- bol_water_scored |>
  # Unique parameters in your water data
  distinct(parameter) |>
  # Non-Bolivian standards available
  anti_join(
    stds |>
      filter(!str_detect(regulator, "Bol"), 
             unit != "u pH", 
             hqcr == "hq", 
             str_detect(media, "water")) |> 
      distinct(parameter),
    by = "parameter"
  ) |>
  pull(parameter) |>
  sort()

print("Water parameters MISSING intl standards:")
print(missing_stds)

#### Compare BOL & non-bol standards
std_comparison <- water_stds |>
  filter(
    hqcr == "hq",
    str_detect(media, "water")
  ) |>
  mutate(
    regulator = as.factor(regulator),
    value_norm = case_when(
      tolower(unit) == "ug/l" ~ value / 1000,  # Convert to mg/L
      tolower(unit) == "mg/l" ~ value,
      TRUE ~ NA_real_
    ),
    std_class = ifelse(str_detect(regulator, "Bol"), "BOL", "INTL")
  ) |>
  filter(!is.na(value_norm)) |>  # Only comparable
  distinct(parameter, std_class, value_norm, regulator, .keep_all = TRUE) |> #get one line per value (many in BOL bc classes)
  # Take strictest per regulator-parameter
  group_by(std_class, parameter) |>
  slice_min(value_norm, n = 1, with_ties = FALSE) |>
  ungroup() |>
  tidyr::complete(std_class, parameter, fill = list(value_norm = NA_real_)) |>
  # Compare pairwise
  pivot_wider(names_from = std_class, values_from = value_norm) |>
  group_by(parameter) |>
  summarize(BOL = min(BOL, na.rm=TRUE),
            INTL = min(INTL, na.rm=TRUE),
            .groups = "drop") |>
  filter(BOL != Inf, INTL != Inf) |>
    mutate(
    bol_limit = BOL,
    intl_limit = INTL,
    ratio = if_else(
      !is.na(bol_limit) & !is.na(intl_limit),
      bol_limit / intl_limit,
      NA_real_
    ),
    stricter = case_when(
      is.na(bol_limit) & !is.na(intl_limit) ~ "Only INTL",
      !is.na(bol_limit) & is.na(intl_limit) ~ "Only BOL",
      is.na(ratio) ~ "Missing",
      ratio > 1 ~ sprintf("INTL (x%.1f)", round(ratio, 1)),
      ratio < 1 ~ sprintf("BOL (x%.1f)", round(1/ratio, 1)),
      TRUE ~ "Equal"
    )
  ) |>
  ungroup() |>
  select(parameter, BOL = bol_limit, INTL = intl_limit, stricter, ratio) |>
  arrange(desc(coalesce(ratio, 0)))

# Pretty table
library(gt)
std_comparison |>
  gt() |>
  fmt_number(columns = c(Bolivia, Intl, Ratio), decimals = 3) |>
  tab_header(
    title = "Bolivia vs International Drinking Water Standards",
    subtitle = "Lower limit = stricter | Intl = min(WHO, EPA)"
  ) |>
  cols_label(
    Stricter = "Stricter Standard",
    Ratio = "Intl/Bolivia"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )
