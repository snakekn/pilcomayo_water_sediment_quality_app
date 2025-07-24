# visualization_functions.R
# River Remedy - Standardized visualization functions for contamination analysis
# Author: River Remedy Team  
# Date: 2025-01-16

# Load required libraries
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(scales)) install.packages("scales")
if (!require(viridis)) install.packages("viridis")
if (!require(patchwork)) install.packages("patchwork")
if (!require(plotly)) install.packages("plotly")
if (!require(leaflet)) install.packages("leaflet")
if (!require(DT)) install.packages("DT")

library(ggplot2)
library(scales)
library(viridis)
library(patchwork)
library(plotly)
library(leaflet)
library(DT)

# Source the standards loading script
if (file.exists("src/scripts/load_standards.R")) {
  source("src/scripts/load_standards.R")
}

# River Remedy standardized color palette
river_remedy_colors <- list(
  risk_levels = c(
    "Safe" = "#2E8B57",      # Sea Green
    "Moderate" = "#FFD700",   # Gold  
    "High" = "#FF6347",       # Tomato
    "Critical" = "#DC143C",   # Crimson
    "Extreme" = "#8B0000"     # Dark Red
  ),
  study_periods = c(
    "2006" = "#4A90E2",       # Blue
    "2011" = "#F5A623",       # Orange  
    "2024" = "#7ED321"        # Green
  ),
  matrices = c(
    "Water" = "#3498DB",
    "Soil" = "#8B4513", 
    "Sediment" = "#A0522D",
    "Vegetation" = "#228B22",
    "Fish" = "#4682B4",
    "Human Blood" = "#DC143C",
    "Animal Blood" = "#B22222"
  )
)

# Enhanced River Remedy theme
theme_river_remedy <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Text elements
      plot.title = element_text(
        hjust = 0.5, 
        size = rel(1.3), 
        face = "bold", 
        color = "#2c3e50",
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(
        hjust = 0.5, 
        size = rel(1.1), 
        color = "#7f8c8d",
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        hjust = 0, 
        size = rel(0.8), 
        color = "#95a5a6"
      ),
      
      # Axes
      axis.title = element_text(
        size = rel(1.0), 
        face = "bold",
        color = "#34495e"
      ),
      axis.text = element_text(
        size = rel(0.9),
        color = "#2c3e50"
      ),
      axis.text.x = element_text(
        angle = 45, 
        hjust = 1,
        vjust = 1
      ),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(
        size = rel(1.0), 
        face = "bold"
      ),
      legend.text = element_text(size = rel(0.9)),
      legend.box.margin = margin(t = 15),
      
      # Panels
      panel.grid.major = element_line(
        color = "#bdc3c7", 
        linewidth = 0.3
      ),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(
        color = "#95a5a6", 
        fill = NA, 
        linewidth = 0.5
      ),
      
      # Strips for facets
      strip.background = element_rect(
        fill = "#ecf0f1", 
        color = "white"
      ),
      strip.text = element_text(
        face = "bold", 
        size = rel(1.0),
        color = "#2c3e50"
      ),
      
      # Overall appearance
      plot.background = element_rect(
        fill = "white", 
        color = NA
      ),
      panel.background = element_rect(
        fill = "white", 
        color = NA
      )
    )
}

#' Create Multi-Standard Exceedance Plot
#' 
#' @param data Data frame with columns: metal, measured_value, location
#' @param standards Standards list from load_regulatory_standards()
#' @param standard_types Vector of standard types to compare
#' @param title Plot title
#' @return ggplot object
plot_standards_exceedance <- function(data, standards, 
                                      standard_types = c("who_water", "codex_food", "epa_soil"),
                                      title = "Standards Exceedance Analysis") {
  
  # Prepare data for plotting
  plot_data <- data %>%
    mutate(
      metal = str_to_title(metal),
      location_metal = paste(location, metal, sep = " - ")
    )
  
  # Calculate exceedances for WHO water standards (primary)
  exceedance_data <- plot_data %>%
    rowwise() %>%
    mutate(
      standard_value = get_standard_value(metal, "who_water", standards),
      exceedance_ratio = ifelse(is.na(standard_value), NA, measured_value / standard_value),
      risk_level = calculate_risk_level(measured_value, standard_value)
    ) %>%
    ungroup() %>%
    filter(!is.na(exceedance_ratio), exceedance_ratio > 0) %>%
    arrange(desc(exceedance_ratio))
  
  if (nrow(exceedance_data) == 0) {
    return(ggplot() + 
             labs(title = "No data available for plotting") + 
             theme_river_remedy())
  }
  
  # Create the plot
  p <- exceedance_data %>%
    ggplot(aes(x = reorder(location_metal, exceedance_ratio),
               y = exceedance_ratio,
               fill = risk_level)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 1) +
    scale_fill_manual(
      values = river_remedy_colors$risk_levels,
      name = "Risk Level"
    ) +
    scale_y_continuous(
      trans = "log10",
      labels = function(x) paste0(round(x, 1), "x"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = title,
      subtitle = "Exceedance ratios vs WHO drinking water standards (log scale)",
      x = "Location - Metal",
      y = "Exceedance Ratio (log scale)",
      caption = "Red dashed line = WHO limit | Higher values indicate greater contamination"
    ) +
    theme_river_remedy() +
    coord_flip()
  
  return(p)
}

#' Create Contamination Heatmap
#' 
#' @param data Data frame with columns: location, metal, measured_value
#' @param standards Standards list
#' @param standard_type Which standard to use for comparison
#' @param title Plot title
#' @return ggplot object
plot_contamination_heatmap <- function(data, standards, 
                                       standard_type = "who_water",
                                       title = "Contamination Heatmap") {
  
  # Calculate risk levels
  heatmap_data <- data %>%
    rowwise() %>%
    mutate(
      standard_value = get_standard_value(metal, standard_type, standards),
      risk_result = calculate_risk_level(measured_value, standard_value, include_ratio = TRUE),
      risk_level = risk_result$level,
      exceedance_ratio = risk_result$ratio
    ) %>%
    ungroup() %>%
    filter(!is.na(risk_level)) %>%
    # Ensure we have both location and metal data
    filter(!is.na(location), !is.na(metal))
  
  if (nrow(heatmap_data) == 0) {
    return(ggplot() + 
             labs(title = "No data available for heatmap") + 
             theme_river_remedy())
  }
  
  # Create heatmap
  p <- heatmap_data %>%
    ggplot(aes(x = str_to_title(metal), y = str_wrap(location, 15), fill = risk_level)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(
      aes(label = ifelse(exceedance_ratio > 1, 
                         paste0(round(exceedance_ratio, 1), "x"), "")),
      color = "white",
      size = 3,
      fontface = "bold"
    ) +
    scale_fill_manual(
      values = river_remedy_colors$risk_levels,
      name = "Risk Level"
    ) +
    labs(
      title = title,
      subtitle = paste("Based on", str_replace(standard_type, "_", " "), "standards"),
      x = "Heavy Metal",
      y = "Sampling Location",
      caption = "Numbers show exceedance ratios above regulatory limits"
    ) +
    theme_river_remedy() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
  
  return(p)
}

#' Create Interactive Contamination Map
#' 
#' @param data Data frame with lat, lng, location, and contamination data
#' @param standards Standards list
#' @param standard_type Standard type for risk assessment
#' @return Leaflet map object
create_contamination_map <- function(data, standards, standard_type = "who_water") {
  
  # Check if we have coordinate data
  if (!all(c("lat", "lng") %in% names(data))) {
    cat("⚠ No coordinate data (lat, lng) found. Adding example coordinates...\n")
    
    # Add example coordinates for Pilcomayo region
    data <- data %>%
      mutate(
        lat = -19.5723 + runif(n(), -0.1, 0.1),
        lng = -65.7550 + runif(n(), -0.1, 0.1)
      )
  }
  
  # Calculate overall risk for each location
  map_data <- data %>%
    group_by(location, lat, lng) %>%
    summarise(
      n_metals = n(),
      avg_exceedance = mean(get_exceedance_ratio(metal, measured_value, standard_type, standards), na.rm = TRUE),
      max_exceedance = max(get_exceedance_ratio(metal, measured_value, standard_type, standards), na.rm = TRUE),
      metals_above_limit = sum(get_exceedance_ratio(metal, measured_value, standard_type, standards) > 1, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(avg_exceedance)) %>%
    mutate(
      risk_category = case_when(
        avg_exceedance <= 1 ~ "Safe",
        avg_exceedance <= 2 ~ "Moderate",
        avg_exceedance <= 10 ~ "High", 
        avg_exceedance <= 100 ~ "Critical",
        TRUE ~ "Extreme"
      )
    )
  
  if (nrow(map_data) == 0) {
    cat("⚠ No data available for mapping\n")
    return(NULL)
  }
  
  # Create color palette
  pal <- colorFactor(
    palette = river_remedy_colors$risk_levels,
    domain = names(river_remedy_colors$risk_levels)
  )
  
  # Create map
  leaflet(map_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lng,
      lat = ~lat,
      radius = ~pmax(5, pmin(25, log10(avg_exceedance + 1) * 5)),
      color = ~pal(risk_category),
      fillOpacity = 0.8,
      stroke = TRUE,
      weight = 2,
      popup = ~paste(
        "<b>", location, "</b><br>",
        "Risk Category: ", risk_category, "<br>",
        "Average Exceedance: ", round(avg_exceedance, 2), "x<br>",
        "Max Exceedance: ", round(max_exceedance, 2), "x<br>",
        "Metals Above Limit: ", metals_above_limit, " of ", n_metals
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = ~risk_category,
      title = "Contamination Risk"
    )
}

#' Create Temporal Comparison Plot
#' 
#' @param data_list List of data frames for different time periods, or single data frame
#' @param metal Metal to analyze
#' @param standards Standards list
#' @param standard_type Standard type for comparison
#' @return ggplot object
plot_temporal_comparison <- function(data_list, metal = "Lead", standards, standard_type = "who_water") {
  
  # Handle both list input and single data frame
  if (is.data.frame(data_list)) {
    # Single data frame - check for period column
    if ("study_period" %in% names(data_list)) {
      temporal_data <- data_list
    } else {
      # No period info, create single period
      temporal_data <- data_list %>% mutate(study_period = "Current")
    }
  } else {
    # List of data frames
    temporal_data <- map_dfr(names(data_list), function(period) {
      data_list[[period]] %>%
        mutate(study_period = period)
    })
  }
  
  # Filter for specific metal and calculate exceedance
  standard_value <- get_standard_value(metal, standard_type, standards)
  
  metal_data <- temporal_data %>%
    filter(str_to_lower(metal) == str_to_lower(!!metal)) %>%
    mutate(
      exceedance_ratio = measured_value / standard_value,
      risk_level = calculate_risk_level(measured_value, standard_value)
    ) %>%
    filter(!is.na(exceedance_ratio))
  
  if (nrow(metal_data) == 0) {
    return(ggplot() + 
             labs(title = paste("No", metal, "data available for temporal analysis")) + 
             theme_river_remedy())
  }
  
  # Create plot
  p <- metal_data %>%
    ggplot(aes(x = study_period, y = exceedance_ratio)) +
    geom_boxplot(aes(fill = study_period), alpha = 0.7, outlier.alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.6, size = 2) +
    geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 1) +
    scale_fill_manual(
      values = river_remedy_colors$study_periods,
      name = "Study Period"
    ) +
    scale_y_continuous(
      trans = "log10",
      labels = function(x) paste0(round(x, 1), "x"),
      expand = expansion(mult = c(0, 0.1))
    ) +
    labs(
      title = paste("Temporal Trend Analysis:", metal),
      subtitle = paste("Exceedance ratios vs", str_replace(standard_type, "_", " "), "standard"),
      x = "Study Period",
      y = "Exceedance Ratio (log scale)",
      caption = "Red line = regulatory limit"
    ) +
    theme_river_remedy()
  
  return(p)
}

#' Create Summary Table with Risk Assessment
#' 
#' @param data Data frame with contamination data
#' @param standards Standards list  
#' @param standard_types Vector of standard types to include
#' @return DT datatable object
create_summary_table <- function(data, standards, 
                                 standard_types = c("who_water", "codex_food", "epa_soil")) {
  
  # Calculate summary statistics and risk levels
  summary_data <- data %>%
    group_by(metal, location) %>%
    summarise(
      measured_value = round(mean(measured_value, na.rm = TRUE), 4),
      n_samples = n(),
      .groups = "drop"
    ) %>%
    # Add WHO water standard analysis (primary)
    rowwise() %>%
    mutate(
      who_standard = get_standard_value(metal, "who_water", standards),
      who_exceedance = round(measured_value / who_standard, 2),
      who_risk = calculate_risk_level(measured_value, who_standard),
      epa_standard = get_standard_value(metal, "epa_soil", standards),
      epa_exceedance = round(measured_value / epa_standard, 2),
      epa_risk = calculate_risk_level(measured_value, epa_standard),
      codex_standard = get_standard_value(metal, "codex_food", standards),
      codex_exceedance = round(measured_value / codex_standard, 2),
      codex_risk = calculate_risk_level(measured_value, codex_standard)
    ) %>%
    ungroup() %>%
    select(location, metal, measured_value, n_samples,
           who_standard, who_exceedance, who_risk,
           epa_standard, epa_exceedance, epa_risk,
           codex_standard, codex_exceedance, codex_risk)
  
  # Create interactive table
  DT::datatable(
    summary_data,
    options = list(
      pageLength = 15,
      scrollX = TRUE,
      dom = 'Bfrtip',
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    ),
    caption = "Contamination Summary with Multi-Standard Risk Assessment",
    colnames = c("Location", "Metal", "Measured (mg/L)", "Samples", 
                 "WHO Limit", "WHO Ratio", "WHO Risk",
                 "EPA Limit", "EPA Ratio", "EPA Risk", 
                 "Codex Limit", "Codex Ratio", "Codex Risk")
  ) %>%
    formatRound(columns = c("measured_value", "who_standard", "epa_standard", "codex_standard"), digits = 4) %>%
    formatRound(columns = c("who_exceedance", "epa_exceedance", "codex_exceedance"), digits = 2) %>%
    formatStyle(
      columns = c("who_risk", "epa_risk", "codex_risk"),
      backgroundColor = styleEqual(
        names(river_remedy_colors$risk_levels),
        river_remedy_colors$risk_levels
      ),
      color = "white",
      fontWeight = "bold"
    )
}

#' Create Risk Distribution Plot
#' 
#' @param data Data frame with contamination data
#' @param standards Standards list
#' @param standard_type Standard type for risk assessment
#' @return ggplot object
plot_risk_distribution <- function(data, standards, standard_type = "who_water") {
  
  risk_data <- data %>%
    rowwise() %>%
    mutate(
      standard_value = get_standard_value(metal, standard_type, standards),
      risk_level = calculate_risk_level(measured_value, standard_value)
    ) %>%
    ungroup() %>%
    filter(!is.na(risk_level)) %>%
    count(risk_level) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 1),
      risk_level = factor(risk_level, levels = names(river_remedy_colors$risk_levels))
    )
  
  ggplot(risk_data, aes(x = risk_level, y = n, fill = risk_level)) +
    geom_col(alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(n, "\n(", percentage, "%)")), 
              vjust = -0.5, fontface = "bold") +
    scale_fill_manual(
      values = river_remedy_colors$risk_levels,
      name = "Risk Level"
    ) +
    labs(
      title = "Risk Level Distribution",
      subtitle = paste("Based on", str_replace(standard_type, "_", " "), "standards"),
      x = "Risk Level",
      y = "Number of Observations",
      caption = "Percentages show proportion of total observations"
    ) +
    theme_river_remedy() +
    theme(legend.position = "none")
}

#' Create Metal Priority Plot
#' 
#' @param data Data frame with contamination data
#' @param standards Standards list
#' @param top_n Number of top metals to show
#' @return ggplot object
plot_metal_priority <- function(data, standards, top_n = 10) {
  
  priority_data <- data %>%
    group_by(metal) %>%
    summarise(
      n_locations = n_distinct(location),
      avg_concentration = mean(measured_value, na.rm = TRUE),
      max_concentration = max(measured_value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      who_standard = get_standard_value(metal, "who_water", standards),
      avg_exceedance = avg_concentration / who_standard,
      max_exceedance = max_concentration / who_standard,
      priority_score = log10(avg_exceedance + 1) * n_locations
    ) %>%
    ungroup() %>%
    arrange(desc(priority_score)) %>%
    head(top_n) %>%
    mutate(
      risk_category = case_when(
        avg_exceedance <= 1 ~ "Safe",
        avg_exceedance <= 2 ~ "Moderate",
        avg_exceedance <= 10 ~ "High",
        avg_exceedance <= 100 ~ "Critical",
        TRUE ~ "Extreme"
      )
    )
  
  ggplot(priority_data, aes(x = reorder(str_to_title(metal), priority_score), 
                            y = priority_score, fill = risk_category)) +
    geom_col(alpha = 0.8, width = 0.7) +
    scale_fill_manual(
      values = river_remedy_colors$risk_levels,
      name = "Risk Category"
    ) +
    labs(
      title = "Metal Contamination Priority Ranking",
      subtitle = "Priority score = log(average exceedance + 1) × number of locations",
      x = "Heavy Metal",
      y = "Priority Score",
      caption = "Higher scores indicate greater priority for intervention"
    ) +
    theme_river_remedy() +
    coord_flip()
}

# Utility function to prepare data for analysis
prepare_contamination_data <- function(raw_data, location_col = 1, value_cols = NULL) {
  
  # Clean column names
  data_clean <- raw_data %>%
    janitor::clean_names()
  
  # Auto-detect metal columns if not specified
  if (is.null(value_cols)) {
    metal_patterns <- c("pb", "as", "cd", "hg", "cu", "zn", "fe", "mn", "ni", "cr", "al")
    value_cols <- names(data_clean)[str_detect(names(data_clean), 
                                               paste(metal_patterns, collapse = "|"))]
  }
  
  if (length(value_cols) == 0) {
    warning("No metal columns detected")
    return(NULL)
  }
  
  # Convert to long format
  data_long <- data_clean %>%
    select(location = !!location_col, all_of(value_cols)) %>%
    pivot_longer(
      cols = all_of(value_cols),
      names_to = "parameter",
      values_to = "measured_value"
    ) %>%
    mutate(
      # Standardize metal names
      metal = case_when(
        str_detect(parameter, "pb|plomo") ~ "Lead",
        str_detect(parameter, "as|arsenico") ~ "Arsenic",
        str_detect(parameter, "cd|cadmio") ~ "Cadmium", 
        str_detect(parameter, "hg|mercurio") ~ "Mercury",
        str_detect(parameter, "cu|cobre") ~ "Copper",
        str_detect(parameter, "zn|zinc") ~ "Zinc",
        str_detect(parameter, "fe|hierro") ~ "Iron",
        str_detect(parameter, "mn|manganeso") ~ "Manganese",
        str_detect(parameter, "ni|niquel") ~ "Nickel",
        str_detect(parameter, "cr|cromo") ~ "Chromium",
        str_detect(parameter, "al|aluminio") ~ "Aluminum",
        TRUE ~ str_to_title(str_extract(parameter, "^[A-Za-z]+"))
      ),
      # Convert to numeric
      measured_value = as.numeric(measured_value)
    ) %>%
    filter(!is.na(measured_value), measured_value > 0, !is.na(metal)) %>%
    select(location, metal, measured_value)
  
  return(data_long)
}

# Example usage functions:
# standards <- load_regulatory_standards()
# data_clean <- prepare_contamination_data(raw_data)
# plot_standards_exceedance(data_clean, standards)
# create_contamination_map(data_clean, standards)
# create_summary_table(data_clean, standards)