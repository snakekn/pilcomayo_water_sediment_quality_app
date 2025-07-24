# main_analysis.R
# River Remedy - Main analysis script that integrates all components
# Author: River Remedy Team
# Date: 2025-01-16

# Setup and initialization ----
cat("🌊 Starting River Remedy Analysis\n")
cat("==================================\n")

# Load required libraries
library(tidyverse)
library(readxl)
library(janitor)
library(ggplot2)
library(scales)
library(viridis)
library(patchwork)
library(DT)

# Set working directory
cat("📁 Working directory:", getwd(), "\n")

# Source analysis scripts
if (file.exists("src/scripts/load_standards.R")) {
  source("src/scripts/load_standards.R")
  cat("✓ Loaded standards functions\n")
} else {
  cat("⚠ Standards script not found, using inline functions\n")
  
  # Inline backup functions
  get_standard_value <- function(metal, standard_type, standards = NULL) {
    ref_values <- list(
      "Lead" = list(who_water = 0.01, epa_soil = 70, codex_food = 0.3),
      "Arsenic" = list(who_water = 0.01, epa_soil = 39, codex_food = 0.2),
      "Cadmium" = list(who_water = 0.003, epa_soil = 10, codex_food = 0.1),
      "Mercury" = list(who_water = 0.006, epa_soil = NA, codex_food = 0.5),
      "Copper" = list(who_water = 2.0, epa_soil = NA, codex_food = NA),
      "Iron" = list(who_water = 3.0, epa_soil = NA, codex_food = NA),
      "Zinc" = list(who_water = 3.0, epa_soil = NA, codex_food = NA)
    )
    
    metal_clean <- str_to_title(metal)
    if (metal_clean %in% names(ref_values)) {
      return(ref_values[[metal_clean]][[standard_type]] %||% 0.01)
    }
    return(0.01)
  }
  
  calculate_risk_level <- function(measured_value, standard_value) {
    if (is.na(measured_value) || is.na(standard_value) || standard_value <= 0) {
      return("Unknown")
    }
    ratio <- measured_value / standard_value
    case_when(
      ratio <= 1 ~ "Safe",
      ratio <= 2 ~ "Moderate", 
      ratio <= 10 ~ "High",
      ratio <= 100 ~ "Critical",
      TRUE ~ "Extreme"
    )
  }
}

if (file.exists("src/scripts/visualization_functions.R")) {
  source("src/scripts/visualization_functions.R")
  cat("✓ Loaded visualization functions\n")
} else {
  cat("⚠ Visualization script not found, using simplified functions\n")
  
  # Set theme
  theme_river_remedy <- function() {
    theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  }
  
  # Color palette
  risk_colors <- c(
    "Safe" = "#2E8B57", "Moderate" = "#FFD700", "High" = "#FF6347", 
    "Critical" = "#DC143C", "Extreme" = "#8B0000"
  )
}

# Load regulatory standards ----
cat("\n📊 Loading regulatory standards...\n")

if (exists("load_regulatory_standards")) {
  standards <- load_regulatory_standards()
} else {
  cat("Using backup standards...\n")
  standards <- list(
    reference_values = tibble(
      metal = c("Lead", "Arsenic", "Cadmium", "Mercury", "Copper", "Iron", "Zinc"),
      who_water_mg_l = c(0.01, 0.01, 0.003, 0.006, 2.0, 3.0, 3.0),
      epa_soil_mg_kg = c(70, 39, 10, NA, NA, NA, NA),
      codex_food_mg_kg = c(0.3, 0.2, 0.1, 0.5, NA, NA, NA)
    )
  )
}

# Load contamination data ----
cat("\n📈 Loading contamination data...\n")

# Function to prepare data
prepare_data <- function(file_path, study_period, matrix_type) {
  if (!file.exists(file_path)) {
    cat("⚠ File not found:", file_path, "\n")
    return(NULL)
  }
  
  tryCatch({
    raw_data <- read_csv(file_path, show_col_types = FALSE) %>%
      clean_names()
    
    # Auto-detect metal columns
    metal_cols <- names(raw_data)[str_detect(names(raw_data), 
                                             "pb|as|cd|hg|cu|zn|fe|mn|\\(mg")]
    
    if (length(metal_cols) == 0) {
      cat("⚠ No metal columns found in", basename(file_path), "\n")
      return(NULL)
    }
    
    # Convert to long format
    processed <- raw_data %>%
      select(location = 1, all_of(metal_cols)) %>%
      pivot_longer(
        cols = all_of(metal_cols),
        names_to = "parameter", 
        values_to = "measured_value"
      ) %>%
      mutate(
        metal = case_when(
          str_detect(parameter, "pb|plomo") ~ "Lead",
          str_detect(parameter, "as|arsenico") ~ "Arsenic",
          str_detect(parameter, "cd|cadmio") ~ "Cadmium",
          str_detect(parameter, "hg|mercurio") ~ "Mercury", 
          str_detect(parameter, "cu|cobre") ~ "Copper",
          str_detect(parameter, "zn|zinc") ~ "Zinc",
          str_detect(parameter, "fe|hierro") ~ "Iron",
          str_detect(parameter, "mn|manganeso") ~ "Manganese",
          TRUE ~ "Unknown"
        ),
        measured_value = as.numeric(measured_value),
        study_period = study_period,
        matrix_type = matrix_type
      ) %>%
      filter(!is.na(measured_value), measured_value > 0, metal != "Unknown") %>%
      select(location, metal, measured_value, study_period, matrix_type)
    
    cat("✓ Processed", basename(file_path), ":", nrow(processed), "observations\n")
    return(processed)
    
  }, error = function(e) {
    cat("✗ Error processing", basename(file_path), ":", e$message, "\n")
    return(NULL)
  })
}

# Load 2006 ITA data
data_2006_list <- list(
  water = prepare_data("data/raw/ITA_water_2006.csv", "2006", "Water"),
  soil = prepare_data("data/raw/ITA_soil_2006.csv", "2006", "Soil"),
  sediment = prepare_data("data/raw/ITA_sed_2006.csv", "2006", "Sediment"),
  vegetation = prepare_data("data/raw/ITA_veg_2006.csv", "2006", "Vegetation"),
  fish = prepare_data("data/raw/ITA_fish_2006.csv", "2006", "Fish"),
  human = prepare_data("data/raw/ITA_human_2006.csv", "2006", "Human Blood"),
  animal = prepare_data("data/raw/ITA_animal_2006.csv", "2006", "Animal Blood")
)

# Load 2024 TNC data  
data_2024_list <- list(
  water = prepare_data("data/raw/calidad_agua_20250711060422.csv", "2024", "Water"),
  sediment = prepare_data("data/raw/calidad_sedimentos_20250711060913.csv", "2024", "Sediment")
)

# Combine all data
all_data_list <- c(data_2006_list, data_2024_list)
all_data_list <- all_data_list[!map_lgl(all_data_list, is.null)]

if (length(all_data_list) > 0) {
  all_data <- bind_rows(all_data_list)
  cat("✓ Combined dataset:", nrow(all_data), "total observations\n")
  cat("  Study periods:", paste(unique(all_data$study_period), collapse = ", "), "\n")
  cat("  Matrix types:", paste(unique(all_data$matrix_type), collapse = ", "), "\n")
  cat("  Metals:", paste(unique(all_data$metal), collapse = ", "), "\n")
} else {
  cat("⚠ No data loaded successfully\n")
  all_data <- tibble()
}

# Generate analysis and visualizations ----
cat("\n📊 Generating analysis...\n")

if (nrow(all_data) > 0) {
  
  # 1. Summary statistics
  cat("Calculating summary statistics...\n")
  
  summary_stats <- all_data %>%
    group_by(study_period, matrix_type, metal) %>%
    summarise(
      n_locations = n_distinct(location),
      n_samples = n(),
      mean_conc = round(mean(measured_value, na.rm = TRUE), 4),
      max_conc = round(max(measured_value, na.rm = TRUE), 4),
      .groups = "drop"
    ) %>%
    arrange(desc(max_conc))
  
  cat("📋 Summary Statistics:\n")
  print(summary_stats %>% head(20))
  
  # 2. Risk assessment
  cat("\nPerforming risk assessment...\n")
  
  risk_assessment <- all_data %>%
    rowwise() %>%
    mutate(
      who_standard = get_standard_value(metal, "who_water", standards),
      exceedance_ratio = measured_value / who_standard,
      risk_level = calculate_risk_level(measured_value, who_standard)
    ) %>%
    ungroup()
  
  # Risk summary
  risk_summary <- risk_assessment %>%
    count(risk_level) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    arrange(desc(n))
  
  cat("🚨 Risk Level Distribution:\n")
  print(risk_summary)
  
  # 3. Worst contamination sites
  worst_sites <- risk_assessment %>%
    arrange(desc(exceedance_ratio)) %>%
    select(location, metal, measured_value, exceedance_ratio, risk_level, study_period, matrix_type) %>%
    head(15)
  
  cat("\n🔴 Worst Contamination Sites:\n")
  print(worst_sites)
  
  # 4. Create visualizations
  cat("\nCreating visualizations...\n")
  
  # Lead contamination plot (most critical)
  lead_data <- risk_assessment %>%
    filter(metal == "Lead") %>%
    arrange(desc(exceedance_ratio))
  
  if (nrow(lead_data) > 0) {
    p1 <- lead_data %>%
      ggplot(aes(x = reorder(paste(location, study_period, sep = " - "), exceedance_ratio),
                 y = exceedance_ratio,
                 fill = risk_level)) +
      geom_col(alpha = 0.8) +
      geom_hline(yintercept = 1, color = "red", linetype = "dashed", linewidth = 1) +
      scale_fill_manual(values = risk_colors, name = "Risk Level") +
      scale_y_continuous(trans = "log10", labels = function(x) paste0(round(x, 1), "x")) +
      coord_flip() +
      labs(
        title = "Lead Contamination Across All Studies",
        subtitle = "Exceedance ratios vs WHO drinking water standard (0.01 mg/L)",
        x = "Location - Study Period",
        y = "Exceedance Ratio (log scale)",
        caption = "Red line = WHO limit"
      ) +
      theme_river_remedy()
    
    print(p1)
  }
  
  # Risk distribution by study period
  p2 <- risk_assessment %>%
    count(study_period, risk_level) %>%
    ggplot(aes(x = study_period, y = n, fill = risk_level)) +
    geom_col(position = "stack", alpha = 0.8) +
    scale_fill_manual(values = risk_colors, name = "Risk Level") +
    labs(
      title = "Risk Level Distribution by Study Period",
      subtitle = "Number of observations in each risk category",
      x = "Study Period",
      y = "Number of Observations"
    ) +
    theme_river_remedy()
  
  print(p2)
  
  # Metal priority ranking
  metal_priority <- risk_assessment %>%
    group_by(metal) %>%
    summarise(
      n_locations = n_distinct(location),
      avg_exceedance = mean(exceedance_ratio, na.rm = TRUE),
      max_exceedance = max(exceedance_ratio, na.rm = TRUE),
      priority_score = log10(avg_exceedance + 1) * n_locations,
      .groups = "drop"
    ) %>%
    arrange(desc(priority_score)) %>%
    mutate(
      risk_category = case_when(
        avg_exceedance <= 1 ~ "Safe",
        avg_exceedance <= 2 ~ "Moderate",
        avg_exceedance <= 10 ~ "High",
        avg_exceedance <= 100 ~ "Critical",
        TRUE ~ "Extreme"
      )
    )
  
  p3 <- metal_priority %>%
    ggplot(aes(x = reorder(metal, priority_score), y = priority_score, fill = risk_category)) +
    geom_col(alpha = 0.8) +
    scale_fill_manual(values = risk_colors, name = "Risk Category") +
    coord_flip() +
    labs(
      title = "Metal Contamination Priority Ranking",
      subtitle = "Priority score = log(avg exceedance + 1) × number of locations",
      x = "Heavy Metal",
      y = "Priority Score"
    ) +
    theme_river_remedy()
  
  print(p3)
  
  # 5. Create interactive summary table
  cat("\nCreating summary table...\n")
  
  summary_table_data <- risk_assessment %>%
    group_by(location, metal, study_period, matrix_type) %>%
    summarise(
      measured_value = round(mean(measured_value), 4),
      exceedance_ratio = round(mean(exceedance_ratio), 2),
      risk_level = first(risk_level),
      .groups = "drop"
    ) %>%
    arrange(desc(exceedance_ratio))
  
  summary_table <- DT::datatable(
    summary_table_data %>% head(50),
    options = list(pageLength = 15, scrollX = TRUE),
    caption = "Top 50 Contamination Cases - River Remedy Analysis"
  ) %>%
    formatStyle(
      "risk_level",
      backgroundColor = styleEqual(
        names(risk_colors),
        risk_colors
      ),
      color = "white",
      fontWeight = "bold"
    )
  
  print(summary_table)
  
} else {
  cat("⚠ No data available for analysis\n")
}

# Analysis summary ----
cat("\n🎯 ANALYSIS SUMMARY\n")
cat("==================\n")

if (exists("all_data") && nrow(all_data) > 0) {
  cat("📊 Data Overview:\n")
  cat("- Total observations:", nrow(all_data), "\n")
  cat("- Unique locations:", length(unique(all_data$location)), "\n")
  cat("- Study periods:", paste(unique(all_data$study_period), collapse = ", "), "\n")
  cat("- Matrix types:", paste(unique(all_data$matrix_type), collapse = ", "), "\n")
  
  if (exists("risk_summary")) {
    cat("\n🚨 Risk Assessment:\n")
    critical_extreme <- sum(risk_summary$n[risk_summary$risk_level %in% c("Critical", "Extreme")])
    cat("- Critical/Extreme cases:", critical_extreme, "observations\n")
    cat("- Percentage above WHO limits:", 
        round(sum(risk_summary$n[risk_summary$risk_level != "Safe"]) / sum(risk_summary$n) * 100, 1), "%\n")
  }
  
  if (exists("worst_sites")) {
    cat("\n🔴 Worst Contamination:\n")
    worst_case <- worst_sites[1, ]
    cat("- Worst case:", worst_case$location, "-", worst_case$metal, "\n")
    cat("- Exceedance:", round(worst_case$exceedance_ratio, 1), "x WHO limit\n")
    cat("- Study period:", worst_case$study_period, "\n")
  }
}

cat("\n✅ Available objects for further analysis:\n")
cat("- standards: Regulatory standards database\n")
cat("- all_data: Combined contamination data\n")
cat("- risk_assessment: Data with risk calculations\n")
cat("- summary_stats: Summary statistics by group\n")
cat("- worst_sites: Most contaminated locations\n")

cat("\n🔧 Next steps:\n")
cat("1. Examine the generated plots above\n")
cat("2. Use View(worst_sites) to see worst contamination\n")
cat("3. Run create_contamination_map(all_data, standards) for interactive map\n")
cat("4. Customize analysis for specific research questions\n")

cat("\n🌊 River Remedy Analysis Complete!\n")
