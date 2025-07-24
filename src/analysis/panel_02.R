# main_analysis.R
# River Remedy - Main analysis script that integrates all components
# Author: Your Name
# Date: 2025-01-16

# Setup and initialization ----
cat("🌊 Starting River Remedy Analysis\n")
cat("==================================\n")

# Load required libraries
library(tidyverse)
library(here)
library(patchwork)

# Set working directory to project root
if (!exists("project_root")) {
  project_root <- here()
  setwd(project_root)
  cat("📁 Working directory set to:", getwd(), "\n")
}

# Source all analysis scripts
source("src/scripts/load_standards.R")
source("src/scripts/visualization_functions.R")

# Load regulatory standards ----
cat("\n📊 Loading regulatory standards...\n")
standards <- load_regulatory_standards("data/raw/Permissible Limits of Heavy Metals.xlsx")

# Load your existing data ----
cat("\n📈 Loading contamination data...\n")

# 2024 TNC Data
if (file.exists("data/raw/calidad_agua_20250711060422.csv")) {
  water_2024_raw <- read_csv("data/raw/calidad_agua_20250711060422.csv")
  cat("✓ Loaded 2024 TNC water data:", nrow(water_2024_raw), "rows\n")
} else {
  cat("⚠ 2024 TNC water data not found\n")
  water_2024_raw <- NULL
}

# 2006 ITA Data  
data_files_2006 <- c(
  "water" = "data/raw/ITA_water_2006.csv",
  "soil" = "data/raw/ITA_soil_2006.csv", 
  "sediment" = "data/raw/ITA_sed_2006.csv",
  "vegetation" = "data/raw/ITA_veg_2006.csv",
  "fish" = "data/raw/ITA_fish_2006.csv",
  "human" = "data/raw/ITA_human_2006.csv",
  "animal" = "data/raw/ITA_animal_2006.csv"
)

data_2006 <- map(data_files_2006, function(file) {
  if (file.exists(file)) {
    read_csv(file)
  } else {
    cat("⚠ File not found:", file, "\n")
    NULL
  }
})

# Remove NULL entries
data_2006 <- data_2006[!map_lgl(data_2006, is.null)]
cat("✓ Loaded 2006 ITA data:", length(data_2006), "matrices\n")

# 2011 Strosnider Data
strosnider_files <- list.files("data/raw", pattern = "Strosnider.*\\.xlsx", full.names = TRUE)
if (length(strosnider_files) > 0) {
  cat("✓ Found", length(strosnider_files), "Strosnider data files\n")
} else {
  cat("⚠ No Strosnider data files found\n")
}

# Data processing functions ----

#' Process 2024 TNC water data
process_2024_data <- function(raw_data) {
  if (is.null(raw_data)) return(NULL)
  
  # Transform from wide to long format (adapt based on your actual data structure)
  processed <- raw_data %>%
    # Clean column names
    janitor::clean_names() %>%
    # Convert to long format - adjust column selection based on your data
    pivot_longer(
      cols = -c(1), # Adjust based on your ID columns
      names_to = "parameter", 
      values_to = "value"
    ) %>%
    # Extract metal concentrations
    filter(str_detect(parameter, "total|pb|as|cd|hg|cu|zn|fe|mn")) %>%
    mutate(
      # Clean parameter names
      metal = case_when(
        str_detect(parameter, "arsenico|arsenic") ~ "Arsenic",
        str_detect(parameter, "cadmio|cadmium") ~ "Cadmium", 
        str_detect(parameter, "plomo|lead|pb") ~ "Lead",
        str_detect(parameter, "mercurio|mercury|hg") ~ "Mercury",
        str_detect(parameter, "cobre|copper|cu") ~ "Copper",
        str_detect(parameter, "zinc|zn") ~ "Zinc",
        str_detect(parameter, "hierro|iron|fe") ~ "Iron",
        str_detect(parameter, "manganeso|manganese|mn") ~ "Manganese",
        TRUE ~ str_to_title(parameter)
      ),
      # Convert values to numeric
      measured_value = as.numeric(value),
      # Add metadata
      study_period = "2024",
      matrix_type = "Water",
      data_source = "TNC Pilcomayo.net"
    ) %>%
    filter(!is.na(measured_value), measured_value > 0) %>%
    select(location = 1, metal, measured_value, study_period, matrix_type, data_source)
  
  return(processed)
}

#' Process 2006 ITA data
process_2006_data <- function(data_list) {
  if (length(data_list) == 0) return(NULL)
  
  processed_list <- map2_dfr(data_list, names(data_list), function(data, matrix_name) {
    if (is.null(data)) return(NULL)
    
    # Clean and standardize column names
    data_clean <- data %>%
      janitor::clean_names()
    
    # Find metal columns (adjust patterns based on your data)
    metal_cols <- names(data_clean)[str_detect(names(data_clean), "pb|as|cd|hg|cu|zn|fe|mn")]
    
    if (length(metal_cols) == 0) return(NULL)
    
    # Convert to long format
    processed <- data_clean %>%
      select(location = 1, all_of(metal_cols)) %>%
      pivot_longer(
        cols = all_of(metal_cols),
        names_to = "parameter",
        values_to = "measured_value"
      ) %>%
      mutate(
        # Standardize metal names
        metal = case_when(
          str_detect(parameter, "pb") ~ "Lead",
          str_detect(parameter, "as") ~ "Arsenic", 
          str_detect(parameter, "cd") ~ "Cadmium",
          str_detect(parameter, "hg") ~ "Mercury",
          str_detect(parameter, "cu") ~ "Copper",
          str_detect(parameter, "zn") ~ "Zinc",
          str_detect(parameter, "fe") ~ "Iron",
          str_detect(parameter, "mn") ~ "Manganese",
          TRUE ~ str_to_title(parameter)
        ),
        # Convert to numeric
        measured_value = as.numeric(measured_value),
        # Add metadata
        study_period = "2006",
        matrix_type = str_to_title(matrix_name),
        data_source = "ITA Baseline Study"
      ) %>%
      filter(!is.na(measured_value), measured_value > 0) %>%
      select(location, metal, measured_value, study_period, matrix_type, data_source)
    
    return(processed)
  })
  
  return(processed_list)
}

# Process all data ----
cat("\n🔄 Processing contamination data...\n")

# Process 2024 data
data_2024_processed <- process_2024_data(water_2024_raw)
if (!is.null(data_2024_processed)) {
  cat("✓ Processed 2024 data:", nrow(data_2024_processed), "observations\n")
}

# Process 2006 data
data_2006_processed <- process_2006_data(data_2006)
if (!is.null(data_2006_processed)) {
  cat("✓ Processed 2006 data:", nrow(data_2006_processed), "observations\n")
}

# Combine all processed data
all_data <- bind_rows(
  data_2024_processed,
  data_2006_processed
) %>%
  filter(!is.na(metal), !is.na(measured_value))

if (nrow(all_data) > 0) {
  cat("✓ Combined dataset:", nrow(all_data), "total observations\n")
  cat("  Metals analyzed:", paste(unique(all_data$metal), collapse = ", "), "\n")
  cat("  Study periods:", paste(unique(all_data$study_period), collapse = ", "), "\n")
  cat("  Matrix types:", paste(unique(all_data$matrix_type), collapse = ", "), "\n")
} else {
  cat("⚠ No data available for analysis\n")
}

# Generate visualizations ----
cat("\n📊 Generating visualizations...\n")

if (nrow(all_data) > 0) {
  
  # 1. Multi-standard exceedance plot
  cat("Creating standards exceedance plot...\n")
  p1 <- plot_standards_exceedance(
    all_data, 
    standards,
    standard_types = c("who_water", "codex_food", "epa_soil"),
    title = "River Remedy: Multi-Standard Exceedance Analysis"
  )
  
  # 2. Contamination heatmap
  cat("Creating contamination heatmap...\n")
  p2 <- plot_contamination_heatmap(
    all_data,
    standards,
    standard_type = "who_water", 
    title = "River Remedy: Contamination Risk Heatmap"
  )
  
  # 3. Temporal comparison (if multiple periods available)
  if (length(unique(all_data$study_period)) > 1) {
    cat("Creating temporal comparison...\n")
    
    # Split data by period for temporal analysis
    data_by_period <- split(all_data, all_data$study_period)
    
    # Create temporal plot for Lead (most critical)
    p3 <- plot_temporal_comparison(
      data_by_period,
      metal = "Lead",
      standards = standards,
      standard_type = "who_water"
    )
  } else {
    p3 <- ggplot() + 
      labs(title = "Temporal Analysis", 
           subtitle = "Requires multiple study periods") +
      theme_river_remedy()
  }
  
  # 4. Summary table
  cat("Creating summary table...\n")
  summary_table <- create_summary_table(
    all_data,
    standards,
    standard_types = c("who_water", "codex_food", "epa_soil")
  )
  
  # Combine plots
  combined_plot <- (p1 / p2) | p3
  combined_plot <- combined_plot + 
    plot_annotation(
      title = "River Remedy: Comprehensive Contamination Analysis",
      subtitle = "Pilcomayo River Basin Heavy Metal Assessment (2006-2024)",
      caption = "Data sources: TNC Pilcomayo.net (2024), ITA Baseline Study (2006), Strosnider et al. (2011)"
    )
  
  # Display results
  print(combined_plot)
  
  # Display summary table
  cat("\n📋 Summary Table:\n")
  print(summary_table)
  
} else {
  cat("⚠ Cannot generate visualizations - no processed data available\n")
}

# Generate reports ----
cat("\n📄 Report generation options:\n")
cat("1. Run render_enhanced_report() to create full HTML report\n")
cat("2. Run create_contamination_map() for interactive mapping\n") 
cat("3. Access 'standards' object for regulatory reference data\n")
cat("4. Access 'all_data' object for processed contamination data\n")

#' Render enhanced Quarto report with standards integration
render_enhanced_report <- function(output_file = "outputs/html/enhanced_river_remedy_report.html") {
  
  # Ensure output directory exists
  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  
  # Create enhanced report template
  report_template <- here("src/analysis/enhanced_river_remedy_report.qmd")
  
  if (!file.exists(report_template)) {
    cat("Creating enhanced report template...\n")
    
    # Create the enhanced report (this would be a separate .qmd file)
    enhanced_qmd <- '---
title: "River Remedy: Enhanced Multi-Temporal Analysis"
author: "River Remedy Team"
date: "`r Sys.Date()`"
format:
  html:
    theme: flatly
    toc: true
    toc-location: left
    code-fold: true
    embed-resources: true
    fig-width: 12
    fig-height: 8
execute:
  echo: false
  warning: false
  message: false
---

```{r setup}
source("../../src/scripts/main_analysis.R")
```

# Executive Summary

This report presents a comprehensive analysis of heavy metal contamination in the Pilcomayo River Basin, integrating data from three major studies spanning 2006-2024.

## Key Findings

```{r summary-stats}
if (exists("all_data") && nrow(all_data) > 0) {
  summary_stats <- all_data %>%
    group_by(study_period) %>%
    summarise(
      n_observations = n(),
      n_locations = n_distinct(location),
      n_metals = n_distinct(metal),
      .groups = "drop"
    )
  
  knitr::kable(summary_stats, caption = "Study Overview")
}
```

# Standards-Based Risk Assessment

```{r standards-analysis}
if (exists("p1")) {
  print(p1)
}
```

# Contamination Patterns

```{r contamination-heatmap}
if (exists("p2")) {
  print(p2)
}
```

# Temporal Trends

```{r temporal-analysis}
if (exists("p3")) {
  print(p3)
}
```

# Interactive Summary

```{r summary-table}
if (exists("summary_table")) {
  summary_table
}
```
'

writeLines(enhanced_qmd, report_template)
  }

# Render the report
if (requireNamespace("quarto", quietly = TRUE)) {
  quarto::quarto_render(report_template, output_file = output_file)
  cat("✓ Enhanced report generated:", output_file, "\n")
} else {
  cat("⚠ Quarto package not available. Install with: install.packages('quarto')\n")
}
}

# Final status ----
cat("\n🎉 River Remedy Analysis Complete!\n")
cat("=====================================\n")
cat("Available objects:\n")
cat("- standards: Regulatory standards database\n")
cat("- all_data: Processed contamination data\n") 
cat("- p1, p2, p3: Generated plots\n")
cat("- summary_table: Interactive summary table\n")
cat("\nNext steps:\n")
cat("1. Review visualizations above\n")
cat("2. Run render_enhanced_report() for full report\n")
cat("3. Customize analysis for specific research questions\n")

