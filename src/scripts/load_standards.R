# load_standards.R
# River Remedy - Load and process regulatory standards from Excel file
# Author: River Remedy Team
# Date: 2025-01-16

# Load required libraries
if (!require(readxl)) install.packages("readxl")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")

library(readxl)
library(tidyverse)
library(janitor)

#' Load Regulatory Standards from Excel File
#' 
#' This function loads all regulatory standards from the Permissible Limits Excel file
#' and creates a standardized data structure for use across all River Remedy analyses.
#' 
#' @param file_path Path to the Excel file with standards
#' @return List containing all regulatory standards organized by category
load_regulatory_standards <- function(file_path = "./data/raw/Permissible Limits of Heavy Metals.xlsx") {
  
  cat("Loading regulatory standards from:", file_path, "\n")
  
  # Check if file exists
  if (!file.exists(file_path)) {
    warning("Standards file not found at: ", file_path, 
            "\nUsing backup standards from literature")
    return(create_backup_standards())
  }
  
  # Initialize standards list
  standards <- list()
  
  tryCatch({
    # Get all sheet names
    sheet_names <- excel_sheets(file_path)
    cat("Available sheets:", paste(sheet_names, collapse = ", "), "\n")
    
    # WHO Drinking Water Standards
    if ("WHO Drinking Water" %in% sheet_names) {
      tryCatch({
        who_data <- read_excel(file_path, sheet = "WHO Drinking Water", skip = 0) %>%
          clean_names() %>%
          filter(!is.na(metal_ion), !is.na(who_limit_mg_l_ppm))
        
        standards$who_drinking_water <- who_data %>%
          select(metal = metal_ion, 
                 limit = who_limit_mg_l_ppm, 
                 effects = effects) %>%
          mutate(
            unit = "mg/L",
            source = "WHO Guidelines for Drinking-water Quality (2017)",
            category = "drinking_water"
          )
        
        cat("✓ Loaded WHO drinking water standards:", nrow(standards$who_drinking_water), "metals\n")
      }, error = function(e) {
        warning("Could not load WHO standards: ", e$message)
      })
    }
    
    # Bolivian Law 1333 Standards
    if ("Bolivian Law 1333" %in% sheet_names) {
      tryCatch({
        bolivian_data <- read_excel(file_path, sheet = "Bolivian Law 1333", skip = 0) %>%
          clean_names()
        
        # Extract relevant parameters
        bolivian_metals <- bolivian_data %>%
          filter(!is.na(parametros)) %>%
          select(parameter = parametros, 
                 class_a = clase_a,
                 class_b = clase_b, 
                 class_c = clase_c,
                 class_d = clase_d,
                 unit = unidad) %>%
          mutate(
            source = "Bolivian Environmental Law 1333 (1992)",
            category = "discharge_limits"
          )
        
        standards$bolivian_law_1333 <- bolivian_metals
        
        cat("✓ Loaded Bolivian Law 1333 standards:", nrow(standards$bolivian_law_1333), "parameters\n")
      }, error = function(e) {
        warning("Could not load Bolivian standards: ", e$message)
      })
    }
    
    # Fish and Food Standards
    if ("Fish (European Commission; FDA)" %in% sheet_names) {
      tryCatch({
        fish_data <- read_excel(file_path, sheet = "Fish (European Commission; FDA)", skip = 0) %>%
          clean_names()
        
        # Get first two columns that have data
        col_names <- names(fish_data)
        standards$fish_food <- fish_data %>%
          select(metal = 1, limit = 2) %>%
          mutate(
            unit = "mg/kg",
            source = "European Commission / FDA",
            category = "food_safety"
          ) %>%
          filter(!is.na(metal), !is.na(limit))
        
        cat("✓ Loaded fish/food standards:", nrow(standards$fish_food), "metals\n")
      }, error = function(e) {
        warning("Could not load fish/food standards: ", e$message)
      })
    }
    
    # Human Blood Standards
    blood_sheet_names <- sheet_names[str_detect(sheet_names, "Human Blood")]
    if (length(blood_sheet_names) > 0) {
      tryCatch({
        blood_data <- read_excel(file_path, sheet = blood_sheet_names[1]) %>%
          clean_names()
        
        standards$human_blood <- blood_data %>%
          select(metal = 1, limit = 2) %>%
          mutate(
            unit = "µg/dL",
            source = "Mexican Standard / CDC Reference",
            category = "blood_levels"
          ) %>%
          filter(!is.na(metal), !is.na(limit))
        
        cat("✓ Loaded human blood standards:", nrow(standards$human_blood), "metals\n")
      }, error = function(e) {
        warning("Could not load blood standards: ", e$message)
      })
    }
    
    # US EPA Soil Standards
    if ("Soils and Sludges (US EPA 1981)" %in% sheet_names) {
      tryCatch({
        soil_data <- read_excel(file_path, sheet = "Soils and Sludges (US EPA 1981)") %>%
          clean_names()
        
        standards$epa_soil <- soil_data %>%
          select(metal = 1, limit = 2) %>%
          mutate(
            unit = "mg/kg",
            source = "US EPA (1981)",
            category = "soil_quality"
          ) %>%
          filter(!is.na(metal), !is.na(limit))
        
        cat("✓ Loaded EPA soil standards:", nrow(standards$epa_soil), "metals\n")
      }, error = function(e) {
        warning("Could not load EPA soil standards: ", e$message)
      })
    }
    
  }, error = function(e) {
    warning("Error reading Excel file: ", e$message, "\nUsing backup standards")
    return(create_backup_standards())
  })
  
  # Create comprehensive reference table
  standards$reference_values <- create_reference_table()
  
  # Add metadata
  standards$metadata <- list(
    file_path = file_path,
    load_date = Sys.Date(),
    excel_loaded = file.exists(file_path)
  )
  
  cat("✓ Created comprehensive reference table with", nrow(standards$reference_values), "metals\n")
  cat("\n📊 Standards loading complete!\n")
  cat("Total standards categories loaded:", length(standards) - 2, "\n")
  
  return(standards)
}

#' Create backup standards when Excel file is not available
create_backup_standards <- function() {
  list(
    reference_values = create_reference_table(),
    metadata = list(
      source = "Literature values",
      load_date = Sys.Date(),
      excel_loaded = FALSE
    )
  )
}

#' Create comprehensive reference table with all standards
create_reference_table <- function() {
  tibble(
    metal = c("Lead", "Arsenic", "Cadmium", "Mercury", "Copper", "Iron", "Zinc", "Nickel", "Chromium", "Aluminum", "Manganese"),
    symbol = c("Pb", "As", "Cd", "Hg", "Cu", "Fe", "Zn", "Ni", "Cr", "Al", "Mn"),
    who_water_mg_l = c(0.01, 0.01, 0.003, 0.006, 2.0, 3.0, 3.0, 0.07, 0.05, 0.2, 0.4),
    codex_food_mg_kg = c(0.3, 0.2, 0.1, 0.5, NA, NA, NA, NA, NA, NA, NA),
    cdc_blood_ug_dl = c(5, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    epa_soil_mg_kg = c(70, 39, 10, NA, NA, NA, NA, NA, NA, NA, NA),
    bolivian_class_a_mg_l = c(0.05, 0.05, 0.005, 0.001, 0.05, 0.3, 0.2, 0.05, 0.05, 0.2, 0.5),
    health_priority = c("High", "High", "High", "High", "Medium", "Low", "Low", "Medium", "High", "Medium", "Medium")
  )
}

#' Get Standard Value for Specific Metal and Category
#' 
#' @param metal Metal name or symbol
#' @param standard_type Type of standard (e.g., "who_water", "codex_food")
#' @param standards Standards list from load_regulatory_standards()
#' @return Numeric value of the standard limit
get_standard_value <- function(metal, standard_type, standards) {
  
  # Standardize metal name
  metal_clean <- case_when(
    str_to_lower(metal) %in% c("pb", "lead", "plomo") ~ "Lead",
    str_to_lower(metal) %in% c("as", "arsenic", "arsenico") ~ "Arsenic", 
    str_to_lower(metal) %in% c("cd", "cadmium", "cadmio") ~ "Cadmium",
    str_to_lower(metal) %in% c("hg", "mercury", "mercurio") ~ "Mercury",
    str_to_lower(metal) %in% c("cu", "copper", "cobre") ~ "Copper",
    str_to_lower(metal) %in% c("fe", "iron", "hierro") ~ "Iron",
    str_to_lower(metal) %in% c("zn", "zinc") ~ "Zinc",
    str_to_lower(metal) %in% c("ni", "nickel", "niquel") ~ "Nickel",
    str_to_lower(metal) %in% c("cr", "chromium", "cromo") ~ "Chromium",
    str_to_lower(metal) %in% c("al", "aluminum", "aluminio") ~ "Aluminum",
    str_to_lower(metal) %in% c("mn", "manganese", "manganeso") ~ "Manganese",
    TRUE ~ str_to_title(metal)
  )
  
  # Look up in reference table
  ref_data <- standards$reference_values %>%
    filter(metal == metal_clean)
  
  if (nrow(ref_data) == 0) {
    warning("Metal not found: ", metal_clean)
    return(NA)
  }
  
  # Return appropriate standard value
  if (standard_type == "who_water") {
    return(ref_data$who_water_mg_l[1])
  } else if (standard_type == "codex_food") {
    return(ref_data$codex_food_mg_kg[1])
  } else if (standard_type == "cdc_blood") {
    return(ref_data$cdc_blood_ug_dl[1])
  } else if (standard_type == "epa_soil") {
    return(ref_data$epa_soil_mg_kg[1])
  } else if (standard_type == "bolivian_class_a") {
    return(ref_data$bolivian_class_a_mg_l[1])
  }
  
  # Default to WHO water standard
  return(ref_data$who_water_mg_l[1])
}

#' Calculate Risk Level Based on Measured vs Standard Values
#' 
#' @param measured_value Measured concentration
#' @param standard_value Standard/limit value  
#' @param include_ratio Include numerical ratio in output
#' @return Character vector with risk level or list with level and ratio
calculate_risk_level <- function(measured_value, standard_value, include_ratio = FALSE) {
  
  if (is.na(measured_value) || is.na(standard_value) || standard_value <= 0) {
    return(if (include_ratio) list(level = "Unknown", ratio = NA) else "Unknown")
  }
  
  ratio <- measured_value / standard_value
  
  level <- case_when(
    ratio <= 1 ~ "Safe",
    ratio <= 2 ~ "Moderate", 
    ratio <= 10 ~ "High",
    ratio <= 100 ~ "Critical",
    TRUE ~ "Extreme"
  )
  
  if (include_ratio) {
    return(list(level = level, ratio = ratio))
  } else {
    return(level)
  }
}

#' Calculate exceedance ratio for a metal
get_exceedance_ratio <- function(metal, measured_value, standard_type, standards) {
  standard_value <- get_standard_value(metal, standard_type, standards)
  if (is.na(standard_value) || standard_value <= 0) {
    return(NA)
  }
  return(measured_value / standard_value)
}

# Example usage:
# standards <- load_regulatory_standards()
# who_lead_limit <- get_standard_value("Lead", "who_water", standards)
# risk <- calculate_risk_level(0.8, who_lead_limit, include_ratio = TRUE)