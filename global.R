## global.R runs before ui.R and server.R. All of them replace app.R to make for easier reading & finding code
# app will run automatically using shiny::runApp("."), which happens when you click the "Run App" function

## Import libraries
pacman::p_load(
  shiny, tidyverse, leaflet, leaflet.extras, sf, rsconnect,
  readxl, plotly, DT, zoo, missMDA, ggfortify,
  FactoMineR, factoextra, shinyWidgets, bslib, terra,
  ggiraph, shinyjs, shinyBS, ggrepel, stringr,
  gstat, whitebox
)

options(shiny.trace = FALSE)
options(shiny.fullstacktrace = FALSE)
options(warn = 1)   # make warnings into errors
options(ts_debug = TRUE) # specifically for debugging our own stuff, can use wherever

#### load all scripts  ####
load_scripts <- function(dir = "scripts/risk_analysis") {
  if (!dir.exists(dir)) return(invisible())
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
  for (f in files) {
    # source into the *current* app environment to avoid globals
    sys.source(f, envir = globalenv()) 
  }
}

load_scripts(dir = "R")
load_scripts(dir = "scripts/risk_analysis")

#### define paths to things ####
## Define file paths to data
sed_data_path_usgs <- "data/sed/usgs"
water_data_path_1333 <- "data/water/1333"

sed_data_path_clean <- "data/sed/clean"
water_data_path_clean <- "data/water/clean"

included_water_files_path = "data/compiled/water_data_list.csv"
included_sed_files_path = "data/compiled/sed_data_list.csv"

compiled_water_data_path = "data/compiled/water_compiled.csv"
compiled_sed_data_path = "data/compiled/sed_compiled.csv"

#### load pre-compiled data ####
# initial_water_clean = readr::read_csv(here::here("data/processed/merged_water_clean.csv"))
# initial_sed_clean = readr::read_csv(here::here("data/processed/merged_sed_clean.csv"))
# 
# initial_water_scored = readRDS(here::here("data/processed/water_scored.rds")) # with HQCRWL scores
# initial_sed_scored = readRDS(here::here("data/processed/sed_scored.rds"))
# 
# initial_water_locyear = readRDS(here::here("data/processed/water_locyear.rds")) # scores are nested by location for easy access
# initial_sed_locyear = readRDS(here::here("data/processed/sed_locyear.rds"))

#### load global values ####
stds = readr::read_csv(here::here("data/standards/all_standards.csv"))
### Put together an easy-to-load standards list
# Load csv's & prepare for standards & weights. STDs include Cancer Risk
make_key = function(parameter, media, std_type) paste0(parameter, "||", media, "||", std_type)

stds = readr::read_csv(here::here("data/standards/all_standards.csv")) |>
  mutate(.key = make_key(parameter, media, hqcr)) |>
  filter(!is.na(value)) # skip any values that we don't have data for, HQ/CR/WL
std_map <- split(stds, stds$.key)

# these are kept centrally to help us easily redefine if needed

# 1) Shared constants
EXCLUDED_COLS <- c(
  "Decimal Latitude","Decimal Longitude","Latitude Decimal","Longitude Decimal",
  "Lat_dd","Long_dd","Distance from Bank","Distance from Shore",
  "Average Velocity (m/s)","Flow (m3/s)",
  "Clay (%)","Silt (%)","Sand (%)",
  "0.032 mm - No. 450 (ASTM) (%)","0.063 mm - No. 230 (ASTM) (%)","0.125 mm - No. 120 (ASTM) (%)",
  "0.250 mm - No. 060 (ASTM) (%)","0.500 mm - No. 035 (ASTM) (%)","1.00 mm - No. 018 (ASTM) (%)",
  "2.00 mm - No. 010 (ASTM) (%)","4.75 mm - No. 004 (ASTM) (%)","0.016 mm (%)",
  "Year","num_unclass","num_class_b","num_class_c","num_class_d"
)

ID_COLS = c("data_source",
             "Station","Code","Date","Time","Campaign","Responsible","Institution",
             "River","Basin","Latitude","Longitude","Latitude Decimal","Longitude Decimal",
             "Year")

REVERSE_PARAMS <- c("Oxygen Saturation (%)","Dissolved Oxygen (mg/l O2)","pH","Resistivity (Ohm.cm)")

CLASS_MAP <- c("Class A"=0,"Class B"=1,"Class C"=2,"Class D"=3,"Unclassified"=4)
USGS_MAP  <- c("Below TEL"=0,"Above TEL"=1,"Above PEL"=2)

# Station HQ bins
HQ_STATION_BINS = list(
  breaks = c(8,14,26,35,47,128),
  labels = c("Lowest Priority", "Low Priority", "Medium Priority", 
             "High Priority", "Extreme Priority"),
  colors = c("Lowest Priority" = "#2E7D32",   # Dark green
             "Low Priority" = "#66BB6A",       # Light green
             "Medium Priority" = "#FDD835",    # Yellow
             "High Priority" = "#FF9800",      # Orange
             "Extreme Priority" = "#C62828")   # Dark red
)