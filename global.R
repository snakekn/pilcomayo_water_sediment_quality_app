## global.R runs before ui.R and server.R. All of them replace app.R to make for easier reading & finding code
# app will run automatically using shiny::runApp("."), which happens when you click the "Run App" function

## Import libraries
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)
library(rsconnect)
library(readxl)
library(plotly)
library(DT)
library(zoo)
library(missMDA)
library(ggfortify)
library(FactoMineR)
library(factoextra)
library(shinyWidgets)
library(bslib)

## Define file paths to data
sed_data_path_usgs <- "data/sed/usgs"
water_data_path_1333 <- "data/water/1333"

sed_data_path_clean <- "data/sed/clean"
water_data_path_clean <- "data/water/clean"


## load all scripts 
load_scripts <- function(dir = "scripts/risk_analysis") {
  if (!dir.exists(dir)) return(invisible())
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
  for (f in files) {
    # source into the *current* app environment to avoid globals
    sys.source(f, envir = environment()) 
  }
}

load_scripts(dir = "R")
load_scripts(dir = "scripts/risk_analysis")