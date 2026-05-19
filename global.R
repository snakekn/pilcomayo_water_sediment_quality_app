## global.R runs before ui.R and server.R. All of them replace app.R to make for easier reading & finding code
# app will run automatically using shiny::runApp("."), which happens when you click the "Run App" function
# Note: this means that refreshing the page does not re-run global.R, just server.R and ui.R
message("Starting global.R")

#### ShinyLive package installation checks ####

## Import libraries using verbose process
pkgs <- c(
  "shiny", "tidyverse", "leaflet", "sf", "rsconnect",
  "readxl", "DT", "zoo", "shinyWidgets", "bslib", "terra",
  "shinyjs", "shinyBS", "stringr",
  "gstat", "whitebox", "memoise", "htmltools", "here", "janitor",
  "munsell" # dependency required by many other packages, available according to repo.r-wasm.org
)

# for safe keeping
pkgs_removed = c(
  "plotly", "missMDA", "ggfortify", "FactoMineR", "factoextra", "ggiraph", "qs2", "ggrepel"
)

pkg_results <- vector("list", length(pkgs))
names(pkg_results) <- pkgs

# install packages
for (pkg in pkgs) {
  message("---- Checking package: ", pkg)
  
  ns_ok <- FALSE
  attach_ok <- FALSE
  err_msg <- ""
  
  ns_ok <- tryCatch({
    loadNamespace(pkg)
    TRUE
  }, error = function(e) {
    err_msg <<- paste0("loadNamespace failed: ", conditionMessage(e))
    FALSE
  })
  
  if (ns_ok) {
    attach_ok <- tryCatch({
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
      TRUE
    }, error = function(e) {
      err_msg <<- paste0("library failed: ", conditionMessage(e))
      FALSE
    })
  }
  
  pkg_results[[pkg]] <- data.frame(
    package = pkg,
    namespace_ok = ns_ok,
    attach_ok = attach_ok,
    error = err_msg,
    stringsAsFactors = FALSE
  )
  
  if (attach_ok) {
    message("[OK] ", pkg)
  } else {
    message("[FAIL] ", pkg, " -- ", err_msg)
  }
}

pkg_results_df <- do.call(rbind, pkg_results)
print(pkg_results_df)

## check if any have issues with calling their functions
pkg_checks <- list(
  shiny        = "reactive",
  tidyverse    = "glimpse",
  leaflet      = "leaflet",
  sf           = "st_read",
  rsconnect    = "deployApp",
  readxl       = "read_excel",
  plotly       = "plot_ly",
  DT           = "datatable",
  zoo          = "rollmean",
  missMDA      = "imputePCA",
  ggfortify    = "autoplot",
  FactoMineR   = "PCA",
  factoextra   = "fviz_pca_ind",
  shinyWidgets = "pickerInput",
  bslib        = "bs_theme",
  terra        = "rast",
  ggiraph      = "girafe",
  shinyjs      = "useShinyjs",
  shinyBS      = "bsTooltip",
  ggrepel      = "geom_text_repel",
  stringr      = "str_detect",
  gstat        = "gstat",
  whitebox     = "wbt_init",
  memoise      = "memoise",
  htmltools    = "HTML",
  here         = "here",
  janitor      = "clean_names",
  munsell      = "mnsl"
)

pkg_report <- lapply(names(pkg_checks), function(pkg) {
  fn <- pkg_checks[[pkg]]
  
  namespace_ok <- FALSE
  attach_ok <- FALSE
  attached <- FALSE
  exported_fn_in_namespace <- FALSE
  fn_visible_from_search <- FALSE
  error_msg <- ""
  
  namespace_ok <- tryCatch({
    loadNamespace(pkg)
    TRUE
  }, error = function(e) {
    error_msg <<- paste0("loadNamespace failed: ", conditionMessage(e))
    FALSE
  })
  
  if (namespace_ok) {
    exported_fn_in_namespace <- tryCatch({
      fn %in% getNamespaceExports(pkg) &&
        exists(fn, envir = asNamespace(pkg), mode = "function", inherits = FALSE)
    }, error = function(e) FALSE)
    
    attach_ok <- tryCatch({
      suppressPackageStartupMessages(
        library(pkg, character.only = TRUE)
      )
      TRUE
    }, error = function(e) {
      error_msg <<- paste0("library failed: ", conditionMessage(e))
      FALSE
    })
    
    attached <- paste0("package:", pkg) %in% search()
    
    fn_visible_from_search <- exists(fn, mode = "function", inherits = TRUE)
  }
  
  data.frame(
    package = pkg,
    test_function = fn,
    namespace_ok = namespace_ok,
    attach_ok = attach_ok,
    attached = attached,
    exported_fn_in_namespace = exported_fn_in_namespace,
    fn_visible_from_search = fn_visible_from_search,
    error = error_msg,
    stringsAsFactors = FALSE
  )
})

pkg_report_df <- do.call(rbind, pkg_report)
print(pkg_report_df, row.names = FALSE)

# debug: attempt sf::st_read()
message("sf attached? ", "package:sf" %in% search())
message("st_read exists in sf namespace? ", exists("st_read", where = asNamespace("sf"), inherits = FALSE))
message("st_read visible from current env? ", exists("st_read", mode = "function", inherits = TRUE))

options(shiny.trace = FALSE)
options(shiny.fullstacktrace = FALSE)
# options(warn = 1)   # make warnings into errors
# options(ts_debug = FALSE) # specifically for debugging our own stuff, can use wherever

#### State that we're starting ####
message("Libraries imported and options set. Loading scripts...")

#### load all scripts  ####
load_scripts <- function(dir = "scripts/risk_analysis") {
  if (!dir.exists(dir)) return(invisible())
  files <- list.files(dir, pattern = "[.]R$", full.names = TRUE, recursive = TRUE)
  for (f in files) {
    # source into the *current* app environment to avoid globals
    sys.source(f, envir = globalenv()) 
    message("Loaded: ", f)
  }
}

load_scripts(dir = "R")
# load_scripts(dir = "scripts/risk_analysis")
message("All scripts loaded.")

# hash some functions so we're saving time on repetitive calls
compare_units <- memoise::memoise(compare_units)
parse_unit <- memoise::memoise(parse_unit) # called in compare_units
filter_to_border <- memoise::memoise(filter_to_border) # called each time we filter the border
get_param_list <- memoise::memoise(get_param_list) # each time we populate the parameter dropdown
plot_top_hq_params = memoise::memoise(plot_top_hq_params)
plot_top_hq_stations = memoise::memoise(plot_top_hq_stations)
plot_top_hq_sieve = memoise::memoise(plot_top_hq_sieve)

#### define paths to things ####
message("global.R: Defining paths and loading shared datasets (standards, constants)")
#### load global values ####
stds = readr::read_csv(here::here("data/standards/all_standards.csv"))
### Put together an easy-to-load standards list
# Load csv's & prepare for standards & weights. STDs include Cancer Risk
make_key <- function(parameter, media, std_type) {
  paste0(parameter, "||", media, "||", std_type)
}

stds <- readr::read_csv(here::here("data/standards/all_standards.csv")) |>
  janitor::clean_names()

strict_stds <- set_strict_stds()

strict_stds <- strict_stds |>
  dplyr::mutate(
    hqcr = "hq",
    .key = make_key(parameter, media, hqcr)
  )

std_map <- split(strict_stds, strict_stds$.key)


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

# FIXED COLOR SCALE - hardcoded breaks at 1 and 10
# Create 300 total colors distributed across the three zones
RISK_ZONE1_COLORS <- colorRampPalette(c("#1a9850", "#91cf60", "#d9ef8b", "#ffffbf"))(100)
RISK_ZONE2_COLORS <- colorRampPalette(c("#ffffbf", "#fee08b", "#fc8d59", "#e34a33", "#d73027"))(100)
RISK_ZONE3_COLORS <- colorRampPalette(c("#d73027", "#a50026", "#67001f", "#000000"))(100)
RISK_ALL_COLORS   <- c(RISK_ZONE1_COLORS, RISK_ZONE2_COLORS, RISK_ZONE3_COLORS)

# for setting up standard hover text in TS plots
CLASS_ORDER <- c("Class A", "Class B", "Class C", "Class D", "Unclassified")


message("Completed global.R")