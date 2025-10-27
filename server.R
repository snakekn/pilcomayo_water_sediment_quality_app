# Define Server
server <- function(input, output, session) {
  
  ################# LOAD DATA #########################
  
  ## Nadav's area - risk analysis work
  
  # create the leaflet that will show the risk map
  output$risk_map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng=-16.95, lat=-65.3, zoom=4) # set to potosi
    # below will be more code to show on the map
    # - will need to call in vector layer
    # - will need to conduct calculations based on vectors and environmental hazards data
  })
  
  ## End Risk Analysis work

  ## For importing data
  
  initial_water = reactive(all_water_clean())
  merged_out <- dataUploadServer(
    id = "upload_data",
    base_data = initial_water
    )  
  
  output$import_meta <- renderPrint({
    req(imported$data())
    list(
      rows = nrow(imported$data()),
      cols = ncol(imported$data()),
      settings = imported$settings()
    )
  })
  
  pilco_line <- st_read("data/geojson/pilco_line.geojson")
  
  bol_border <- st_read("data/geojson/bol_borders.geojson")
  
  usgs_sqg <- read_csv("data/standards/USGS_SQG.csv") |>
    mutate(match_name = c("Arsenic (mg/kg As)",
                          "Cadmium (mg/kg Cd)",
                          "Copper (mg/kg Cu)",
                          "Chromium (mg/kg Cr)",
                          "Lead (mg/kg Pb)",
                          "Mercury (mg/kg Hg)",
                          "Nickel (mg/kg Ni)",
                          "Zinc (mg/kg Zn)"))
  
  bolivian_1333 <- read_csv("data/standards/bolivian_standards_1333.csv") |>
    mutate(match_name = c("pH", "pH", 
                          "Color (u PtCo)", 
                          "Total Dissolved Solids (mg/l)", 
                          "Oxygen Saturation (%)", 
                          "Biochemical Oxygen Demand (mg/l O2)", 
                          "Chemical Oxygen Demand (mg/l O2)", 
                          NA, NA, NA, 
                          "Total Arsenic (ug/l As)", 
                          NA, NA, 
                          "Total Boron (ug/l B)", 
                          "Total Cadmium (ug/l Cd)",
                          "Total Calcium (mg/l Ca)",
                          "Chlorides (mg/l Cl-)",
                          "Total Chromium (ug/l Cr)",
                          "Total Chromium (ug/l Cr)",
                          NA,
                          "Total Copper (ug/l Cu)",
                          "Total Iron (ug/l Fe)",
                          "Total Lead (ug/l Pb)",
                          NA,
                          "Total Magnesium (mg/l Mg)",
                          "Total Manganese (ug/l Mn)",
                          "Total Mercury (ug/l Hg)",
                          "Total Nickel (ug/l Ni)",
                          "Nitrate (mg/l NO3)",
                          "Total Kjeldahl Nitrogen (mg/l N)",
                          "Total Phosphorus (mg/l PO4)",
                          "Total Selenium (ug/l Se)",
                          "Total Silver (ug/l Ag)",
                          "Total Sodium (mg/l Na)",
                          "Sulfates (mg/l SO4)",
                          NA, NA, NA, 
                          "Total Zinc (ug/l Zn)"
    ))
  
  # Read and combine water data (clean version)
  all_water_clean <- reactive({
    water_files <- list.files(water_data_path_clean, pattern = "^water_\\d{4}_clean\\.xlsx$", full.names = TRUE)
    
    water_dfs <- lapply(water_files, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$Date <- as.Date(df$Date, "%d/%m/%Y")
      df
    })
    
    all_data <- bind_rows(water_dfs) |> 
      mutate(Station = str_replace(Station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(Station = str_replace(Station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba")) |>
      filter(!is.na(`Latitude Decimal`))
    
    return(all_data)
  })
  
  
  # Only points in Bolivia
  bol_water_clean <- reactive({
    water_data <- all_water_clean() |>
      st_as_sf(coords = c("Longitude Decimal", "Latitude Decimal"), crs = st_crs(bol_border))
    
    filtered_data <- st_filter(water_data, bol_border)
    
    coords <- st_coordinates(filtered_data)
    filtered_data |>
      mutate(
        `Longitude Decimal` = coords[, 1],
        `Latitude Decimal` = coords[, 2]
      ) |>
      st_drop_geometry()
  })
  
  
  # Read and combine water data (1333 version)
  all_water_1333 <- reactive({
    ## Nadav's Note: there are a bunch of these data manipulation functions. We want to:
    ### 1. All data (which can be in several formats) goes thru a unified function, where their media is considered and all relevant standards are calculated
    ### 2. Data is all compiled into one dataset that can be loaded in any part of the app
    ### 3. Uploaded data is added to this dataset 
    ### 4. All maps showing data use this single dataset
    ### 5. Users can still download pre-loaded datasets, or filter as needed
    
    water_files <- list.files(water_data_path_1333, pattern = "^water_\\d{4}_1333\\.xlsx$", full.names = TRUE)
    
    water_dfs <- lapply(water_files, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$Date <- as.Date(df$Date, "%Y-%m-%d")
      df
    })
    
    all_data <- bind_rows(water_dfs) |> 
      mutate(Station = str_replace(Station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(Station = str_replace(Station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba")) |>
      filter(!is.na(`Latitude Decimal`))
    
    # Count "Unclassified" in columns ending with "Class"
    all_data$num_unclass <- rowSums(
      select(all_data, ends_with("Class")) == "Unclassified",
      na.rm = TRUE
    )
    
    # Count "Class D" in columns ending with "Class"
    all_data$num_class_d <- rowSums(
      select(all_data, ends_with("Class")) == "Class D",
      na.rm = TRUE
    )
    
    # Count "Class C" in columns ending with "Class"
    all_data$num_class_c <- rowSums(
      select(all_data, ends_with("Class")) == "Class C",
      na.rm = TRUE
    )
    
    # Count "Class B" in columns ending with "Class"
    all_data$num_class_b <- rowSums(
      select(all_data, ends_with("Class")) == "Class B",
      na.rm = TRUE
    )
    
    all_data = all_data |>
      mutate(potato = rowSums(
        select(all_data, ends_with("Class")) == "Class B", na.rm = TRUE
      ))
    
    return(all_data)
  })
  
  
  # Only points in Bolivia
  bol_water_1333 <- reactive({
    water_data <- all_water_1333() |>
      st_as_sf(coords = c("Longitude Decimal", "Latitude Decimal"), crs = st_crs(bol_border))
    
    filtered_data <- st_filter(water_data, bol_border)
    
    coords <- st_coordinates(filtered_data)
    filtered_data |>
      mutate(
        `Longitude Decimal` = coords[, 1],
        `Latitude Decimal` = coords[, 2]
      ) |>
      st_drop_geometry()
  })
  
  
  
  ## Load sediment data ##
  
  all_sed_clean <- reactive({
    sed_files_clean <- list.files(sed_data_path_clean, pattern = "^sed_\\d{4}_clean\\.xlsx$", full.names = TRUE)
    
    sed_dfs_clean <- lapply(sed_files_clean, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$Date <- as.Date(df$Date, "%d/%m/%Y")
      df
    })
    
    df <- bind_rows(sed_dfs_clean) |>
      mutate(Station = str_replace(Station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(Station = str_replace(Station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba"))
    
    return(df)
  })
  
  
  # Only points in Bolivia
  bol_sed_clean <- reactive({
    # Convert to sf
    sed_data <- all_sed_clean() |>
      st_as_sf(coords = c("Long_dd", "Lat_dd"), crs = st_crs(bol_border))
    
    # Spatial filter
    filtered_data <- st_filter(sed_data, bol_border)
    
    # Extract coords and convert back
    coords <- st_coordinates(filtered_data)
    filtered_data |>
      mutate(
        Long_dd = coords[, 1],
        Lat_dd = coords[, 2]
      ) |>
      st_drop_geometry()
  })
  
  all_sed_usgs <- reactive({
    sed_files_usgs <- list.files(sed_data_path_usgs, pattern = "^sed_\\d{4}_usgs\\.xlsx$", full.names = TRUE)
    
    sed_dfs_usgs <- lapply(sed_files_usgs, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$Date <- as.Date(df$Date, "%d/%m/%Y")
      df
    })
    
    df <- bind_rows(sed_dfs_usgs) |>
      mutate(Station = str_replace(Station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(Station = str_replace(Station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba"))
    
    # Add Distance from Bank column from all_sed_clean
    df <- df |>
      left_join(
        all_sed_clean() |> select(Station, Date, `Sieve Size`, `Distance from Bank`),
        by = c("Station", "Date", "Sieve Size")
      )
    
    # 1. Get the USGS columns
    usgs_columns <- df %>% select(ends_with("USGS"))
    
    # 2. Count total number of non-NA parameters per row
    df$n_params <- rowSums(!is.na(usgs_columns))
    
    # 3. Count "Above TEL" and "Above PEL" per row
    df$num_above_tel <- rowSums(usgs_columns == "Above TEL", na.rm = TRUE)
    df$num_above_pel <- rowSums(usgs_columns == "Above PEL", na.rm = TRUE)
    
    # 4. Calculate standardized score
    df$sed_score <- (df$num_above_tel + df$num_above_pel * 2) / df$n_params
    
    df$unique <- paste(df$Station, df$Date, sep = " - ")
    
    return(df)
  })
  
  # Only Points west of Villamontes (only points in Bolivia)
  bol_border <- st_read("data/geojson/bol_borders.geojson")
  
  bol_sed_usgs <- reactive({
    # Convert the sediment data to sf object if it isn't already
    sed_data <- all_sed_usgs() |>
      st_as_sf(coords = c("Long_dd", "Lat_dd"), crs = st_crs(bol_border))
    
    # Filter points that fall within the Bolivia border polygon
    filtered_data <- st_filter(sed_data, bol_border)
    
    # If you need the data back as a regular dataframe with coordinates
    # you can extract coordinates and convert back
    coords <- st_coordinates(filtered_data)
    filtered_data |>
      mutate(
        Long_dd = coords[, 1],
        Lat_dd = coords[, 2]
      ) |>
      st_drop_geometry()
  })
  
  
  active_water_clean <- reactive({
    if(input$plot_data_scope == "bol") {
      bol_water_clean()
    } else {
      all_water_clean()
    }
  })
  
  active_water_1333 <- reactive({
    if(input$plot_data_scope == "bol") {
      bol_water_1333()
    } else {
      all_water_1333()
    }
  })
  
  
  active_sed_clean <- reactive({
    if(input$plot_data_scope == "bol") {
      bol_sed_clean()
    } else {
      all_sed_clean()
    }
  })
  
  
  active_sed_usgs <- reactive({
    if(input$plot_data_scope == "bol") {
      bol_sed_usgs()
    } else {
      all_sed_usgs()
    }
  })
  
  
  # Get list of years in the data
  
  
  water_years <- reactive({
    unique(active_water_clean()$Year)
  })
  
  water_years_1333 <- reactive({
    unique(active_water_1333()$Year)
  })
  
  sed_years <- reactive({
    unique(active_sed_clean()$Year)
  })
  
  sed_years_usgs <- reactive({
    unique(active_sed_usgs()$Year)
  })
  
  all_years <- reactive({
    sort(unique(c(water_years(), 
                  water_years_1333(),
                  sed_years(),
                  sed_years_usgs()
    )
    )
    ) 
  })
  
  ################# DOWNLOAD BUTTONS #########################
  
  output$download_year_ui <- renderUI({
    all_years <- all_years()
    
    selectInput("download_year", "Filter by Year (optional):",
                choices = c("All", all_years),
                selected = "All")
  })
  
  
  # Helper function to filter by year
  filter_by_year <- function(df, year_input) {
    if (year_input == "all") {
      return(df)
    } else {
      return(df %>% filter(Year == as.integer(year_input)))
    }
  }
  
  # Sediment Data (Clean)
  output$download_sed_clean <- downloadHandler(
    filename = function() {
      paste0("sed_", str_to_lower(input$download_year), "_clean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- if (input$data_scope == "bol") {
        bol_sed_clean()
      } else {
        all_sed_clean()
      }
      
      if (input$download_year != "All") {
        data <- data |> filter_by_year(input$download_year)
      }
      
      write_csv(data, file)
    }
  )
  
  # Sediment Data (USGS)
  output$download_sed_usgs <- downloadHandler(
    filename = function() {
      paste0("sed_", str_to_lower(input$download_year), "_usgs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- if (input$data_scope == "bol") {
        bol_sed_usgs()
      } else {
        all_sed_usgs()
      }
      
      if (input$download_year != "All") {
        data <- data |> filter_by_year(input$download_year)
      }
      
      write_csv(data, file)
    }
  )
  
  # Water Data (Clean)
  output$download_water_clean <- downloadHandler(
    filename = function() {
      paste0("water_", str_to_lower(input$download_year), "_clean_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- if (input$data_scope == "bol") {
        bol_water_clean()
      } else {
        all_water_clean()
      }
      
      if (input$download_year != "All") {
        data <- data |> filter_by_year(input$download_year)
      }
      
      write_csv(data, file)
    }
  )
  
  # Water Data (1333)
  output$download_water_1333 <- downloadHandler(
    filename = function() {
      paste0("water_", str_to_lower(input$download_year), "_1333_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- if (input$data_scope == "bol") {
        bol_water_1333()
      } else {
        all_water_1333()
      }
      
      if (input$download_year != "All") {
        data <- data |> filter_by_year(input$download_year)
      }
      
      write_csv(data, file)
    }
  )
  
  # USGS Standards Table
  output$download_usgs_standards <- downloadHandler(
    filename = function() {
      paste0("usgs_sqgs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      data <- usgs_sqg |>
        select(-match_name)
      
      write_csv(data, file)
    }
  )
  
  # Bolivian 1333 Standards Table
  output$download_1333_standards <- downloadHandler(
    filename = function() {
      paste0("bolivian_1333_stds_", Sys.Date(), ".csv")
    },
    content = function(file) {
      
      data <- bolivian_1333 |>
        select(-match_name)
      
      write_csv(data, file)
    }
  )
  
  
  ################# PCA #########################
  
  numeric_columns <- reactive({
    df <- active_water_1333()
    
    # Columns to exclude from parameter dropdown
    excluded_columns <- c("Decimal Latitude", "Decimal Longitude",
                          "Latitude Decimal", "Longitude Decimal", 
                          "Lat_dd", "Long_dd",
                          "Distance from Bank", "Distance from Shore",
                          "Clay (%)", "Silt (%)", "Sand (%)",
                          "0.032 mm - No. 450 (ASTM) (%)",
                          "0.063 mm - No. 230 (ASTM) (%)",
                          "0.125 mm - No. 120 (ASTM) (%)",
                          "0.250 mm - No. 060 (ASTM) (%)",
                          "0.500 mm - No. 035 (ASTM) (%)",
                          "1.00 mm - No. 018 (ASTM) (%)",
                          "2.00 mm - No. 010 (ASTM) (%)",
                          "Year", "0.016 mm (%)",
                          "4.75 mm - No. 004 (ASTM) (%)",
                          "num_unclass",
                          "num_class_b",
                          "num_class_c",
                          "num_class_d")
    
    possible_columns <- setdiff(names(df), excluded_columns)
    numeric_columns <- possible_columns[sapply(df[possible_columns], is.numeric)]
    
    numeric_columns
  })
  
  observe({
    updateSelectizeInput(inputId = "pca_parameters",
                         choices = numeric_columns(),
                         selected = c("pH", "pH (mV)", "Oxygen Saturation (%)", "Dissolved Oxygen (mg/l O2)"))
  })
  
  observeEvent(input$deselect_all_pca, {
    updateSelectizeInput(session, "pca_parameters", selected = character(0))
  })
  
  pca_result <- eventReactive(input$run_pca, {
    
    if (length(input$pca_parameters) < 2) stop("Please select 2 more more variables")
    
    df <- active_water_1333() %>%
      select(all_of(input$pca_parameters))
    
    # Remove columns that are entirely NA
    df <- df %>% select(where(~ !all(is.na(.))))
    
    # Estimate optimal number of components for imputation
    est <- estim_ncpPCA(df, method.cv = "Kfold", nbsim = 5)
    
    # Impute missing values
    impute_result <- imputePCA(df, ncp = est$ncp)
    
    # Run PCA
    PCA(impute_result$completeObs, graph = FALSE)
  })
  
  output$pca_plot <- renderPlot({
    req(pca_result())  # Assuming you have a reactive `pca_result()`
    
    fviz_pca_var(
      pca_result(),
      col.var = "cos2",  # Color by quality of representation
      gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
      repel = TRUE       # Avoid label overlap
    )
  })
  
  # Scree plot
  output$scree_plot <- renderPlot({
    req(pca_result())
    fviz_screeplot(pca_result(), addlabels = TRUE)
  })
  
  
  ################# RANKING PLOTS ############################
  
  
  # Map classes to numeric scores (0 = best, 4 = worst)
  class_map <- c(
    "Class A" = 0,
    "Class B" = 1,
    "Class C" = 2,
    "Class D" = 3,
    "Unclassified" = 4
  )
  
  # Identify classification columns
  class_cols <- reactive({
    grep(" Class$", colnames(active_water_1333()), value = TRUE)
  })
  
  observe({
    
    excluded_columns <- c("Decimal Latitude", "Decimal Longitude",
                          "Latitude Decimal", "Longitude Decimal", 
                          "Lat_dd", "Long_dd",
                          "Distance from Bank", "Distance from Shore",
                          "Average Velocity (m/s)", "Flow (m3/s)",
                          "Clay (%)", "Silt (%)", "Sand (%)",
                          "0.032 mm - No. 450 (ASTM) (%)",
                          "0.063 mm - No. 230 (ASTM) (%)",
                          "0.125 mm - No. 120 (ASTM) (%)",
                          "0.250 mm - No. 060 (ASTM) (%)",
                          "0.500 mm - No. 035 (ASTM) (%)",
                          "1.00 mm - No. 018 (ASTM) (%)",
                          "2.00 mm - No. 010 (ASTM) (%)",
                          "Year", "0.016 mm (%)",
                          "4.75 mm - No. 004 (ASTM) (%)",
                          "num_unclass",
                          "num_class_b",
                          "num_class_c",
                          "num_class_d")
    
    numeric_params <- active_water_1333() %>%  
      select(where(is.numeric)) %>%
      select(-any_of(excluded_columns)) %>%
      names()
    
    updateSelectInput(inputId = "station_plot_param",
                      choices = numeric_params,
                      selected = "Total Arsenic (ug/l As)")
    
    updateSelectInput(inputId = "observation_plot_param",
                      choices = numeric_params,
                      selected = "Total Arsenic (ug/l As)")
  })
  
  usgs_map <- c(
    "Below TEL" = 0,
    "Above TEL" = 1,
    "Above PEL" = 2
  )
  
  usgs_cols <- reactive({
    grep(" USGS$", colnames(active_sed_usgs()), value = TRUE)
  })
  
  observe({
    
    excluded_columns <- c("Decimal Latitude", "Decimal Longitude",
                          "Latitude Decimal", "Longitude Decimal", 
                          "Lat_dd", "Long_dd",
                          "Distance from Bank", "Distance from Shore",
                          "Average Velocity (m/s)", "Flow (m3/s)",
                          "Clay (%)", "Silt (%)", "Sand (%)",
                          "0.032 mm - No. 450 (ASTM) (%)",
                          "0.063 mm - No. 230 (ASTM) (%)",
                          "0.125 mm - No. 120 (ASTM) (%)",
                          "0.250 mm - No. 060 (ASTM) (%)",
                          "0.500 mm - No. 035 (ASTM) (%)",
                          "1.00 mm - No. 018 (ASTM) (%)",
                          "2.00 mm - No. 010 (ASTM) (%)",
                          "Year", "0.016 mm (%)",
                          "4.75 mm - No. 004 (ASTM) (%)",
                          "num_unclass",
                          "num_class_b",
                          "num_class_c",
                          "num_class_d")
    
    numeric_params_sed <- active_sed_clean() |>
      select(where(is.numeric)) |>
      select(-any_of(excluded_columns)) |>
      names()
    
    updateSelectInput(inputId = "station_plot_param_sed",
                      choices = numeric_params_sed,
                      selected = "Arsenic (mg/kg As)")
    
    updateSelectInput(inputId = "observation_plot_param_sed",
                      choices = numeric_params_sed,
                      selected = "Arsenic (mg/kg As)")
    
    updateSelectInput(inputId = "sieve_plot_param",
                      choices = numeric_params_sed,
                      selected = "Arsenic (mg/kg As)")
  })
  
  
  # Compute water quality score per observation (row)
  observation_scores <- reactive({
    active_water_1333() %>%
      rowwise() %>%
      mutate(
        water_score = mean(
          unlist(across(all_of(class_cols()), ~ class_map[.x])),
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      select(Station, Campaign, Date, Time, `Latitude Decimal`, `Longitude Decimal`, water_score, num_class_b, num_class_c, num_class_d, num_unclass) %>%
      filter(!is.nan(water_score))
  })
  
  
  output$observation_scores_plot <- renderPlotly({
    
    if (input$observation_plot_type == "class") {
      
      if (input$observation_plot_class == "worst_score") {
        p <- observation_scores() |>
          slice_max(water_score, n = 15) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, water_score)) |>
          ggplot(aes(x = label, y = water_score, 
                     text = paste("Water Quality Score:", round(water_score, 2)))) +
          geom_col(fill = "darkslateblue") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = "Overall Water Score: Top 15 Worst Observations (Bolivia)", 
            subtitle = "Lower scores indicate better water quality",
            x = NULL, y = "Water Quality Score (0=best, 4=worst)"
          )
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_class == "class_b") {
        p <- observation_scores() |>
          slice_max(num_class_b, n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, num_class_b)) |>
          ggplot(aes(x = label, y = num_class_b,
                     text = paste("# Class B Parameters:", num_class_b))) +
          geom_col(fill = "lightgreen") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = "# Class B: Top 15 Observations (Bolivia)",
            x = NULL, y = "Number of Class B Parameters"
          )
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_class == "class_c") {
        p <- observation_scores() |>
          slice_max(num_class_c, n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, num_class_c)) |>
          ggplot(aes(x = label, y = num_class_c,
                     text = paste("# Class C Parameters:", num_class_c))) +
          geom_col(fill = "gold") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = "# Class C: Top 15 Observations (Bolivia)",
            x = NULL, y = "Number of Class C Parameters"
          )
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_class == "class_d") {
        p <- observation_scores() |>
          slice_max(num_class_d, n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, num_class_d)) |>
          ggplot(aes(x = label, y = num_class_d,
                     text = paste("# Class D Parameters:", num_class_d))) +
          geom_col(fill = "darkorange") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = "# Class D: Top 15 Observations (Bolivia)",
            x = NULL, y = "Number of Class D Parameters"
          )
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_class == "unclassified") {
        p <- observation_scores() |>
          slice_max(num_unclass, n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, num_unclass)) |>
          ggplot(aes(x = label, y = num_unclass,
                     text = paste("# Unclassified Parameters:", num_unclass))) +
          geom_col(fill = "firebrick") +
          coord_flip() +
          theme_minimal() +
          labs(
            title = "# Unclassified: Top 15 Observations (Bolivia)",
            x = NULL, y = "Number of Unclassified Parameters"
          )
        ggplotly(p, tooltip = "text")
      }
      
    } else if (input$observation_plot_type == "value") {
      param <- input$observation_plot_param
      
      if (param == "Oxygen Saturation (%)" | param == "Dissolved Oxygen (mg/l O2)" | param == "pH" | param == "Resistivity (Ohm.cm)") {
        req(param)
        p <- active_water_1333() |>
          slice_min(.data[[param]], n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, -.data[[param]])) |>
          ggplot(aes(x = label, y = .data[[param]],
                     text = paste0(param, ": ", round(.data[[param]], 3)))) +
          geom_col(fill = "steelblue") +
          labs(title = paste("15 Lowest Observations for", param),
               x = NULL, y = param) +
          coord_flip() +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else {
        req(param)
        p <- active_water_1333() |>
          slice_max(.data[[param]], n = 15, with_ties = FALSE) |>
          mutate(label = paste0(Station, " (", Date, ")"),
                 label = fct_reorder(label, .data[[param]])) |>
          ggplot(aes(x = label, y = .data[[param]],
                     text = paste0(param, ": ", round(.data[[param]], 3)))) +
          geom_col(fill = "steelblue") +
          labs(title = paste("15 Highest Observations for", param),
               x = NULL, y = param) +
          coord_flip() +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      }
    } else if (input$observation_plot_type == "usgs") {
      
      df <- active_sed_usgs()
      
      if (input$observation_plot_usgs == "above_tel") {
        p <- df |>
          slice_max(num_above_tel, n = 15, with_ties = FALSE) |>
          mutate(
            label = paste0(Station, " (", Date, ")"),
            label = make.unique(label),
            label = fct_reorder(label, num_above_tel)) |>
          ggplot(aes(x = label, y = num_above_tel,
                     text = paste("# Above TEL:", num_above_tel, "<br>",
                                  "Sieve Size:", `Sieve Size`, "<br>",
                                  "Distance from Bank:", `Distance from Bank`))) +
          geom_col(fill = "darkorange") +
          labs(title = "# Above TEL: Top 15 Observations (Bolivia)",
               x = NULL, y = "Number of Parameters Above TEL") +
          coord_flip() +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_usgs == "above_pel") {
        p <- df |>
          slice_max(num_above_pel, n = 15, with_ties = FALSE) |>
          mutate(
            label = paste0(Station, " (", Date, ")"),
            label = make.unique(label),
            label = fct_reorder(label, num_above_pel)) |>
          ggplot(aes(x = label, y = num_above_pel,
                     text = paste("# Above PEL:", num_above_pel, "<br>",
                                  "Sieve Size:", `Sieve Size`, "<br>",
                                  "Distance from Bank:", `Distance from Bank`))) +
          geom_col(fill = "firebrick") +
          labs(title = "# Above PEL: Top 15 Observations (Bolivia)",
               x = NULL, y = "Number of Parameters Above PEL") +
          coord_flip() +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$observation_plot_usgs == "worst_score") {
        p <- df |>
          slice_max(sed_score, n = 15, with_ties = FALSE) |>
          mutate(
            label = paste0(Station, " (", Date, ")"),
            label = make.unique(label),
            label = fct_reorder(label, sed_score)) |>
          ggplot(aes(x = label, y = sed_score,
                     text = paste("Sediment Quality Score:", round(sed_score, 2), "<br>",
                                  "Sieve Size:", `Sieve Size`, "<br>",
                                  "Distance from Bank:", `Distance from Bank`))) +
          geom_col(fill = "darkslateblue") +
          labs(title = "Overall Sediment Score: Top 15 Observations (Bolivia)",
               x = NULL, y = "Sediment Quality Score (0=best, 2=worst)") +
          coord_flip() +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      }
      
      
      
    } else if (input$observation_plot_type == "sed_value") {
      
      param <- input$observation_plot_param_sed
      
      df <- active_sed_clean()
      
      req(param)
      p <- df |>
        slice_max(.data[[param]], n = 15, with_ties = FALSE) |>
        mutate(
          label = paste0(Station, " (", Date, ")"),
          label = make.unique(label),
          label = fct_reorder(label, .data[[param]])) |>
        ggplot(aes(x = label, y = .data[[param]],
                   text = paste0(param, ": ", round(.data[[param]], 3), "<br>",
                                 "Sieve Size:", `Sieve Size`, "<br>",
                                 "Distance from Bank:", `Distance from Bank`))) +
        geom_col(fill = "tan") +
        labs(title = paste("15 Highest Observations for", param),
             x = NULL, y = param) +
        coord_flip() +
        theme_minimal()
      
      ggplotly(p, tooltip = "text")
      
    }
    
  })
  
  
  
  # Calculate max date for recency weighting
  max_date <- reactive({
    max(active_water_1333()$Date, na.rm = TRUE)
  })
  
  # Calculate weighted normalized score per observation, then aggregate by Station
  station_scores <- reactive({
    observation_scores() %>%
      mutate(weight = 1 / (1 + as.numeric(difftime(max_date(), Date, units = "days")) / 365.25)) %>%
      group_by(Station) %>%
      summarise(
        mean_water_score = if (input$station_plot_recency == TRUE) weighted.mean(water_score, weight, na.rm = TRUE) else mean(water_score, na.rm = TRUE),
        mean_class_b = mean(num_class_b),
        mean_class_c = mean(num_class_c),
        mean_class_d = mean(num_class_d),
        mean_unclass = mean(num_unclass),
        `Latitude Decimal` = mean(`Latitude Decimal`),
        `Longitude Decimal` = mean(`Longitude Decimal`),
        n_obs = n(),
        .groups = "drop"
      ) %>%
      arrange(mean_water_score)  # lower = better water quality
  })
  
  station_scores_sed <- reactive({
    active_sed_usgs() |>
      mutate(weight = 1 / (1 + as.numeric(difftime(max_date(), Date, units = "days")))) |>
      group_by(Station) |>
      summarize(
        mean_sed_score = if (input$station_plot_recency_sed == TRUE) weighted.mean(sed_score, weight, na.rm = TRUE) else mean(sed_score, na.rm = TRUE),
        mean_above_tel = mean(num_above_tel),
        mean_above_pel = mean(num_above_pel),
        Lat_dd = mean(Lat_dd),
        Long_dd = mean(Long_dd),
        n_obs = n(),
        .groups = "drop"
      )
  })
  
  output$station_scores_plot <- renderPlotly({
    if (input$station_plot_type == "class") {
      
      if (input$station_plot_class == "worst_score") {
        p <- station_scores() |>
          slice_max(mean_water_score, n = 15) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_water_score), y = mean_water_score,
                     text = paste("Mean Water Quality Score:", round(mean_water_score, 2)))) +
          geom_col(fill = "darkslateblue") +
          coord_flip() +
          labs(
            title = "Overall Water Score: Top 15 Worst Stations (Bolivia)",
            subtitle = "Lower scores indicate better water quality",
            x = NULL,
            y = "Mean Water Quality Score (0=best, 4=worst)"
          ) +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$station_plot_class == "class_b") {
        p <- station_scores() |>
          arrange(mean_class_b) |>
          slice_max(mean_class_b, n = 15) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_class_b), y = mean_class_b,
                     text = paste("Mean # Class B Parameters:", round(mean_class_b, 2)))) +
          geom_col(fill = "lightgreen") +
          coord_flip() +
          labs(
            title = "Mean # Class B: Top 15 Stations (Bolivia)",
            subtitle = "Ranked by mean number of Class B parameters",
            x = NULL,
            y = "Mean number of Class B parameters"
          ) +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$station_plot_class == "class_c") {
        p <- station_scores() |>
          arrange(mean_class_c) |>
          slice_max(mean_class_c, n = 15) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_class_c), y = mean_class_c,
                     text = paste("Mean # Class C Parameters:", round(mean_class_c, 2)))) +
          geom_col(fill = "gold") +
          coord_flip() +
          labs(
            title = "Mean # Class C: Top 15 Stations (Bolivia)",
            subtitle = "Ranked by mean number of Class C parameters",
            x = NULL,
            y = "Mean number of Class C parameters"
          ) +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$station_plot_class == "class_d") {
        p <- station_scores() |>
          arrange(mean_class_d) |>
          slice_max(mean_class_d, n = 15) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_class_d), y = mean_class_d,
                     text = paste("Mean # Class D Parameters:", round(mean_class_d, 2)))) +
          geom_col(fill = "darkorange") +
          coord_flip() +
          labs(
            title = "Mean # Class D: Top 15 Stations (Bolivia)",
            subtitle = "Ranked by mean number of Class D parameters",
            x = NULL,
            y = "Mean number of Class D parameters"
          ) +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      } else if (input$station_plot_class == "unclassified") {
        p <- station_scores() |>
          arrange(mean_unclass) |>
          slice_max(mean_unclass, n = 15) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_unclass), y = mean_unclass,
                     text = paste("Mean # Unclassified Parameters:", round(mean_unclass, 2)))) +
          geom_col(fill = "firebrick") +
          coord_flip() +
          labs(
            title = "Mean # Unclassified: Top 15 Stations (Bolivia)",
            subtitle = "Ranked by mean number of Unclassified parameters",
            x = NULL,
            y = "Mean number of Unclassified parameters"
          ) +
          theme_minimal()
        ggplotly(p, tooltip = "text")
      }
      
    } else if (input$station_plot_type == "value" && !is.null(input$station_plot_param)) {
      
      # Get selected parameter
      param <- input$station_plot_param
      
      # Parameters that should use lowest values
      reverse_params <- c("Oxygen Saturation (%)", "Dissolved Oxygen (mg/l O2)", "pH", "Resistivity (Ohm.cm)")
      
      if (input$station_param_type == "max") {
        
        # Summarize max value per station
        summary_df <- active_water_1333() %>%
          group_by(Station) %>%
          summarise(
            max_value = max(.data[[param]], na.rm = TRUE),
            min_value = min(.data[[param]], na.rm = TRUE),
            n_obs = sum(!is.na(.data[[param]])),
            .groups = "drop"
          ) %>%
          filter(is.finite(max_value))
        
        if (param %in% reverse_params) {
          summary_df <- slice_min(summary_df, min_value, n = 15)
          
          req(param)
          
          p <- summary_df %>%
            mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
            ggplot(aes(x = reorder(Station_label, -min_value), y = min_value,
                       text = paste0("Min ", param, ": ", round(min_value, 3)))) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(
              title = paste("Bottom 15 Stations by Min", param),
              subtitle = "Minimum recorded value between 2016–2024",
              x = NULL,
              y = param
            ) +
            theme_minimal()
          
          ggplotly(p, tooltip = "text")
          
        } else {
          summary_df <- slice_max(summary_df, max_value, n = 15)
          
          req(param)
          
          p <- summary_df %>%
            mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
            ggplot(aes(x = reorder(Station_label, max_value), y = max_value,
                       text = paste0("Max ", param, ": ", round(max_value, 3)))) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(
              title = paste("Top 15 Stations by Max", param),
              subtitle = "Maximum recorded value between 2016–2024",
              x = NULL,
              y = param
            ) +
            theme_minimal()
          
          ggplotly(p, tooltip = "text")
          
        }
        
      } else if (input$station_param_type == "avg") {
        
        # Summarize average value per station
        summary_df <- active_water_1333() %>%
          group_by(Station) %>%
          summarise(
            avg_value = mean(.data[[param]], na.rm = TRUE),
            n_obs = sum(!is.na(.data[[param]])),
            .groups = "drop"
          ) %>%
          filter(is.finite(avg_value))
        
        if (param %in% reverse_params) {
          summary_df <- slice_min(summary_df, avg_value, n = 15)
          
          req(param)
          
          p <- summary_df %>%
            mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
            ggplot(aes(x = reorder(Station_label, -avg_value), y = avg_value,
                       text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(
              title = paste("Bottom 15 Stations by Average", param),
              subtitle = "Average value between 2016–2024",
              x = NULL,
              y = param
            ) +
            theme_minimal()
          
          ggplotly(p, tooltip = "text")
          
        } else {
          summary_df <- slice_max(summary_df, avg_value, n = 15)
          
          req(param)
          
          p <- summary_df %>%
            mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
            ggplot(aes(x = reorder(Station_label, avg_value), y = avg_value,
                       text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
            geom_col(fill = "steelblue") +
            coord_flip() +
            labs(
              title = paste("Top 15 Stations by Average", param),
              subtitle = "Average value between 2016–2024",
              x = NULL,
              y = param
            ) +
            theme_minimal()
          
          ggplotly(p, tooltip = "text")
          
        }
        
      }
    } else if (input$station_plot_type == "usgs") {
      
      if (input$station_plot_usgs == "worst_score") {
        
        p <- station_scores_sed() |>
          arrange(mean_sed_score) |>
          slice_max(mean_sed_score, n = 15, with_ties = FALSE) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_sed_score), y = mean_sed_score,
                     text = paste("Mean Sediment Quality Score:", round(mean_sed_score, 2)))) +
          geom_col(fill = "darkslateblue") +
          coord_flip() +
          labs(title = "Overall Sediment Score: Top 15 Worst Stations (Bolivia)",
               x = NULL, y = "Mean Sediment Quality Score (0=best, 2=worst)") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
        
        
      } else if (input$station_plot_usgs == "above_tel") {
        
        p <- station_scores_sed() |>
          arrange(mean_above_tel) |>
          slice_max(mean_above_tel, n = 15, with_ties = FALSE) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_above_tel), y = mean_above_tel,
                     text = paste("Mean # Above TEL:", round(mean_above_tel, 2)))) +
          geom_col(fill = "darkorange") +
          coord_flip() +
          labs(title = "Mean # Above TEL: Top 15 Worst Stations (Bolivia)",
               x = NULL, y = "Mean Number of Parameters Above TEL") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
        
        
      } else if (input$station_plot_usgs == "above_pel") {
        
        p <- station_scores_sed() |>
          arrange(mean_above_pel) |>
          slice_max(mean_above_pel, n = 15, with_ties = FALSE) |>
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) |>
          ggplot(aes(x = reorder(Station_label, mean_above_pel), y = mean_above_pel,
                     text = paste("Mean # Above PEL:", round(mean_above_pel, 2)))) +
          geom_col(fill = "firebrick") +
          coord_flip() +
          labs(title = "Mean # Above PEL: Top 15 Worst Stations (Bolivia)",
               x = NULL, y = "Mean Number of Parameters Above PEL") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
        
        
      }
    } else if (input$station_plot_type == "sed_value") {
      
      # Get selected parameter
      param <- input$station_plot_param_sed
      
      summary_df <- active_sed_clean() %>%
        group_by(Station) %>%
        summarise(
          max_value = max(.data[[param]], na.rm = TRUE),
          min_value = min(.data[[param]], na.rm = TRUE),
          avg_value = mean(.data[[param]], na.rm = TRUE),
          n_obs = sum(!is.na(.data[[param]])),
          .groups = "drop"
        ) %>%
        filter(is.finite(max_value))
      
      
      if (input$station_param_type == "max") {
        
        summary_df <- slice_max(summary_df, max_value, n = 15)
        
        req(param)
        
        p <- summary_df %>%
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
          ggplot(aes(x = reorder(Station_label, max_value), y = max_value,
                     text = paste0("Max ", param, ": ", round(max_value, 3)))) +
          geom_col(fill = "tan") +
          coord_flip() +
          labs(
            title = paste("Top 15 Stations by Max", param),
            subtitle = "Maximum recorded value between 2016–2024",
            x = NULL,
            y = param
          ) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      } else if (input$station_param_type == "avg") {
        
        summary_df <- slice_max(summary_df, avg_value, n = 15)
        
        req(param)
        
        p <- summary_df %>%
          mutate(Station_label = paste0(Station, " (n = ", n_obs, ")")) %>%
          ggplot(aes(x = reorder(Station_label, avg_value), y = avg_value,
                     text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
          geom_col(fill = "tan") +
          coord_flip() +
          labs(
            title = paste("Top 15 Stations by Average", param),
            subtitle = "Average value between 2016–2024",
            x = NULL,
            y = param
          ) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
      }
      
      
    }
  })
  
  plot_class_proportions_overlay <- function(data, class_cols = NULL, class_label, bar_color, plot_title, plot_subtitle = NULL, hover_text = NULL) {
    total_rows <- nrow(data)  # Total number of observations
    
    summary_df <- sapply(data[class_cols], function(col) {
      num_class <- sum(col == class_label, na.rm = TRUE)
      non_na_count <- sum(!is.na(col))
      
      prop_total <- num_class / total_rows
      prop_non_na <- if (non_na_count == 0) NA else num_class / non_na_count
      
      c(Percent_Total = prop_total*100,
        Percent_NonNA = prop_non_na*100)
    }) %>%
      t() %>%
      as.data.frame() %>%
      mutate(Parameter = rownames(.)) %>%
      rename(Percent_Total = Percent_Total,
             Percent_NonNA = Percent_NonNA)
    
    # Clean up parameter names
    summary_df$Parameter <- str_remove(summary_df$Parameter, " Class$")
    summary_df$Parameter <- str_remove(summary_df$Parameter, " USGS$")
    
    # Select top 15 parameters by total proportion
    top_15 <- summary_df %>%
      arrange(desc(Percent_Total)) %>%
      slice(1:15)
    
    # Prepare long format for plotting
    plot_data <- top_15 %>%
      pivot_longer(cols = c(Percent_Total, Percent_NonNA),
                   names_to = "Metric", values_to = "Value")
    
    # Set factor levels to preserve bar order
    plot_data$Parameter <- factor(plot_data$Parameter,
                                  levels = rev(top_15$Parameter))
    
    # Add hover text if provided
    if (!is.null(hover_text)) {
      # Create Type column to match the expected format
      plot_data$Type <- ifelse(plot_data$Metric == "Percent_Total", "Raw", "Standardized")
      plot_data$Proportion <- plot_data$Value
      
      # Apply the hover text function
      plot_data <- hover_text(plot_data)
      
      # Create the plot with hover text
      ggplot(plot_data, aes(x = Parameter, y = Value, fill = Metric, text = hover_text)) +
        geom_col(
          position = "identity",
          alpha = ifelse(plot_data$Metric == "Percent_Total", 1, 0.4)
        ) +
        scale_fill_manual(
          values = c(Percent_Total = bar_color, Percent_NonNA = bar_color),
          labels = c(
            Percent_Total = paste("Percent of", class_label, "over all observations"),
            Percent_NonNA = paste("Percent of", class_label, "over non-NA observations")
          )
        ) +
        coord_flip() +
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = NULL,
          y = "Percent",
          fill = NULL
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    } else {
      # Create the plot without hover text (original functionality)
      ggplot(plot_data, aes(x = Parameter, y = Value, fill = Metric)) +
        geom_col(
          position = "identity",
          alpha = ifelse(plot_data$Metric == "Percent_Total", 1, 0.4)
        ) +
        scale_fill_manual(
          values = c(Percent_Total = bar_color, Percent_NonNA = bar_color),
          labels = c(
            Percent_Total = paste("Percent of", class_label, "over all observations"),
            Percent_NonNA = paste("Percent of", class_label, "over non-NA observations")
          )
        ) +
        coord_flip() +
        labs(
          title = plot_title,
          subtitle = plot_subtitle,
          x = NULL,
          y = "Percent",
          fill = NULL
        ) +
        theme_minimal() +
        theme(legend.position = "none")
    }
  }
  
  observe({
    df <- active_water_1333()
    stations <- unique(df$Station)
    updateSelectInput(inputId = "param_plot_station",
                      choices = sort(stations))
  })
  
  observe({
    df <- active_sed_usgs()
    stations <- unique(df$Station)
    updateSelectInput(inputId = "sieve_plot_station",
                      choices = sort(stations))
  })
  
  active_water_1333_param_plot <- reactive({
    df <- active_water_1333()
    
    if (isTRUE(input$param_plot_checkbox)) {
      df <- df |>
        filter(Station == input$param_plot_station)
    }
    
    df
  })
  
  active_sed_usgs_param_plot <- reactive({
    df <- active_sed_usgs()
    
    if (isTRUE(input$param_plot_checkbox)) {
      df <- df |>
        filter(Station == input$param_plot_station)
    }
    
    df
  })
  
  # Standardized (per observation)
  param_scores_std <- reactive({
    active_water_1333 <- active_water_1333_param_plot()
    
    class_cols <- class_cols()
    
    sapply(active_water_1333[class_cols], function(col) {
      (sum(col == "Class B", na.rm = TRUE) * 1 +
         sum(col == "Class C", na.rm = TRUE) * 2 +
         sum(col == "Class D", na.rm = TRUE) * 3 +
         sum(col == "Unclassified", na.rm = TRUE) * 4) /
        sum(!is.na(col))
    })
  })
  
  # Unstandardized (raw totals)
  param_scores_raw <- reactive({
    active_water_1333 <- active_water_1333_param_plot()
    
    class_cols <- class_cols()
    
    sapply(active_water_1333[class_cols], function(col) {
      (sum(col == "Class B", na.rm = TRUE) * 1 +
         sum(col == "Class C", na.rm = TRUE) * 2 +
         sum(col == "Class D", na.rm = TRUE) * 3 +
         sum(col == "Unclassified", na.rm = TRUE) * 4) /
        length(col)
    })
  })
  
  # Same thing fro sediment data
  param_scores_std_sed <- reactive({
    active_sed_usgs <- active_sed_usgs_param_plot()
    
    usgs_cols <- usgs_cols()
    
    sapply(active_sed_usgs[usgs_cols], function(col) {
      (sum(col == "Above TEL", na.rm = TRUE) * 1 +
         sum(col == "Above PEL", na.rm = TRUE) * 2) /
        sum(!is.na(col))
    })
  })
  
  param_scores_raw_sed <- reactive({
    active_sed_usgs <- active_sed_usgs_param_plot()
    
    usgs_cols <- usgs_cols()
    
    sapply(active_sed_usgs[usgs_cols], function(col) {
      (sum(col == "Above TEL", na.rm = TRUE) * 1 +
         sum(col == "Above PEL", na.rm = TRUE) * 2) /
        length(col)
    })
  })
  
  
  
  # Combine into a long-format data frame
  param_scores_df <- reactive({
    data.frame(
      Parameter = names(param_scores_std()),
      Standardized = param_scores_std(),
      Raw = param_scores_raw()
    ) %>%
      pivot_longer(cols = c(Standardized, Raw), names_to = "Type", values_to = "Score")
  })
  
  param_scores_df_sed <- reactive({
    data.frame(
      Parameter = names(param_scores_std_sed()),
      Standardized = param_scores_std_sed(),
      Raw = param_scores_raw_sed()
    ) |>
      pivot_longer(cols = c(Standardized, Raw), names_to = "Type", values_to = "Score")
  })
  
  plot_data <- reactive({
    df <- param_scores_df()
    
    # Clean parameter names
    df$Parameter <- str_remove(df$Parameter, " Class$")
    
    # Get top 15 by Raw score
    top_15_params <- df %>%
      filter(Type == "Raw") %>%
      slice_max(Score, n = 15, with_ties = FALSE) %>%
      pull(Parameter)
    
    # Filter and reorder factor levels
    df_filtered <- df %>%
      filter(Parameter %in% top_15_params)
    
    df_filtered$Parameter <- factor(df_filtered$Parameter, levels = df_filtered %>%
                                      filter(Type == "Raw") %>%
                                      arrange(Score) %>%
                                      pull(Parameter))
    # Return final df
    df_filtered
  })
  
  plot_data_sed <- reactive({
    df <- param_scores_df_sed()
    
    # Clean parameter names
    df$Parameter <- str_remove(df$Parameter, " USGS$")
    
    # Get top 15 by Raw score
    top_15_params <- df %>%
      filter(Type == "Raw") %>%
      slice_max(Score, n = 15, with_ties = FALSE) %>%
      pull(Parameter)
    
    # Filter and reorder factor levels
    df_filtered <- df %>%
      filter(Parameter %in% top_15_params)
    
    df_filtered$Parameter <- factor(df_filtered$Parameter, levels = df_filtered %>%
                                      filter(Type == "Raw") %>%
                                      arrange(Score) %>%
                                      pull(Parameter))
    # Return final df
    df_filtered
  })
  
  
  # --- Step 3: Assign unique colors ---
  
  output$param_scores_plot <- renderPlotly({
    
    if (input$param_plot_type == "class") {
      plot_type <- input$param_plot_class
      
      if (plot_type == "unclassified") {
        p <- plot_class_proportions_overlay(
          data = active_water_1333_param_plot(),
          class_cols = class_cols(),
          class_label = "Unclassified",
          bar_color = "firebrick",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Unclassified: Top 15 Parameters at", input$param_plot_station),
            "% Unclassified: Top 15 Parameters"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% Unclassified (all observations): ", round(data$Proportion, 2)),
              paste0("% Unclassified (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
      } else if (plot_type == "class_d") {
        p <- plot_class_proportions_overlay(
          data = active_water_1333_param_plot(),
          class_cols = class_cols(),
          class_label = "Class D",
          bar_color = "darkorange",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Class D: Top 15 Worst Parameters at", input$param_plot_station),
            "% Class D: Top 15 Worst Parameters"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% Class D (all observations): ", round(data$Proportion, 2)),
              paste0("% Class D (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
      } else if (plot_type == "class_c") {
        p <- plot_class_proportions_overlay(
          data = active_water_1333_param_plot(),
          class_cols = class_cols(),
          class_label = "Class C",
          bar_color = "gold",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Class C: Top 15 Parameters at", input$param_plot_station),
            "% Class C: Top 15 Parameters"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% Class C (all observations): ", round(data$Proportion, 2)),
              paste0("% Class C (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
      } else if (plot_type == "class_b") {
        p <- plot_class_proportions_overlay(
          data = active_water_1333_param_plot(),
          class_cols = class_cols(),
          class_label = "Class B",
          bar_color = "lightgreen",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Class B: Top 15 Parameters at", input$param_plot_station),
            "% Class B: Top 15 Parameters"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% Class B (all observations): ", round(data$Proportion, 2)),
              paste0("% Class B (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
      } else if (plot_type == "worst_score") {
        
        p <- ggplot(plot_data(), aes(x = Parameter, y = Score)) +
          geom_col(position = "identity",
                   aes(alpha = Type, 
                       text = ifelse(
                         Type == "Raw",
                         paste0("Overall Score (all observations): ", round(Score, 2)),
                         paste0("Overall Score (non-NA observations): ", round(Score, 2))
                       )), show.legend = FALSE,
                   fill = "darkslateblue") +
          scale_alpha_manual(values = c(Raw = 1, Standardized = 0.4)) +
          coord_flip() +
          labs(
            title = plot_title <- ifelse(
              isTRUE(input$param_plot_checkbox),
              paste("Overall: Top 15 Worst Parameters at", input$param_plot_station),
              "Overall: Top 15 Worst Parameters"
            ),
            subtitle = "Dark bars = weighted counts / total observations\nLight bars = weighted counts / total non-NA observations",
            x = NULL,
            y = "Water Quality Score (0=best, 4=worst)"
          ) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
      }
    } else if (input$param_plot_type == "usgs") {
      
      plot_type <- input$param_plot_usgs
      
      if (plot_type == "worst_score") {
        
        p <- ggplot(plot_data_sed(), aes(x = Parameter, y = Score)) +
          geom_col(position = "identity",
                   aes(alpha = Type,
                       text = ifelse(
                         Type == "Raw",
                         paste0("Overall Score (all observations): ", round(Score, 2)),
                         paste0("Overall Score (non-NA observations): ", round(Score, 2))
                       )), show.legend = FALSE,
                   fill = "darkslateblue") +
          scale_alpha_manual(values = c(Raw = 1, Standardized = 0.4)) +
          coord_flip() +
          labs(
            title = plot_title <- ifelse(
              isTRUE(input$param_plot_checkbox),
              paste("Overall Score: Sediment Parameters Ranked at", input$param_plot_station),
              "Overall Score: Sediment Parameters Ranked"
            ),
            subtitle = "Dark bars = weighted counts / total observations\nLight bars = weighted counts / total non-NA observations",
            x = NULL,
            y = "Water Quality Score (0=best, 4=worst)"
          ) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      } else if(plot_type == "above_tel") {
        
        p <- plot_class_proportions_overlay(
          data = active_sed_usgs_param_plot(),
          class_cols = usgs_cols(),
          class_label = "Above TEL",
          bar_color = "darkorange",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Above TEL: Sediment Parameters Ranked at", input$param_plot_station),
            "% Above TEL: Sediment Parameters Ranked"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% ", data$Parameter, " Above TEL (all observations): ", round(data$Proportion, 2)),
              paste0("% ", data$Parameter, " Above TEL (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
        
      } else if (plot_type == "above_pel") {
        
        p <- plot_class_proportions_overlay(
          data = active_sed_usgs_param_plot(),
          class_cols = usgs_cols(),
          class_label = "Above PEL",
          bar_color = "firebrick",
          plot_title <- ifelse(
            isTRUE(input$param_plot_checkbox),
            paste("% Above PEL: Sediment Parameters Ranked at", input$param_plot_station),
            "% Above PEL: Sediment Parameters Ranked"
          ),
          plot_subtitle = "Dark bars = count / total observations\nLight bars = count / total non-NA observations",
          hover_text = function(data) {
            data$hover_text <- ifelse(
              data$Type == "Raw",
              paste0("% ", data$Parameter, " Above PEL (all observations): ", round(data$Proportion, 2)),
              paste0("% ", data$Parameter, " Above PEL (non-NA observations): ", round(data$Proportion, 2))
            )
            return(data)
          }
        )
        
        ggplotly(p, tooltip = "text")
        
      }
      
    }
    
  })
  
  
  output$sieve_scores_plot <- renderPlotly({
    req(input$sieve_plot_type)
    
    if (input$sieve_plot_type == "sed_value") {
      req(input$sieve_plot_param)
      df <- active_sed_clean()
      
      param <- input$sieve_plot_param
      
      plot_df <- df |>
        group_by(`Sieve Size`) |>
        summarise(mean_value = mean(.data[[param]]),
                  max_value = max(.data[[param]]))
      
      if (input$sieve_param_type == "max") {
        
        p <- plot_df |>
          ggplot(aes(x = reorder(`Sieve Size`, max_value), 
                     y = max_value,
                     text = paste0("Max ", param, ": ", round(max_value, 2)))) +
          geom_col(fill = "tan") +
          coord_flip() + 
          labs(title = paste("Sieve Sizes Ranked by Max", param),
               x = NULL, y = paste("Max", param)) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      } else if (input$sieve_param_type == "avg") {
        
        p <- plot_df |>
          ggplot(aes(x = reorder(`Sieve Size`, mean_value), 
                     y = mean_value,
                     text = paste0("Mean ", param, ": ", round(mean_value, 2)))) +
          geom_col(fill = "tan") +
          coord_flip() +
          labs(title = paste("Sieve Sizes Ranked by Mean", param),
               x = NULL, y = paste("Mean", param)) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      }
      
      
    } else if (input$sieve_plot_type == "usgs") {
      req(input$sieve_plot_usgs)
      
      df <- active_sed_usgs()
      
      if (input$sieve_plot_checkbox == TRUE) {
        df <- df |>
          filter(Station == input$sieve_plot_station)
      }
      
      plot_df <- df |>
        group_by(`Sieve Size`) |>
        summarise(mean_score = mean(sed_score),
                  mean_above_pel = mean(num_above_pel),
                  mean_above_tel = mean(num_above_tel)
        )
      
      if (input$sieve_plot_usgs == "worst_score") {
        
        p <- plot_df |>
          ggplot(aes(x = reorder(`Sieve Size`, mean_score), 
                     y = mean_score,
                     text = paste0("Overall Score: ", round(mean_score, 3)))) +
          geom_col(fill = "darkslateblue") +
          coord_flip() +
          labs(title = "Sieve Sizes Ranked by Overall Score",
               x = NULL, y = "Mean Overall Score (0=best, 2=worst)") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      } else if (input$sieve_plot_usgs == "above_pel") {
        
        p <- plot_df |>
          ggplot(aes(x = reorder(`Sieve Size`, mean_above_pel), 
                     y = mean_above_pel,
                     text = paste0("Mean # Params Above PEL: ", round(mean_above_pel, 3)))) +
          geom_col(fill = "firebrick") +
          coord_flip() +
          labs(title = "Sieve Sizes Ranked by Mean # Above PEL",
               x = NULL, y = "Mean Number of Parameters Above PEL (per sample)") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      } else if (input$sieve_plot_usgs == "above_tel") {
        
        p <- plot_df |>
          ggplot(aes(x = reorder(`Sieve Size`, mean_above_tel), 
                     y = mean_above_tel,
                     text = paste0("Mean # Params Above TEL: ", round(mean_above_tel, 3)))) +
          geom_col(fill = "darkorange") +
          coord_flip() +
          labs(title = "Sieve Sizes Ranked by Mean # Above TEL",
               x = NULL, y = "Mean Number of Parameters Above TEL (per sample)") +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
        
      }
      
    }
    
  })
  
  
  ################# SLIDER MAPS ########################
  
  # Get current active dataset based on selected tab
  current_data <- reactive({
    if(is.null(input$map_tabs) || input$map_tabs == "parameter_map") {
      active_water_clean()
    } else {
      active_water_1333()
    }
  })
  
  # Chronologically sorted Campaigns (uses current active dataset)
  unique_campaigns <- reactive({
    df <- current_data()
    req(nrow(df) > 0)
    
    campaigns <- unique(df$Campaign)
    campaigns <- campaigns[!is.na(campaigns)]
    
    campaign_dates <- my(campaigns)  # Convert to Date using lubridate
    sorted_campaigns <- campaigns[order(campaign_dates)]
    
    return(sorted_campaigns)
  })
  
  # ===== PARAMETER MAP (Tab 1) LOGIC =====
  
  # Columns to exclude from parameter dropdown
  excluded_columns <- c("Decimal Latitude", "Decimal Longitude",
                        "Latitude Decimal", "Longitude Decimal", 
                        "Lat_dd", "Long_dd",
                        "Distance from Bank", "Distance from Shore",
                        "Clay (%)", "Silt (%)", "Sand (%)",
                        "0.032 mm - No. 450 (ASTM) (%)",
                        "0.063 mm - No. 230 (ASTM) (%)",
                        "0.125 mm - No. 120 (ASTM) (%)",
                        "0.250 mm - No. 060 (ASTM) (%)",
                        "0.500 mm - No. 035 (ASTM) (%)",
                        "1.00 mm - No. 018 (ASTM) (%)",
                        "2.00 mm - No. 010 (ASTM) (%)",
                        "Year", "0.016 mm (%)",
                        "4.75 mm - No. 004 (ASTM) (%)"
  )
  
  output$parameter_selector_ui <- renderUI({
    df <- active_water_clean()
    possible_columns <- setdiff(names(df), excluded_columns)
    # Only numeric columns
    numeric_columns <- possible_columns[sapply(df[possible_columns], is.numeric)]
    
    selectInput("selected_parameter", 
                "Select Parameter:", 
                choices = sort(numeric_columns),
                selected = "Total Arsenic (ug/l As)")
  })
  
  # Reactive color palette based on selected parameter
  color_pal <- reactive({
    req(input$selected_parameter)
    df <- active_water_clean()
    vals <- df[[input$selected_parameter]]
    vals <- vals[vals > 0 & !is.na(vals)]  # exclude zeros/non-positives for log scale
    
    if(length(vals) == 0) vals <- c(1, 10)
    
    # Generate log-spaced breaks
    bins <- 10^seq(floor(log10(min(vals))),
                   ceiling(log10(max(vals))),
                   length.out = 8)
    
    bins <- c(-Inf, bins[-1], Inf)
    
    colorBin(
      palette = "Reds",
      bins = bins,
      domain = vals,
      pretty = FALSE
    )
  })
  
  # Radius scaling function for parameter map
  size_pal_param <- reactive({
    req(input$selected_parameter)
    df <- active_water_clean()
    vals <- df[[input$selected_parameter]]
    vals <- vals[!is.na(vals)]
    
    function(values) {
      if(length(vals) == 0) return(rep(4, length(values)))
      scaled <- scales::rescale(values, to = c(4, 14), from = range(vals, na.rm = TRUE))
      scaled[is.na(scaled)] <- 4
      scaled
    }
  })
  
  # ===== CLASSIFICATION MAP (Tab 2) LOGIC =====
  
  # Detect metals from columns ending with " Class"
  metals <- reactive({
    df <- active_water_1333()
    class_cols <- names(df)[stringr::str_ends(names(df), " Class")]
    metals <- stringr::str_remove(class_cols, " Class$")
    metals
  })
  
  output$metal_selector_ui <- renderUI({
    metals_list <- metals()
    selectInput("selected_metal", "Select Parameter:", choices = metals_list, selected = "Arsenic")
  })
  
  # Selected class and value columns based on selected metal
  class_col <- reactive({
    req(input$selected_metal)
    paste0(input$selected_metal, " Class")
  })
  
  value_col <- reactive({
    req(input$selected_metal)
    df <- active_water_1333()
    selected <- str_to_lower(input$selected_metal)
    
    non_class_cols <- names(df)[!str_detect(names(df), regex("class$", ignore_case = TRUE))]
    
    # Use whole-word matching to avoid partial matches like "Phosphorus"
    pattern <- regex(paste0("\\b", selected, "\\b"), ignore_case = TRUE)
    matching_cols <- non_class_cols[str_detect(non_class_cols, pattern)]
    
    if (length(matching_cols) == 0) {
      cat("No matching columns found.\n")
      return(NULL)
    }
    
    total_cols <- matching_cols[str_detect(matching_cols, regex("total", ignore_case = TRUE))]
    other_cols <- setdiff(matching_cols, total_cols)
    
    rank_cols <- function(cols) {
      cols[order(
        !str_to_lower(cols) == selected,
        !str_starts(str_to_lower(cols), selected)
      )]
    }
    
    ranked_total <- rank_cols(total_cols)
    ranked_other <- rank_cols(other_cols)
    
    final_choice <- c(ranked_total, ranked_other)[1]
    
    final_choice
  })
  
  
  
  
  
  # Reactive color palette for classes
  class_levels <- c("Class A", "Class B", "Class C", "Class D", "Unclassified")
  
  class_pal <- reactive({
    colorFactor(
      palette = c("lightblue", "lightgreen", "gold", "darkorange", "darkred"),
      levels = class_levels,
      na.color = "lightgray"
    )
  })
  
  # Radius scaling for classification map
  size_pal_class <- reactive({
    req(value_col())
    df <- active_water_1333()
    vals <- df[[value_col()]]
    vals <- vals[!is.na(vals)]
    
    function(values) {
      if(length(vals) == 0) return(rep(4, length(values)))
      scaled <- scales::rescale(values, to = c(4, 14), from = range(vals, na.rm = TRUE))
      scaled[is.na(scaled)] <- 4
      scaled
    }
  })
  
  # ===== COMMON UI ELEMENTS =====
  
  output$map_date_slider_ui <- renderUI({
    
    campaigns <- unique_campaigns()
    req(length(campaigns) > 0)
    
    sliderTextInput("campaign_index",
                    "Select Campaign:",
                    choices = campaigns,
                    selected = campaigns[1],
                    animate = animationOptions(interval = 300, loop = FALSE))
  })
  
  
  # ===== MAP DATA PREPARATION =====
  
  # Map data for parameter map
  map_data_param <- reactive({
    df <- active_water_clean()
    campaigns <- unique_campaigns()
    req(input$campaign_index, length(campaigns) > 0)
    req(input$selected_parameter)
    
    selected_campaign <- input$campaign_index
    
    df$CampaignYM <- as.yearmon(df$Campaign, "%B %Y")
    selected_campaign_ym <- as.yearmon(selected_campaign, "%B %Y")
    
    stations <- unique(df$Station[!is.na(df$Station)])
    
    map_points <- lapply(stations, function(station) {
      station_data <- df %>%
        filter(Station == station,
               !is.na(CampaignYM),
               CampaignYM <= selected_campaign_ym)
      
      # Filter out NA for selected parameter
      station_data <- station_data[!is.na(station_data[[input$selected_parameter]]), ]
      
      if (nrow(station_data) > 0) {
        station_data <- station_data[order(station_data$Date, decreasing = TRUE), ]
        
        # Precompute popup text for leaflet
        popup_text <- paste0(
          "Station: ", station_data$Station[1], "<br>",
          "Campaign: ", station_data$Campaign[1], "<br>",
          "Date: ", format(station_data$Date[1], "%Y-%m-%d"), "<br>",
          input$selected_parameter, ": ", station_data[[input$selected_parameter]][1]
        )
        
        station_data$popup_text <- popup_text
        return(station_data[1, ])
      } else {
        return(NULL)
      }
    })
    
    map_points <- do.call(rbind, map_points[!sapply(map_points, is.null)])
    return(map_points)
  })
  
  # Map data for classification map
  map_data_class <- reactive({
    df <- active_water_1333()
    campaigns <- unique_campaigns()
    req(input$campaign_index, length(campaigns) > 0)
    req(input$selected_metal)
    req(value_col())
    
    selected_campaign <- input$campaign_index
    
    df$CampaignYM <- as.yearmon(df$Campaign, "%B %Y")
    selected_campaign_ym <- as.yearmon(selected_campaign, "%B %Y")
    
    stations <- unique(df$Station[!is.na(df$Station)])
    
    map_points <- lapply(stations, function(station) {
      station_data <- df %>%
        filter(Station == station,
               !is.na(CampaignYM),
               CampaignYM <= selected_campaign_ym)
      
      # Filter to rows with class and value available
      station_data <- station_data[
        !is.na(station_data[[class_col()]]) &
          !is.na(station_data[[value_col()]]),
      ]
      
      if (nrow(station_data) > 0) {
        station_data <- station_data[order(station_data$Date, decreasing = TRUE), ]
        
        popup_text <- paste0(
          "Station: ", station_data$Station[1], "<br>",
          "Campaign: ", station_data$Campaign[1], "<br>",
          "Date: ", format(station_data$Date[1], "%Y-%m-%d"), "<br>",
          class_col(), ": ", station_data[[class_col()]][1], "<br>",
          value_col(), ": ", station_data[[value_col()]][1]
        )
        
        station_data$popup_text <- popup_text
        return(station_data[1, ])
      } else {
        return(NULL)
      }
    })
    
    map_points <- do.call(rbind, map_points[!sapply(map_points, is.null)])
    return(map_points)
  })
  
  # ===== MAP OUTPUTS =====
  
  # Initialize parameter map
  output$parameter_timeline_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = pilco_line, 
                   color = "darkcyan", 
                   weight = 3, 
                   opacity = 0.8) %>%
      addPolygons(data = bol_border,
                  color = "black",
                  weight = 3,
                  fill = FALSE) %>%
      setView(lng = -63.5, lat = -21.3, zoom = 7)
  })
  
  # Initialize classification map
  output$classification_timeline_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = pilco_line, 
                   color = "darkcyan", 
                   weight = 3, 
                   opacity = 0.8) %>%
      addPolygons(data = bol_border,
                  color = "black",
                  weight = 3,
                  fill = FALSE) %>%
      setView(lng = -63.5, lat = -21.3, zoom = 7)
  })
  
  # Update parameter map markers
  observe({
    req(input$map_tabs == "parameter_map")
    map_data_points <- map_data_param()
    req(nrow(map_data_points) > 0)
    
    pal <- color_pal()
    param <- input$selected_parameter
    
    radius_values <- size_pal_param()(map_data_points[[param]])
    
    leafletProxy("parameter_timeline_map", data = map_data_points) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~`Longitude Decimal`,
        lat = ~`Latitude Decimal`,
        radius = radius_values,
        popup = ~popup_text,
        stroke = TRUE,
        fillOpacity = 0.8,
        fillColor = ~pal(get(param)),
        color = "black",
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = map_data_points[[param]],
        title = paste0(param, "<br>(log transformed)"),
        opacity = 1
      )
  })
  
  # Update classification map markers
  observe({
    req(input$map_tabs == "classification_map")
    map_data_points <- map_data_class()
    req(nrow(map_data_points) > 0)
    req(class_pal())
    
    pal <- class_pal()
    size_fun <- size_pal_class()
    
    fill_colors <- pal(map_data_points[[class_col()]])
    
    leafletProxy("classification_timeline_map", data = map_data_points) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~`Longitude Decimal`,
        lat = ~`Latitude Decimal`,
        radius = size_fun(map_data_points[[value_col()]]),
        popup = ~popup_text,
        stroke = TRUE,
        fillOpacity = 0.8,
        fillColor = fill_colors,
        color = "black",
        weight = 1
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = map_data_points[[class_col()]],
        title = class_col(),
        opacity = 1
      )
  })
  
  
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  ############# TIME SERIES #######################
  
  
  
  
  
  # # Bind data from all years into one data frame for use in time series
  # all_sediment_data <- reactive({
  #   sed_files <- list.files(sed_data_path_clean, pattern = "^sed_\\d{4}_clean\\.xlsx$", full.names = TRUE)
  #   
  #   sed_dfs <- lapply(sed_files, function(f) {
  #     year <- stringr::str_extract(basename(f), "\\d{4}")
  #     df <- readxl::read_xlsx(f)
  #     df$Year <- as.integer(year)
  #     df$Date <- as.Date(df$Date, "%d/%m/%Y")
  #     df
  #   })
  #   
  #   bind_rows(sed_dfs)
  # })
  # 
  # all_water_data <- reactive({
  #   water_files <- list.files(water_data_path_clean, pattern = "^water_\\d{4}_clean\\.xlsx$", full.names = TRUE)
  #   
  #   water_dfs <- lapply(water_files, function(f) {
  #     year <- stringr::str_extract(basename(f), "\\d{4}")
  #     df <- readxl::read_xlsx(f)
  #     df$Year <- as.integer(year)
  #     df$Date <- as.Date(df$Date, "%d/%m/%Y")
  #     df
  #   })
  #   
  #   bind_rows(water_dfs)
  # })
  
  # dynamically update choices for time series station & parameters
  observe({
    req(input$ts_tabs)  
    
    if (input$ts_tabs == "Water Samples") {
      df <- active_water_clean()
    } else if (input$ts_tabs == "Sediment Samples") {
      df <- active_sed_clean()
    } else {
      return()
    }
    
    updateSelectInput(session, "ts_station", choices = sort(unique(df$Station)), selected = "Tarapaya")
    
    param_cols <- df %>%
      select(where(is.numeric)) %>%
      select(-any_of(c("Decimal Latitude",
                       "Decimal Longitude",
                       "Latitude Decimal", 
                       "Longitude Decimal", 
                       "Lat_dd", 
                       "Long_dd",
                       "Distance from Bank",
                       "Distance from Shore",
                       "Clay (%)", "Silt (%)", "Sand (%)",
                       "0.032 mm - No. 450 (ASTM) (%)",
                       "0.063 mm - No. 230 (ASTM) (%)",
                       "0.125 mm - No. 120 (ASTM) (%)",
                       "0.250 mm - No. 060 (ASTM) (%)",
                       "0.500 mm - No. 035 (ASTM) (%)",
                       "1.00 mm - No. 018 (ASTM) (%)",
                       "2.00 mm - No. 010 (ASTM) (%)",
                       "Year", "0.016 mm (%)",
                       "4.75 mm - No. 004 (ASTM) (%)"
      ))) %>%
      colnames()
    
    updateSelectInput(session, "ts_param", choices = param_cols)
    
    observeEvent(input$ts_station, {
      req(input$ts_station)  # Ensure a station is selected
      
      # Filter to the selected station
      station_data <- df %>% filter(Station == input$ts_station)
      
      # Find param columns with any non-NA values
      valid_params <- station_data %>%
        select(all_of(param_cols)) %>%
        select(where(~ any(!is.na(.)))) %>%
        colnames()
      
      # Update parameter dropdown
      updateSelectInput(session, "ts_param",
                        choices = sort(valid_params),
                        selected = ifelse(
                          input$ts_tabs == "Water Samples",
                          "Total Arsenic (ug/l As)",
                          "Arsenic (mg/kg As)"))
    })
    
  })
  
  
  ts_filtered_data_water <- reactive({
    df <- active_water_clean()
    req(input$ts_station, input$ts_param)
    
    if (!(input$ts_param %in% colnames(df))) {
      return(data.frame())  # Prevent error by returning empty
    }
    
    df %>%
      filter(Station == input$ts_station) %>%
      select(Date, value = all_of(input$ts_param)) %>%
      filter(!is.na(value))
  })
  
  # Initial filtering of parameter and station
  ts_filtered_data_sed_init <- reactive({
    df <- active_sed_clean()
    req(input$ts_station, input$ts_param)
    
    if (!(input$ts_param %in% colnames(df))) {
      return(data.frame())  # Prevent error by returning empty
    }
    
    df %>%
      filter(Station == input$ts_station) %>%
      select(Date, `Sieve Size`, value = all_of(input$ts_param), `Distance from Bank`) %>%
      filter(!is.na(value))
  })
  
  # Update sieve (tamiz) choices based on station and parameter
  observe({
    req(input$ts_tabs == "Sediment Samples", input$ts_tamiz_checkbox)
    df <- ts_filtered_data_sed_init()
    
    if (!"Sieve Size" %in% colnames(df)) return()
    
    ts_tamiz_choices <- sort(unique(df$`Sieve Size`))
    
    updateSelectInput(session, "ts_tamiz", choices = ts_tamiz_choices, selected = ts_tamiz_choices[1])
  })
  
  # filter for sieve (tamiz)
  ts_filtered_data_sed <- reactive({
    df <- ts_filtered_data_sed_init()
    
    if (isTRUE(input$ts_tamiz_checkbox)) {
      req(input$ts_tamiz)
      df <- df %>% filter(`Sieve Size` == input$ts_tamiz)
    }
    
    df
    
  })
  
  ts_standard_values <- reactive({
    req(input$ts_tabs, input$ts_param)
    
    if (input$ts_tabs == "Water Samples") {
      df <- bolivian_1333
      
      # Handle NA values in the comparison
      matches <- df$match_name == input$ts_param & !is.na(df$match_name)
      
      if (sum(matches) > 0) {
        class_a <- df$`Class A`[matches][1]
        class_b <- df$`Class B`[matches][1]
        class_c <- df$`Class C`[matches][1]
        class_d <- df$`Class D`[matches][1]
        
        if (any(is.na(c(class_a, class_b, class_c, class_d)))) {
          return(NULL)
        }
        classes <- c(class_a, class_b, class_c, class_d)
        
        if (grepl("ug/", input$ts_param)){
          classes <- 1000*classes
        }
        
        return(classes)
      } else {
        return(NULL)
      }
      
    } else if (input$ts_tabs == "Sediment Samples") {
      df <- usgs_sqg
      
      matches <- df$match_name == input$ts_param & !is.na(df$match_name)
      
      if(sum(matches) > 0) {
        tel <- df$TEL[matches][1]
        pel <- df$PEL[matches][1]
        
        if (any(is.na(c(tel, pel)))) {
          return(NULL)
        }
        
        thresholds <- c(tel, pel)
        
        return(thresholds)
      } else {
        return(NULL)
      }
      
    }
  })
  
  output$ts_plot_water <- renderPlotly({
    df <- ts_filtered_data_water()
    req(nrow(df) > 0)
    
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")  
    
    ymin_buffer <- min(df$value) - min(df$value)*0.05
    ymax_buffer <- max(df$value) + max(df$value)*0.05
    
    p <- ggplot(df, aes(x = Date, y = value, group = 1,
                        text = paste0("Date: ", Date, "<br>",
                                      input$ts_param, ": ", value)))
    
    standard_vals <- ts_standard_values()
    if (!is.null(standard_vals)) {
      
      class_a <- standard_vals[1]
      class_b <- standard_vals[2]
      class_c <- standard_vals[3]
      class_d <- standard_vals[4]
      ts_unit <- if(input$ts_param == "pH"){
        NULL
      }  else if (grepl("ug/", input$ts_param)) {
        "ug/l"
      } else {
        "mg/l"
      }
      
      ymin_limit <- min(df$value, na.rm = TRUE)
      ymax_limit <- max(df$value, na.rm = TRUE)
      
      # Detect direction
      increasing <- class_d < class_c  # TRUE = D < C = increasing scale
      
      # Initialize empty frame
      rect_data <- data.frame(
        ymin = numeric(0),
        ymax = numeric(0),
        class = character(0),
        fill_color = character(0)
      )
      
      # For increasing scale (normal case: low = bad, high = good)
      if (increasing) {
        # Optional bottom zone
        if (ymin_limit < class_d) {
          rect_data <- rbind(rect_data, data.frame(
            ymin = ymin_limit,
            ymax = class_d,
            class = "Unclassified",
            fill_color = "darkred"
          ))
        }
        
        # Middle bands
        rect_data <- rbind(rect_data, data.frame(
          ymin = c(class_d, class_c, class_b),
          ymax = c(class_c, class_b, class_a),
          class = c(paste("Class D", " (>", class_d, " ", ts_unit, ")", sep = ""), 
                    paste("Class C", " (>", class_c, " ", ts_unit, ")", sep = ""), 
                    paste("Class B", " (>", class_b, " ", ts_unit, ")", sep = "")),
          fill_color = c("darkorange", "gold", "lightgreen")
        ))
        
        # Optional top zone
        if (ymax_limit > class_a) {
          rect_data <- rbind(rect_data, data.frame(
            ymin = class_a,
            ymax = ymax_limit,
            class = paste("Class A", " (>", class_a, " ", ts_unit, ")", sep = ""),
            fill_color = "lightblue"
          ))
        }
        
      } else {
        # For decreasing scale (high = bad, low = good)
        
        # Optional bottom zone
        if (ymin_limit < class_a) {
          rect_data <- rbind(rect_data, data.frame(
            ymin = ymin_limit,
            ymax = class_a,
            class = paste("Class A", " (<", class_a, " ", ts_unit, ")", sep = ""),
            fill_color = "lightblue"
          ))
        }
        
        # Middle bands
        rect_data <- rbind(rect_data, data.frame(
          ymin = c(class_a, class_b, class_c),
          ymax = c(class_b, class_c, class_d),
          class = c(paste("Class B", " (<", class_b, " ", ts_unit, ")", sep = ""), 
                    paste("Class C", " (<", class_c, " ", ts_unit, ")", sep = ""), 
                    paste("Class D", " (<", class_d, " ", ts_unit, ")", sep = "")),
          fill_color = c("lightgreen", "gold", "darkorange")
        ))
        
        # Optional top zone
        if (ymax_limit > class_d) {
          rect_data <- rbind(rect_data, data.frame(
            ymin = class_d,
            ymax = ymax_limit,
            class = "Unclassified",
            fill_color = "darkred"
          ))
        }
      }
      
      
      p <- p +
        geom_rect(
          data = rect_data,
          aes(
            xmin = min(df$Date),
            xmax = max(df$Date),
            ymin = ymin,
            ymax = ymax,
            fill = class
          ),
          alpha = 0.05,
          inherit.aes = FALSE
        ) +
        scale_fill_manual(values = setNames(rect_data$fill_color, rect_data$class)) +
        
        geom_hline(yintercept = class_d, color = "darkorange", linetype = "dashed", linewidth = 0.7) +
        geom_hline(yintercept = class_c, color = "gold", linetype = "dashed", linewidth = 0.7) +
        geom_hline(yintercept = class_b, color = "lightgreen", linetype = "dashed", linewidth = 0.7) +
        geom_hline(yintercept = class_a, color = "lightblue", linetype = "dashed", linewidth = 0.7)
      
    }
    
    # Add line and points on top
    p <- p + 
      geom_line(color = "black") +
      geom_point(size = 1.5, alpha = 0.5, color = "black") +
      labs(
        title = paste("Time Series of", input$ts_param, "from Water Samples at", input$ts_station),
        x = "Time",
        y = input$ts_param
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  
  
  output$ts_plot_sed <- renderPlotly({
    df <- ts_filtered_data_sed()
    req(nrow(df) > 0)
    
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")  
    
    # Create aggregated data for the line (average per date)
    df_line <- df %>%
      group_by(Date) %>%
      summarise(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')
    
    # Start with the base plot using individual points
    p <- ggplot(df, aes(x = Date, y = value,
                        text = paste0("Date: ", Date, "<br>",
                                      input$ts_param, ": ", value, "<br>",
                                      "Sieve Size: ", `Sieve Size`, "<br>",
                                      "Distance from Bank: ", `Distance from Bank`)))
    
    standard_vals <- ts_standard_values()
    if (!is.null(standard_vals)) {
      
      tel <- standard_vals[1]
      pel <- standard_vals[2]
      
      y_range <- max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)
      offset_amount <- y_range * 0.05
      
      p <- p +
        geom_hline(yintercept = tel, color = "darkorange", linetype = "dashed", linewidth = 0.7) +
        geom_hline(yintercept = pel, color = "firebrick", linetype = "dashed", linewidth = 0.7) +
        annotate("text", x = min(df$Date), y = tel - offset_amount, label = paste("TEL =", tel, "mg/kg"), 
                 hjust = 1.1, vjust = 0.5, color = "darkorange", size = 3, fontface = "bold") +
        annotate("text", x = min(df$Date), y = pel + offset_amount, label = paste("PEL =", pel, "mg/kg"), 
                 hjust = 1.1, vjust = 0.5, color = "firebrick", size = 3, fontface = "bold") +
        scale_x_date(expand = expansion(mult = c(0.2, 0.05))) +
        coord_cartesian(clip = "off")
    }
    
    # Check if Distance from Bank has variation
    has_variation <- length(unique(df$`Distance from Bank`)) > 1
    
    p <- p +
      # Add the line using averaged data
      geom_line(data = df_line, aes(x = Date, y = avg_value, group = 1,
                                    text = paste0("Date: ", Date, "<br>",
                                                  "Average ", input$ts_param, ": ", round(avg_value, 3))),
                color = "black") +
      {if(has_variation) {
        # Multiple values - use fill aesthetic with legend
        geom_point(shape = 21, size = 1.5, fill = "black", stroke = 0.3, color = "black", aes(alpha = `Distance from Bank`))
      } else {
        # All same - black fill, no legend
        geom_point(shape = 21, size = 1.5, alpha = 0.5, fill = "black")
      }} +
      labs(
        title = paste("Time Series of", input$ts_param, "from Sediment Samples at", input$ts_station),
        x = "Time",
        y = input$ts_param,
        fill = if(has_variation) "Distance from Bank" else NULL  # Only show fill label if there's variation
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if(length(unique(df$`Distance from Bank`)) < 2) {
      p <- p + 
        scale_fill_continuous(guide = "none") +  # Remove legend
        # Optionally override colors to black
        scale_fill_manual(values = "black", guide = "none")
    }
    
    ggplotly(p, tooltip = "text")  # Show only the text tooltip
  })
  
  
  
  
  
  
  
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  ##############  MAPS  #############################
  
  
  
  
  
  
  
  # Dynamically populate year choices for map
  observe({
    sed_years <- sed_years_usgs()
    updateSelectInput(session, "sed_year", choices = sed_years, selected = max(sed_years))
  })
  
  observe({
    water_years <- water_years_1333()
    updateSelectInput(session, "water_year", choices = water_years, selected = max(water_years))
  })
  
  # Load selected dataset for map
  sed_selected_data <- reactive({
    req(input$sed_year)
    active_sed_usgs() |>
      filter(Year == input$sed_year)
  })
  
  water_selected_data <- reactive({
    req(input$water_year)
    read_xlsx(file.path(water_data_path_1333, paste0("water_", input$water_year, "_1333.xlsx"))) |>
      select(-matches("Antimony"))
  })
  
  # Populate campaign dropdown for map
  output$sed_campaign_ui <- renderUI({
    req(sed_selected_data())
    sed_campaigns <- unique(sed_selected_data()$Campaign)
    selectInput("sed_campaign", "Select Campaign:", choices = sed_campaigns)
  })
  
  output$water_campaign_ui <- renderUI({
    req(water_selected_data())
    water_campaigns <- unique(water_selected_data()$Campaign)
    selectInput("water_campaign", "Select Campaign:", choices = water_campaigns)
  })
  
  # Populate sieve size dropdown for map
  output$tamiz_ui <- renderUI({
    req(sed_selected_data())
    tamiz <- unique(sed_selected_data()$`Sieve Size`)
    tamiz <- tamiz[!is.na(tamiz)]
    selectInput("tamiz", "Select Sieve Size:", choices = tamiz)
  })
  
  # Populate metal dropdown dynamically for map
  observe({
    sed_df <- sed_selected_data()
    sed_metal_cols <- c("Arsenic (mg/kg As)", "Cadmium (mg/kg Cd)", "Copper (mg/kg Cu)",
                        "Chromium (mg/kg Cr)", "Mercury (mg/kg Hg)", "Nickel (mg/kg Ni)",
                        "Lead (mg/kg Pb)", "Zinc (mg/kg Zn)")
    
    sed_metal_cols <- intersect(sed_metal_cols, colnames(sed_df))
    
    updateSelectInput(session, "sed_metal", choices = sed_metal_cols)
  })
  
  observe({
    water_df <- water_selected_data()
    req(ncol(water_df) > 0)
    
    metal_base_names <- colnames(water_df) %>%
      str_subset("Class$") %>%  # match all relevant types
      str_remove("Class$") %>%
      unique() %>%
      trimws() %>%
      sort()
    
    
    
    updateSelectInput(session, "water_metal", choices = metal_base_names, selected = metal_base_names[1])
  })
  
  # Filtered data based on all inputs
  sed_filtered_data <- reactive({
    sed_df <- sed_selected_data()
    req(input$sed_campaign, input$tamiz, input$sed_metal)
    sed_df %>%
      filter(Campaign == input$sed_campaign,
             `Sieve Size` == input$tamiz)
  })
  
  water_filtered_data <- reactive({
    water_df <- water_selected_data()
    req(input$water_campaign, input$water_metal)
    water_df %>%
      filter(Campaign == input$water_campaign)
  })
  
  # Get USGS status column based on selected metal
  # Function to get USGS column based on selected metal column
  get_usgs_column <- function(sed_metal_col, sed_df) {
    # Extract the last parenthetical symbol from metal_col, e.g. "As" from "Arsenic (mg/kg fraccion) (mg/kg As)"
    symbol <- stringr::str_extract(sed_metal_col, "(?<=\\(mg/kg )[A-Za-z]+(?=\\)$)")
    
    pattern <- paste0(symbol, ".*USGS")
    usgs_cols <- grep(pattern, names(sed_df), value = TRUE)
    
    if (length(usgs_cols) > 0) {
      return(usgs_cols[1])
    } else {
      return(NULL)
    }
  }
  
  get_water_column_name <- function(metal, type) {
    # Normalize input to match expected format
    metal <- tolower(metal)
    type <- tolower(type)
    
    # Lookup table for proper casing and symbols
    metal_info <- list(
      arsenic   = list(name = "Arsenic",   symbol = "As"),
      silver    = list(name = "Silver",    symbol = "Ag"),
      cadmium   = list(name = "Cadmium",   symbol = "Cd"),
      copper    = list(name = "Copper",    symbol = "Cu"),
      chromium  = list(name = "Chromium",  symbol = "Cr"),
      iron      = list(name = "Iron",      symbol = "Fe"),
      mercury   = list(name = "Mercury",   symbol = "Hg"),
      magnesium = list(name = "Magnesium", symbol = "Mg"),
      manganese = list(name = "Manganese", symbol = "Mn"),
      nickel    = list(name = "Nickel",    symbol = "Ni"),
      lead      = list(name = "Lead",      symbol = "Pb"),
      zinc      = list(name = "Zinc",      symbol = "Zn")
    )
    
    if (!metal %in% names(metal_info)) {
      stop("Unrecognized metal: ", metal)
    }
    
    metal_name <- metal_info[[metal]]$name
    metal_symbol <- metal_info[[metal]]$symbol
    
    column_name <- switch(type,
                          "water_dissolved" = paste0("Dissolved ", metal_name, " (mg/l ", metal_symbol, ")"),
                          "water_suspended" = paste0("Suspended ", metal_name, " (mg/kg ", metal_symbol, ")"),
                          "water_total"     = paste0("Total ", metal_name, " (mg/l ", metal_symbol, ")"),
                          "water_1333"     = paste0(metal_name, " Class"),
                          stop("Unrecognized type: ", type)
    )
    
    return(column_name)
  }
  
  
  
  # Render leaflet map
  output$sed_map <- renderLeaflet({
    sed_df <- sed_filtered_data()
    req(nrow(sed_df) > 0)
    
    col_to_plot <- if (input$sed_value_type == "sed_value") {
      input$sed_metal
    } else {
      usgs_col <- get_usgs_column(input$sed_metal, sed_df)
      req(!is.null(usgs_col))  # stops rendering if no USGS col found
      usgs_col
    }
    
    sed_values_for_label <- sed_df[[col_to_plot]]
    
    if (input$sed_value_type == "sed_value") {
      pal <- colorNumeric(
        palette = "Reds",  # or use the custom palette above
        domain = sed_df[[col_to_plot]],
        na.color = "gray"
      )
      colors <- pal(sed_df[[col_to_plot]])
    } else {
      sed_df[[col_to_plot]] <- trimws(as.character(sed_df[[col_to_plot]]))
      valid_levels <- c("Below TEL", "Above TEL", "Above PEL")
      sed_df[[col_to_plot]] <- ifelse(sed_df[[col_to_plot]] %in% valid_levels,
                                      sed_df[[col_to_plot]],
                                      NA_character_)
      
      pal <- colorFactor(
        palette = c("lightblue", "darkorange", "firebrick"),
        levels = valid_levels,
        na.color = "gray"
      )
      colors <- pal(sed_df[[col_to_plot]])
    }
    
    leaflet(sed_df) %>%
      addTiles() %>%
      addPolylines(data = pilco_line, 
                   color = "darkcyan", 
                   weight = 3, 
                   opacity = 0.8) %>%
      addPolygons(data = bol_border,
                  color = "black",
                  weight = 3,
                  fill = FALSE) %>%
      addCircleMarkers(
        lng = ~Long_dd,
        lat = ~Lat_dd,
        radius = 6,
        stroke = TRUE,
        color = "black",
        weight = 1.5,
        fillOpacity = 0.8,
        fillColor = colors,
        label = lapply(seq_len(nrow(sed_df)), function(i) {
          htmltools::HTML(paste0(
            "Site: ", sed_df$Station[i], "<br>",
            "Lat: ", sed_df$Lat_dd[i], "<br>",
            "Long: ", sed_df$Long_dd[i], "<br>",
            input$sed_metal, ": ", sed_values_for_label[i]
          ))
        })
      ) %>%
      setView(lng = -63.5, lat = -21.3, zoom = 7)
  })
  
  output$water_map <- renderLeaflet({
    water_df <- water_filtered_data()
    req(nrow(water_df) > 0)
    
    col_to_plot <- get_water_column_name(input$water_metal, input$water_value_type)
    req(col_to_plot %in% colnames(water_df))  # Make sure column exists
    
    values <- water_df[[col_to_plot]]
    
    # Pick a palette depending on type
    if (input$water_value_type == "water_1333") {
      water_df[[col_to_plot]] <- trimws(as.character(values))
      valid_levels <- c("Class A", "Class B", "Class C", "Class D", "Unclassified") 
      water_df[[col_to_plot]] <- ifelse(water_df[[col_to_plot]] %in% valid_levels, 
                                        water_df[[col_to_plot]], NA_character_)
      
      pal <- colorFactor(
        palette = c("lightblue", "lightgreen", "gold", "darkorange", "darkred"),
        levels = valid_levels,
        na.color = "gray"
      )
      colors <- pal(water_df[[col_to_plot]])
    } else {
      # Ensure values are numeric and not all NA
      values <- suppressWarnings(as.numeric(values))  # convert safely if needed
      
      if (all(is.na(values)) || length(values) == 0) {
        # Fallback palette for empty data
        pal <- colorNumeric("Reds", domain = c(0, 1), na.color = "gray")
        colors <- rep("gray", nrow(water_df))
      } else {
        pal <- colorNumeric("Reds", domain = values, na.color = "gray")
        colors <- pal(values)
      }
    }
    
    leaflet(water_df) %>%
      addTiles() %>%
      addPolylines(data = pilco_line, 
                   color = "darkcyan", 
                   weight = 3, 
                   opacity = 0.8) %>%
      addCircleMarkers(
        lng = ~`Longitude Decimal`,
        lat = ~`Latitude Decimal`,
        radius = 6,
        stroke = TRUE,
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        fillColor = colors,
        label = lapply(seq_len(nrow(water_df)), function(i) {
          htmltools::HTML(paste0(
            "Station: ", water_df$Station[i], "<br>",
            col_to_plot, ": ", values[i]
          ))
        })
      ) %>%
      setView(
        lng = mean(water_df$`Longitude Decimal`, na.rm = TRUE),
        lat = mean(water_df$`Latitude Decimal`, na.rm = TRUE),
        zoom = 7
      )
  })
  
  output$sed_legend <- renderUI({
    req(input$sed_value_type)
    
    if (input$sed_value_type == "usgs") {
      tags$div(
        tags$h5("Legend:"),
        tags$ul(
          tags$li(tags$span(style = "color:lightblue;", "⬤"), " Below TEL"),
          tags$li(tags$span(style = "color:darkorange;", "⬤"), " Above TEL"),
          tags$li(tags$span(style = "color:firebrick;", "⬤"), " Above PEL")
        )
      )
    } else if (input$sed_value_type == "sed_value") {
      sed_df <- sed_filtered_data()
      req(nrow(sed_df) > 0)
      values <- sed_df[[input$sed_metal]]
      rng <- range(values, na.rm = TRUE)
      
      tags$div(
        tags$h5("Concentration (mg/kg)"),
        tags$div(style = "height: 20px; background: linear-gradient(to right, #fff5f0, #fb6a4a, #67000d);"),
        tags$div(
          style = "display: flex; justify-content: space-between;",
          tags$span(format(round(rng[1], 2), nsmall = 2)),
          tags$span(format(round(rng[2], 2), nsmall = 2))
        )
      )
    } else {
      NULL
    }
  })
  
  
  output$water_legend <- renderUI({
    req(input$water_value_type)
    
    # Define units per type
    units_lookup <- list(
      water_dissolved = "mg/l",
      water_suspended = "mg/kg",
      water_total = "mg/l",
      water_1333 = ""  # Classification, no numeric units
    )
    
    if (input$water_value_type == "water_1333") {
      tags$div(
        tags$h5("Legend:"),
        tags$ul(
          tags$li(tags$span(style = "color:lightblue;", "⬤"), " Class A"),
          tags$li(tags$span(style = "color:lightgreen;", "⬤"), " Class B"),
          tags$li(tags$span(style = "color:gold;", "⬤"), " Class C"),
          tags$li(tags$span(style = "color:darkorange;", "⬤"), " Class D"),
          tags$li(tags$span(style = "color:firebrick;", "⬤"), " Unclassified")
        )
      )
    } else {
      water_df <- water_filtered_data()
      req(nrow(water_df) > 0)
      
      col_to_plot <- get_water_column_name(input$water_metal, input$water_value_type)
      req(col_to_plot %in% colnames(water_df))
      
      values <- water_df[[col_to_plot]]
      rng <- range(values, na.rm = TRUE)
      
      # Handle small ranges:
      epsilon <- 1e-6
      min_val <- rng[1]
      max_val <- rng[2]
      
      # If range is too small, show fixed decimal places or just one label
      if ((max_val - min_val) < epsilon) {
        label_min <- format(min_val, scientific = TRUE, digits = 3)
        label_max <- label_min
      } else {
        # Normal display with 3 decimal places (adjust digits as needed)
        label_min <- format(min_val, digits = 3, nsmall = 3)
        label_max <- format(max_val, digits = 3, nsmall = 3)
      }
      
      legend_title <- paste0("Concentration (", units_lookup[[input$water_value_type]], ")")
      
      tags$div(
        tags$h5(legend_title),
        tags$div(style = "height: 20px; background: linear-gradient(to right, #fff5f0, #fb6a4a, #67000d);"),
        tags$div(
          style = "display: flex; justify-content: space-between; font-family: monospace;",
          tags$span(label_min),
          tags$span(label_max)
        )
      )
    }
  })
  
  output$sed_table <- renderDT({
    sed_filtered_data()
  })
  
  output$water_table <- renderDT({
    water_filtered_data()
  })
  
  output$stds_usgs_table <- renderDT({
    usgs_sqg |>
      select(-match_name)
  })
  
  output$stds_1333_table <- renderDT({
    bolivian_1333 |>
      select(-match_name)
  })
  
  output$stds_usgs_table_ts <- renderDT({
    usgs_sqg |>
      select(-match_name)
  })
  
  output$stds_1333_table_ts <- renderDT({
    bolivian_1333 |>
      select(-match_name)
  })
  
}
