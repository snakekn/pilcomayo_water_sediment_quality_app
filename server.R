

#Define Server
server <- function(input, output, session) {
  ## Prepare master_data
  
  # initialize master_data
  master_data = reactiveValues(
    water_scored = NULL,
    water_locyear = NULL,
    sed_scored = NULL,
    sed_locyear = NULL,
    sed_loctime = NULL,
    water_loctime = NULL,
    all_media_scored = NULL,
    all_media_locyear = NULL,
    all_media_loctime = NULL
  )
  
  # to print out the new master_data
  observeEvent(reactiveValuesToList(master_data), {
    cat("\n===== Loading Master Data =====\n")
    
    master_data$water_scored <- if (file.exists("data/processed/all_water_scored.rds")) readRDS("data/processed/all_water_scored.rds") else { print("no all_water_scored.rds"); tibble() }
    master_data$water_locyear <- if (file.exists("data/processed/all_water_locyear.rds")) readRDS("data/processed/all_water_locyear.rds") else { print("no all_water_locyear.rds"); tibble() }
    master_data$sed_scored <- if (file.exists("data/processed/all_sed_scored.rds")) readRDS("data/processed/all_sed_scored.rds") else { print("no all_sed_scored.rds"); tibble() }
    master_data$sed_locyear <- if (file.exists("data/processed/all_sed_locyear.rds")) readRDS("data/processed/all_sed_locyear.rds") else { print("no all_sed_locyear.rds"); tibble() }
    master_data$sed_loctime <<- if(file.exists("data/processed/all_sed_loctime.rds")) readRDS("data/processed/all_sed_loctime.rds") else { print("no all_sed_loctime.rds"); tibble() }
    master_data$water_loctime <<- if(file.exists("data/processed/all_water_loctime.rds")) readRDS("data/processed/all_water_loctime.rds") else { print("no all_water_loctime.rds"); tibble() }
    
    master_data$all_media_scored <<- if(file.exists("data/processed/all_media_scored.rds")) readRDS("data/processed/all_media_scored.rds") else { print("no all_media_scored.rds"); tibble() }
    master_data$all_media_locyear <<- if(file.exists("data/processed/all_media_locyear.rds")) readRDS("data/processed/all_media_locyear.rds") else { print("no all_media_locyear.rds"); tibble() }
    master_data$all_media_loctime <<- if(file.exists("data/processed/all_media_loctime.rds")) readRDS("data/processed/all_media_loctime.rds") else { print("no all_media_loctime.rds"); tibble() }
    
    cat("\n========================================\n")
    cat("MASTER DATA LOADED\n")
    cat("========================================\n")
    
    cat("water_scored:", nrow(master_data$water_scored), "rows\n")
    cat("water_locyear:", nrow(master_data$water_locyear), "rows\n")
    cat("water_loctime:", nrow(master_data$water_loctime), "rows\n")
    cat("sed_scored:", nrow(master_data$sed_scored), "rows\n")
    cat("sed_locyear:", nrow(master_data$sed_locyear), "rows\n")
    cat("sed_loctime:", nrow(master_data$sed_loctime), "rows\n")
    
    cat("all_media_scored:", nrow(master_data$all_media_scored), "rows\n")
    cat("all_media_locyear:", nrow(master_data$all_media_locyear), "rows\n")
    cat("all_media_loctime:", nrow(master_data$all_media_loctime), "rows\n")
    
    cat("\nwater_scored columns:", ncol(master_data$water_scored), "\n")
    cat("sed_scored columns:", ncol(master_data$sed_scored), "\n")
    
    cat("========================================\n\n")
  }, once = TRUE, ignoreInit = FALSE)  # Only run once on startup
  
  output$sed_data_ready <- reactive({
    !is.null(master_data$sed_scored) && nrow(master_data$sed_scored) > 0
  })
  outputOptions(output, "sed_data_ready", suspendWhenHidden = FALSE)

  # Combined data ready check
  output$map_data_ready <- reactive({
    water_ready <- !is.null(master_data$water_scored) && nrow(master_data$water_scored) > 0
    sed_ready <- !is.null(master_data$sed_scored) && nrow(master_data$sed_scored) > 0
    water_ready || sed_ready  # At least one must be ready
  })
  outputOptions(output, "map_data_ready", suspendWhenHidden = FALSE)
  
  # Unified campaign/date range UI that works for both
  output$map_campaign_ui <- renderUI({
    cat("\n=== DEBUG map_campaign_ui ===\n")
    
    # Get data based on selected media
    df <- if (!is.null(input$plot_media) && input$plot_media == "water") {
      req(master_data$water_scored)
      master_data$water_scored
    } else {
      req(master_data$sed_scored)
      master_data$sed_scored
    }
    
    req(nrow(df) > 0)
    cat("  Using", input$plot_media, "data with", nrow(df), "rows\n")
    
    # Determine date column
    date_col <- if ("date" %in% names(df)) {
      "date"
    } else if ("date" %in% names(df)) {
      "date"
    } else {
      cat("  ERROR: No date column found\n")
      return(p("No date column found in data"))
    }
    
    dates <- df[[date_col]]
    dates <- dates[!is.na(dates)]
    
    if (length(dates) == 0) {
      return(p("No date data available"))
    }
    
    if (!inherits(dates, "date")) {
      dates <- as.date(dates)
    }
    
    min_date <- min(dates, na.rm = TRUE)
    max_date <- max(dates, na.rm = TRUE)
    
    cat("  date range:", min_date, "to", max_date, "\n")
    
    dateRangeInput(
      "map_date_range",  # Changed from sed_date_range
      "Select date Range:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date,
      format = "yyyy-mm-dd",
      separator = " to "
    )
  })
  
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
  
  # Upload button
  upload_result <- dataUploadServer(
    id = "upload_data",
    base_data = initial_water,
    master_data = master_data
    )  
  
  # save upload to master_data file for all to access
  observe({
    result <- upload_result$parsed()
    req(result)
    
    if (result$media == "water") {
      master_data$water_scored <- result$scored
      master_data$water_locyear <- result$locyear
    } else { # assuming it's sediment
      master_data$sed_scored <- result$scored
      master_data$sed_locyear <- result$locyear
    }
    
    showNotification(
      paste("Updated", result$media, "data in master_data"),
      type = "message"
    )
  })
  
  output$import_meta <- renderPrint({
    req(imported$data())
    list(
      rows = nrow(imported$data()),
      cols = ncol(imported$data()),
      settings = imported$settings()
    )
  })
  
  # Read and combine water data (clean version)
  all_water_clean <- reactive({
    req(master_data$water_scored)
    return(master_data$water_scored) # skip all the other combining work...
    
    # merging occurs elsewhere now
    {
    water_files <- list.files(water_data_path_clean, pattern = "^water_\\d{4}_clean\\.xlsx$", full.names = TRUE)
    
    water_dfs <- lapply(water_files, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$date <- as.date(df$date, "%d/%m/%Y")
      df
    })
    
    all_data <- bind_rows(water_dfs) |> 
      mutate(station = str_replace(station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(station = str_replace(station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba")) |>
      filter(!is.na(`Latitude Decimal`))
    
    return(all_data)
    } # the old stuff to combine it all
  })
  
  
  # Only points in Bolivia
  bol_water_clean <- reactive({
    cat("all_water_clean() in bol_water_clean")
    filter_to_border(all_water_clean(), "longitude_decimal", "latitude_decimal", bol_border)
  })
  
  
  # Read and combine water data (1333 version)
  all_water_1333 <- reactive({
    ## Nadav's Note: there are a bunch of these data manipulation functions. We want to:
    ### 1. All data (which can be in several formats) goes thru a unified function, where their media is considered and all relevant standards are calculated
    ### 2. Data is all compiled into one dataset that can be loaded in any part of the app
    ### 3. Uploaded data is added to this dataset 
    ### 4. All maps showing data use this single dataset
    ### 5. Users can still download pre-loaded datasets, or filter as needed
    
    req(master_data$water_scored)
    return(master_data$water_scored) # skip the rest, just get the data we want
    {
    # 
    # # can replace much of this with manual_compile_water. Goal is to replace with datahub later on
    # water_files <- list.files(water_data_path_1333, pattern = "^water_\\d{4}_1333\\.xlsx$", full.names = TRUE)
    # 
    # water_dfs <- lapply(water_files, function(f) {
    #   year <- stringr::str_extract(basename(f), "\\d{4}")
    #   df <- read_xlsx(f)
    #   df$Year <- as.integer(year)
    #   df$date <- as.date(df$date, "%Y-%m-%d")
    #   df
    # })
    # 
    # all_data <- bind_rows(water_dfs) |> 
    #   mutate(station = str_replace(station,
    #                                "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
    #                                "Tacobamba arriba Pilcomayo")) |>
    #   mutate(station = str_replace(station,
    #                                "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
    #                                "Pilcomayo arriba Tacobamba")) |>
    #   filter(!is.na(`Latitude Decimal`))
    # 
    # # Count "Unclassified" in columns ending with "Class"
    # all_data$num_unclass <- rowSums(
    #   select(all_data, ends_with("Class")) == "Unclassified",
    #   na.rm = TRUE
    # )
    # 
    # # Count "Class D" in columns ending with "Class"
    # all_data$num_class_d <- rowSums(
    #   select(all_data, ends_with("Class")) == "Class D",
    #   na.rm = TRUE
    # )
    # 
    # # Count "Class C" in columns ending with "Class"
    # all_data$num_class_c <- rowSums(
    #   select(all_data, ends_with("Class")) == "Class C",
    #   na.rm = TRUE
    # )
    # 
    # # Count "Class B" in columns ending with "Class"
    # all_data$num_class_b <- rowSums(
    #   select(all_data, ends_with("Class")) == "Class B",
    #   na.rm = TRUE
    # )
    # 
    # all_data = all_data |>
    #   mutate(potato = rowSums(
    #     select(all_data, ends_with("Class")) == "Class B", na.rm = TRUE
    #   ))
    # 
    # return(all_data)
    }
  })
  
  
  # Only points in Bolivia
  bol_water_1333 <- reactive({
    cat("all_water_clean in bol_water_1333")
    filter_to_border(isolate(all_water_clean()), "longitude_decimal", "latitude_decimal", bol_border)
  })
  
  
  
  ## Load sediment data ##
  
  all_sed_clean <- reactive({
    return(master_data$sed_scored)
    
    {
    
    sed_files_clean <- list.files(sed_data_path_clean, pattern = "^sed_\\d{4}_clean\\.xlsx$", full.names = TRUE)
    
    sed_dfs_clean <- lapply(sed_files_clean, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$date <- as.date(df$date, "%d/%m/%Y")
      df
    })
    
    df <- bind_rows(sed_dfs_clean) |>
      mutate(station = str_replace(station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(station = str_replace(station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba"))
    
    return(df)
    } # the old stuff to combine the files
  })
  
  
  # Only points in Bolivia
  bol_sed_clean <- reactive({
    # Convert to sf
    cat("all_sed_clean() in bol_sed_clean")
    filter_to_border(all_sed_clean(), "longitude_decimal", "latitude_decimal", bol_border)
    
  })
  
  all_sed_usgs <- reactive({
    std_file = stds |>
      filter(media == "sediment", regulator == "USGS")
    return(std_file)
    
    sed_files_usgs <- list.files(sed_data_path_usgs, pattern = "^sed_\\d{4}_usgs\\.xlsx$", full.names = TRUE)
    
    sed_dfs_usgs <- lapply(sed_files_usgs, function(f) {
      year <- stringr::str_extract(basename(f), "\\d{4}")
      df <- read_xlsx(f)
      df$Year <- as.integer(year)
      df$date <- as.date(df$date, "%d/%m/%Y")
      df
    })
    
    df <- bind_rows(sed_dfs_usgs) |>
      mutate(station = str_replace(station,
                                   "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Tacobamba arriba Pilcomayo")) |>
      mutate(station = str_replace(station,
                                   "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
                                   "Pilcomayo arriba Tacobamba"))
    
    # Add Distance from Bank column from all_sed_clean
    df <- df |>
      left_join(
        all_sed_clean() |> select(station, date, sieve_size, distance_from_bank),
        by = c("station", "date", "sieve_size")
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
    
    df$unique <- paste(df$station, df$date, sep = " - ")
    
    return(df)
  })
  
  # Only Points west of Villamontes (only points in Bolivia)
  # bol_border <- st_read("data/geojson/bol_borders.geojson")
  
  bol_sed_usgs <- reactive({
    # Convert the sediment data to sf object if it isn't already
    cat("all_sed_clean() in bol_sed_usgs")
    filter_to_border(all_sed_clean(), "longitude_decimal", "latitude_decimal", bol_border)
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
    unique(master_data$water_scored$year)
    #unique(active_water_clean()$Year)
  })
  
  water_years_1333 <- reactive({
    unique(master_data$water_scored$year)
  })
  
  sed_years <- reactive({
    unique(master_data$sed_scored$year)
  })
  
  sed_years_usgs <- reactive({
    unique(master_data$sed_scored$year)
  })
  
  all_years <- reactive({
    sort(unique(c(water_years(), 
                  sed_years()
    ))) 
  })
  
  ################# DOWNLOAD BUTTONS #########################
  # 
  # output$download_year_ui <- renderUI({
  #   all_years <- all_years()
  #   
  #   selectInput("download_year", "Filter by Year (optional):",
  #               choices = c("All", all_years),
  #               selected = "All")
  # })
  # 
  
  # # Helper function to filter by year
  # filter_by_year <- function(df, year_input) {
  #   if (year_input == "all") {
  #     return(df)
  #   } else {
  #     return(df %>% filter(Year == as.integer(year_input)))
  #   }
  # }
  
  # Sediment Data (Clean)
  # output$download_sed_clean <- downloadHandler(
  #   filename = function() {
  #     paste0("sed_", str_to_lower(input$download_year), "_clean_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     data <- if (input$data_scope == "bol") {
  #       bol_sed_clean()
  #     } else {
  #       all_sed_clean()
  #     }
  #     
  #     if (input$download_year != "All") {
  #       data <- data |> filter_by_year(input$download_year)
  #     }
  #     
  #     write_csv(data, file)
  #   }
  # )
  
  # Sediment Data (USGS)
  # output$download_sed_usgs <- downloadHandler(
  #   filename = function() {
  #     paste0("sed_", str_to_lower(input$download_year), "_usgs_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     data <- if (input$data_scope == "bol") {
  #       bol_sed_usgs()
  #     } else {
  #       all_sed_usgs()
  #     }
  #     
  #     if (input$download_year != "All") {
  #       data <- data |> filter_by_year(input$download_year)
  #     }
  #     
  #     write_csv(data, file)
  #   }
  # )
  
  # Water Data (Clean)
  # output$download_water_clean <- downloadHandler(
  #   filename = function() {
  #     paste0("water_", str_to_lower(input$download_year), "_clean_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     data <- if (input$data_scope == "bol") {
  #       bol_water_clean()
  #     } else {
  #       all_water_clean()
  #     }
  #     
  #     if (input$download_year != "All") {
  #       data <- data |> filter_by_year(input$download_year)
  #     }
  #     
  #     write_csv(data, file)
  #   }
  # )
  # 
  # Water Data (1333)
  # output$download_water_1333 <- downloadHandler(
  #   filename = function() {
  #     paste0("water_", str_to_lower(input$download_year), "_1333_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     data <- if (input$data_scope == "bol") {
  #       bol_water_1333()
  #     } else {
  #       all_water_1333()
  #     }
  #     
  #     if (input$download_year != "All") {
  #       data <- data |> filter_by_year(input$download_year)
  #     }
  #     
  #     write_csv(data, file)
  #   }
  # )
  
  # USGS Standards Table
  # output$download_usgs_standards <- downloadHandler(
  #   filename = function() {
  #     paste0("usgs_sqgs_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     
  #     data <- usgs_sqg |>
  #       select(-match_name)
  #     
  #     write_csv(data, file)
  #   }
  # )
  # 
  # Bolivian 1333 Standards Table
  # output$download_1333_standards <- downloadHandler(
  #   filename = function() {
  #     paste0("bolivian_1333_stds_", Sys.date(), ".csv")
  #   },
  #   content = function(file) {
  #     
  #     data <- bolivian_1333 |>
  #       select(-match_name)
  #     
  #     write_csv(data, file)
  #   }
  # )
  # 
  
  ################# PCA #########################
  
  numeric_columns <- reactive({
    df <- master_data$all_media_scored
    
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
    df = master_data$all_media_scored
    req(df, input$pca_media)
    
    updateSelectizeInput(inputId = "pca_parameters",
                         choices = get_param_list(df, media_type = input$pca_media),
                         selected = NULL)
  })
  
  observeEvent(input$deselect_all_pca, {
    updateSelectizeInput(session, "pca_parameters", selected = character(0))
  })
  
  pca_result <- eventReactive(input$run_pca, {
    calc_pca(data = master_data$all_media_scored, 
             params = input$pca_parameters, 
             media_selection = input$pca_media, 
             station_selection = input$pca_station)
  })
  
  output$pca_plot <- renderPlotly({
    res <- pca_result()
    req(res)
    
    df   <- res$df
    rpca <- res$pca
    
    scores   <- as.data.frame(rpca$ind$coord[, 1:2])
    loadings <- as.data.frame(rpca$var$coord[, 1:2])
    
    scores$station <- df$station
    scores$date    <- df$date
    scores$media   <- df$media
    
    arrow_scale <- 1.5   # try between 1 and 3
    
    g <- ggplot() +
      # points
      geom_point(
        data = scores,
        aes(
          x = Dim.1, y = Dim.2,
          colour = media,
          text   = paste(
            "Station:", station,
            "<br>Date:", date,
            "<br>Media:", media
          )
        ),
        alpha = 0.8, size = 2.5
      ) +
      # arrows (scaled up a bit)
      geom_segment(
        data = transform(loadings,
                         Dim.1 = Dim.1 * arrow_scale,
                         Dim.2 = Dim.2 * arrow_scale),
        aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
        arrow  = arrow(length = unit(0.25, "cm")),
        colour = "steelblue4",
        linewidth = 0.4
      ) +
      # labels offset from arrow tips
      geom_text(
        data = transform(loadings,
                         Dim.1 = Dim.1 * arrow_scale * 1.05,
                         Dim.2 = Dim.2 * arrow_scale * 1.05),
        aes(x = Dim.1, y = Dim.2, label = rownames(loadings)),
        colour = "steelblue4",
        size = 3
      ) +
      labs(x = "Dim1", y = "Dim2", colour = "Media") +
      theme_minimal()
    
    # Convert to plotly – this is what makes the plot appear
    plotly::ggplotly(g, tooltip = "text")
  })
  
  # Scree plot
  output$scree_plot <- renderPlot({
    res = pca_result()
    req(res)
    
    fviz_screeplot(res$pca, addlabels = TRUE)
  })
  
  
  
  ##### Risk Map #####
  # prepare the raster layers we'll utilize
  
  
  # Risk layer definitions (add before your existing riskraster reactive)
  risk_layer_info <- list(
    list(id = 1, name = "Environmental\nHazards", checkbox = "risk_hq", class = "risk1"),
    list(id = 2, name = "Vulnerability", checkbox = "risk_vul", class = "risk2"),
    list(id = 3, name = "Air Quality", checkbox = "risk_air", class = "risk3"),
    list(id = 4, name = "Mining Sites", checkbox = "risk_mining", class = "risk4"),
    list(id = 5, name = "Population", checkbox = "risk_pop", class = "risk5")
  )
  risk_raster = reactive({
    req(input$main_tab == "Risk Scores Map")   # do nothing unless Map tab active
    
    layers = list()
    cat("\n[risk_raster reactive] Checkboxes on: ",
        "\n   HQ: ", isTRUE(input$risk_hq),
        "\n   Vul: ", isTRUE(input$risk_vul),
        "\n   Air: ", isTRUE(input$risk_air),
        "\n   Mining: ", isTRUE(input$risk_mining),
        "\n   Pop: ", isTRUE(input$risk_pop))
    
    r = load_risk_rasters(debug=TRUE)
    
    if (isTRUE(input$risk_hq)) {
      layers[["hq"]] <- r$hq
    }
    if (isTRUE(input$risk_vul)) {
      layers[["vul"]] <- r$vul
    }
    if (isTRUE(input$risk_air)) {
      layers[["air"]] <- r$air
    }
    if (isTRUE(input$risk_mining)) {
      layers[["mining"]] <- r$mining
    }
    if (isTRUE(input$risk_pop)) {
      layers[["pop"]] <- r$pop
    }
    
    if (length(layers) == 0) {
      cat("\n[risk_raster reactive] No layers detected")
      return(NULL)
    } else {
      cat("\n", length(layers), " layers included in map. Rastering into one map now.")
    }
    
    r_stack <- terra::rast(layers)
    r_merge = terra::app(r_stack, fun = sum, na.rm = TRUE)
    rlist = list(merged = r_merge, individuals = layers)
    cat("\n[risk_raster] returning r_stack and r_merge. Layers in r_merge: ", names(rlist$individuals))
    return(rlist)
  })
  
  # render the output map
  output$risk_map <- renderLeaflet({
    leaflet() |> 
      addTiles() |>
      setView(lng = -65.3, lat = -19.0, zoom = 7)
  })
  
  observe({
    req(input$main_tab == "Risk Scores Map")   # do nothing unless Map tab active
    
    r = risk_raster()
    if(is.null(r)) { # no layers selected
      stop("No layers selected. Please select at least 1 layer to map risk on the region.")
    }
    req(!is.null(r), !is.null(r$merged))

    proxy = leafletProxy("risk_map", data = r$merged) |> clearImages()
    
    # note: this may throw a projection error, which means you need to reinstall terra
    pal <- colorNumeric("viridis", terra::values(r$merged), na.color = "transparent")
    proxy %>%
      addRasterImage(r$merged, colors = pal, opacity = 0.7, layerId = "risk_merged", project = FALSE) # will need to project later
  })
  
  observe({
    map_clicked <- input$risk_map_click
    req(map_clicked, !is.null(risk_raster()))
    
    click_lat <- map_clicked$lat
    click_lng <- map_clicked$lng
    cat("\n--- risk_map_click ---\n")
    cat("Click at lat:", click_lat, "lng:", click_lng, "\n")
    
    point <- terra::vect(
      matrix(c(click_lng, click_lat), ncol = 2),
      crs = "EPSG:4326"
    )
    
    # Extract values from each individual layer
    r <- risk_raster()
    cat("\nClicked point: ", click_lat, click_lng,
        "\nAvailable keys in r$individuals:", paste(names(r$individuals), collapse = ", "), "\n")
    print(names(r))
    req(!is.null(r$individuals))
    print(names(r$individuals))
    
    cat("Names(r$individuals):", paste(names(r$individuals), collapse = ", "), "\n")
    
    # fixed order to match UI
    layer_keys  <- c("hq", "vul", "air", "mining", "pop")
    layer_names <- c(
      "Environmental Hazards",
      "Vulnerability",
      "Air Quality",
      "Mining Sites",
      "Population"
    )
    
    values_vec <- rep(NA_real_, 5)
    
    i <- 1
    for (layer_name in layer_keys) {
      cat("\nProcessing layer key:", layer_name, "\n")
      
      if (!layer_name %in% names(r$individuals)) {
        cat("  -> layer not present in r$individuals, setting NA\n")
        values_vec[i] <- NA_real_
        i <- i + 1
        next
      }
      
      layer_rast <- r$individuals[[layer_name]]
      cat("  class(layer_rast):", class(layer_rast), "\n")
      
      if (inherits(layer_rast, "SpatRaster")) {
        layer_val <- terra::extract(
          layer_rast, point,
          method = "bilinear", ID = FALSE
        )
        cat("  raw extracted layer_val[1,1]:", layer_val[1, 1], "\n")
        
        if (!is.na(layer_val[1, 1]) && !is.infinite(layer_val[1, 1])) {
          max_val <- terra::global(layer_rast, "max", na.rm = TRUE)[1, 1]
          min_val <- terra::global(layer_rast, "min", na.rm = TRUE)[1, 1]
          cat("  min_val:", min_val, "max_val:", max_val, "\n")
          
          if (!is.na(max_val) && !is.na(min_val) && max_val > min_val) {
            scaled_val <- scales::rescale(
              layer_val[1, 1],
              to   = c(0, 100),
              from = c(min_val, max_val)
            )
          } else {
            cat("  -> invalid min/max, setting NA\n")
            scaled_val <- NA_real_
          }
        } else {
          cat("  -> layer_val is NA/Inf, setting NA\n")
          scaled_val <- NA_real_
        }
        values_vec[i] <- round(scaled_val, 1)
        cat("  scaled value:", values_vec[i], "\n")
      } else {
        cat("  -> not a SpatRaster, setting NA\n")
        values_vec[i] <- NA_real_
      }
      
      i <- i + 1
    }
    
    values_vec <- values_vec[seq_len(5)]
    cat("\nFinal values_vec:", paste(values_vec, collapse = ", "), "\n")
    
    total_score <- sum(values_vec, na.rm = TRUE)
    cat("Total composite score:", total_score, "\n")
    
    output$risk_sidebar <- renderUI({
      tagList(
        div(
          class = "composite-score",
          "Composite Risk Score: ",
          span(
            style = "font-size: 28px;",
            ifelse(all(is.na(values_vec)), "-", total_score)
          )
        ),
        div(
          class = "risk-squares",
          lapply(seq_along(values_vec), function(i) {
            score_txt <- ifelse(is.na(values_vec[i]), "-", values_vec[i])
            div(
              class = "risk-square-container",
              div(
                class = paste0("risk-square risk", i),
                score_txt
              ),
              div(
                class = "risk-label",
                HTML(layer_names[i])
              )
            )
          })
        ),
        div(
          id = "risk-coords",
          strong("Clicked Location:"), br(),
          paste0(
            "Lat: ", round(click_lat, 6),
            ", Lng: ", round(click_lng, 6)
          )
        )
      )
    })
  })
  
  
  output$risk_sidebar <- renderUI({
    init_vals <- rep("-", 5)
    layer_names <- c(
      "Environmental Hazards",
      "Vulnerability",
      "Air Quality",
      "Mining Sites",
      "Population"
    )
    
    tagList(
      div(
        class = "composite-score",
        "Composite Risk Score: ",
        span(style = "font-size: 28px;", "-")
      ),
      div(
        class = "risk-squares",
        lapply(seq_along(init_vals), function(i) {
          div(
            class = "risk-square-container",
            div(
              class = paste0("risk-square risk", i),
              init_vals[i]
            ),
            div(
              class = "risk-label",
              HTML(layer_names[i])
            )
          )
        })
      ),
      div(
        id = "risk-coords",
        strong("Clicked Location:"), br(),
        "Lat: -, Lng: -"
      )
    )
  })
  
  
  
  # Initialize empty sidebar
  outputOptions(output, "risk_map", suspendWhenHidden = FALSE)
  
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
    df = master_data$all_media_scored
    #View(stds)
    param_list = get_param_list(df, need_std = TRUE) # only show those with HQ
    #View(param_list)
    
    updateSelectInput(inputId = "station_plot_param",
                      choices = param_list,
                      selected = "Arsenic")
    
    updateSelectInput(inputId = "observation_plot_param",
                      choices = param_list,
                      selected = "Arsenic")
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
    df = master_data$sed_scored
    req(df)
    
    params_list = get_param_list(df)
    cat("\n\n", params_list,"\n")

    updateSelectInput(inputId = "sieve_plot_param",
                      choices = c("All Parameters" = "all", params_list),
                      selected = "all")
    
    updateSelectInput(inputId = "sieve_plot_station",
                      choices = c("All Stations" = "all", sort(unique(df$station))),
                      selected = "all")
  })
  
  # Compute water quality score per observation (row)
  observation_scores <- reactive({
    param = input$observation_plot_param
    req(param)
    
    cat("\n=== DEBUG observation_scores ===\n")
    cat("\nparam: ", param, "\n", "nrow(param): ", nrow(param))    
    df <- active_water_1333() |>
      filter(parameter == param)
    
    if (is.null(df) || nrow(df) == 0) {
      cat("\n[observation_scores] No data found.\n")
      return(NULL)
    }
    
    # Ensure HQ exists
    if (!"HQ" %in% names(df)) {
      message("[observation_scores] HQ column not found in active_water_1333()")
      cat("[observation_scores] HQ column not found in active_water_1333()")
      return(NULL) # just a throwaway instead of STOP
    }
    # View(df)
    # Compute HQ-based water score per observation
    out <- df %>%
      group_by(station, date) %>%
      summarise(
        water_score = mean(HQ, na.rm = TRUE),   # <- NEW HQ-based score
        max_HQ      = max(HQ, na.rm = TRUE),    # optional
        n_params    = sum(!is.na(HQ)),
        .groups = "drop"
      ) %>%
      filter(is.finite(water_score))
    # View(out)
    return(out)
  })
  
  output$observation_scores_ui <- renderUI({
    df <- observation_scores()
    
    if (is.null(df) || nrow(df) == 0) {
      return(no_data_callout("water"))
    }
    
    # otherwise return the plot output placeholder
    plotlyOutput("observation_scores_plot", height = "500px")
  })
  
  
  output$observation_scores_plot <- renderPlotly({
    df = observation_scores()
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    p <- df |>
      slice_max(water_score, n = 15) |>
      mutate(label = paste0(station, " (", date, ")"),
             label = fct_reorder(label, water_score))
    # View(p)
    p = p |>
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
      quiet_plotly(p, tooltip = "text")
    }) 
    #   else if (input$observation_plot_class == "class_b") {
    #     p <- observation_scores() |>
    #       slice_max(num_class_b, n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, num_class_b)) |>
    #       ggplot(aes(x = label, y = num_class_b,
    #                  text = paste("# Class B Parameters:", num_class_b))) +
    #       geom_col(fill = "lightgreen") +
    #       coord_flip() +
    #       theme_minimal() +
    #       labs(
    #         title = "# Class B: Top 15 Observations (Bolivia)",
    #         x = NULL, y = "Number of Class B Parameters"
    #       )
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$observation_plot_class == "class_c") {
    #     p <- observation_scores() |>
    #       slice_max(num_class_c, n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, num_class_c)) |>
    #       ggplot(aes(x = label, y = num_class_c,
    #                  text = paste("# Class C Parameters:", num_class_c))) +
    #       geom_col(fill = "gold") +
    #       coord_flip() +
    #       theme_minimal() +
    #       labs(
    #         title = "# Class C: Top 15 Observations (Bolivia)",
    #         x = NULL, y = "Number of Class C Parameters"
    #       )
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$observation_plot_class == "class_d") {
    #     p <- observation_scores() |>
    #       slice_max(num_class_d, n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, num_class_d)) |>
    #       ggplot(aes(x = label, y = num_class_d,
    #                  text = paste("# Class D Parameters:", num_class_d))) +
    #       geom_col(fill = "darkorange") +
    #       coord_flip() +
    #       theme_minimal() +
    #       labs(
    #         title = "# Class D: Top 15 Observations (Bolivia)",
    #         x = NULL, y = "Number of Class D Parameters"
    #       )
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$observation_plot_class == "unclassified") {
    #     p <- observation_scores() |>
    #       slice_max(num_unclass, n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, num_unclass)) |>
    #       ggplot(aes(x = label, y = num_unclass,
    #                  text = paste("# Unclassified Parameters:", num_unclass))) +
    #       geom_col(fill = "firebrick") +
    #       coord_flip() +
    #       theme_minimal() +
    #       labs(
    #         title = "# Unclassified: Top 15 Observations (Bolivia)",
    #         x = NULL, y = "Number of Unclassified Parameters"
    #       )
    #     quiet_plotly(p, tooltip = "text")
    #   }
    #   
    # 
    # else if (input$observation_std == "value") {
    #   param <- input$observation_plot_param
    #   
    #   if (param == "Oxygen Saturation (%)" | param == "Dissolved Oxygen (mg/l O2)" | param == "pH" | param == "Resistivity (Ohm.cm)") {
    #     req(param)
    #     p <- active_water_1333() |>
    #       slice_min(.data[[param]], n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, -.data[[param]])) |>
    #       ggplot(aes(x = label, y = .data[[param]],
    #                  text = paste0(param, ": ", round(.data[[param]], 3)))) +
    #       geom_col(fill = "steelblue") +
    #       labs(title = paste("15 Lowest Observations for", param),
    #            x = NULL, y = param) +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else {
    #     req(param)
    #     p <- active_water_1333() |>
    #       slice_max(.data[[param]], n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, .data[[param]])) |>
    #       ggplot(aes(x = label, y = .data[[param]],
    #                  text = paste0(param, ": ", round(.data[[param]], 3)))) +
    #       geom_col(fill = "steelblue") +
    #       labs(title = paste("15 Highest Observations for", param),
    #            x = NULL, y = param) +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   }
    # } 
    # else if (input$observation_std == "usgs") {
    #   
    #   df <- active_sed_usgs()
    #   
    #   if (input$observation_plot_usgs == "above_tel") {
    #     p <- df |>
    #       slice_max(num_above_tel, n = 15, with_ties = FALSE) |>
    #       mutate(
    #         label = paste0(station, " (", date, ")"),
    #         label = make.unique(label),
    #         label = fct_reorder(label, num_above_tel)) |>
    #       ggplot(aes(x = label, y = num_above_tel,
    #                  text = paste("# Above TEL:", num_above_tel, "<br>",
    #                               "Sieve Size:", `Sieve Size`, "<br>",
    #                               "Distance from Bank:", `Distance from Bank`))) +
    #       geom_col(fill = "darkorange") +
    #       labs(title = "# Above TEL: Top 15 Observations (Bolivia)",
    #            x = NULL, y = "Number of Parameters Above TEL") +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$observation_plot_usgs == "above_pel") {
    #     p <- df |>
    #       slice_max(num_above_pel, n = 15, with_ties = FALSE) |>
    #       mutate(
    #         label = paste0(station, " (", date, ")"),
    #         label = make.unique(label),
    #         label = fct_reorder(label, num_above_pel)) |>
    #       ggplot(aes(x = label, y = num_above_pel,
    #                  text = paste("# Above PEL:", num_above_pel, "<br>",
    #                               "Sieve Size:", `Sieve Size`, "<br>",
    #                               "Distance from Bank:", `Distance from Bank`))) +
    #       geom_col(fill = "firebrick") +
    #       labs(title = "# Above PEL: Top 15 Observations (Bolivia)",
    #            x = NULL, y = "Number of Parameters Above PEL") +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$observation_plot_usgs == "worst_score") {
    #     p <- df |>
    #       slice_max(sed_score, n = 15, with_ties = FALSE) |>
    #       mutate(
    #         label = paste0(station, " (", date, ")"),
    #         label = make.unique(label),
    #         label = fct_reorder(label, sed_score)) |>
    #       ggplot(aes(x = label, y = sed_score,
    #                  text = paste("Sediment Quality Score:", round(sed_score, 2), "<br>",
    #                               "Sieve Size:", `Sieve Size`, "<br>",
    #                               "Distance from Bank:", `Distance from Bank`))) +
    #       geom_col(fill = "darkslateblue") +
    #       labs(title = "Overall Sediment Score: Top 15 Observations (Bolivia)",
    #            x = NULL, y = "Sediment Quality Score (0=best, 2=worst)") +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   }
    #   
    #   
    #   
    # } 
    # else if (input$observation_std == "sed_value") {
    #   
    #   param <- input$observation_plot_param_sed
    #   
    #   df <- active_sed_clean()
    #   
    #   req(param)
    #   p <- df |>
    #     slice_max(.data[[param]], n = 15, with_ties = FALSE) |>
    #     mutate(
    #       label = paste0(station, " (", date, ")"),
    #       label = make.unique(label),
    #       label = fct_reorder(label, .data[[param]])) |>
    #     ggplot(aes(x = label, y = .data[[param]],
    #                text = paste0(param, ": ", round(.data[[param]], 3), "<br>",
    #                              "Sieve Size:", `Sieve Size`, "<br>",
    #                              "Distance from Bank:", `Distance from Bank`))) +
    #     geom_col(fill = "tan") +
    #     labs(title = paste("15 Highest Observations for", param),
    #          x = NULL, y = param) +
    #     coord_flip() +
    #     theme_minimal()
    #   
    #   quiet_plotly(p, tooltip = "text")
    #   
    # }
    # else if (input$observation_std == "hq") {
    #   param <- input$observation_plot_param
    #   
    #   if (param == "Oxygen Saturation (%)" | param == "Dissolved Oxygen (mg/l O2)" | param == "pH" | param == "Resistivity (Ohm.cm)") {
    #     req(param)
    #     p <- active_water_1333() |>
    #       slice_min(.data[[param]], n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, -.data[[param]])) |>
    #       ggplot(aes(x = label, y = .data[[param]],
    #                  text = paste0(param, ": ", round(.data[[param]], 3)))) +
    #       geom_col(fill = "steelblue") +
    #       labs(title = paste("15 Lowest Observations for", param),
    #            x = NULL, y = param) +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else {
    #     req(param)
    #     p <- active_water_1333() |>
    #       slice_max(.data[[param]], n = 15, with_ties = FALSE) |>
    #       mutate(label = paste0(station, " (", date, ")"),
    #              label = fct_reorder(label, .data[[param]])) |>
    #       ggplot(aes(x = label, y = .data[[param]],
    #                  text = paste0(param, ": ", round(.data[[param]], 3)))) +
    #       geom_col(fill = "steelblue") +
    #       labs(title = paste("15 Highest Observations for", param),
    #            x = NULL, y = param) +
    #       coord_flip() +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   }
    # }
  
  # Calculate max date for recency weighting
  max_date <- reactive({
    max(active_water_1333()$date, na.rm = TRUE)
  })
  
  # Calculate weighted normalized score per observation, then aggregate by station
  station_scores <- reactive({
    observation_scores() %>%
      mutate(weight = 1 / (1 + as.numeric(difftime(max_date(), date, units = "days")) / 365.25)) %>%
      group_by(station) %>%
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
      mutate(weight = 1 / (1 + as.numeric(difftime(max_date(), date, units = "days")))) |>
      group_by(station) |>
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
    df <- master_data$all_media_scored

    # 1/8/2026: Update to utilize new param names
    p = plot_top_hq_stations(df, 
                             media = input$station_plot_media, 
                             param = input$station_plot_param, 
                             fraction = input$station_plot_fraction, 
                             method = input$station_plot_method)
    quiet_plotly(p, tooltip = "text")
    
    # if (input$station_plot_type == "class") {
    #   
    #   if (input$station_plot_class == "worst_score") {
    #     p <- station_scores() |>
    #       slice_max(mean_water_score, n = 15) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_water_score), y = mean_water_score,
    #                  text = paste("Mean Water Quality Score:", round(mean_water_score, 2)))) +
    #       geom_col(fill = "darkslateblue") +
    #       coord_flip() +
    #       labs(
    #         title = "Overall Water Score: Top 15 Worst stations (Bolivia)",
    #         subtitle = "Lower scores indicate better water quality",
    #         x = NULL,
    #         y = "Mean Water Quality Score (0=best, 4=worst)"
    #       ) +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$station_plot_class == "class_b") {
    #     p <- station_scores() |>
    #       arrange(mean_class_b) |>
    #       slice_max(mean_class_b, n = 15) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_class_b), y = mean_class_b,
    #                  text = paste("Mean # Class B Parameters:", round(mean_class_b, 2)))) +
    #       geom_col(fill = "lightgreen") +
    #       coord_flip() +
    #       labs(
    #         title = "Mean # Class B: Top 15 stations (Bolivia)",
    #         subtitle = "Ranked by mean number of Class B parameters",
    #         x = NULL,
    #         y = "Mean number of Class B parameters"
    #       ) +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$station_plot_class == "class_c") {
    #     p <- station_scores() |>
    #       arrange(mean_class_c) |>
    #       slice_max(mean_class_c, n = 15) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_class_c), y = mean_class_c,
    #                  text = paste("Mean # Class C Parameters:", round(mean_class_c, 2)))) +
    #       geom_col(fill = "gold") +
    #       coord_flip() +
    #       labs(
    #         title = "Mean # Class C: Top 15 stations (Bolivia)",
    #         subtitle = "Ranked by mean number of Class C parameters",
    #         x = NULL,
    #         y = "Mean number of Class C parameters"
    #       ) +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$station_plot_class == "class_d") {
    #     p <- station_scores() |>
    #       arrange(mean_class_d) |>
    #       slice_max(mean_class_d, n = 15) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_class_d), y = mean_class_d,
    #                  text = paste("Mean # Class D Parameters:", round(mean_class_d, 2)))) +
    #       geom_col(fill = "darkorange") +
    #       coord_flip() +
    #       labs(
    #         title = "Mean # Class D: Top 15 stations (Bolivia)",
    #         subtitle = "Ranked by mean number of Class D parameters",
    #         x = NULL,
    #         y = "Mean number of Class D parameters"
    #       ) +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   } else if (input$station_plot_class == "unclassified") {
    #     p <- station_scores() |>
    #       arrange(mean_unclass) |>
    #       slice_max(mean_unclass, n = 15) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_unclass), y = mean_unclass,
    #                  text = paste("Mean # Unclassified Parameters:", round(mean_unclass, 2)))) +
    #       geom_col(fill = "firebrick") +
    #       coord_flip() +
    #       labs(
    #         title = "Mean # Unclassified: Top 15 stations (Bolivia)",
    #         subtitle = "Ranked by mean number of Unclassified parameters",
    #         x = NULL,
    #         y = "Mean number of Unclassified parameters"
    #       ) +
    #       theme_minimal()
    #     quiet_plotly(p, tooltip = "text")
    #   }
    #   
    # } else if (input$station_plot_type == "value" && !is.null(input$station_plot_param)) 
    #   {
    #   
    #   # Get selected parameter
    #   param <- input$station_plot_param
    #   
    #   # Parameters that should use lowest values
    #   reverse_params <- c("Oxygen Saturation (%)", "Dissolved Oxygen (mg/l O2)", "pH", "Resistivity (Ohm.cm)")
    #   
    #   if (input$station_param_type == "max") {
    #     
    #     # Summarize max value per station
    #     summary_df <- active_water_1333() %>%
    #       group_by(station) %>%
    #       summarise(
    #         max_value = max(.data[[param]], na.rm = TRUE),
    #         min_value = min(.data[[param]], na.rm = TRUE),
    #         n_obs = sum(!is.na(.data[[param]])),
    #         .groups = "drop"
    #       ) %>%
    #       filter(is.finite(max_value))
    #     
    #     if (param %in% reverse_params) {
    #       summary_df <- slice_min(summary_df, min_value, n = 15)
    #       
    #       req(param)
    #       
    #       p <- summary_df %>%
    #         mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #         ggplot(aes(x = reorder(station_label, -min_value), y = min_value,
    #                    text = paste0("Min ", param, ": ", round(min_value, 3)))) +
    #         geom_col(fill = "steelblue") +
    #         coord_flip() +
    #         labs(
    #           title = paste("Bottom 15 stations by Min", param),
    #           subtitle = "Minimum recorded value between 2016–2024",
    #           x = NULL,
    #           y = param
    #         ) +
    #         theme_minimal()
    #       
    #       quiet_plotly(p, tooltip = "text")
    #       
    #     } else {
    #       summary_df <- slice_max(summary_df, max_value, n = 15)
    #       
    #       req(param)
    #       
    #       p <- summary_df %>%
    #         mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #         ggplot(aes(x = reorder(station_label, max_value), y = max_value,
    #                    text = paste0("Max ", param, ": ", round(max_value, 3)))) +
    #         geom_col(fill = "steelblue") +
    #         coord_flip() +
    #         labs(
    #           title = paste("Top 15 stations by Max", param),
    #           subtitle = "Maximum recorded value between 2016–2024",
    #           x = NULL,
    #           y = param
    #         ) +
    #         theme_minimal()
    #       
    #       quiet_plotly(p, tooltip = "text")
    #       
    #     }
    #     
    #   } else if (input$station_param_type == "avg") {
    #     
    #     # Summarize average value per station
    #     summary_df <- active_water_1333() %>%
    #       group_by(station) %>%
    #       summarise(
    #         avg_value = mean(.data[[param]], na.rm = TRUE),
    #         n_obs = sum(!is.na(.data[[param]])),
    #         .groups = "drop"
    #       ) %>%
    #       filter(is.finite(avg_value))
    #     
    #     if (param %in% reverse_params) {
    #       summary_df <- slice_min(summary_df, avg_value, n = 15)
    #       
    #       req(param)
    #       
    #       p <- summary_df %>%
    #         mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #         ggplot(aes(x = reorder(station_label, -avg_value), y = avg_value,
    #                    text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
    #         geom_col(fill = "steelblue") +
    #         coord_flip() +
    #         labs(
    #           title = paste("Bottom 15 stations by Average", param),
    #           subtitle = "Average value between 2016–2024",
    #           x = NULL,
    #           y = param
    #         ) +
    #         theme_minimal()
    #       
    #       quiet_plotly(p, tooltip = "text")
    #       
    #     } else {
    #       summary_df <- slice_max(summary_df, avg_value, n = 15)
    #       
    #       req(param)
    #       
    #       p <- summary_df %>%
    #         mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #         ggplot(aes(x = reorder(station_label, avg_value), y = avg_value,
    #                    text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
    #         geom_col(fill = "steelblue") +
    #         coord_flip() +
    #         labs(
    #           title = paste("Top 15 stations by Average", param),
    #           subtitle = "Average value between 2016–2024",
    #           x = NULL,
    #           y = param
    #         ) +
    #         theme_minimal()
    #       
    #       quiet_plotly(p, tooltip = "text")
    #       
    #     }
    #     
    #   }
    # } else if (input$station_plot_type == "usgs") 
    #   {
    #   
    #   if (input$station_plot_usgs == "worst_score") {
    #     
    #     p <- station_scores_sed() |>
    #       arrange(mean_sed_score) |>
    #       slice_max(mean_sed_score, n = 15, with_ties = FALSE) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_sed_score), y = mean_sed_score,
    #                  text = paste("Mean Sediment Quality Score:", round(mean_sed_score, 2)))) +
    #       geom_col(fill = "darkslateblue") +
    #       coord_flip() +
    #       labs(title = "Overall Sediment Score: Top 15 Worst stations (Bolivia)",
    #            x = NULL, y = "Mean Sediment Quality Score (0=best, 2=worst)") +
    #       theme_minimal()
    #     
    #     quiet_plotly(p, tooltip = "text")
    #     
    #     
    #     
    #   } else if (input$station_plot_usgs == "above_tel") {
    #     
    #     p <- station_scores_sed() |>
    #       arrange(mean_above_tel) |>
    #       slice_max(mean_above_tel, n = 15, with_ties = FALSE) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_above_tel), y = mean_above_tel,
    #                  text = paste("Mean # Above TEL:", round(mean_above_tel, 2)))) +
    #       geom_col(fill = "darkorange") +
    #       coord_flip() +
    #       labs(title = "Mean # Above TEL: Top 15 Worst stations (Bolivia)",
    #            x = NULL, y = "Mean Number of Parameters Above TEL") +
    #       theme_minimal()
    #     
    #     quiet_plotly(p, tooltip = "text")
    #     
    #     
    #     
    #   } else if (input$station_plot_usgs == "above_pel") {
    #     
    #     p <- station_scores_sed() |>
    #       arrange(mean_above_pel) |>
    #       slice_max(mean_above_pel, n = 15, with_ties = FALSE) |>
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) |>
    #       ggplot(aes(x = reorder(station_label, mean_above_pel), y = mean_above_pel,
    #                  text = paste("Mean # Above PEL:", round(mean_above_pel, 2)))) +
    #       geom_col(fill = "firebrick") +
    #       coord_flip() +
    #       labs(title = "Mean # Above PEL: Top 15 Worst stations (Bolivia)",
    #            x = NULL, y = "Mean Number of Parameters Above PEL") +
    #       theme_minimal()
    #     
    #     quiet_plotly(p, tooltip = "text")
    #     
    #     
    #     
    #   }
    # } else if (input$station_plot_type == "sed_value") 
    #   {
    #   
    #   # Get selected parameter
    #   param <- input$station_plot_param_sed
    #   
    #   summary_df <- active_sed_clean() %>%
    #     group_by(station) %>%
    #     summarise(
    #       max_value = max(.data[[param]], na.rm = TRUE),
    #       min_value = min(.data[[param]], na.rm = TRUE),
    #       avg_value = mean(.data[[param]], na.rm = TRUE),
    #       n_obs = sum(!is.na(.data[[param]])),
    #       .groups = "drop"
    #     ) %>%
    #     filter(is.finite(max_value))
    #   
    #   
    #   if (input$station_param_type == "max") {
    #     
    #     summary_df <- slice_max(summary_df, max_value, n = 15)
    #     
    #     req(param)
    #     
    #     p <- summary_df %>%
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #       ggplot(aes(x = reorder(station_label, max_value), y = max_value,
    #                  text = paste0("Max ", param, ": ", round(max_value, 3)))) +
    #       geom_col(fill = "tan") +
    #       coord_flip() +
    #       labs(
    #         title = paste("Top 15 stations by Max", param),
    #         subtitle = "Maximum recorded value between 2016–2024",
    #         x = NULL,
    #         y = param
    #       ) +
    #       theme_minimal()
    #     
    #     quiet_plotly(p, tooltip = "text")
    #     
    #   } else if (input$station_param_type == "avg") {
    #     
    #     summary_df <- slice_max(summary_df, avg_value, n = 15)
    #     
    #     req(param)
    #     
    #     p <- summary_df %>%
    #       mutate(station_label = paste0(station, " (n = ", n_obs, ")")) %>%
    #       ggplot(aes(x = reorder(station_label, avg_value), y = avg_value,
    #                  text = paste0("Mean ", param, ": ", round(avg_value, 3)))) +
    #       geom_col(fill = "tan") +
    #       coord_flip() +
    #       labs(
    #         title = paste("Top 15 stations by Average", param),
    #         subtitle = "Average value between 2016–2024",
    #         x = NULL,
    #         y = param
    #       ) +
    #       theme_minimal()
    #     
    #     quiet_plotly(p, tooltip = "text")
    #   }
    #   
    #   
    # }
  })
  
  observe({
    df <- active_water_1333()
    stations <- sort(unique(df$station))
    updateSelectInput(inputId = "param_plot_station",
                      choices = c("All Stations", stations))
  })
  
  active_water_1333_param_plot <- reactive({
    df <- active_water_1333()
    
    if (isTRUE(input$param_plot_checkbox)) {
      df <- df |>
        filter(station == input$param_plot_station)
    }
    
    df
  })
  
  active_sed_usgs_param_plot <- reactive({
    df <- active_sed_usgs()
    
    if (isTRUE(input$param_plot_checkbox)) {
      df <- df |>
        filter(station == input$param_plot_station)
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
    req(master_data$all_media_scored)
    req(input$param_plot_media, input$param_plot_method_spatial, input$param_plot_method_temporal)
    
    cat("\n\nworking to render\n\n")
    validate(need(!is.null(master_data$all_media_scored),
                  "No data available for ranking."))
    
    View(master_data$all_media_scored)
    
    # 1/8/2026: Update to utilize new param names
    p = plot_top_hq_params(data = master_data$all_media_scored, 
                           media = input$param_plot_media,
                           fraction = input$param_plot_fraction,
                           temporal_aggregation = input$param_plot_method_temporal,
                           spatial_aggregation = input$param_plot_method_spatial,
                           station = input$param_plot_station,
                           decay_per_day = input$param_plot_decay)
    
    quiet_plotly(p, tooltip = "text")
  })
  
  # Nadav's Notes: Next to do. Will want to set up in a separate file like the others
  output$sieve_scores_plot <- renderPlotly({
    req(input$sieve_plot_method, input$sieve_plot_param, input$sieve_plot_station, master_data$sed_scored)
    
    p = plot_top_hq_sieve(data = master_data$sed_scored,
                          param_selection = input$sieve_plot_param,
                          method = input$sieve_plot_method,
                          station_selection = input$sieve_plot_station)
    quiet_plotly(p, tooltip = "text")
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
    
    campaign_dates <- my(campaigns)  # Convert to date using lubridate
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
    cat("  Rendering parameter_selector_ui\n")
    selectInput("water_metal_param", "Select Parameter:", choices = NULL)
  })
  
  # observe({
  #   cat("\n=== DEBUG water parameter dropdown ===\n")
  #   
  #   # Only run when water is selected
  #   if (is.null(input$plot_media)) {
  #     cat("  plot_media is NULL, skipping\n")
  #     return()
  #   }
  #   
  #   if (input$plot_media != "water") {
  #     cat("  Not water media, skipping\n")
  #     return()
  #   }
  #   
  #   cat("  Water media selected\n")
  #   
  #   req(master_data$water_scored)
  #   cat("  Water data exists with", nrow(master_data$water_scored), "rows\n")
  #   
  #   req(nrow(master_data$water_scored) > 0)
  #   
  #   # Check what columns exist
  #   cat("  Columns:", paste(names(master_data$water_scored)[1:min(10, ncol(master_data$water_scored))], collapse = ", "), "...\n")
  #   
  #   # Get unique parameters
  #   if (!"parameter" %in% names(master_data$water_scored)) {
  #     cat("  ERROR: No 'parameter' column found!\n")
  #     cat("  All columns:", paste(names(master_data$water_scored), collapse = ", "), "\n")
  #     return()
  #   }
  #   
  #   water_params <- unique(master_data$water_scored$parameter)
  #   cat("  Unique parameters found:", length(water_params), "\n")
  #   cat("  Sample parameters:", paste(head(water_params, 10), collapse = ", "), "\n")
  #   
  #   water_params <- water_params[!is.na(water_params)]
  #   
  #   # Filter out non-metal parameters (adjust this list based on what you see in the output)
  #   excluded_params <- c("Decimal latitude", "Decimal longitude", "Distance from Bank", 
  #                        "Distance from Shore", "Average Velocity", "Flow", "Clay", "Silt", 
  #                        "Sand", "Moisture", "Organic Matter", "pH", "Conductivity",
  #                        "Oxygen Saturation", "Dissolved Oxygen", "Resistivity", "Temperature",
  #                        "Turbidity", "Salinity")
  #   
  #   water_params <- setdiff(water_params, excluded_params)
  #   water_params <- sort(water_params)
  #   
  #   cat("  Final parameter count after filtering:", length(water_params), "\n")
  #   cat("  Final parameters:", paste(water_params[1:min(20, length(water_params))], collapse = ", "), "\n")
  #   
  #   if (length(water_params) == 0) {
  #     cat("  WARNING: No parameters available after filtering!\n")
  #     # Don't filter at all if nothing is left
  #     water_params <- sort(unique(master_data$water_scored$parameter))
  #     water_params <- water_params[!is.na(water_params)]
  #     cat("  Using all parameters instead:", length(water_params), "parameters\n")
  #   }
  #   
  #   # Update both dropdowns
  #   updateSelectInput(session, "water_metal_class", 
  #                     choices = water_params,
  #                     selected = if(length(water_params) > 0) water_params[1] else NULL)
  #   
  #   updateSelectInput(session, "water_metal_param", 
  #                     choices = water_params,
  #                     selected = if(length(water_params) > 0) water_params[1] else NULL)
  #   
  #   cat("  Dropdowns updated successfully\n")
  #   cat("================================\n\n")
  # })  
  # # Reactive color palette based on selected parameter
  # color_pal <- reactive({
  #   req(input$selected_parameter)
  #   df <- active_water_clean()
  #   vals <- df[[input$selected_parameter]]
  #   vals <- vals[vals > 0 & !is.na(vals)]  # exclude zeros/non-positives for log scale
  #   
  #   if(length(vals) == 0) vals <- c(1, 10)
  #   
  #   # Generate log-spaced breaks
  #   bins <- 10^seq(floor(log10(min(vals))),
  #                  ceiling(log10(max(vals))),
  #                  length.out = 8)
  #   
  #   bins <- c(-Inf, bins[-1], Inf)
  #   
  #   colorBin(
  #     palette = "Reds",
  #     bins = bins,
  #     domain = vals,
  #     pretty = FALSE
  #   )
  # })
  # 
  # # Radius scaling function for parameter map
  # size_pal_param <- reactive({
  #   req(input$selected_parameter)
  #   df <- active_water_clean()
  #   vals <- df[[input$selected_parameter]]
  #   vals <- vals[!is.na(vals)]
  #   
  #   function(values) {
  #     if(length(vals) == 0) return(rep(4, length(values)))
  #     scaled <- scales::rescale(values, to = c(4, 14), from = range(vals, na.rm = TRUE))
  #     scaled[is.na(scaled)] <- 4
  #     scaled
  #   }
  # })
  
  # ===== CLASSIFICATION MAP (Tab 2) LOGIC =====
  
  # Detect metals from columns ending with " Class"
  metals <- reactive({
    df <- active_water_1333()
    class_cols <- names(df)[stringr::str_ends(names(df), " Class")]
    metals <- stringr::str_remove(class_cols, " Class$")
    metals
  })
  
  output$metal_selector_ui <- renderUI({
    cat("  Rendering metal_selector_ui\n")
    selectInput("water_metal_class", "Select Parameter:", choices = NULL)
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
    
    stations <- unique(df$station[!is.na(df$station)])
    
    map_points <- lapply(stations, function(station) {
      station_data <- df %>%
        filter(station == station,
               !is.na(CampaignYM),
               CampaignYM <= selected_campaign_ym)
      
      # Filter out NA for selected parameter
      station_data <- station_data[!is.na(station_data[[input$selected_parameter]]), ]
      
      if (nrow(station_data) > 0) {
        station_data <- station_data[order(station_data$date, decreasing = TRUE), ]
        
        # Precompute popup text for leaflet
        popup_text <- paste0(
          "station: ", station_data$station[1], "<br>",
          "Campaign: ", station_data$campaign[1], "<br>",
          "date: ", format(station_data$date[1], "%Y-%m-%d"), "<br>",
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
    
    df$CampaignYM <- as.yearmon(df$campaign, "%B %Y")
    selected_campaign_ym <- as.yearmon(selected_campaign, "%B %Y")
    
    stations <- unique(df$station[!is.na(df$station)])
    
    map_points <- lapply(stations, function(station) {
      station_data <- df %>%
        filter(station == station,
               !is.na(CampaignYM),
               CampaignYM <= selected_campaign_ym)
      
      # Filter to rows with class and value available
      station_data <- station_data[
        !is.na(station_data[[class_col()]]) &
          !is.na(station_data[[value_col()]]),
      ]
      
      if (nrow(station_data) > 0) {
        station_data <- station_data[order(station_data$date, decreasing = TRUE), ]
        
        popup_text <- paste0(
          "station: ", station_data$station[1], "<br>",
          "Campaign: ", station_data$campaign[1], "<br>",
          "date: ", format(station_data$date[1], "%Y-%m-%d"), "<br>",
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
  #     df$date <- as.date(df$date, "%d/%m/%Y")
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
  #     df$date <- as.date(df$date, "%d/%m/%Y")
  #     df
  #   })
  #   
  #   bind_rows(water_dfs)
  # })
  
  # dynamically update choices for time series station & parameters
  observe({
    cat("\n\nin observe")
    df = master_data$all_media_scored
    req(df)
    cat("\n\nall_media_scored observer triggered")
    
    cat("\n\nall_media_scored class: ", class(master_data$all_media_scored)[1])
    
    cat("\n\nall_media_scored nrows: ", nrow(master_data$all_media_scored))
    
    df <- master_data$all_media_scored
  
    cat("\n=== DEBUG observe ts_tabs ===\n")
    cat(names(df))
    updateSelectInput(session, "ts_station", choices = c("All Stations" = "all", sort(unique(df$station))), selected = "All Stations")
    
    exclude_cols = c("Average Velocity",
                     "Decimal Latitude",
                     "Decimal Longitude",
                     "latitude_decimal",
                     "longitude_decimal",
                     "Latitude Decimal", 
                     "Longitude Decimal", 
                     "Lat_dd", 
                     "Long_dd",
                     "Distance from Bank",
                     "distance_from_bank",
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
                     "4.75 mm - No. 004 (ASTM) (%)",
                     "Flow",
                     "0.016 mm",
                     "0.032 mm - No. 450",
                     "0.063 mm - No. 230",
                     "0.125 mm - No. 120",
                     "0.250 mm - No. 060",
                     "0.500 mm - No. 035",
                     "1.00 mm - No. 018",
                     "2.00 mm - No. 010",
                     "4.75 mm - N° 004"
                     )
    
    cat("\n\nnames in df", names(df), "\n\n\n")
    
    param_cols <- df |>
      filter(!parameter %in% exclude_cols) |>        # remove metadata rows
      mutate(concentration = suppressWarnings(as.numeric(concentration))) |>
      filter(!is.na(concentration)) |>                         # keep real numeric params
      pull(parameter) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "ts_param", choices = param_cols)
    
    # observeEvent(input$ts_station, {
    #   req(input$ts_station)  # Ensure a station is selected
    #   
    #   # Filter to the selected station
    #   station_data <- df %>% filter(station == input$ts_station)
    #   
    #   # Find param columns with any non-NA values
    #   valid_params <- station_data %>%
    #     select(any_of(param_cols)) %>%
    #     select(where(~ any(!is.na(.)))) %>%
    #     colnames()
    #   
    #   # Update parameter dropdown
    #   updateSelectInput(session, "ts_param",
    #                     choices = sort(valid_params),
    #                     selected = ifelse(
    #                       input$ts_tabs == "Water Samples",
    #                       "Total Arsenic (ug/l As)",
    #                       "Arsenic (mg/kg As)"))
    # })
    
  })
  
  observeEvent(input$ts_station, {
    cat("\n\nts_station changed to: ", input$ts_station)
  })
  
  observeEvent(input$ts_param, {
    cat("\n\nts_param changed to: ", input$ts_param)
  })
  
  ts_filtered_data_water <- reactive({
    df = master_data$water_scored
    cat("\n\nnrows master_data", nrow(df))
    req(df)
    cat(paste0("\nmaster_data$water_scored rows: ", nrow(master_data$water_scored)))
    
    ts_station = input$ts_station
    ts_param = input$ts_param
    cat("\n\n\nts_station and ts_param\n\n\n")
    cat(ts_station)
    cat(ts_param)
    req(ts_station, ts_param)
    cat("after rec\n\n\n\n")
    # not using locyear since we don't want it in year format, we'd have to aggregate again by detail_rows and that sounds sucky
    # View(df)
    cat("\n=== DEBUG ts_filtered_data_water ===\n")
    cat(names(df)) # for debugging 
    cat(nrow(df))
    cat("Have HQ: ", df |> filter(!is.na(HQ)) |> nrow())
    
    # Check if parameter column exists in locyear
    if (!"parameter" %in% colnames(df)) {
      cat("ERROR: No parameter column in water data within detail_rows of locyear \n")
      return(data.frame())
    }
    
    # Filter by station and parameter
    cat(paste0("station selected: ", input$ts_station, ". Parameter selected: ", input$ts_param))
    cat(paste0("Total rows: ", nrow(df), ". Rows matching: "))
    df <- df %>%
      filter(station == input$ts_station,
             parameter == input$ts_param)
    cat(nrow(df))
    df = df %>%
      select(date, concentration, unit) %>%  # REMOVED classification - it's optional
      filter(!is.na(concentration))
    
    # Rename for consistency with existing code
    df <- df %>%
      rename(date = date, value = concentration)
    cat(nrow(df))
    return(df)
  })  
  
  # Initial filtering of parameter and station
  ts_filtered_data_sed_init <- reactive({
    df <- active_sed_clean()
    req(input$ts_station, input$ts_param)
    
    if (!(input$ts_param %in% colnames(df))) {
      return(data.frame())  # Prevent error by returning empty
    }
    
    df %>%
      filter(station == input$ts_station) %>%
      select(date, sieve_size, value = any_of(input$ts_param), distance_from_bank) %>%
      filter(!is.na(concentration))
  })
  
  # Update sieve (tamiz) choices based on station and parameter
  # observe({
  #   
  #   req(input$media == , input$ts_tamiz_checkbox)
  #   df <- ts_filtered_data_sed_init()
  #   
  #   if (!sieve_size %in% names(df)) return()
  #   
  #   ts_tamiz_choices <- sort(unique(df$sieve_size))
  #   
  #   updateSelectInput(session, "ts_tamiz", choices = ts_tamiz_choices, selected = ts_tamiz_choices[1])
  # })
  
  # filter for sieve (tamiz)
  ts_filtered_data_sed <- reactive({
    df <- ts_filtered_data_sed_init()
    
    if (isTRUE(input$ts_tamiz_checkbox)) {
      req(input$ts_tamiz)
      df <- df %>% filter(sieve_size == input$ts_tamiz)
    }
    
    df
    
  })
  
  # ts_standard_values <- reactive({
  #   req(input$ts_param)
  #   
  #   media <- if (input$ts_tabs == "Water Samples") "water" else "sediment"
  #   
  #   # default is all
  #   mode  <- if (is.null(input$ts_standard_mode)) "all" else input$ts_standard_mode
  #   
  #   # retrieve all relevant standards
  #   ts_get_standards(
  #     param_name = input$ts_param,
  #     media      = media,
  #     mode       = mode
  #   )
  # })
  
  output$ts_plot_water <- renderUI({
    cat("\n\n\n\nIN TS_PLOT_WATER\n\n\n\n")
    # Unwrap the reactive data with ()
    data_df <- master_data$water_scored
    
    # DIAGNOSTIC: What exactly is this?
    message("\n[DIAGNOSTIC] ts_filtered_data_water() output:")
    message("Class: ", paste(class(data_df), collapse = ", "))
    message("Is data.frame? ", is.data.frame(data_df))
    message("Is tibble? ", inherits(data_df, "tbl"))
    message("Is NULL? ", is.null(data_df))
    
    if (!is.null(data_df)) {
      message("Nrows: ", nrow(data_df))
      message("Columns: ", paste(colnames(data_df), collapse = ", "))
      message("First few rows:")
      print(head(data_df))
    }
    
    # Check that data exists and has rows
    if (is.null(data_df) || nrow(data_df) == 0) {
      message("No data to plot")
      return(NULL)
    }
    
    p <- plot_pilcomayo_ts(
      data = data_df,  # Pass as regular dataframe, not reactive
      media = "water",
      param = input$ts_param,
      station = input$ts_station,
      fraction = "any",
      standard_mode = input$ts_standard_mode
    )
    
    # The function returns a girafe object, not ggplot
    # so we return it directly without quiet_plotly
    return(p)
  })  
  output$ts_plot_sed <- renderUI({
    cat("\n\n\n\nIN TS_PLOT_SED\n\n\n\n")
    # Unwrap the reactive data with ()
    data_df <- master_data$sed_scored
    
    # DIAGNOSTIC: What exactly is this?
    message("\n[DIAGNOSTIC] ts_filtered_data_sed() output:")
    message("Class: ", paste(class(data_df), collapse = ", "))
    message("Is data.frame? ", is.data.frame(data_df))
    message("Is tibble? ", inherits(data_df, "tbl"))
    message("Is NULL? ", is.null(data_df))
    
    if (!is.null(data_df)) {
      message("Nrows: ", nrow(data_df))
      message("Columns: ", paste(colnames(data_df), collapse = ", "))
      message("First few rows:")
      print(head(data_df))
    }
    
    # Check that data exists and has rows
    if (is.null(data_df) || nrow(data_df) == 0) {
      message("No data to plot")
      return(NULL)
    }
    
    p <- plot_pilcomayo_ts(
      data = data_df,  # Pass as regular dataframe, not reactive
      media = "sediment",
      param = input$ts_param,
      station = input$ts_station,
      fraction = "any",
      standard_mode = input$ts_standard_mode
    )
    
    # The function returns a girafe object, not ggplot
    # so we return it directly without quiet_plotly
    return(p)
  })  
  
  # output$ts_plot_sed <- renderPlotly({
  #   df <- ts_filtered_data_sed()
  #   req(nrow(df) > 0)
  #   
  #   df$date <- as.date(df$date, format = "%d/%m/%Y")  
  #   
  #   # Create aggregated data for the line (average per date)
  #   df_line <- df %>%
  #     group_by(date) %>%
  #     summarise(avg_value = mean(concentration, na.rm = TRUE), .groups = 'drop')
  #   
  #   # Start with the base plot using individual points
  #   p <- ggplot(df, aes(x = date, y = concentration,
  #                       text = paste0("date: ", date, "<br>",
  #                                     input$ts_param, ": ", concentration, "<br>",
  #                                     "Sieve Size: ", sieve_size, "<br>",
  #                                     "Distance from Bank: ", distance_from_bank)))
  #   
  #   standard_vals <- ts_standard_values()
  #   if (!is.null(standard_vals)) {
  #     
  #     tel <- standard_vals[1]
  #     pel <- standard_vals[2]
  #     
  #     y_range <- max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)
  #     offset_amount <- y_range * 0.05
  #     
  #     p <- p +
  #       geom_hline(yintercept = tel, color = "darkorange", linetype = "dashed", linewidth = 0.7) +
  #       geom_hline(yintercept = pel, color = "firebrick", linetype = "dashed", linewidth = 0.7) +
  #       annotate("text", x = min(df$date), y = tel - offset_amount, label = paste("TEL =", tel, "mg/kg"), 
  #                hjust = 1.1, vjust = 0.5, color = "darkorange", size = 3, fontface = "bold") +
  #       annotate("text", x = min(df$date), y = pel + offset_amount, label = paste("PEL =", pel, "mg/kg"), 
  #                hjust = 1.1, vjust = 0.5, color = "firebrick", size = 3, fontface = "bold") +
  #       scale_x_date(expand = expansion(mult = c(0.2, 0.05))) +
  #       coord_cartesian(clip = "off")
  #   }
  #   
  #   # Check if Distance from Bank has variation
  #   has_variation <- length(unique(df$`Distance from Bank`)) > 1
  #   
  #   p <- p +
  #     # Add the line using averaged data
  #     geom_line(data = df_line, aes(x = date, y = avg_value, group = 1,
  #                                   text = paste0("date: ", date, "<br>",
  #                                                 "Average ", input$ts_param, ": ", round(avg_value, 3))),
  #               color = "black") +
  #     {if(has_variation) {
  #       # Multiple values - use fill aesthetic with legend
  #       geom_point(shape = 21, size = 1.5, fill = "black", stroke = 0.3, color = "black", aes(alpha = `Distance from Bank`))
  #     } else {
  #       # All same - black fill, no legend
  #       geom_point(shape = 21, size = 1.5, alpha = 0.5, fill = "black")
  #     }} +
  #     labs(
  #       title = paste("Time Series of", input$ts_param, "from Sediment Samples at", input$ts_station),
  #       x = "Time",
  #       y = input$ts_param,
  #       fill = if(has_variation) "Distance from Bank" else NULL  # Only show fill label if there's variation
  #     ) +
  #     theme_minimal() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   
  #   if(length(unique(df$`Distance from Bank`)) < 2) {
  #     p <- p + 
  #       scale_fill_continuous(guide = "none") +  # Remove legend
  #       # Optionally override colors to black
  #       scale_fill_manual(values = "black", guide = "none")
  #   }
  #   
  #   quiet_plotly(p, tooltip = "text")  # Show only the text tooltip
  # })
  
  
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
    req(master_data$sed_scored, input$sed_date_range)
    req(nrow(master_data$sed_scored) > 0)  # Make sure it has data
    
    master_data$sed_scored |>
      filter(date >= input$sed_date_range[1],
             date <= input$sed_date_range[2])
  })
  
  water_selected_data <- reactive({
    req(master_data$water_scored, input$water_date_range)
    req(nrow(master_data$water_scored) > 0)  # Make sure it has data
    
    master_data$sed_scored |>
      filter(date >= input$water_date_range[1],
             date <= input$water_date_range[2])
  })
  
  # Populate campaign dropdown for map
  output$sed_campaign_ui <- renderUI({
    # checks that our data works as expected
    req(master_data$sed_scored)
    req(nrow(master_data$sed_scored) > 0)  # Make sure it has data
    
    # get date range
    # Get min and max dates from data
    dates <- master_data$sed_scored$date
    
    # Make sure dates exist and are valid
    # needed bc fails at start otherwise
    dates <- dates[!is.na(dates)]
    
    if (length(dates) == 0) {
      return(p("No date data available"))
    }
    
    min_date <- min(dates, na.rm = TRUE)
    max_date <- max(dates, na.rm = TRUE)
    
    dateRangeInput(
      "sed_date_range",
      "Select date Range:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date,
      format = "yyyy-mm-dd",
      separator = " to "
    )
  })
  
  output$water_campaign_ui <- renderUI({
    req(master_data$water_scored)
    req(nrow(master_data$water_scored) > 0)
    
    date_col <- if ("date" %in% names(master_data$water_scored)) "date" else "date"
    dates <- master_data$water_scored[[date_col]]
    dates <- dates[!is.na(dates)]
    
    if (!inherits(dates, "date")) {
      dates <- as.date(dates)
    }
    
    min_date <- min(dates, na.rm = TRUE)
    max_date <- max(dates, na.rm = TRUE)
    
    dateRangeInput(
      "water_date_range",
      "Select date Range:",
      start = min_date,
      end = max_date,
      min = min_date,
      max = max_date,
      format = "yyyy-mm-dd",
      separator = " to "
    )
  })
  
  # Populate sieve size dropdown for map
  output$tamiz_ui <- renderUI({
    req(master_data$sed_scored)
    req(nrow(master_data$sed_scored) > 0)  # Make sure it has data
    
    tamiz <- unique(master_data$sed_scored$sieve_size)
    tamiz <- tamiz[!is.na(tamiz)]
    tamiz <- sort(tamiz)
    
    tamiz_choices = c("All" = "All", setNames(tamiz, tamiz))
    selectInput("tamiz", "Select Sieve Size:", choices = tamiz_choices, selected = "All")
  })
  
  # Populate metal dropdown dynamically for map
  observe({
    req(master_data$sed_scored)
    req(nrow(master_data$sed_scored) > 0)
    
    # Get unique parameters from the long-format data
    sed_params <- unique(master_data$sed_scored$parameter)
    sed_params <- sed_params[!is.na(sed_params)]
    
    # Filter out parameters that start with a digit (sieve sizes)
    sed_params <- sed_params[!grepl("^\\d", sed_params)]
    sed_params <- sort(sed_params)
    
    cat("Available sediment parameters:", paste(sed_params, collapse = ", "), "\n")
    
    updateSelectInput(session, "sed_metal", 
                      choices = sed_params,
                      selected = if(length(sed_params) > 0) sed_params[1] else NULL)
  })
  
  observe({
    cat("\n=== DEBUG new water param dropdown filler based on existing sed_data ===\n")
    req(master_data$water_scored)
    req(nrow(master_data$water_scored) > 0)
    
    # Get unique parameters from the long-format data
    water_params <- unique(master_data$water_scored$parameter)
    water_params <- water_params[!is.na(water_params)]
    water_params <- sort(water_params)
    
    cat("Available water parameters:", paste(water_params, collapse = ", "), "\n")
    
    updateSelectInput(session, "water_metal_param", 
                      choices = water_params,
                      selected = if(length(water_params) > 0) water_params[1] else NULL)
  })
  
  # for updatin the water parameter select option
  observe({
    req(master_data$water_scored)
    req(nrow(master_data$water_scored) > 0)
    
    water_params <- unique(master_data$water_scored$parameter)
    water_params <- water_params[!is.na(water_params)]
    
    # Filter out non-chemical parameters (adjust as needed)
    excluded <- c("pH", "Conductivity", "Temperature", "Dissolved Oxygen", 
                  "Oxygen Saturation", "Turbidity", "Flow", "Average Velocity")
    water_params <- setdiff(water_params, excluded)
    water_params <- sort(water_params)
    
    cat("Water parameters:", paste(water_params, collapse = ", "), "\n")
    
    updateSelectInput(session, "water_metal", 
                      choices = water_params,
                      selected = if(length(water_params) > 0) water_params[1] else NULL)
  })
  
  # Filtered data based on all inputs
  sed_filtered_data <- reactive({
    cat("\n=== DEBUG sed_filtered_data ===\n")
    
    req(master_data$sed_scored)
    req(input$plot_media == "sediment")  # Only run for sediment
    
    cat("  Initial rows:", nrow(master_data$sed_scored), "\n")
    
    df <- master_data$sed_scored
    
    date_col <- if ("date" %in% names(df)) "date" else "date"
    
    # Filter by Bolivia/All Locations
    if (!is.null(input$plot_data_scope) && input$plot_data_scope == "bol") {
      cat("  Filtering for Bolivia only\n")
      
      lng_col <- if ("longitude_decimal" %in% names(df)) {
        "longitude_decimal"
      } else if ("Long_dd" %in% names(df)) {
        "Long_dd"
      } else {
        "longitude"
      }
      
      lat_col <- if ("latitude_decimal" %in% names(df)) {
        "latitude_decimal"
      } else if ("Lat_dd" %in% names(df)) {
        "Lat_dd"
      } else {
        "latitude"
      }
      
      cat("all_sed_clean() in sed_filtered_data")
      df = filter_to_border(all_sed_clean(), "longitude_decimal", "latitude_decimal", bol_border)
    }
    
    cat("  Rows after location filter:", nrow(df), "\n")
    
    # Filter by date range - NOW USING map_date_range instead of sed_date_range
    if (!is.null(input$map_date_range) && length(input$map_date_range) == 2) {
      cat("  date range input:", paste(input$map_date_range, collapse = " to "), "\n")
      
      if (!inherits(df[[date_col]], "date")) {
        df[[date_col]] <- as.date(df[[date_col]])
      }
      
      start_date <- as.date(input$map_date_range[1])
      end_date <- as.date(input$map_date_range[2])
      
      df <- df %>%
        filter(.data[[date_col]] >= start_date,
               .data[[date_col]] <= end_date)
      
      cat("  Rows after date filter:", nrow(df), "\n")
    }
    
    # Filter by parameter (metal)
    if (!is.null(input$sed_metal) && input$sed_metal != "") {
      cat("  Filtering for parameter:", input$sed_metal, "\n")
      df <- df %>%
        filter(parameter == input$sed_metal)
      cat("  Rows after parameter filter:", nrow(df), "\n")
    }
    
    # Filter by sieve size
    if (!is.null(input$tamiz) && input$tamiz != "All") {
      cat("  Filtering for sieve size:", input$tamiz, "\n")
      df <- df %>%
        filter(sieve_size == input$tamiz)
      cat("  Rows after sieve filter:", nrow(df), "\n")
    }
    
    cat("  Final rows:", nrow(df), "\n")
    
    return(df)
  })
  
  # Create similar water_filtered_data reactive for water maps
  water_filtered_data <- reactive({
    req(master_data$water_scored)
    req(input$plot_media == "water")
    
    df <- master_data$water_scored
    date_col <- if ("date" %in% names(df)) "date" else "date"
    
    # Bolivia filter
    if (!is.null(input$plot_data_scope) && input$plot_data_scope == "bol") {
      lng_col <- if ("longitude_decimal" %in% names(df)) "longitude_decimal" else "Longitude Decimal"
      lat_col <- if ("latitude_decimal" %in% names(df)) "latitude_decimal" else "Latitude Decimal"
      
      df_sf <- df %>%
        filter(!is.na(.data[[lng_col]]), !is.na(.data[[lat_col]])) %>%
        st_as_sf(coords = c(lng_col, lat_col), crs = st_crs(bol_border))
      
      df_sf <- st_filter(df_sf, bol_border)
      
      coords <- st_coordinates(df_sf)
      df <- df_sf %>%
        mutate(
          !!lng_col := coords[, 1],
          !!lat_col := coords[, 2]
        ) %>%
        st_drop_geometry()
    }
    
    # date filter
    if (!is.null(input$water_date_range) && length(input$water_date_range) == 2) {
      if (!inherits(df[[date_col]], "date")) {
        df[[date_col]] <- as.date(df[[date_col]])
      }
      
      start_date <- as.date(input$water_date_range[1])
      end_date <- as.date(input$water_date_range[2])
      
      df <- df %>%
        filter(.data[[date_col]] >= start_date, .data[[date_col]] <= end_date)
    }
    
    # Parameter filter
    if (!is.null(input$water_metal) && input$water_metal != "") {
      df <- df %>% filter(parameter == input$water_metal)
    }
    
    return(df)
  })  
  # Render leaflet map
  output$sed_map <- renderLeaflet({
    # Only render once on initialization with base map
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
  
  observe({
    cat("\n=== DEBUG sed_map update ===\n")
    
    sed_df <- sed_filtered_data()
    
    req(nrow(sed_df) > 0)
    req(input$sed_metal)
    req(input$sed_value_type)
    
    cat("  Updating map with", nrow(sed_df), "points\n")
    
    # Determine coordinate columns
    lng_col <- if ("longitude_decimal" %in% names(sed_df)) {
      "longitude_decimal"
    } else if ("Long_dd" %in% names(sed_df)) {
      "Long_dd"
    } else {
      "longitude"
    }
    
    lat_col <- if ("latitude_decimal" %in% names(sed_df)) {
      "latitude_decimal"
    } else if ("Lat_dd" %in% names(sed_df)) {
      "Lat_dd"
    } else {
      "latitude"
    }
    
    cat("  Using coordinates:", lng_col, "and", lat_col, "\n")
    
    # Create palette based on value type
    if (input$sed_value_type == "sed_value") {
      cat("  Using numeric palette for concentrations\n")
      
      # Make sure concentration is numeric
      sed_df$concentration <- as.numeric(sed_df$concentration)
      
      # Remove any non-numeric or infinite values
      sed_df <- sed_df %>% filter(!is.na(concentration), is.finite(concentration))
      
      if (nrow(sed_df) == 0) {
        cat("  ERROR: No valid concentration values after cleaning\n")
        return()
      }
      
      cat("  Concentration range:", range(sed_df$concentration, na.rm = TRUE), "\n")
      
      pal <- colorNumeric(
        palette = "Reds",
        domain = sed_df$concentration,
        na.color = "gray"
      )
      colors <- pal(sed_df$concentration)
      
      label_text <- paste0(
        "station: ", sed_df$station, "<br>",
        "date: ", sed_df$date, "<br>",
        "Parameter: ", sed_df$parameter, "<br>",
        "Sieve Size: ", sed_df$sieve_size, "<br>",
        "Concentration: ", round(sed_df$concentration, 3), " ", sed_df$unit
      )
      
    } 
    else if (input$sed_value_type == "usgs") {
      cat("  Using factor palette for USGS standards\n")
      req("std_info" %in% names(sed_df))
      
      sed_df$std_info <- trimws(as.character(sed_df$std_info))
      valid_levels <- c("Below TEL", "Above TEL", "Above PEL")
      sed_df$std_info <- ifelse(sed_df$std_info %in% valid_levels,
                                sed_df$std_info,
                                NA_character_)
      
      pal <- colorFactor(
        palette = c("lightblue", "darkorange", "firebrick"),
        levels = valid_levels,
        na.color = "gray"
      )
      colors <- pal(sed_df$std_info)
      
      label_text <- paste0(
        "station: ", sed_df$station, "<br>",
        "date: ", sed_df$date, "<br>",
        "Parameter: ", sed_df$parameter, "<br>",
        "Sieve Size: ", sed_df$sieve_size, "<br>",
        "Concentration: ", round(sed_df$concentration, 3), " ", sed_df$unit, "<br>",
        "Standard: ", sed_df$std_info
      )
      
    } 
    else if (input$sed_value_type == "hq") {
      cat("  Using HQ palette\n")
      cat(names(sed_df))
      # Make sure HQ is numeric
      sed_df$HQ <- as.numeric(sed_df$HQ)
      
      # Remove any non-numeric or infinite HQ values
      sed_df <- sed_df %>% filter(!is.na(HQ), is.finite(HQ))
      
      if (nrow(sed_df) == 0) {
        cat("  ERROR: No valid HQ values after cleaning\n")
        ## reset the user to the concentrations page if none exist
        return()
      }
      
      cat("  HQ range:", range(sed_df$HQ, na.rm = TRUE), "\n")
      
      # Create color palette only for HQ >= 1
      hq_above_1 <- sed_df$HQ[sed_df$HQ >= 1]
      
      if (length(hq_above_1) > 0) {
        # Palette for values >= 1
        pal <- colorNumeric(
          palette = "YlOrRd",  # Yellow-Orange-Red palette
          domain = c(1, max(hq_above_1)),
          na.color = "gray"
        )
        
        # Assign colors: bland for <1, palette for >=1
        colors <- ifelse(sed_df$HQ < 1, 
                         "#E0E0E0",  # Light gray for HQ < 1
                         pal(sed_df$HQ))
      } else {
        # All values are < 1
        colors <- rep("#E0E0E0", nrow(sed_df))
        pal <- NULL
      }
      
      label_text <- paste0(
        "station: ", sed_df$station, "<br>",
        "date: ", sed_df$date, "<br>",
        "Parameter: ", sed_df$parameter, "<br>",
        "Sieve Size: ", sed_df$sieve_size, "<br>",
        "HQ: ", round(sed_df$HQ, 3), "<br>",
        "Concentration: ", round(sed_df$concentration, 3), " ", sed_df$unit
      )
    }
    
    # Use leafletProxy to update only the markers
    leafletProxy("sed_map", data = sed_df) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~get(lng_col),
        lat = ~get(lat_col),
        radius = 6,
        stroke = TRUE,
        color = "black",
        weight = 1.5,
        fillOpacity = 0.8,
        fillColor = colors,
        label = lapply(label_text, htmltools::HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = if(input$sed_value_type == "sed_value") {
          pal
        } else if (input$sed_value_type == "usgs") {
          pal
        } else if (input$sed_value_type == "hq" && !is.null(pal)) {
          pal  # HQ palette (only if there are HQ >= 1)
        } else {
          NULL
        },
        values = if(input$sed_value_type == "sed_value") {
          ~concentration
        } else if (input$sed_value_type == "usgs") {
          ~std_info
        } else if (input$sed_value_type == "hq" && !is.null(pal)) {
          ~HQ[HQ >= 1]  # Only show legend for HQ >= 1
        } else {
          NULL
        },
        title = if(input$sed_value_type == "sed_value") {
          paste0(input$sed_metal, "<br>(", sed_df$unit[1], ")")
        } else if (input$sed_value_type == "usgs") {
          "USGS Standard"
        } else if (input$sed_value_type == "hq") {
          "Hazard Quotient (HQ)<br>HQ ≥ 1"
        } else {
          ""
        },
        opacity = 1
      )
    
    cat("  Map updated successfully!\n")
    cat("=====================\n\n")
  })  
  output$water_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = pilco_line, color = "darkcyan", weight = 3, opacity = 0.8) %>%
      addPolygons(data = bol_border, color = "black", weight = 3, fill = FALSE) %>%
      setView(lng = -63.5, lat = -21.3, zoom = 7)
  })
  
  # Use observe() with leafletProxy to update water markers
  observe({
    cat("\n=== DEBUG water_map update ===\n")
    
    # Check if water is selected
    if (is.null(input$plot_media)) {
      cat("  plot_media is NULL\n")
      return()
    }
    
    if (input$plot_media != "water") {
      cat("  Not water media, skipping\n")
      return()
    }
    
    cat("  Water media confirmed\n")
    
    # Get filtered data
    water_df <- water_filtered_data()
    cat("  water_filtered_data returned:", nrow(water_df), "rows\n")
    
    if (nrow(water_df) == 0) {
      cat("  No data after filtering\n")
      leafletProxy("water_map") %>%
        clearMarkers() %>%
        clearControls() %>%
        addControl(
          html = "<div style='background: white; padding: 10px; border-radius: 5px; border: 2px solid #e74c3c;'>
                  <strong>No data found</strong><br>
                  Try adjusting your filters
                </div>",
          position = "topright"
        )
      return()
    }
    
    # Check required inputs
    if (is.null(input$water_metal)) {
      cat("  water_metal is NULL\n")
      return()
    }
    
    if (is.null(input$water_value_type)) {
      cat("  water_value_type is NULL\n")
      return()
    }
    
    cat("  water_metal:", input$water_metal, "\n")
    cat("  water_value_type:", input$water_value_type, "\n")
    
    # Determine coordinate columns
    cat("  Available columns:", paste(names(water_df)[1:min(10, ncol(water_df))], collapse = ", "), "...\n")
    
    lng_col <- if ("longitude_decimal" %in% names(water_df)) {
      "longitude_decimal"
    } else if ("Longitude Decimal" %in% names(water_df)) {
      "Longitude Decimal"
    } else {
      "longitude"
    }
    
    lat_col <- if ("latitude_decimal" %in% names(water_df)) {
      "latitude_decimal"
    } else if ("Latitude Decimal" %in% names(water_df)) {
      "Latitude Decimal"
    } else {
      "latitude"
    }
    
    cat("  Using coordinates:", lng_col, "and", lat_col, "\n")
    
    # Check if coordinates exist
    if (!lng_col %in% names(water_df) || !lat_col %in% names(water_df)) {
      cat("  ERROR: Coordinate columns not found!\n")
      return()
    }
    
    # Check concentration column
    if (!"concentration" %in% names(water_df)) {
      cat("  ERROR: No concentration column!\n")
      return()
    }
    
    cat("  Concentration range:", range(water_df$concentration, na.rm = TRUE), "\n")
    
    # Create palette
    if (input$water_value_type == "water_value") {
      cat("  Creating numeric palette\n")
      
      conc_values <- water_df$concentration[!is.na(water_df$concentration)]
      if (length(conc_values) == 0) {
        cat("  ERROR: No non-NA concentration values\n")
        return()
      }
      
      pal <- colorNumeric(palette = "Reds", domain = conc_values, na.color = "gray")
      colors <- pal(water_df$concentration)
      
      label_text <- paste0(
        "station: ", water_df$station, "<br>",
        "date: ", water_df$date, "<br>",
        "Parameter: ", water_df$parameter, "<br>",
        "Concentration: ", round(water_df$concentration, 3), " ", water_df$unit
      )
      
    } else if (input$water_value_type == "water_class") {
      cat("  Creating classification palette\n")
      
      # Check for classification column
      class_col <- if ("classification" %in% names(water_df)) {
        "classification"
      } else if ("class" %in% names(water_df)) {
        "class"
      } else if ("std_info" %in% names(water_df)) {
        "std_info"
      } else {
        cat("  ERROR: No classification column found\n")
        cat("  Available columns:", paste(names(water_df), collapse = ", "), "\n")
        return()
      }
      
      cat("  Using classification column:", class_col, "\n")
      
      water_df[[class_col]] <- trimws(as.character(water_df[[class_col]]))
      valid_levels <- c("Class A", "Class B", "Class C", "Class D", "Unclassified")
      
      pal <- colorFactor(
        palette = c("lightblue", "lightgreen", "gold", "darkorange", "darkred"),
        levels = valid_levels,
        na.color = "gray"
      )
      colors <- pal(water_df[[class_col]])
      
      label_text <- paste0(
        "station: ", water_df$station, "<br>",
        "date: ", water_df$date, "<br>",
        "Parameter: ", water_df$parameter, "<br>",
        "Concentration: ", round(water_df$concentration, 3), " ", water_df$unit, "<br>",
        "Class: ", water_df[[class_col]]
      )
    } else if (input$water_value_type == "hq") {
      cat("  Creating numeric palette\n")
      cat(names(water_df))
      View(water_df)
      conc_values <- water_df$HQ[!is.na(water_df$HQ)]
      if (length(conc_values) == 0) {
        cat("  ERROR: No non-NA HQ values\n")
        return()
      }
      
      pal <- colorNumeric(palette = "Reds", domain = conc_values, na.color = "gray")
      colors <- pal(water_df$HQ)
      
      label_text <- paste0(
        "station: ", water_df$station, "<br>",
        "date: ", water_df$date, "<br>",
        "Parameter: ", water_df$parameter, "<br>",
        "Hazard Quotient: ", round(water_df$HQ, 3), " ", water_df$unit
      )
    } else { cat("water_value_type isn't recognized!!") }
    
    # Calculate radius
    cat("  Calculating radius values\n")
    conc_range <- range(water_df$concentration, na.rm = TRUE)
    cat("  Concentration range for scaling:", conc_range, "\n")
    
    radius_values <- scales::rescale(water_df$concentration, to = c(4, 14), from = conc_range)
    radius_values[is.na(radius_values)] <- 4
    
    cat("  Creating map with", nrow(water_df), "points\n")
    
    # Update map
    leafletProxy("water_map", data = water_df) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng = ~get(lng_col),
        lat = ~get(lat_col),
        radius = radius_values,
        stroke = TRUE,
        color = "black",
        weight = 1.5,
        fillOpacity = 0.8,
        fillColor = colors,
        label = lapply(label_text, htmltools::HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = if(input$water_value_type == "water_value") {
          ~concentration
        } else {
          ~get(class_col)
        },
        title = input$water_metal,
        opacity = 1
      )
    
    cat("  Water map updated successfully!\n")
    cat("=====================\n\n")
  })
  
  output$sed_legend <- renderUI({
    req(input$sed_value_type)
    
    # Use isolate to prevent this from triggering on every change
    sed_df <- isolate(sed_filtered_data())
    
    if (input$sed_value_type == "usgs") {
      tags$div(
        tags$h5("Legend:"),
        tags$ul(
          tags$li(tags$span(style = "color:lightblue;", "⬤"), " Below TEL"),
          tags$li(tags$span(style = "color:darkorange;", "⬤"), " Above TEL"),
          tags$li(tags$span(style = "color:firebrick;", "⬤"), " Above PEL")
        )
      )
    } else if (input$sed_value_type == "sed_value" && nrow(sed_df) > 0) {
      values <- sed_df$concentration[!is.na(sed_df$concentration)]
      
      if (length(values) > 0) {
        rng <- range(values, na.rm = TRUE)
        
        tags$div(
          tags$h5(paste0("Concentration (", sed_df$unit[1], ")")),
          tags$div(style = "height: 20px; background: linear-gradient(to right, #fff5f0, #fb6a4a, #67000d);"),
          tags$div(
            style = "display: flex; justify-content: space-between;",
            tags$span(format(round(rng[1], 2), nsmall = 2)),
            tags$span(format(round(rng[2], 2), nsmall = 2))
          )
        )
      }
    } else {
      NULL
    }
  })
  
  
  output$water_legend <- renderUI({
    req(input$water_value_type)
    
    # Get the filtered data
    water_df <- water_filtered_data()
    
    # If no data or not water media, return nothing
    if (is.null(water_df) || nrow(water_df) == 0 || 
        is.null(input$plot_media) || input$plot_media != "water") {
      return(NULL)
    }
    
    if (input$water_value_type == "water_class") {
      # Classification legend
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
    } else if (input$water_value_type == "water_value") {
      # Concentration gradient legend
      
      # Get concentration values for the selected parameter
      values <- water_df$concentration[!is.na(water_df$concentration)]
      
      if (length(values) > 0) {
        rng <- range(values, na.rm = TRUE)
        
        # Get the unit from the data
        unit <- unique(water_df$unit[!is.na(water_df$unit)])[1]
        if (is.na(unit)) unit <- ""
        
        tags$div(
          tags$h5(paste0("Concentration (", unit, ")")),
          tags$div(style = "height: 20px; background: linear-gradient(to right, #fff5f0, #fb6a4a, #67000d);"),
          tags$div(
            style = "display: flex; justify-content: space-between;",
            tags$span(format(round(rng[1], 3), nsmall = 3)),
            tags$span(format(round(rng[2], 3), nsmall = 3))
          )
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  })
  
  
  output$sed_table <- renderDT({
    sed_filtered_data()
  })
  
  output$water_table <- renderDT({
    water_filtered_data()
  })
  
  # usgs_sqg loads data in format "Arsenic (mg/kg As)"
  output$stds_sed_table <- renderDT({
    cat("\n=== DEBUG stds_sed_table ===\n")
    cat(names(stds))
    stds |>
      filter(media == "sediment") |>
      select(-c("...1", ".key"))
  })
  
  output$stds_1333_table <- renderDT({
    stds |>
      filter(regulator == "Bolivian Law 1333")
  })
  
  output$stds_usgs_table_ts <- renderDT({
    stds |>
      filter(regulator == "USGS")
  })
  
  output$stds_all <- renderDT({
    stds
  })
}
