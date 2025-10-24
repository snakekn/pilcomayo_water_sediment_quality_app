# helpers.R - Place this in your app directory and source it before ui.R/server.R
# Small helper to reconcile legacy coord names the app expects
.reconcile_legacy_names <- function(df) {
  rename_map <- c(
    "Decimal latitude"  = "Latitude Decimal",
    "Decimal Longitude" = "Longitude Decimal",
    "Decimal longitude" = "Longitude Decimal",
    "Latitud Decimal"   = "Latitude Decimal",
    "Longitud Decimal"  = "Longitude Decimal",
    "Estación"          = "Station",
    "Fecha"             = "Date"
  )
  hit <- intersect(names(rename_map), names(df))
  names(df)[match(hit, names(df))] <- rename_map[hit]
  df
}

# Align a list of data.frames to same columns (union), keeping col order of the first
.align_cols <- function(dfs) {
  all_cols <- Reduce(union, lapply(dfs, names))
  lapply(dfs, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
}
dataMergeServer <- function(id, base_data) {
  moduleServer(id, function(input, output, session) {
    
    # keep parsed uploads (filename -> df)
    r_store <- reactiveVal(list())
    
    # add files using your canonical loader
    observeEvent(input$files, {
      req(input$files)
      
      cur <- r_store()
      for (i in seq_len(nrow(input$files))) {
        nm  <- input$files$name[i]
        pth <- input$files$datapath[i]
        
        df <- load_water_data(path = pth, translate_to = translate_to)
        
        # normalize to app conventions
        df <- .reconcile_legacy_names(df)
        df <- .coerce_key_types(df)
        
        df$SourceFile <- nm
        cur[[nm]] <- df
      }
      r_store(cur)
    }, ignoreInit = TRUE)
    
    # file list
    output$files_table <- renderTable({
      lst <- r_store()
      if (!length(lst)) return(NULL)
      data.frame(
        file = names(lst),
        rows = vapply(lst, nrow, integer(1)),
        cols = vapply(lst, ncol, integer(1)),
        check.names = FALSE
      )
    })
    
    # parsed uploads (appended)
    parsed_uploads <- reactive({
      lst <- r_store()
      if (!length(lst)) return(NULL)
      dfs <- lapply(unname(lst), .coerce_key_types)
      dfs <- .align_cols(dfs)
      dplyr::bind_rows(dfs)
    })
    
    output$parsed_table <- renderTable({
      req(parsed_uploads())
      head(parsed_uploads(), 12)
    })
    
    # merged = initial dataset + uploads
    merged <- reactive({
      base <- if (is.function(base_data)) base_data() else base_data
      req(!is.null(base))
      base <- .coerce_key_types(.reconcile_legacy_names(base))
      up <- parsed_uploads()
      if (is.null(up)) return(base)
      dfs <- .align_cols(list(base, up))
      dplyr::bind_rows(dfs)
    })
    
    output$merged_head <- renderTable({ head(merged(), 12) })
    
    output$download_merged <- downloadHandler(
      filename = function() paste0("merged_", Sys.Date(), ".csv"),
      content  = function(file) readr::write_csv(merged(), file)
    )
    
    list(merged = merged, parsed = parsed_uploads, files = reactive(names(r_store())))
  })
}

# ============================================================================
# STATIC DATA LOADING (load once at app startup)
# ============================================================================

# ensure legacy names used by the rest of the app
.reconcile_legacy_names <- function(df) {
  rename_map <- c(
    "Decimal latitude"  = "Latitude Decimal",
    "Decimal Longitude" = "Longitude Decimal",
    "Decimal longitude" = "Longitude Decimal",
    "Latitud Decimal"   = "Latitude Decimal",
    "Longitud Decimal"  = "Longitude Decimal",
    "Estación"          = "Station",
    "Fecha"             = "Date"
  )
  hit <- intersect(names(rename_map), names(df))
  names(df)[match(hit, names(df))] <- rename_map[hit]
  df
}

# coerce key columns to stable types so bind_rows never clashes
.coerce_key_types <- function(df) {
  # Station
  if ("Station" %in% names(df)) df$Station <- as.character(df$Station)
  # Date / Year
  if ("Date" %in% names(df) && !inherits(df$Date, "Date")) {
    suppressWarnings({
      a <- try(as.Date(df$Date, "%Y-%m-%d"))
      b <- try(as.Date(df$Date, "%d/%m/%Y"))
      df$Date <- if (all(!is.na(a))) a else if (all(!is.na(b))) b else as.Date(df$Date)
    })
  }
  if ("Date" %in% names(df) && !"Year" %in% names(df)) {
    df$Year <- as.integer(format(df$Date, "%Y"))
  }
  if ("Year" %in% names(df)) df$Year <- suppressWarnings(as.integer(df$Year))
  # Coordinates
  for (nm in c("Latitude Decimal","Longitude Decimal","Lat_dd","Long_dd","Lat_dd","Long_dd")) {
    if (nm %in% names(df) && !is.numeric(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
  }
  df
}

# align list of dfs to same columns (union) with the column order of the first
.align_cols <- function(dfs) {
  all_cols <- Reduce(union, lapply(dfs, names))
  lapply(dfs, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) df[[m]] <- NA
    df[, all_cols, drop = FALSE]
  })
}


# Load spatial data
pilco_line <- st_read("data/geojson/pilco_line.geojson", quiet = TRUE)
bol_border <- st_read("data/geojson/bol_borders.geojson", quiet = TRUE)

# Load standards with match names
usgs_sqg <- read_csv("data/standards/USGS_SQG.csv", show_col_types = FALSE) |>
  mutate(match_name = c("Arsenic (mg/kg As)",
                        "Cadmium (mg/kg Cd)",
                        "Copper (mg/kg Cu)",
                        "Chromium (mg/kg Cr)",
                        "Lead (mg/kg Pb)",
                        "Mercury (mg/kg Hg)",
                        "Nickel (mg/kg Ni)",
                        "Zinc (mg/kg Zn)"))

bolivian_1333 <- read_csv("data/standards/bolivian_standards_1333.csv", show_col_types = FALSE) |>
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

# Classification mapping
CLASS_MAP <- c("Class A" = 0, "Class B" = 1, "Class C" = 2, "Class D" = 3, "Unclassified" = 4)

# ============================================================================
# DATA LOADING HELPERS
# ============================================================================

#' Load and combine data from multiple Excel files by year
#' @param path Directory containing the files
#' @param pattern Regex pattern to match files
#' @param date_format Format string for date parsing
#' @param station_renames Named vector of station name replacements
load_yearly_data <- function(path, pattern, date_format = "%d/%m/%Y", 
                             station_renames = NULL) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  dfs <- lapply(files, function(f) {
    year <- stringr::str_extract(basename(f), "\\d{4}")
    df <- read_xlsx(f)
    df$Year <- as.integer(year)
    df$Date <- as.Date(df$Date, date_format)
    df
  })
  
  result <- bind_rows(dfs)
  
  # Apply station name replacements if provided
  if (!is.null(station_renames)) {
    for (old_name in names(station_renames)) {
      result$Station <- str_replace(result$Station, old_name, station_renames[[old_name]])
    }
  }
  
  result
}

#' Extract coordinates and drop geometry from sf object
extract_coords_and_drop_geometry <- function(sf_obj) {
  coords <- st_coordinates(sf_obj)
  
  # Determine coordinate column names based on what exists
  lon_col <- intersect(c("Longitude Decimal", "Long_dd"), names(sf_obj))[1]
  lat_col <- intersect(c("Latitude Decimal", "Lat_dd"), names(sf_obj))[1]
  
  sf_obj |>
    mutate(
      !!lon_col := coords[, 1],
      !!lat_col := coords[, 2]
    ) |>
    st_drop_geometry()
}

#' Apply spatial filter to keep only points in Bolivia
spatial_filter_bolivia <- function(data, lon_col, lat_col) {
  data |>
    st_as_sf(coords = c(lon_col, lat_col), crs = st_crs(bol_border)) |>
    st_filter(bol_border) |>
    extract_coords_and_drop_geometry()
}

# ============================================================================
# RANKING PLOT HELPERS
# ============================================================================

#' Create a ranking bar plot
#' @param data Data frame
#' @param metric Column name to rank by
#' @param n Number of top items to show
#' @param color Bar fill color
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param ylabel Y-axis label
#' @param reverse If TRUE, slice_min instead of slice_max
#' @param extra_tooltip_cols Additional columns to include in tooltip
create_ranking_plot <- function(data, metric, n = 15, color, title, 
                                subtitle = NULL, ylabel = NULL,
                                reverse = FALSE, extra_tooltip_cols = NULL) {
  
  ylabel <- ylabel %||% metric
  
  if (reverse) {
    data <- data |> slice_min(!!sym(metric), n = n, with_ties = FALSE)
  } else {
    data <- data |> slice_max(!!sym(metric), n = n, with_ties = FALSE)
  }
  
  data <- data |>
    mutate(
      label = paste0(Station, " (", Date, ")"),
      label = make.unique(label),
      label = fct_reorder(label, !!sym(metric))
    )
  
  # Build tooltip text
  tooltip_text <- paste0(metric, ": ", round(data[[metric]], 3))
  if (!is.null(extra_tooltip_cols)) {
    for (col in extra_tooltip_cols) {
      if (col %in% names(data)) {
        tooltip_text <- paste0(tooltip_text, "<br>", col, ": ", data[[col]])
      }
    }
  }
  data$tooltip_text <- tooltip_text
  
  p <- ggplot(data, aes(x = label, y = !!sym(metric), text = tooltip_text)) +
    geom_col(fill = color) +
    coord_flip() +
    labs(title = title, subtitle = subtitle, x = NULL, y = ylabel) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}

#' Plot class proportions with overlay (standardized vs raw)
plot_class_proportions_overlay <- function(data, class_cols, class_label, 
                                           bar_color, plot_title, 
                                           plot_subtitle = NULL) {
  total_rows <- nrow(data)
  
  summary_df <- sapply(data[class_cols], function(col) {
    num_class <- sum(col == class_label, na.rm = TRUE)
    non_na_count <- sum(!is.na(col))
    
    prop_total <- num_class / total_rows
    prop_non_na <- if (non_na_count == 0) NA else num_class / non_na_count
    
    c(Percent_Total = prop_total * 100, Percent_NonNA = prop_non_na * 100)
  }) %>%
    t() %>%
    as.data.frame() %>%
    mutate(Parameter = rownames(.))
  
  # Clean parameter names
  summary_df$Parameter <- str_remove(summary_df$Parameter, " Class$")
  summary_df$Parameter <- str_remove(summary_df$Parameter, " USGS$")
  
  # Top 15
  top_15 <- summary_df %>%
    arrange(desc(Percent_Total)) %>%
    slice(1:15)
  
  plot_data <- top_15 %>%
    pivot_longer(cols = c(Percent_Total, Percent_NonNA),
                 names_to = "Metric", values_to = "Value")
  
  plot_data$Parameter <- factor(plot_data$Parameter, levels = rev(top_15$Parameter))
  
  # Create hover text
  plot_data$hover_text <- ifelse(
    plot_data$Metric == "Percent_Total",
    paste0("% ", class_label, " (all observations): ", round(plot_data$Value, 2)),
    paste0("% ", class_label, " (non-NA observations): ", round(plot_data$Value, 2))
  )
  
  p <- ggplot(plot_data, aes(x = Parameter, y = Value, fill = Metric, text = hover_text)) +
    geom_col(
      position = "identity",
      alpha = ifelse(plot_data$Metric == "Percent_Total", 1, 0.4)
    ) +
    scale_fill_manual(values = c(Percent_Total = bar_color, Percent_NonNA = bar_color)) +
    coord_flip() +
    labs(title = plot_title, subtitle = plot_subtitle, x = NULL, y = "Percent", fill = NULL) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggplotly(p, tooltip = "text")
}

# ============================================================================
# DOWNLOAD HANDLER HELPER
# ============================================================================

#' Create a generic download handler
create_download_handler <- function(data_fn, prefix) {
  downloadHandler(
    filename = function() {
      paste0(prefix, "_", str_to_lower(input$download_year), "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- data_fn()
      if (input$download_year != "All") {
        data <- data |> filter(Year == as.integer(input$download_year))
      }
      write_csv(data, file)
    }
  )
}

# ============================================================================
# MAP HELPERS
# ============================================================================

#' Get USGS column name based on metal column
get_usgs_column <- function(metal_col, df) {
  symbol <- stringr::str_extract(metal_col, "(?<=\\(mg/kg )[A-Za-z]+(?=\\)$)")
  pattern <- paste0(symbol, ".*USGS")
  usgs_cols <- grep(pattern, names(df), value = TRUE)
  if (length(usgs_cols) > 0) usgs_cols[1] else NULL
}

#' Get water column name by metal and type
get_water_column_name <- function(metal, type) {
  metal <- tolower(trimws(metal))
  type <- tolower(type)
  
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
  
  switch(type,
         "water_dissolved" = paste0("Dissolved ", metal_name, " (mg/l ", metal_symbol, ")"),
         "water_suspended" = paste0("Suspended ", metal_name, " (mg/kg ", metal_symbol, ")"),
         "water_total"     = paste0("Total ", metal_name, " (mg/l ", metal_symbol, ")"),
         "water_1333"      = paste0(metal_name, " Class"),
         stop("Unrecognized type: ", type)
  )
}

#' Create base leaflet map
create_base_map <- function(zoom = 7, center_lng = -63.5, center_lat = -21.3) {
  leaflet() %>%
    addTiles() %>%
    addPolylines(data = pilco_line, color = "darkcyan", weight = 3, opacity = 0.8) %>%
    addPolygons(data = bol_border, color = "black", weight = 3, fill = FALSE) %>%
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
}

# ============================================================================
# TIME SERIES HELPERS
# ============================================================================

#' Get standard threshold values for a parameter
get_standard_thresholds <- function(param, data_type = c("water", "sediment")) {
  data_type <- match.arg(data_type)
  
  if (data_type == "water") {
    matches <- bolivian_1333$match_name == param & !is.na(bolivian_1333$match_name)
    if (sum(matches) == 0) return(NULL)
    
    thresholds <- c(
      class_a = bolivian_1333$`Class A`[matches][1],
      class_b = bolivian_1333$`Class B`[matches][1],
      class_c = bolivian_1333$`Class C`[matches][1],
      class_d = bolivian_1333$`Class D`[matches][1]
    )
    
    if (any(is.na(thresholds))) return(NULL)
    
    # Convert ug/l to mg/l if needed
    if (grepl("ug/", param)) {
      thresholds <- thresholds * 1000
    }
    
    return(thresholds)
    
  } else {  # sediment
    matches <- usgs_sqg$match_name == param & !is.na(usgs_sqg$match_name)
    if (sum(matches) == 0) return(NULL)
    
    thresholds <- c(
      tel = usgs_sqg$TEL[matches][1],
      pel = usgs_sqg$PEL[matches][1]
    )
    
    if (any(is.na(thresholds))) return(NULL)
    return(thresholds)
  }
}

# taking away from server.R to help with clarity, and so we're not declaring constants in one place and not the next

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

REVERSE_PARAMS <- c("Oxygen Saturation (%)","Dissolved Oxygen (mg/l O2)","pH","Resistivity (Ohm.cm)")

CLASS_MAP <- c("Class A"=0,"Class B"=1,"Class C"=2,"Class D"=3,"Unclassified"=4)
USGS_MAP  <- c("Below TEL"=0,"Above TEL"=1,"Above PEL"=2)

use_scope <- function(scope, all_df, bol_df) if (identical(scope,"bol")) bol_df else all_df

# Clip a data.frame with lon/lat columns to the Bolivia border sf polygon.
# - df: data.frame (or tibble)
# - lon_col / lat_col: column names (strings)
# - bol_border: sf polygon (already read with st_read)
clip_to_bolivia <- function(df, lon_col, lat_col, bol_border) {
  # empty/invalid input guard
  if (is.null(df) || !nrow(df)) return(df)
  
  # coerce coords to numeric if they came in as character
  if (!is.numeric(df[[lon_col]])) suppressWarnings(df[[lon_col]] <- as.numeric(df[[lon_col]]))
  if (!is.numeric(df[[lat_col]])) suppressWarnings(df[[lat_col]] <- as.numeric(df[[lat_col]]))
  
  # drop rows with missing coords (matches legacy behavior)
  df <- df[!is.na(df[[lon_col]]) & !is.na(df[[lat_col]]), , drop = FALSE]
  if (!nrow(df)) return(df)
  
  # ensure a valid border geometry and a CRS
  bb <- bol_border
  if (!inherits(bb, "sf")) stop("bol_border must be an sf object")
  if (is.na(sf::st_crs(bb))) sf::st_crs(bb) <- 4326
  
  # cast df to sf in same CRS
  sfobj <- sf::st_as_sf(df, coords = c(lon_col, lat_col), crs = sf::st_crs(bb), remove = FALSE)
  
  # robust spatial filter (intersects covers boundary/precision issues)
  sf::st_agr(sfobj) <- "constant"
  bb <- sf::st_make_valid(bb)
  clipped <- suppressWarnings(sf::st_filter(sfobj, bb, .predicate = sf::st_intersects))
  
  # return plain data.frame with geometry dropped (preserves original columns)
  sf::st_drop_geometry(clipped)
}

# ============================================================================
# FLEXIBLE DATA LOADING FUNCTIONS
# ============================================================================

#' Load water data with optional cleaning and translation
#' @param path File path to water data
#' @param is_clean Logical, whether data is already cleaned
#' @param translate_to Target language ('en' or 'es'), NULL for no translation
load_water_data <- function(path, is_clean = FALSE, translate_to = NULL) {
  
  file_path <- path
  
  # Detect format
  if (str_detect(file_path, ".csv")) {
    format <- "csv"
  } else if (str_detect(file_path, ".xlsx")) {
    format <- "xlsx"
  } else {
    stop("Unsupported file format. Use .csv or .xlsx")
  }
  
  # Read data
  if (format == "csv") {
    data_raw <- read_csv(file_path, show_col_types = FALSE)
  } else {
    data_raw <- read_xlsx(file_path, col_names = FALSE)
  }
  
  # Clean if needed
  if (!is_clean) {
    data <- clean_water_data(data_raw, source = "TNC")
  } else {
    data <- data_raw
  }
  
  # Translate if requested
  translate_to = input$translate_to
  
  if (!is.null(translate_to)) {
    target_lang <- translate_to
    source_lang <- ifelse(translate_to == "en", "es", "en")
    data <- translate_water_data(data, source_lang = source_lang, target_lang = target_lang)
  }
  
  return(data)
}

#' Clean raw water data from TNC format
#' @param data Raw data frame from TNC
#' @param source Data source ("TNC" currently supported)
clean_water_data <- function(data, source = "TNC") {
  
  if (source == "TNC") {
    raw <- data
    
    # Remove blank rows
    raw_clean <- raw |> filter(if_any(-c(1, 2), ~ !is.na(.)))
    
    # Transpose and convert to data frame
    df <- as.data.frame(t(raw_clean))
    
    # Remove blank rows
    df_clean <- df %>% filter(if_any(everything(), ~ !is.na(.)))
    
    # Combine parameter names with units
    new_names <- ifelse(!is.na(df_clean[2, ]), 
                        paste0(df_clean[1, ], " (", df_clean[2, ], ")"), 
                        df_clean[1, ])
    
    colnames(df_clean) <- new_names
    
    # Remove first 2 rows
    df_clean <- df_clean[-c(1, 2), ]
    
    # Replace "SIN DATOS" with NA
    df_clean <- df_clean %>%
      mutate(across(everything(), ~ na_if(., "SIN DATOS")))
    
    # Handle < and > symbols
    df_clean <- df_clean %>%
      mutate(across(where(~ any(grepl("^[<>]", .[!is.na(.)]))), 
                    ~ case_when(
                      grepl("^<", .) ~ 0.5 * as.numeric(gsub("^<", "", .)),
                      grepl("^>", .) ~ 1.5 * as.numeric(gsub("^>", "", .)),
                      TRUE ~ as.numeric(.)
                    )))
    
    return(df_clean)
  }
  
  stop(paste("Source", source, "not supported"))
}

#' Translate water data column names between English and Spanish
#' @param data Data frame with water quality data
#' @param source_lang Source language ('en' or 'es')
#' @param target_lang Target language ('en' or 'es')
translate_water_data <- function(data, source_lang, target_lang) {
  
  # Validate inputs
  if (!source_lang %in% c("en", "es") || !target_lang %in% c("en", "es")) {
    stop("Languages must be 'en' (English) or 'es' (Spanish)")
  }
  
  if (source_lang == target_lang) {
    warning("Source and target languages are the same. Returning data unchanged.")
    return(data)
  }
  
  # Get current column names
  current_cols <- colnames(data)
  
  # Create translation based on direction
  if (source_lang == "es" && target_lang == "en") {
    translation_map <- param_mapping
  } else {
    translation_map <- setNames(names(param_mapping), unname(unlist(param_mapping)))
  }
  
  # Translate column names
  new_cols <- sapply(current_cols, function(col) {
    if (col %in% names(translation_map)) {
      return(translation_map[[col]])
    } else {
      # Keep original if not in mapping
      return(col)
    }
  }, USE.NAMES = FALSE)
  
  colnames(data) <- new_cols
  
  return(data)
}

#' Enhanced yearly data loader with flexible cleaning and translation
#' @param path Directory containing the files
#' @param pattern Regex pattern to match files
#' @param date_format Format string for date parsing
#' @param station_renames Named vector of station name replacements
#' @param is_clean Whether files are pre-cleaned
#' @param translate_to Target language for translation
load_yearly_data_flexible <- function(path, pattern, date_format = "%d/%m/%Y", 
                                      station_renames = NULL, is_clean = TRUE,
                                      translate_to = NULL) {
  files <- list.files(path, pattern = pattern, full.names = TRUE)
  
  dfs <- lapply(files, function(f) {
    year <- stringr::str_extract(basename(f), "\\d{4}")
    
    # Use flexible loader
    df <- load_water_data(f, is_clean = is_clean, translate_to = translate_to)
    
    df$Year <- as.integer(year)
    
    # Handle date parsing
    if ("Date" %in% names(df)) {
      df$Date <- as.Date(df$Date, date_format)
    } else if ("Fecha" %in% names(df)) {
      df$Date <- as.Date(df$Fecha, date_format)
      df$Fecha <- NULL
    }
    
    df
  })
  
  result <- bind_rows(dfs)
  
  # Apply station name replacements if provided
  if (!is.null(station_renames)) {
    for (old_name in names(station_renames)) {
      result$Station <- str_replace(result$Station, old_name, station_renames[[old_name]])
    }
  }
  
  result
}