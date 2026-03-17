# helpers_server.R - many helper functions
library(sf)

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

# Unit conversion helper function (defined once at top level)
convert_units <- function(value, from_unit, to_unit) {
  from <- tolower(gsub("\\s+", "", from_unit))
  to <- tolower(gsub("\\s+", "", to_unit))
  if (from == to) return(value)
  
  conversions <- list(
    "kg" = 1000, "g" = 1, "mg" = 0.001, "ug" = 0.000001, "µg" = 0.000001,
    "mg/kg" = 1, "ug/kg" = 0.001, "µg/kg" = 0.001,
    "mg/l" = 1, "ug/l" = 0.001, "µg/l" = 0.001,
    "ppm" = 1, "ppb" = 0.001
  )
  
  from_factor <- conversions[[from]]
  to_factor <- conversions[[to]]
  
  if (is.null(from_factor) || is.null(to_factor)) {
    warning(paste("Cannot convert from", from_unit, "to", to_unit, "- using original values"))
    return(value)
  }
  
  converted <- value * (from_factor / to_factor)
  return(converted)
}

# Remove zeros to reformat a number
trim_zeros <- function(x) {
  s <- format(x, scientific = FALSE, trim = TRUE)  # e.g. "0.010000"
  sub("\\.?0+$", "", s)                            # -> "0.01"
}

# filter to Bolivia
filter_to_border <- function(df, lon_col, lat_col, border_sf) {

  # 1. Check column existence
  if (!lon_col %in% names(df)) stop(paste("Longitude column not found:", lon_col))
  if (!lat_col %in% names(df)) stop(paste("Latitude column not found:", lat_col))
  
  # 2. Force numeric & report coercion failures
  df <- df |>
    mutate(
      across(all_of(c(lon_col, lat_col)), ~ suppressWarnings(as.numeric(.x)))
    )
  
  rows_initial = nrow(df)
  
  df = df |> filter(!is.na(.data[[lon_col]]), !is.na(.data[[lat_col]])) # skip the ones that we can't handle
  cat(paste0("Skipping rows without lat/lon data formatted properly: ", rows_initial - nrow(df)))
  
  # Convert to sf
  sf_df <- st_as_sf(
    df,
    coords = c(lon_col, lat_col),
    crs = st_crs(border_sf),
    remove = FALSE
  )
  
  # Spatial filter
  filtered <- st_filter(sf_df, border_sf)
  
  coords <- st_coordinates(filtered)
  
  filtered |>
    mutate(
      !!lon_col := coords[,1],
      !!lat_col := coords[,2]
    ) |>
    st_drop_geometry()
}

# read an uploaded file
read_uploaded_file = function(path) {
  
  # get the file type
  ftype = get_file_type(path)
  
  # read the data
  switch(ftype,
         "csv" = readr::read_csv(path),
         "tsv" = readr::read_tsv(path),
         "xls" = readxl::read_xls(path),
         "xlsx" = readxl::read_xlsx(path),
         abort("Unsupported file type: ", ftype)
  )
}

# determine the file type of an uploaded file
get_file_type <- function(path) {
  ext <- tolower(tools::file_ext(path)[1])
  print(paste("get_file_type: ", ext))
  ext
}



#### Manage File Uploads ####
# take data in either of 2 formats, format & score, merge
# Output: locyear and scored files in master_data
dataUploadServer <- function(id, base_data, master_data) {
  moduleServer(id, function(input, output, session) {
    parsed_upload <- reactiveVal(NULL)
    
    observeEvent(input$upload_data, {
      
      # Validation
      if (is.null(input$files) || nrow(input$files) == 0) {
        showNotification("Please select a file before processing.", type="error")
        return()  # ✅ This return() exits the observeEvent, not the module
      }
      
      src_format <- input$source_format
      src_lang <- input$current_lang
      src_media <- input$media_type
      src_target_lang <- input$translate_to
      
      print(paste("Format:", src_format,
                  "Lang:", src_lang,
                  "Media:", src_media,
                  "Target:", src_target_lang))
      
      req(input$files)
      fpath <- input$files$datapath[1]
      fname <- input$files$name[1]
      
      showNotification(paste("Processing:", fname), type="message")
      
      withProgress(message = "Processing uploads...", value = 0, {
        n_files <- nrow(input$files)
        existing_scored <- if (src_media == "water") {
          isolate(master_data$water_scored)
        } else {
          isolate(master_data$sed_scored)
        }
        scored_merged <- existing_scored
        
        for (i in seq_len(n_files)) {
          fname_i <- input$files$name[i]
          fpath_i <- input$files$datapath[i]
          
          incProgress(1/n_files, message = paste("Processing", fname_i))
          
          tryCatch({
            file_data_i <- read_uploaded_file(fpath_i)
            print(paste0("[dataUploadServer]: ", fname_i))
            
            df_i <- upload_sampled_data(
              file_data_i,
              media = src_media,
              format = src_format,
              debug_prepped = FALSE,
              src_lang = src_lang,
              target_lang = src_target_lang
            )
            print("completed upload_sampled_data")
            
            # Check for duplicate names
            dup_names <- names(df_i)[duplicated(names(df_i))]
            if (length(dup_names)) {
              warning(sprintf("File '%s' has duplicate column names: %s", 
                              fname_i, paste(unique(dup_names), collapse=", ")))
              print(names(df_i))
            }
            
            # Clean column names
            if (any(duplicated(names(df_i)))) {
              names(df_i) <- janitor::make_clean_names(names(df_i), unique = TRUE)
            }
            
            df_i$data_source <- fname_i
            
            upload_scored_i <- score_data(df_i)
            print("[dataUploadServer]: finished score_data")
            
            scored_merged <- merge_scored(scored_merged, upload_scored_i)
            print("[dataUploadServer]: finished merge_scored")
            
          }, error = function(e) {
            showNotification(
              paste("Error processing", fname_i, ":", e$message),
              type = "error"
            )
          })
        }
        
        # ✅ Update the reactiveVal
        print("updating parsed_upload")
        parsed_upload(list(
          scored = scored_merged,
          locyear = score_to_loc_year(scored_merged),
          media = src_media
        ))
        
        # Optional persistence
        save_path <- if (src_media == "water") {
          "data/processed/water_scored_user_update.rds"
        } else {
          "data/processed/sed_scored_user_updated.rds"
        }
        saveRDS(scored_merged, save_path)
        
        showNotification("Upload processing complete!", type = "message")
      })
      
    }, ignoreInit = TRUE)
    
    # ✅ Debug REACTIVELY (optional)
    observe({
      result <- parsed_upload()
      if (!is.null(result)) {
        cat("parsed_upload contains:\n")
        cat("  - media:", result$media, "\n")
        cat("  - scored rows:", nrow(result$scored), "\n")
        cat("  - locyear rows:", nrow(result$locyear), "\n")
      }
    })
    
    # ✅ Return the reactiveVal (NOT inside observeEvent)
    return(list(parsed = parsed_upload))
    
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
river_network <- st_read("data/shp/River_Network.shp", quiet = TRUE)
pilco_basin <- st_read("data/shp/Pilcomayo_Basin.shp", quiet = TRUE)

# Load census data
census_potosi <- st_read("data/census/shp/potosi_census_summary_shape.shp", quiet = TRUE)
names(census_potosi) <- c("province", "iprov", "pop", "prop_ch_u6", "prop_elder65", "prop_age_vuln", "prop_no_health", "prop_pub_health", "prop_trad_care", "prop_inf_only", "prop_farm", "prop_mine", "prop_manu", "prop_cons", "prop_indig", "prop_agro_part", "prop_agro_sale", "prop_agro_cons", "prop_disab", "prop_child_loss", "hh_count", "prop_river_w", "prop_unprot_w", "prop_no_pipe", "prop_solid_ws", "prop_liq_ws", "prop_struct_vuln", "deaths_tot", "deaths_avg_age", "deaths_under50", "prop_under50", "deaths_u5", "prop_u5", "deaths_u15", "prop_u15", "geometry")

#### To quiet down plotly warnings ####
quiet_ggplotly <- function(p, tooltip = "text") {
  plotly::ggplotly(p, tooltip = tooltip) %>%
    plotly::config(displayModeBar = FALSE)
}



quiet_plotly <- function(p, ...) {
  # save current options
  old_opts <- options(
    shiny.trace = getOption("shiny.trace"),
    warn        = getOption("warn"),
    ts_debug    = getOption("ts_debug")
  )
  
  # turn noisy options off just for this call
  options(shiny.trace = FALSE,
          warn        = 0,      # or whatever you normally use
          ts_debug    = FALSE)
  
  on.exit(options(old_opts), add = TRUE)  # restore on exit
  
  withCallingHandlers(
    {
      suppressWarnings(
        ggplotly(p, ...)
      )
    },
    warning = function(w) {
      msg <- conditionMessage(w)
      
      if (grepl("plotly", msg, ignore.case = TRUE) ||
          grepl("JSON", msg) ||
          grepl("incompatible with", msg) ||
          grepl("Couldn't transform", msg) ||
          grepl("data for this geom", msg)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#### Get standards dynamically and based on regulator ####
# ============================================================================
# Time Series: Convert ts_get_standards() output into ggplot layers
# ============================================================================
# =============================================================================
# Standard → ggplot layers (rectangles, hlines, labels)
# =============================================================================
ts_standard_layers <- function(df, standards) {
  if (getOption("ts_debug")) {
    cat("\n=== ts_standard_layers() START ===\n")
    cat("Standard types received: ", paste(names(standards), collapse=", "), "\n")
  }
  
  if (is.null(standards) || length(standards) == 0) return(list())
  
  layers <- list()
  
  xmin <- min(df$date, na.rm = TRUE)
  xmax <- max(df$date, na.rm = TRUE)
  ymin <- min(df$value, na.rm = TRUE)
  ymax <- max(df$value, na.rm = TRUE)
  
  unit <- df$unit[1]
  label_y_offset <- (ymax - ymin) * 0.02
  
  # ---------------------------------------------------------------------------
  # Loop through each standard type: HQ, CR, WL, bolivian_1333, usgs, etc.
  # ---------------------------------------------------------------------------
  for (sname in names(standards)) {
    
    std <- standards[[sname]]
    
    # Each standard entry should declare "type"
    # strict standards have no type → assign one
    if (is.null(std$type)) std$type <- "strict"
    
    type <- std$type
    cat("\n===type=", type)
    # ================================================================
    # STRICT STANDARDS (HQ / CR / WL from strict_standards.csv)
    # ================================================================
    if (type == "strict") {
      
      # expect tibble: parameter | media | hqcr | standard | value
      if (!all(c("value","standard") %in% colnames(std))) next
      
      vals <- std$value
      labs <- std$standard
      
      # auto-colors (consistent by HQ/CR/WL)
      palette <- c(
        HQ = "#3366cc",
        CR = "#dc3912",
        WL = "#ff9900"
      )
      color <- palette[sname] %||% "purple"
      
      # hlines
      for (i in seq_along(vals)) {
        layers <- append(layers, list(
          geom_hline(
            yintercept = vals[i],
            color = color,
            linetype = "solid",
            linewidth = 0.7
          )
        ))
        
        layers <- append(layers, list(
          annotate(
            "text",
            x = xmin + 0.01 * (xmax - xmin),
            y = vals[i] + label_y_offset,
            label = paste0(sname, ": ", labs[i], " (", vals[i], " ", unit, ")"),
            color = color,
            hjust = 0,
            size = 3
          )
        ))
      }
    }
    
    # ================================================================
    # BOLIVIAN 1333 CLASS STANDARD
    # ================================================================
    if (type == "bolivian_1333") {
      
      vals <- std$values  # numeric thresholds A,B,C,D
      labs <- std$labels
      cols <- std$colors
      
      A <- vals[1]; B <- vals[2]; C <- vals[3]; D <- vals[4]
      
      increasing <- D < C  # correct logic for your dataset
      
      if (increasing) {
        rect_df <- tibble(
          ymin  = c(ymin, D, C, B),
          ymax  = c(D,    C, B, A),
          label = paste0(labs, ": ", round(vals, 3), " ", unit),
          fill  = cols
        )
      } else {
        rect_df <- tibble(
          ymin  = c(ymin, A, B, C),
          ymax  = c(A,    B, C, D),
          label = paste0(labs, ": ", round(vals, 3), " ", unit),
          fill  = cols
        )
      }
      
      layers <- append(layers, list(
        geom_rect(
          data = rect_df,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label),
          alpha = 0.06,
          inherit.aes = FALSE
        )
      ))
      
      # lines + labels
      for (i in seq_along(vals)) {
        layers <- append(layers, list(
          geom_hline(
            yintercept = vals[i],
            color = cols[i],
            linetype = "dashed",
            linewidth = 0.6
          )
        ))
        
        layers <- append(layers, list(
          annotate(
            "text",
            x = xmin + 0.01 * (xmax - xmin),
            y = vals[i] + label_y_offset,
            label = labs[i],
            color = cols[i],
            hjust = 0,
            vjust = 0,
            size = 3,
            fontface = "bold"
          )
        ))
      }
    }
    
    # ================================================================
    # USGS TEL / PEL
    # ================================================================
    if (type == "usgs") {
      
      vals <- std$values  # TEL, PEL
      labs <- std$labels
      cols <- std$colors %||% c("darkorange", "red")
      
      for (i in seq_along(vals)) {
        layers <- append(layers, list(
          geom_hline(
            yintercept = vals[i],
            color = cols[i],
            linetype = "dotted",
            linewidth = 0.8
          )
        ))
        
        layers <- append(layers, list(
          annotate(
            "text",
            x = xmin + 0.01 * (xmax - xmin),
            y = vals[i] + label_y_offset,
            label = paste0(labs[i], " (", vals[i], " ", unit, ")"),
            color = cols[i],
            hjust = 0,
            size = 3
          )
        ))
      }
    }
  }
  
  if (getOption("ts_debug")) {
    cat("Layers created: ", length(layers), "\n")
    cat("Layer classes: ", paste(sapply(layers, class), collapse=", "), "\n")
    cat("=== END layers ===\n")
  }
  
  return(layers)
}


# =====================================================================
# Unified TS Standards (returns ALL relevant standards by default)
# =====================================================================
ts_get_standards <- function(param_name, media, mode = "all") {
  
  stds_matching <- stds %>%
    filter(
      tolower(parameter) == tolower(param_name),
      tolower(media) == tolower(media)
    )
  
  if (getOption("ts_debug")) {
    cat("\n=== ts_get_standards() ===\n")
    cat("Param: ", param_name, "\n")
    cat("Media: ", media, "\n")
    cat("Mode:  ", mode, "\n")
    cat("Rows matched in stds: ", nrow(stds_matching), "\n")
  }
  
  if (nrow(stds_matching) == 0) {
    return(list())
  }
  
  # Return NONE
  if (mode == "none") return(list())
  
  # Return ALL regulators for this param/media
  if (mode == "all") {
    regulators <- unique(stds_matching$regulator)
    out <- split(stds_matching, stds_matching$regulator)
    
    if (getOption("ts_debug")) {
      cat("Returned regulator types: ", paste(regulators, collapse=", "), "\n")
    }
    
    return(out)
  }
  
  # Otherwise return only rows matching the regulator
  out <- stds_matching %>%
    filter(tolower(regulator) == tolower(mode))
  
  if (nrow(out) == 0) return(list())
  
  return(split(out, out$regulator))
}

# ============================================================================
# RANKING PLOT HELPERS
# ============================================================================



# ============================================================================
# TIME SERIES HELPERS
# ============================================================================

# callout no data
no_data_callout <- function(media_label = "sample") {
  HTML(sprintf("
    <div style='
        padding: 20px;
        background-color: #f8f9fa;
        border-left: 5px solid #dc3545;
        border-radius: 4px;
        font-size: 16px;
        width: 80%%;
        margin: 20px auto;
    '>
      <strong>No %s data found.</strong><br>
      No measurements are available for the selected station, parameter, and filters.
    </div>
  ", media_label))
}

#### Clip a data.frame with lon/lat columns to the Bolivia border sf polygon ####
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

#### Unit conversion helpers ####

# ---- helper maps ----
.prefix_to_g <- list(
  pg = 1e-12,
  ng = 1e-9,
  ug = 1e-6,  # also for "µg"
  mg = 1e-3,
  g  = 1,
  kg = 1e3
)
#### Safely merge file types ####
merge_media_safely <- function(water_df, sediment_df) {
  
  message("\n========== merge_media_safely() ==========")
  message("[1] Starting safe merge of water + sediment data.")
  
  # ---- 1. Capture names ----
  w_names <- names(water_df)
  s_names <- names(sediment_df)
  
  message("\n[2] Column sets before merging:")
  message("  • Water columns (", length(w_names), "): ", paste(w_names, collapse = ", "))
  message("  • Sediment columns (", length(s_names), "): ", paste(s_names, collapse = ", "))
  
  # ---- 2. Compute missing column sets ----
  missing_in_water <- setdiff(s_names, w_names)
  missing_in_sed   <- setdiff(w_names, s_names)
  
  message("\n[3] Columns *only* in sediment: ", 
          ifelse(length(missing_in_water) == 0, "none", paste(missing_in_water, collapse=", ")))
  
  message("[4] Columns *only* in water: ", 
          ifelse(length(missing_in_sed) == 0, "none", paste(missing_in_sed, collapse=", ")))
  
  # ---- 3. Add missing sediment columns to water ----
  if (length(missing_in_water) > 0) {
    message("\n[5] Adding ", length(missing_in_water), " missing columns to WATER:")
    for (col in missing_in_water) {
      message("    → Adding ", col, " (filled with NA)")
      water_df[[col]] <- NA
    }
  } else {
    message("\n[5] No missing columns to add to water.")
  }
  
  # ---- 4. Add missing water columns to sediment ----
  if (length(missing_in_sed) > 0) {
    message("\n[6] Adding ", length(missing_in_sed), " missing columns to SEDIMENT:")
    for (col in missing_in_sed) {
      message("    → Adding ", col, " (filled with NA)")
      sediment_df[[col]] <- NA
    }
  } else {
    message("\n[6] No missing columns to add to sediment.")
  }
  
  # ---- 5. Reorder columns consistently ----
  # use the WATER column order as canonical
  final_col_order <- names(water_df)
  
  message("\n[7] Applying consistent column order (", length(final_col_order), " columns).")
  
  water_aligned <- water_df[, final_col_order]
  sediment_aligned <- sediment_df[, final_col_order]
  
  # ---- 6. Final merge ----
  message("\n[8] Binding rows…")
  merged <- dplyr::bind_rows(water_aligned, sediment_aligned)
  
  message("[9] Merge complete. Final dimensions: ", 
          nrow(merged), " rows × ", ncol(merged), " columns.")
  
  message("============================================\n")
  
  return(merged)
}

#### get_param_list(): extract valid parameter names for a media type ####
get_param_list <- function(df, media_type = "all", need_std = FALSE) {
  
  # Require the expected columns
  required_cols <- c("media", "parameter")
  if (!all(required_cols %in% names(df))) {
    message("get_param_list(): dataset missing required columns: ",
         paste(setdiff(required_cols, names(df)), collapse = ", "))
  }
  
  # Columns we never want treated as parameters
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
  
  # only filter by media if we don't want to see them all
  if(media_type != "all") df = df |> filter(media == media_type)
  # filter the entire list by those in the stds list, so we can only choose those that can calculate a HQ
  if(need_std == TRUE) df = df |> filter(parameter %in% stds$parameter)
  
  df %>%
    filter(!parameter %in% exclude_cols) %>%
    mutate(concentration = suppressWarnings(as.numeric(concentration))) %>%
    filter(!is.na(concentration)) %>%
    pull(parameter) %>%
    unique() %>%
    sort()
}

#### Figure development helpers ####
### build a standard legend color
legend_color_bar <- function(palette, title, min_val, max_val, bins = NULL) {
  if (!is.null(bins)) {
    # discrete bins — horizontal gradient bar with min/max labels
    colors  <- palette(bins)
    n       <- length(colors)
    # build a stepped gradient using hard stops
    stops <- paste0(
      mapply(function(col, i) {
        pct_start <- round((i - 1) / n * 100)
        pct_end   <- round(i / n * 100)
        paste0(col, " ", pct_start, "%, ", col, " ", pct_end, "%")
      }, colors, seq_along(colors)),
      collapse = ", "
    )
    paste0(
      '<div style="margin-bottom:8px;">',
      '<div style="font-weight:bold;font-size:12px;margin-bottom:4px;">', title, '</div>',
      '<div style="display:flex;align-items:center;gap:6px;">',
      '<span style="font-size:10px;">', bins[1], '</span>',
      '<div style="width:80px;height:12px;background:linear-gradient(to right,',
      stops,
      ');border-radius:2px;"></div>',
      '<span style="font-size:10px;">', bins[n], '</span>',
      '</div></div>'
    )
  } else {
    # continuous gradient
    paste0(
      '<div style="margin-bottom:8px;">',
      '<div style="font-weight:bold;font-size:12px;margin-bottom:4px;">', title, '</div>',
      '<div style="display:flex;align-items:center;gap:6px;">',
      '<span style="font-size:10px;">', round(min_val, 1), '</span>',
      '<div style="width:80px;height:12px;background:linear-gradient(to right,',
      paste(palette(seq(min_val, max_val, length.out = 6)), collapse = ","),
      ');border-radius:2px;"></div>',
      '<span style="font-size:10px;">', round(max_val, 1), '</span>',
      '</div></div>'
    )
  }
}

### create a legend for categorical values
legend_categorical <- function(title, colors, labels) {
  swatches <- paste0(
    mapply(function(col, lab) {
      paste0('<div style="display:flex;align-items:center;gap:6px;margin-bottom:3px;">',
             '<div style="width:14px;height:14px;background:', col, 
             ';border-radius:2px;flex-shrink:0;"></div>',
             '<span style="font-size:11px;">', lab, '</span></div>')
    }, colors, labels),
    collapse = ""
  )
  paste0('<div style="margin-bottom:8px;">',
         '<div style="font-weight:bold;font-size:12px;margin-bottom:4px;">', title, '</div>',
         swatches, '</div>')
}

### add styling to legends
legend_wrapper <- function(html) {
  paste0(
    '<div id="map-legend" style="',
    'background:white;padding:10px 12px;border-radius:6px;',
    'box-shadow:0 1px 5px rgba(0,0,0,0.3);',
    'max-height:400px;overflow-y:auto;',
    'min-width:160px;max-width:200px;',
    'margin-bottom:30px;">',
    '<div style="font-weight:bold;font-size:13px;border-bottom:1px solid #ddd;',
    'margin-bottom:8px;padding-bottom:4px;">Legend</div>',
    html,
    '</div>'
  )
}

#### standardize_raster(): reproject and resample a raster to match a target template ####
standardize_raster <- function(r, template, fill_nas = FALSE) {
  if (!terra::same.crs(r, template)) {
    r <- terra::project(r, terra::crs(template), method = "near")
  }
  if (!isTRUE(all.equal(terra::res(r), terra::res(template))) ||
      !isTRUE(all.equal(as.vector(terra::ext(r)), as.vector(terra::ext(template))))) {
    r <- terra::resample(r, template, method = "near")
  }
  if (fill_nas) {
    repeat {
      na_before <- sum(is.na(terra::values(r)))
      if (na_before == 0) break
      r <- terra::focal(r, w = 3, fun = "modal", na.policy = "only", na.rm = TRUE)
      na_after <- sum(is.na(terra::values(r)))
      if (na_after == na_before) break
    }
  }
  r
}

#### delineate_subcatchments(): delineate where water will flow to ####
delineate_subcatchments <- function(station_df, flow_dir_path, flow_acc_path,
                                     snap_dist = 10000) {
  library(whitebox)
  library(terra)
  library(sf)
  
  message("Loading flow rasters...")
  flow_acc <- terra::rast(flow_acc_path)
  
  pour_points <- station_df %>%
    dplyr::select(where(~!is.list(.))) %>%
    dplyr::filter(!is.na(longitude_decimal) & !is.na(latitude_decimal)) %>%
    dplyr::select(station, HQ, longitude_decimal, latitude_decimal) %>%
    sf::st_as_sf(coords = c("longitude_decimal", "latitude_decimal"), crs = 4326) %>%
    sf::st_filter(sf::st_transform(pilco_basin, 4326)) %>%
    dplyr::mutate(FID = seq_len(nrow(.)))  # assign sequential FIDs AFTER basin filter
  
  tmp_points  <- "data/dem/tmp_pour_points.shp"
  tmp_snapped <- "data/dem/tmp_snapped.shp"
  tmp_wshed   <- "data/dem/tmp_watershed.tif"
  
  # Clean up any existing temp files
  for (f in c(tmp_points, tmp_snapped)) {
    existing <- list.files(dirname(f), pattern = paste0(tools::file_path_sans_ext(basename(f)), "\\."), full.names = TRUE)
    if (length(existing) > 0) file.remove(existing)
  }
  
  sf::st_write(pour_points, tmp_points, quiet = TRUE, append = FALSE)
  
  # Snap pour points to flow accumulation
  message("Snapping pour points...")
  snapped_pts <- snap_to_accumulation_threshold(pour_points, flow_acc, threshold = 1111)
  
  message("Snapped points CRS: ", sf::st_crs(snapped_pts)$epsg)
  message("Flow direction CRS: ", terra::crs(terra::rast(flow_dir_path), describe=TRUE)$code)
  
  message("Snapped coordinates:")
  print(sf::st_coordinates(snapped_pts))
  message("Duplicate points: ", sum(duplicated(sf::st_coordinates(snapped_pts))))
  
  message("Projecting snapped_pts to match flow direction raster CRS...")
  # Reproject snapped points to match flow direction raster CRS
  fdr <- terra::rast(flow_dir_path)
  snapped_pts_proj <- sf::st_transform(snapped_pts, sf::st_crs(fdr))
  sf::st_write(snapped_pts_proj, tmp_snapped, quiet = TRUE, append = FALSE)
  
  test_read <- sf::st_read(tmp_snapped, quiet = TRUE)
  message("Snapped file columns: ", paste(names(test_read), collapse = ", "))
  message("Snapped file FID values: ", paste(test_read$FID, collapse = ", "))
  message("Snapped file CRS: ", sf::st_crs(test_read)$epsg)
  sf::st_write(test_read %>% dplyr::select(FID), 
               "data/dem/debug_pour_points.shp", delete_dsn = TRUE)
  
  # Debug: check a sample of flow direction values
  fdr_check <- terra::rast("data/dem/flow_direction_wbt2.tif")
  message("Flow direction unique values: ", paste(sort(unique(terra::values(fdr_check))), collapse = ", "))
  message("Flow direction value range: ", terra::minmax(fdr_check)[1], " to ", terra::minmax(fdr_check)[2])
  
  # Check snapped points are on high accumulation cells
  snapped_coords <- sf::st_coordinates(snapped_pts)
  acc_at_snapped <- terra::extract(flow_acc, sf::st_transform(snapped_pts, sf::st_crs(flow_acc)))
  message("Flow accumulation at snapped points: ", paste(round(acc_at_snapped[,2]), collapse = ", "))
  
  # Delineate full cumulative watersheds
  message("Delineating watersheds...")
  wbt_watershed(
    d8_pntr  = flow_dir_path,
    pour_pts = tmp_snapped,
    output   = tmp_wshed
  )
  
  # Convert raster to polygons — each watershed has a unique integer ID
  message("Converting to polygons...")
  wshed_raster <- terra::rast(tmp_wshed)
  
  wshed_poly <- terra::as.polygons(wshed_raster) %>%
    sf::st_as_sf() %>%
    sf::st_make_valid()
  
  message("Watershed polygon count: ", nrow(wshed_poly))
  message("Watershed raster unique values: ", paste(sort(unique(na.omit(terra::values(wshed_raster)))), collapse = ", "))
  message("Watershed raster extent: ", paste(as.vector(terra::ext(wshed_raster)), collapse = ", "))
  message("Pour points extent: ", paste(sf::st_bbox(snapped_pts), collapse = ", "))
  
  # Join station info by matching FID to watershed value
  snapped_pts <- sf::st_read(tmp_snapped, quiet = TRUE) %>%
    sf::st_drop_geometry()
  
  colnames(wshed_poly)[1] <- "FID"
  
  wshed_poly <- wshed_poly %>%
    dplyr::left_join(
      snapped_pts %>% dplyr::select(FID, station, HQ),
      by = "FID"
    )
  
  # ── Incremental subcatchments ─────────────────────────────────────────────
  # For each station, subtract all upstream station watersheds
  message("Computing incremental subcatchments...")
  
  # Get snapped point coordinates for upstream/downstream determination
  snapped_sf <- sf::st_read(tmp_snapped, quiet = TRUE)
  
  # For each watershed, find which other station points fall within it
  # Points inside a watershed are upstream of that watershed's outlet
  incremental <- wshed_poly
  
  for (i in seq_len(nrow(wshed_poly))) {
    current_watershed <- wshed_poly[i, ]
    current_fid       <- wshed_poly$FID[i]
    
    # Find snapped points that fall within this watershed
    # excluding the outlet point itself
    pts_in_watershed <- sf::st_filter(
      snapped_sf,
      current_watershed
    )
    
    upstream_fids <- pts_in_watershed$FID[pts_in_watershed$FID != current_fid]
    
    if (length(upstream_fids) > 0) {
      # Get upstream watersheds
      upstream_watersheds <- wshed_poly %>%
        dplyr::filter(FID %in% upstream_fids) %>%
        sf::st_union()
      
      # Subtract upstream area from current watershed
      incremental[i, ] <- tryCatch({
        diff <- sf::st_difference(current_watershed, upstream_watersheds)
        diff$FID     <- current_fid
        diff$station <- current_watershed$station
        diff$HQ      <- current_watershed$HQ
        diff
      }, error = function(e) {
        message("Warning: could not subtract upstream for station ", 
                current_watershed$station, " — using full watershed")
        current_watershed
      })
    }
  }
  
  incremental <- sf::st_make_valid(incremental)
  incremental <- sf::st_transform(incremental, 4326)
  
  message("Done — ", nrow(incremental), " subcatchments delineated")
  return(incremental)
}

#### snap_to_accumulation_threshold(): helper for flow accumulation ####
snap_to_accumulation_threshold <- function(pour_points, flow_acc, threshold = 1111) {
  
  # Reproject pour points to match flow accumulation CRS
  pour_points_proj <- sf::st_transform(pour_points, sf::st_crs(flow_acc))
  
  # Get cell indices above threshold
  high_acc_idx    <- terra::cells(flow_acc > threshold, 1)[[1]]
  high_acc_coords <- terra::xyFromCell(flow_acc, high_acc_idx)
  high_acc_vals   <- terra::extract(flow_acc, high_acc_idx)[[1]]
  
  high_acc_sf <- sf::st_as_sf(
    as.data.frame(high_acc_coords),
    coords = c("x", "y"),
    crs    = sf::st_crs(flow_acc)
  ) %>% dplyr::mutate(acc = high_acc_vals, cell_id = high_acc_idx)
  
  # Snap each pour point to nearest high accumulation cell
  nearest_idx <- sf::st_nearest_feature(pour_points_proj, high_acc_sf)
  snapped     <- high_acc_sf[nearest_idx, ]
  
  snapped <- snapped %>%
    dplyr::mutate(
      FID     = pour_points$FID,
      station = pour_points$station,
      HQ      = pour_points$HQ
    )
  
  return(snapped)
}
