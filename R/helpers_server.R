# helpers.R - Place this in your app directory and source it before ui.R/server.R
# Small helper to reconcile legacy coord names the app expects
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
  message("\nSKIPPING IN DEV TO REDUCE LONG DELAYS, REMOVE THIS BREAK WHEN POSTING TO PRODUCTION")
  return(df)
  # Nadav's Note: Sloppy but may help with quicker loading time
  
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
  
  # # 3. Check for NAs AFTER coercion
  # if (anyNA(df[[lon_col]])) {
  #   bad <- df |> filter(is.na(.data[[lon_col]]))
  #   print("Rows with NA LONG after coercion:")
  #   print(bad)
  #   View(bad)
  #   stop("Longitude contains NA values.")
  # }
  # 
  # if (anyNA(df[[lat_col]])) {
  #   bad <- df |> filter(is.na(.data[[lat_col]]))
  #   print("Rows with NA LAT after coercion:")
  #   print(bad)
  #   View(bad)
  #   stop("Latitude contains NA values.")
  # }
  
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



#### Merge upload with existing locyear data (unused) ####
merge_keep_uploaded_param <- function(existing,
                                      uploaded, 
                                      key_cols = c("station","year"),
                                      param_key_cols = c("parameter","media"),
                                      nested_param_col = "by_parameter",
                                      nested_detail_col = "detail_rows",
                                      scalar_from_params = c(hazard_index = "hazard_index",
                                                             total_CR_cases_10k = "CR_cases_10k",
                                                             wl_index = "wl_index"),
                                      detail_id_col = NULL # optional: column in detail_rows to dedupe by
) {
  # prepare schema
  existing <- ensure_listcol_tbl(existing, nested_param_col)
  existing <- ensure_listcol_tbl(existing, nested_detail_col)
  uploaded <- ensure_listcol_tbl(uploaded, nested_param_col)
  uploaded <- ensure_listcol_tbl(uploaded, nested_detail_col)
  
  # add source flag (preserve order if you want)
  existing2 <- existing %>% mutate(.source = "existing")
  uploaded2 <- uploaded %>% mutate(.source = "uploaded")
  
  # bind so keys + source align order
  all_rows <- bind_rows(existing2, uploaded2)
  
  # helper: compute params merged for one group
  merge_params_for_group <- function(param_list_existing, param_list_uploaded) {
    # param_list_existing/uploaded are tibbles (may be zero-row)
    ex <- if (nrow(param_list_existing)) param_list_existing else tibble()
    up <- if (nrow(param_list_uploaded)) param_list_uploaded else tibble()

    # unify column sets
    cols_union <- union(names(ex), names(up))
    ex <- ex %>% select(any_of(cols_union))
    up <- up %>% select(any_of(cols_union))
    
    if (nrow(up) == 0) return(list(merged = ex, replaced = tibble()))
    if (nrow(ex) == 0) return(list(merged = up, replaced = tibble()))

    # define matching key for parameter equality
    # create an id by pasting the param_key_cols (NA -> "")
    mk_id <- function(df) {
      df %>% mutate(.param_id = pmap_chr(across(all_of(param_key_cols)), ~paste0(..., collapse = "|")))
    }
    
    ex2 <- mk_id(ex)
    up2 <- mk_id(up)
    
    # which param ids appear in both
    common_ids <- intersect(ex2$.param_id, up2$.param_id)
    
    # keep uploaded rows for common ids; keep all unique rows from both
    keep_up <- up2
    keep_ex_only <- ex2 %>% filter(!(.param_id %in% common_ids))
    merged <- bind_rows(keep_ex_only %>% select(-.param_id), keep_up %>% select(-.param_id))
    
    # report replaced params (from existing -> uploaded)
    replaced <- ex2 %>% filter(.param_id %in% common_ids) %>%
      select(-.param_id) %>%
      mutate(replaced_by_uploaded = TRUE)
    
    list(merged = merged, replaced = replaced)
  }
  
  # group, merge
  keys_tbl <- all_rows %>% group_by(across(all_of(key_cols))) %>% group_keys()

  merged_rows <- all_rows %>%
    group_by(across(all_of(key_cols))) %>%
    group_map(~{
      rows <- .x
      keys <- .y
      print(paste0("[merge_keep_uploaded_param merged_rows] Keys: ", .y))
      
      # split by source to get nested param tables
      params_existing <- rows %>% filter(.source == "existing") %>% pull(!!sym(nested_param_col))
      params_uploaded <- rows %>% filter(.source == "uploaded") %>% pull(!!sym(nested_param_col))
      # those are lists of tibbles; if multiple original rows per source exist, bind them first
      params_existing <- if (length(params_existing) == 0) tibble() else bind_rows(params_existing)
      params_uploaded <- if (length(params_uploaded) == 0) tibble() else bind_rows(params_uploaded)

      merged_params_res <- merge_params_for_group(params_existing, params_uploaded)
      merged_params <- merged_params_res$merged
      
      # detail_rows: stack existing + uploaded, optionally dedupe by id
      details_existing <- rows %>% filter(.source == "existing") %>% pull(!!sym(nested_detail_col))
      details_uploaded <- rows %>% filter(.source == "uploaded") %>% pull(!!sym(nested_detail_col))
      details_all <- bind_rows( if (length(details_existing)) bind_rows(details_existing) else tibble(),
                                if (length(details_uploaded)) bind_rows(details_uploaded) else tibble() )
      if (!is.null(detail_id_col) && detail_id_col %in% names(details_all)) {
        details_all <- details_all %>% distinct( !!sym(detail_id_col), .keep_all = TRUE )
      }
      
      # recompute scalars by summing designated columns from merged_params
      compute_scalar <- function(colname, param_colname) {
        if (!param_colname %in% names(merged_params)) return(NA_real_)
        sum_val <- suppressWarnings(sum(as.numeric(merged_params[[param_colname]]), na.rm = TRUE))
        if (is.nan(sum_val)) NA_real_ else sum_val
      }
      scalars <- map_dbl(names(scalar_from_params), ~ compute_scalar(.x, scalar_from_params[[.x]]))
      names(scalars) <- names(scalar_from_params)

      # build output row: keys + scalars + nested list-cols
      out <- tibble(!!!keys)
      # scalars:
      for (nm in names(scalars)) out[[nm]] <- scalars[[nm]]
      if ("env_score" %in% names(rows)) out$env_score <- env_score_val
      # nested list columns
      out[[nested_param_col]] <- list(merged_params)
      out[[nested_detail_col]] <- list(details_all)
      
      # add small provenance counts
      out$N_existing_rows <- nrow(rows %>% filter(.source == "existing"))
      out$N_uploaded_rows <- nrow(rows %>% filter(.source == "uploaded"))
      out$N_params_existing <- nrow(params_existing)
      out$N_params_uploaded <- nrow(params_uploaded)
      out$N_params_merged <- nrow(merged_params)
      
      # also return replaced report as attr for later; but group_map can't return both easily,
      # so collect replaced info into a list column to be used later in a report
      out$replaced_params <- list(merged_params_res$replaced)
      
      out
    }) %>%
    list_rbind()
  
  # prepare a human-readable report: which parameters were replaced in each key
  report <- merged_rows %>%
    select(all_of(key_cols), N_existing_rows, N_uploaded_rows, N_params_existing, N_params_uploaded, N_params_merged, replaced_params) %>%
    mutate(replaced_count = map_int(replaced_params, ~ nrow(.x))) %>%
    select(-replaced_params)
  
  # drop internal .source if present in original bound df (we didn't save it on merged_rows)
  # return list
  list(merged = merged_rows %>% select(-starts_with(".source")), report = report)
}


merge_processed <- function(existing, uploaded,
                            key_cols = c("station","year"),
                            scalar_cols = c("hazard_index","total_CR_cases_10k","wl_index"),
                            nested_cols = c("by_parameter","detail_rows"),
                            scalar_strategy = c("prefer_uploaded","sum","mean","first")) {
  
  scalar_strategy <- match.arg(scalar_strategy)
  # ensure schema: add missing columns with sensible defaults
  existing <- ensure_schema(existing, group_cols = key_cols, scalar_cols = scalar_cols, nested_cols = nested_cols)
  uploaded <- ensure_schema(uploaded, group_cols = key_cols, scalar_cols = scalar_cols, nested_cols = nested_cols)
  
  existing <- existing %>% mutate(.source = "existing")
  uploaded <- uploaded %>% mutate(.source = "uploaded")
  
  all <- bind_rows(existing, uploaded)
  
  pick_scalar_for_group <- function(rows_df, col) {
    vals <- rows_df[[col]]
    src <- rows_df$.source
    
    if (scalar_strategy == "prefer_uploaded") {
      up <- rows_df %>% filter(.source == "uploaded") %>% pull(!!sym(col))
      if (length(up) && !all(is.na(up))) return(up[1])
      ex <- rows_df %>% filter(.source == "existing") %>% pull(!!sym(col))
      if (length(ex) && !all(is.na(ex))) return(ex[1])
      return(NA_real_)
    } else if (scalar_strategy == "sum") {
      num <- suppressWarnings(as.numeric(unlist(vals)))
      if (all(is.na(num))) return(NA_real_) else return(sum(num, na.rm = TRUE))
    } else if (scalar_strategy == "mean") {
      num <- suppressWarnings(as.numeric(unlist(vals)))
      if (all(is.na(num))) return(NA_real_) else return(mean(num, na.rm = TRUE))
    } else { # first
      v <- vals[!is.na(vals)]
      if (length(v)) return(v[[1]]) else return(NA_real_)
    }
  }
  
  # combine per-group
  out_list <- all %>%
    group_by(across(all_of(key_cols))) %>%
    group_map(~{
      rows <- .x
      keys  <- .y
      
      # scalars
      scalars <- map_dbl(scalar_cols, ~ pick_scalar_for_group(rows, .x))
      names(scalars) <- scalar_cols
      
      # nested: bind rows of each nested tibble found in the group
      nested_result <- map(nested_cols, function(nc) {
        # compact removes NULLs
        nested_parts <- compact(rows[[nc]])
        if (length(nested_parts) == 0) {
          tibble()
        } else {
          bind_rows(nested_parts)
        }
      })
      names(nested_result) <- nested_cols
      
      # create output row: keys + scalars + nested list-cols
      out_row <- tibble(!!!keys)
      # add scalar columns
      for (nm in scalar_cols) out_row[[nm]] <- scalars[[nm]]
      # add nested columns as list-columns (wrap combined tibble in list())
      for (nc in nested_cols) out_row[[nc]] <- list(nested_result[[nc]])
      out_row
    }, .keep = TRUE) %>%
    list_rbind()
  
  # tidy: remove .source if present, order cols
  out_list
}

### ensure required columns exist, add empty list-columns if missing
ensure_schema <- function(df, group_cols = c("station","year"),
                          scalar_cols = c("env_score","hazard_index","total_CR_cases_10k","wl_index"),
                          nested_cols = c("by_parameter","detail_rows")) {
  
  required <- c(group_cols, scalar_cols, nested_cols)
  for (nm in required) {
    if (!nm %in% names(df)) {
      # scalar defaults to NA, nested defaults to empty tibble wrapped in list
      if (nm %in% nested_cols) df[[nm]] <- replicate(nrow(df), tibble(), simplify = FALSE)
      else df[[nm]] <- NA_real_
    }
  }
  # coerce nested columns to list-column of tibbles if needed
  for (nc in nested_cols) {
    if (!is.list(df[[nc]])) df[[nc]] <- lapply(df[[nc]], function(x) if (is.data.frame(x)) x else tibble())
  }
  df
}

# helper: ensure nested list-column exists and is a list of tibbles
ensure_listcol_tbl <- function(df, col) {
  if (!col %in% names(df)) df[[col]] <- replicate(nrow(df), tibble(), simplify = FALSE)
  if (!is.list(df[[col]])) df[[col]] <- lapply(df[[col]], function(x) if (is.data.frame(x)) as_tibble(x) else tibble())
  df
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
# pilco_line <- st_read("data/geojson/pilco_line.geojson", quiet = TRUE)
# bol_border <- st_read("data/geojson/bol_borders.geojson", quiet = TRUE)
# river_network <- st_read("data/shp/River_Network.shp", quiet = TRUE)
# pilco_basin <- st_read("data/shp/Pilcomayo_Basin.shp", quiet = TRUE)

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


# Load standards with match names
{
# usgs_sqg <- read_csv("data/standards/USGS_SQG.csv", show_col_types = FALSE) |>
#   mutate(match_name = c("Arsenic (mg/kg As)",
#                         "Cadmium (mg/kg Cd)",
#                         "Copper (mg/kg Cu)",
#                         "Chromium (mg/kg Cr)",
#                         "Lead (mg/kg Pb)",
#                         "Mercury (mg/kg Hg)",
#                         "Nickel (mg/kg Ni)",
#                         "Zinc (mg/kg Zn)"))
# 
# bolivian_1333 <- read_csv("data/standards/bolivian_standards_1333.csv", show_col_types = FALSE) |>
#   mutate(match_name = c("pH", "pH", 
#                         "Color (u PtCo)", 
#                         "Total Dissolved Solids (mg/l)", 
#                         "Oxygen Saturation (%)", 
#                         "Biochemical Oxygen Demand (mg/l O2)", 
#                         "Chemical Oxygen Demand (mg/l O2)", 
#                         NA, NA, NA, 
#                         "Total Arsenic (ug/l As)", 
#                         NA, NA, 
#                         "Total Boron (ug/l B)", 
#                         "Total Cadmium (ug/l Cd)",
#                         "Total Calcium (mg/l Ca)",
#                         "Chlorides (mg/l Cl-)",
#                         "Total Chromium (ug/l Cr)",
#                         "Total Chromium (ug/l Cr)",
#                         NA,
#                         "Total Copper (ug/l Cu)",
#                         "Total Iron (ug/l Fe)",
#                         "Total Lead (ug/l Pb)",
#                         NA,
#                         "Total Magnesium (mg/l Mg)",
#                         "Total Manganese (ug/l Mn)",
#                         "Total Mercury (ug/l Hg)",
#                         "Total Nickel (ug/l Ni)",
#                         "Nitrate (mg/l NO3)",
#                         "Total Kjeldahl Nitrogen (mg/l N)",
#                         "Total Phosphorus (mg/l PO4)",
#                         "Total Selenium (ug/l Se)",
#                         "Total Silver (ug/l Ag)",
#                         "Total Sodium (mg/l Na)",
#                         "Sulfates (mg/l SO4)",
#                         NA, NA, NA, 
#                         "Total Zinc (ug/l Zn)"
#   ))
} # all commented out


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
  
  quiet_plotly(p, tooltip = "text")
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
  
  quiet_plotly(p, tooltip = "text")
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


#' Get standard threshold values for a parameter
get_standard_thresholds <- function(param, data_type = c("water", "sediment")) {
  data_type <- match.arg(data_type)
  # 
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
      result$station <- str_replace(result$Station, old_name, station_renames[[old_name]])
    }
  }
  
  result
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


# ---- take unit out of parameter ----
parse_parameter_and_fraction <- function(x) {
  # Remove anything in parentheses from the parameter part only
  core <- trimws(sub("\\(.*\\)", "", x))
  
  # Fractions we care about. Add more if you need them.
  fraction_keywords <- c(
    "Total", "Dissolved", "Reactive", "Particulate",
    "Organic", "Inorganic", "Filtered", "Unfiltered"
  )
  
  fraction <- NA_character_
  
  # 1) Check for fraction at the *beginning* ("Total Nitrogen")
  for (f in fraction_keywords) {
    pattern <- paste0("^", f, "\\b\\s*")
    if (grepl(pattern, core, ignore.case = TRUE)) {
      fraction <- f
      core <- trimws(sub(pattern, "", core, ignore.case = TRUE))
      return(list(parameter = core, fraction = fraction))
    }
  }
  
  # 2) Check for fraction at the *end* ("Arsenic total")
  for (f in fraction_keywords) {
    pattern <- paste0("\\b", f, "\\s*$")
    if (grepl(pattern, core, ignore.case = TRUE)) {
      fraction <- f
      core <- trimws(sub(pattern, "", core, ignore.case = TRUE))
      return(list(parameter = core, fraction = fraction))
    }
  }
  
  # 3) No fraction keyword found
  list(parameter = core, fraction = NA_character_)
}


# ---- parse a unit string into structured components ----
parse_unit <- function(u) {
  u0 <- as.character(u %||% "")
  u0 <- tolower(u0)
  # normalize micro symbols
  u0 <- str_replace_all(u0, "\u00B5|µ|μ", "u")
  u0 <- str_squish(u0)
  if (u0 == "") return(list(raw = u0, left = NA_character_, right = NA_character_, left_kind = NA_character_, prefix = NA_character_, gram_factor_g = NA_real_, denom_type = NA_character_, denom_scale = NA_real_))
  
  # normalize " per " into "/"
  u0 <- str_replace_all(u0, "\\s+per\\s+", "/")
  
  # if we have a slash, split on the first slash
  if (str_detect(u0, "/")) {
    parts <- str_split_fixed(u0, "/", n = 2)
    left_raw <- str_trim(parts[1])
    right_raw <- str_trim(parts[2])
  } else {
    # no slash: maybe token like "mg" or "ntu" or "ph unit" or "cfu/100 ml" baked into parentheses earlier
    left_raw <- str_trim(u0)
    right_raw <- ""
  }
  
  # identify left kind and prefix
  # counts / indicators
  if (str_detect(left_raw, "\\b(cfu|mpn|coliform|mesophilic|cfu\\)|mpn\\)|coliform)")) {
    left_kind <- "count"
    gram_factor_g <- NA_real_
    prefix <- NA_character_
  } else {
    # attempt to detect prefix (pg, ng, ug, mg, g, kg) at start of token
    prefix_match <- str_match(left_raw, "^(pg|ng|u?g|mg|g|kg)")[,1]
    # normalize µg patterns "ug" already
    prefix_match <- ifelse(prefix_match == "ug" | prefix_match == "ug", "ug", prefix_match)
    if (!is.na(prefix_match) && nzchar(prefix_match)) {
      # canonicalize "ug" vs "µg"
      prefix <- prefix_match
      # convert u?g -> ug
      prefix <- str_replace(prefix, "^ug$", "ug")
      # gram factor (grams per left unit)
      gram_factor_g <- .prefix_to_g[[prefix]]
      left_kind <- "mass"
    } else {
      # left didn't include a mass prefix - it might be "ph", "ntu", "ohm.cm" or other textual unit like "mg/l Zn" (we'll treat it as mass if "mg" appears anywhere)
      if (str_detect(left_raw, "mg|ug|ng|g|kg")) {
        # find first matching prefix anywhere
        prefix_match <- str_extract(left_raw, "pg|ng|ug|mg|g|kg")
        prefix <- prefix_match
        gram_factor_g <- .prefix_to_g[[prefix]]
        left_kind <- "mass"
      } else {
        # it's probably a non-mass token (NTU, mV, pH, etc) or a count token without explicit CFU
        prefix <- NA_character_
        gram_factor_g <- NA_real_
        # check common non-mass
        if (str_detect(left_raw, "ntu|ohm|mv|ph|sal|t ref|m3/s|m3 s")) {
          left_kind <- "other"
        } else {
          left_kind <- "other"
        }
      }
    }
  }
  
  # parse right (denominator) -> detect numeric multiplier (like "100 ml")
  denom_num <- suppressWarnings(as.numeric(str_extract(right_raw, "\\d+")))
  if (is.na(denom_num)) denom_num <- 1
  
  # detect denom unit type and compute denom_scale:
  # denom_scale is the numeric size of the denominator in base units:
  #  - for volume denominators we report denom_scale in L (so "100 ml" -> 0.1)
  #  - for mass denominators we report denom_scale in kg (so "kg" -> 1)
  denom_type <- NA_character_
  denom_scale <- NA_real_
  
  if (str_detect(right_raw, "ml")) {
    denom_type <- "L"
    denom_scale <- denom_num * 0.001
  } else if (str_detect(right_raw, "\\bdl\\b|deciliter|dL")) {
    denom_type <- "L"
    denom_scale <- denom_num * 0.1
  } else if (str_detect(right_raw, "\\bl\\b|liter|litre")) {
    denom_type <- "L"
    denom_scale <- denom_num * 1
  } else if (str_detect(right_raw, "m3|m\\^3")) {
    denom_type <- "L"
    denom_scale <- denom_num * 1000  # 1 m3 = 1000 L
  } else if (str_detect(right_raw, "kg|kilogram")) {
    denom_type <- "kg"
    denom_scale <- denom_num * 1
  } else if (str_trim(right_raw) == "") {
    denom_type <- NA_character_
    denom_scale <- NA_real_
  } else {
    # fallback: look for "100 ml" or 'per 100 ml' already normalized -> handled by ml, but keep fallback
    if (str_detect(right_raw, "100\\s*ml")) {
      denom_type <- "L"; denom_scale <- 100 * 0.001
    } else {
      denom_type <- "other"; denom_scale <- NA_real_
    }
  }
  
  # if left kind is count, treat gram_factor_g as NA but useful for computations we set gram_factor_g = 1 (counts are unit-less in mass sense)
  if (left_kind == "count") {
    gram_factor_g <- NA_real_
  }
  
  list(
    raw = u0,
    left_raw = left_raw,
    right_raw = right_raw,
    left_kind = left_kind,        # "mass", "count", "other"
    prefix = prefix,
    gram_factor_g = gram_factor_g, # grams per left-unit (if left_kind == "mass")
    denom_type = denom_type,      # "L", "kg", "other", NA
    denom_scale = denom_scale     # numeric scale in base (L or kg) or NA
  )
}

# ---- core comparator ----
# compares sample_unit -> standard_unit
# returns a list: convertible (T/F), conversion_factor (multiply sample value by this -> standard), message, parsed_sample, parsed_standard
compare_units <- function(sample_unit, standard_unit) {
  s <- parse_unit(sample_unit)
  t <- parse_unit(standard_unit)
  
  # quick identical after normalization
  if (!is.na(s$raw) && !is.na(t$raw) && s$raw == t$raw) {
    return(list(convertible = TRUE, conversion_factor = 1, message = "Exact match (normalized).", sample_parsed = s, standard_parsed = t))
  }
  
  # If either is NA/empty, can't convert
  if (is.na(s$raw) || s$raw == "" || is.na(t$raw) || t$raw == "") {
    return(list(convertible = FALSE, conversion_factor = NA_real_, message = "One of the units is empty/unparseable.", sample_parsed = s, standard_parsed = t))
  }
  
  # If both are 'other' (like pH, NTU, Ohm.cm), only convertible if exact token match
  if (s$left_kind == "other" || t$left_kind == "other") {
    # allow exact normalized equality only
    if (s$raw == t$raw) {
      return(list(convertible = TRUE, conversion_factor = 1, message = "Other-type exact match.", sample_parsed = s, standard_parsed = t))
    } else {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Non-mass/non-count units differ and are not convertible.", sample_parsed = s, standard_parsed = t))
    }
  }
  
  # both counts (e.g., CFU/100 ml, MPN/100 ml) -> treat like count/volume
  if (s$left_kind == "count" && t$left_kind == "count") {
    # need denom_type both L-like
    if (is.na(s$denom_scale) || is.na(t$denom_scale) || s$denom_type != "L" || t$denom_type != "L") {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Count units but denominator not volume or unparseable.", sample_parsed = s, standard_parsed = t))
    }
    # conversion factor is ratio of denom scales (e.g., sample per 100ml -> denom_scale 0.1 L, standard per 1 L -> denom_scale 1; factor = denom_std/denom_sample)
    factor <- t$denom_scale / s$denom_scale
    return(list(convertible = TRUE, conversion_factor = factor, message = "Count-per-volume convertible (denominator scale applied).", sample_parsed = s, standard_parsed = t))
  }
  
  # both mass-ish
  if (s$left_kind == "mass" && t$left_kind == "mass") {
    # denom must be same family: both L (mass/volume) or both kg (mass/mass)
    if (is.na(s$denom_type) || is.na(t$denom_type)) {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "One of denominators unparseable.", sample_parsed = s, standard_parsed = t))
    }
    if (s$denom_type != t$denom_type) {
      # cannot convert mass/volume <-> mass/mass without density
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Different denominator families (e.g. L vs kg) — requires density; not convertible.", sample_parsed = s, standard_parsed = t))
    }
    # both same denom family (e.g., both L)
    if (is.na(s$gram_factor_g) || is.na(t$gram_factor_g) || is.na(s$denom_scale) || is.na(t$denom_scale)) {
      return(list(convertible = FALSE, conversion_factor = NA_real_, message = "Missing prefix/denominator info; can't compute factor.", sample_parsed = s, standard_parsed = t))
    }
    # conversion factor formula derived so that:
    # sample_value * factor = value expressed in standard units.
    # Let sample "left_s / denom_s", standard "left_t / denom_t"
    # gram_factor_g = grams per left-unit.
    # denom_scale is numeric in base (L or kg), i.e. denom of sample = denom_scale_s (liters or kg).
    # The factor simplifies to:
    # factor = (gram_factor_s / denom_scale_s) / (gram_factor_t / denom_scale_t)
    factor <- (t$gram_factor_g / t$denom_scale) / (s$gram_factor_g / s$denom_scale)
    
    # debugging
    # cat("\n[calculate_hqcr]: HQ values:\nInitial val: ",val," (",unit,")\nStd val: ",
    #     std$value, " (",std$unit,")\nHQ: ", hq, "\nConversion Factor: ", unit_check_hq$conversion_factor)
    # 
    
    res = list(convertible = TRUE, conversion_factor = as.numeric(factor), message = "Mass units convertible via prefix/denom scaling.", sample_parsed = s, standard_parsed = t)
    # str(res, max.level=3, give.attr=FALSE, strict.width="cut")
    
    return(res)
  }
  
  # otherwise fallback: not convertible by metric multipliers only
  cat("[compare_units] Fallback.")
  list(convertible = FALSE, conversion_factor = NA_real_, message = "Units not both mass or count-per-volume families; cannot auto-convert.", sample_parsed = s, standard_parsed = t)
}

# -------------------------------------------------------
# get_param_list(): extract valid parameter names for a media type
# -------------------------------------------------------
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


# ---- convenience wrapper that returns a human-readable summary ----
compare_units_summary <- function(sample_unit, standard_unit) {
  res <- compare_units(sample_unit, standard_unit)
  if (res$convertible) {
    sprintf("Convertible: TRUE — multiply sample values by %g to get %s (message: %s)",
            res$conversion_factor, standard_unit, res$message)
  } else {
    sprintf("Convertible: FALSE — %s (sample='%s', standard='%s')",
            res$message, sample_unit, standard_unit)
  }
}
