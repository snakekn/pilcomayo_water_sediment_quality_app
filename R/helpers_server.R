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
      result$Station <- str_replace(result$Station, old_name, station_renames[[old_name]])
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
    factor <- (s$gram_factor_g / s$denom_scale) / (t$gram_factor_g / t$denom_scale)
    return(list(convertible = TRUE, conversion_factor = as.numeric(factor), message = "Mass units convertible via prefix/denom scaling.", sample_parsed = s, standard_parsed = t))
  }
  
  # otherwise fallback: not convertible by metric multipliers only
  list(convertible = FALSE, conversion_factor = NA_real_, message = "Units not both mass or count-per-volume families; cannot auto-convert.", sample_parsed = s, standard_parsed = t)
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
