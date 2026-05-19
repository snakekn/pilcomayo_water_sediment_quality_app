# helpers_server.R - many helper functions

# Update names across data formats
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
read_uploaded_file = function(path, col_names = TRUE) {

  # get the file type
  ftype = get_file_type(path)

  # read the data
  switch(ftype,
         "csv"  = readr::read_csv( path, col_names = col_names, show_col_types = FALSE),
         "tsv"  = readr::read_tsv( path, col_names = col_names, show_col_types = FALSE),
         "xls"  = readxl::read_xls( path, col_names = col_names),
         "xlsx" = readxl::read_xlsx(path, col_names = col_names),
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
# check if master_data$all_media_scored is ready to utilize
app_has_data <- function(master_data) {
  any(vapply(
    list(
      master_data$water_scored,
      master_data$sed_scored,
      master_data$all_media_scored
    ),
    function(x) is.data.frame(x) && nrow(x) > 0,
    logical(1)
  ))
}

# check if data is ready and provide notifications otherwise
has_data <- function(
    df,
    cols = c("station", "media", "parameter", "date"),
    notify = TRUE,
    msg = "No data loaded yet. Add data in Data Preparation before moving ahead.",
    id = "feature_data_status",
    suppress_when_app_empty = TRUE,
    master_data = NULL,
    session = shiny::getDefaultReactiveDomain()
) {
  ok <- !is.null(df) &&
    is.data.frame(df) &&
    nrow(df) > 0 &&
    all(cols %in% names(df))
  
  app_empty <- suppress_when_app_empty &&
    !is.null(master_data)
  
  if (!ok && isTRUE(notify) && !app_empty && !is.null(session)) {
    showNotification(
      msg,
      id = id,
      type = "message",
      duration = NULL,
      closeButton = FALSE,
      session = session
    )
  }
  
  ok
}

# Helper function: 
process_uploaded_pilco_file <- function(path, media, src_lang, target_lang) {
  message("\n[process_uploaded_pilco_file] START")
  message("[process_uploaded_pilco_file] path = ", path)
  message("[process_uploaded_pilco_file] media = ", media)
  
  message("[1] Loading file")
  loaded <- if (media == "water") {
    load_water_data(path, is.clean = FALSE, translate_to = target_lang)
  } else if (media == "sediment") {
    load_sediment_data(path, is.clean = FALSE, translate_to = target_lang)
  } else {
    stop("[process_uploaded_pilco_file] media must be 'water' or 'sediment'")
  }
  
  message("[1] loaded dim = ", nrow(loaded), " x ", ncol(loaded))
  message("[1] loaded names = ", paste(names(loaded), collapse = ", "))
  
  message("[2] Pivoting using pivot_pilcomayo_data()")
  pivoted <- pivot_pilcomayo_data(loaded, media_type = media)
  
  message("[2] pivoted dim = ", nrow(pivoted), " x ", ncol(pivoted))
  # message("[2] pivoted names = ", paste(names(pivoted), collapse = ", "))
  
  if ("station" %in% names(pivoted)) {
    message("[2] station non-NA = ", sum(!is.na(pivoted$station)))
  }
  if ("concentration" %in% names(pivoted)) {
    message("[2] concentration non-NA = ", sum(!is.na(pivoted$concentration)))
  }
  
  message("[3] Scoring with existing score_data()")
  scored <- score_data(pivoted)
  
  message("[3] scored dim = ", nrow(scored), " x ", ncol(scored))
  message("[process_uploaded_pilco_file] END")
  
  scored
}

# take data in either of 2 formats, format & score, merge
# Output: locyear and scored files in master_data
dataUploadServer <- function(id, base_data, master_data, raw_water = NULL, raw_sed = NULL, lang = NULL) {
  moduleServer(id, function(input, output, session) {
    # Local translation helper — falls back to English if lang not provided
    t <- function(key) {
      l <- if (!is.null(lang)) isolate(lang()) else "en"
      strings <- if (l == "es") STRINGS_ES else STRINGS_EN
      strings[[key]] %||% paste0("[", key, "]")
    }

    # Re-translate upload form labels whenever language changes
    if (!is.null(lang)) {
      observeEvent(lang(), {
        updateRadioButtons(session, "source_format",
          label   = t("upload_format_label"),
          choices = setNames(c("pilco", "by_param"),
                             c(t("upload_pilco"), t("upload_by_param"))))
        updateRadioButtons(session, "current_lang",
          label   = t("upload_lang_label"),
          choices = c("English" = "en", "Español" = "es"))
        updateRadioButtons(session, "media_type",
          label   = t("upload_media_label"),
          choices = setNames(c("sediment", "water"),
                             c(t("media_sed"), t("media_water"))))
        updateRadioButtons(session, "translate_to",
          label   = t("upload_translate_label"),
          choices = c("English" = "en", "Español" = "es"))
        updateActionButton(session, "upload_data", label = t("upload_btn"))
      }, ignoreInit = TRUE)
    }

    parsed_upload  <- reactiveVal(NULL)
    pending_upload <- reactiveVal(NULL)  # stashed while user responds to duplicate modal

    # Columns that together uniquely identify one measurement row
    DEDUPE_COLS <- c("station", "date", "parameter", "media", "fraction")

    # Returns the number of uploaded rows that already exist in existing by DEDUPE_COLS
    count_duplicates <- function(existing, uploaded) {
      if (is.null(existing) || nrow(existing) == 0 || nrow(uploaded) == 0) return(0L)
      common_keys <- intersect(DEDUPE_COLS, intersect(names(existing), names(uploaded)))
      if (length(common_keys) == 0) return(0L)
      nrow(dplyr::semi_join(uploaded, existing, by = common_keys))
    }

    # Merges existing + uploaded according to the chosen strategy, then emits the result
    finalize_upload <- function(existing, uploaded, strategy, media) {
      common_keys <- intersect(DEDUPE_COLS, intersect(names(existing), names(uploaded)))

      merged <- switch(strategy,
        keep_existing = {
          # Only add rows from the upload that are not already in existing
          new_only <- dplyr::anti_join(uploaded, existing, by = common_keys)
          bind_rows(existing, new_only)
        },
        replace = {
          # Uploaded rows overwrite matching existing rows; new rows are appended
          merge_scored(existing, uploaded, replace = TRUE)
        },
        keep_both = {
          # No deduplication — append everything
          bind_rows(existing, uploaded)
        }
      )

      print("updating parsed_upload")
      parsed_upload(list(
        scored  = merged,
        locyear = score_to_loc_year(merged),
        media   = media
      ))

      save_path <- if (media == "water") {
        "data/processed/water_scored_user_update.rds"
      } else {
        "data/processed/sed_scored_user_updated.rds"
      }
      saveRDS(merged, save_path)
      showNotification(t("notif_upload_complete"), type = "message")
    }

    # ── Main upload handler ────────────────────────────────────────────────────
    observeEvent(input$upload_data, {

      if (is.null(input$files) || nrow(input$files) == 0) {
        showNotification(t("notif_select_file"), type = "error")
        return()
      }

      src_format     <- input$source_format
      src_lang       <- input$current_lang
      src_media      <- input$media_type
      src_target_lang <- input$translate_to

      print(paste("Format:", src_format, "Lang:", src_lang,
                  "Media:", src_media, "Target:", src_target_lang))

      req(input$files)
      showNotification(paste("Processing:", input$files$name[1]), type = "message")

      withProgress(message = t("progress_upload"), value = 0, {
        n_files <- nrow(input$files)

        # Use raw (unfiltered) data as the base — see upload/filter interaction notes
        existing_scored <- if (src_media == "water") {
          if (!is.null(raw_water)) isolate(raw_water()) else isolate(master_data$water_scored)
        } else {
          if (!is.null(raw_sed))   isolate(raw_sed())   else isolate(master_data$sed_scored)
        }

        # Score each uploaded file, collecting results before any merging
        all_uploaded <- vector("list", n_files)

        for (i in seq_len(n_files)) {
          fname_i <- input$files$name[i]
          fpath_i <- input$files$datapath[i]

          incProgress(1/n_files, message = paste("Processing", fname_i))

          tryCatch({
            file_data_i <- read_uploaded_file(fpath_i, col_names = (src_format != "pilco"))
            print(paste0("[dataUploadServer]: ", fname_i))

            df_i <- upload_sampled_data(
              file_data_i,
              media        = src_media,
              format       = src_format,
              debug_prepped = FALSE,
              src_lang     = src_lang,
              target_lang  = src_target_lang
            )
            print("completed upload_sampled_data")

            dup_names <- names(df_i)[duplicated(names(df_i))]
            if (length(dup_names)) {
              warning(sprintf("File '%s' has duplicate column names: %s",
                              fname_i, paste(unique(dup_names), collapse = ", ")))
            }
            if (any(duplicated(names(df_i)))) {
              names(df_i) <- janitor::make_clean_names(names(df_i), unique = TRUE)
            }

            df_i$data_source <- fname_i

            all_uploaded[[i]] <- score_data(df_i)
            print("[dataUploadServer]: finished score_data")

          }, error = function(e) {
            full_msg <- paste(conditionMessage(e), collapse = "\n")
            message("[dataUploadServer] Error in ", fname_i, ":\n", full_msg)
            showNotification(paste("Error processing", fname_i, ":", full_msg),
                             type = "error")
          })
        }

        all_uploaded_df <- bind_rows(all_uploaded)

        if (nrow(all_uploaded_df) == 0) {
          showNotification(t("notif_no_valid_data"), type = "warning")
          return()
        }

        # ── Duplicate check ────────────────────────────────────────────────────
        n_dupes <- count_duplicates(existing_scored, all_uploaded_df)

        if (n_dupes > 0) {
          pending_upload(list(
            existing = existing_scored,
            uploaded = all_uploaded_df,
            media    = src_media
          ))

          showModal(modalDialog(
            title = t("modal_dup_title"),
            p(HTML(sprintf(t("modal_dup_body"), format(n_dupes, big.mark = ",")))),
            hr(),
            tags$dl(
              tags$dt(t("modal_keep_existing_btn")),
              tags$dd(t("modal_keep_existing_desc")),
              tags$dt(t("modal_replace_btn")),
              tags$dd(t("modal_replace_desc")),
              tags$dt(t("modal_keep_both_btn")),
              tags$dd(t("modal_keep_both_desc"))
            ),
            footer = tagList(
              modalButton(t("modal_cancel_btn")),
              actionButton(session$ns("dup_keep_existing"), t("modal_keep_existing_btn"), class = "btn-default"),
              actionButton(session$ns("dup_replace"),       t("modal_replace_btn"),       class = "btn-warning"),
              actionButton(session$ns("dup_keep_both"),     t("modal_keep_both_btn"),     class = "btn-danger")
            ),
            easyClose = FALSE
          ))

        } else {
          # No duplicates — finalize immediately with a simple append
          finalize_upload(existing_scored, all_uploaded_df, "keep_both", src_media)
        }
      })

    }, ignoreInit = TRUE)

    # ── Modal resolution handlers ──────────────────────────────────────────────
    observeEvent(input$dup_keep_existing, {
      pending <- pending_upload(); req(pending)
      removeModal()
      finalize_upload(pending$existing, pending$uploaded, "keep_existing", pending$media)
      pending_upload(NULL)
    })

    observeEvent(input$dup_replace, {
      pending <- pending_upload(); req(pending)
      removeModal()
      finalize_upload(pending$existing, pending$uploaded, "replace", pending$media)
      pending_upload(NULL)
    })

    observeEvent(input$dup_keep_both, {
      pending <- pending_upload(); req(pending)
      removeModal()
      finalize_upload(pending$existing, pending$uploaded, "keep_both", pending$media)
      pending_upload(NULL)
    })

    # Debug observer
    observe({
      result <- parsed_upload()
      if (!is.null(result)) {
        cat("parsed_upload contains:\n")
        cat("  - media:", result$media, "\n")
        cat("  - scored rows:", nrow(result$scored), "\n")
        cat("  - locyear rows:", nrow(result$locyear), "\n")
      }
    })

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

# df schema
DF_SCHEMA <- c(
  "parameter"          = "character",
  "media"              = "character",
  "unit"               = "character",
  "data_source"        = "character",
  "station"            = "character",
  "date"               = "Date",
  "time"               = "character",
  "campaign"           = "character",
  "institution"        = "character",
  "river"              = "character",
  "latitude_decimal"   = "numeric",
  "longitude_decimal"  = "numeric",
  "year"               = "integer",
  "distance_from_bank" = "numeric",
  "sieve_size"         = "character",
  "fraction"           = "character",
  "concentration"      = "numeric",
  "HQ"                 = "numeric",
  "has_HQ"             = "logical",
  "converted_from_mg_kg" = "logical"
  
)

# coerce key columns to stable types so bind_rows never clashes
.coerce_key_types <- function(df) {
  
  # 1. Apply strict schema for known columns
  for (col_name in names(DF_SCHEMA)) {
    if (col_name %in% names(df)) {
      target <- DF_SCHEMA[[col_name]]
      
      df[[col_name]] <- switch(target,
                               "character" = as.character(df[[col_name]]),
                               "numeric"   = suppressWarnings(as.numeric(df[[col_name]])),
                               "integer"   = suppressWarnings(as.integer(df[[col_name]])),
                               "logical"   = as.logical(df[[col_name]]),
                               "Date"      = {
                                 # Attempt multi-format parse for Dates
                                 suppressWarnings({
                                   val <- df[[col_name]]
                                   if (!inherits(val, "Date")) {
                                     a <- try(as.Date(val, "%Y-%m-%d"), silent = TRUE)
                                     b <- try(as.Date(val, "%d/%m/%Y"), silent = TRUE)
                                     val <- if (all(!is.na(a))) a else if (all(!is.na(b))) b else as.Date(val)
                                   }
                                   val
                                 })
                               },
                               df[[col_name]]
      )
    }
  }
  
  # 2. Final Fallback: Convert unknown logical placeholders to character
  # This prevents bind_rows() from crashing on placeholder-only columns
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        where(~ is.logical(.x) && all(is.na(.x))),
        as.character
      )
    )
  
  df
}

# align list of dfs to same columns (union) with the column order of the first
.align_cols <- function(dfs) {
  all_cols <- Reduce(union, lapply(dfs, names))
  
  dfs <- lapply(dfs, function(df) {
    miss <- setdiff(all_cols, names(df))
    for (m in miss) {
      if (m %in% c("parameter", "media", "unit", "fraction", "Station")) {
        df[[m]] <- NA_character_
      } else if (m %in% c("HQ", "concentration", "Latitude Decimal", "Longitude Decimal",
                          "latitude_decimal", "longitude_decimal", "Lat_dd", "Long_dd")) {
        df[[m]] <- NA_real_
      } else if (m %in% c("Year", "year")) {
        df[[m]] <- NA_integer_
      } else {
        df[[m]] <- NA
      }
    }
    df[, all_cols, drop = FALSE]
  })
  
  dfs
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

year_range_slider_ui <- function(id, label = "Year Range") {
  ns <- NS(id)
  
  tagList(
    sliderInput(
      ns("year_range"),
      label = label,
      min = 0, max = 1, value = 1,
      sep = "",  # no comma separators
      width = "100%"
    ),
    verbatimTextOutput(ns("year_display"))
  )
}

year_range_slider_server <- function(id, data, year_col = "year") {
  moduleServer(id, function(input, output, session) {
    
    # Extract available years from data
    available_years <- reactive({
      req(data())
      df <- data()
      if (!year_col %in% names(df)) {
        return(integer(0))
      }
      r = sort(unique(df[[year_col]]))
      # print("[available_years]:")
      # print(r)
      r
    })
    
    # Default: last 5 years excluding most recent (e.g., 2018-2022 if max=2023)
    default_range <- reactive({
      yrs <- available_years()
      req(length(yrs) >= 6)  # need at least 6 years for 5‑year window
      max_yr <- max(yrs)
      min_yr <- max_yr - 6  # 5 years back from year‑before‑last
      c(min_yr, max_yr - 1)
    })
    
    # Update slider bounds + default
    observeEvent(available_years(), {
      yrs <- available_years()
      if (length(yrs) == 0) return()
      
      updateSliderInput(session, "year_range",
                        min = min(yrs), max = max(yrs),
                        value = default_range()
      )
    })
    
    # Display current range (e.g., "2018‑2023")
    output$year_display <- renderText({
      range <- input$year_range
      paste0("Showing: ", range[1], "–", range[2])
    })
    
    # Return reactive year range for use in filtering
    return(reactive(input$year_range))
  })
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
  message("\n[merge_media_safely]")
  message("[1] Starting safe merge of water + sediment data.")
  
  # Check if one side is empty and return the other (coerced)
  if (nrow(water_df) == 0) return(.coerce_key_types(sediment_df))
  if (nrow(sediment_df) == 0) return(.coerce_key_types(water_df))
  
  drop_cols <- c()
  
  water_df <- water_df %>% dplyr::select(-dplyr::any_of(drop_cols))
  sediment_df <- sediment_df %>% dplyr::select(-dplyr::any_of(drop_cols))
  
  w_names <- names(water_df)
  s_names <- names(sediment_df)
  
  message("[2] Column sets after dropping deprecated fields")
  message("Water columns (", length(w_names), "): ", paste(w_names, collapse = ", "))
  message("Sediment columns (", length(s_names), "): ", paste(s_names, collapse = ", "))
  
  missing_in_water <- setdiff(s_names, w_names)
  missing_in_sed <- setdiff(w_names, s_names)
  
  message("[3] Columns only in sediment: ",
          ifelse(length(missing_in_water) == 0, "none", paste(missing_in_water, collapse = ", ")))
  message("[4] Columns only in water: ",
          ifelse(length(missing_in_sed) == 0, "none", paste(missing_in_sed, collapse = ", ")))
  
  if (length(missing_in_water) > 0) {
    message("[5] Adding missing columns to WATER")
    for (col in missing_in_water) {
      message("  - ", col)
      water_df[[col]] <- NA
    }
  }
  
  if (length(missing_in_sed) > 0) {
    message("[6] Adding missing columns to SEDIMENT")
    for (col in missing_in_sed) {
      message("  - ", col)
      sediment_df[[col]] <- NA
    }
  }
  
  final_col_order <- names(water_df)
  message("[7] Applying consistent column order (", length(final_col_order), " columns).")
  
  water_aligned <- water_df[, final_col_order, drop = FALSE]
  sediment_aligned <- sediment_df[, final_col_order, drop = FALSE]
  
  water_aligned <- .coerce_key_types(water_aligned)
  sediment_aligned <- .coerce_key_types(sediment_aligned)
  
  message("[8] Binding rows...")
  merged <- dplyr::bind_rows(water_aligned, sediment_aligned)
  
  message("[9] Merge complete. Final dimensions: ",
          nrow(merged), " rows x ", ncol(merged), " columns.")
  
  merged
}

#### get_param_list(): extract valid parameter names for a media type ####
get_param_list <- function(df, media_type = "all", need_std = FALSE, need_hq=FALSE) {
  
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
  if(need_std) df = df |> filter(parameter %in% stds$parameter)
  # filter for HQ if needed
  if(need_hq) df = df |> filter(!is.na(HQ))
  
  df %>%
    filter(!parameter %in% exclude_cols) %>%
    mutate(concentration = suppressWarnings(as.numeric(concentration))) %>%
    filter(!is.na(concentration)) %>%
    pull(parameter) %>%
    unique() %>%
    sort()
}

#### Compare concentrations to BOL standards ####
# Returns the strictest class a concentration meets for a given parameter
get_bol_class <- function(parameter, concentration, unit, stds) {
  param_stds <- stds %>%
    filter(
      regulator == "Bolivian Law 1333",
      media == "water",
      .data$parameter == .env$parameter
    ) %>%
    rowwise() %>%
    mutate(
      conv       = list(compare_units(unit, .data$unit)),
      value_conv = if (conv$convertible) value * conv$conversion_factor else value
    ) %>%
    ungroup() %>%
    arrange(value_conv)  # Class A (strictest) first
  
  if (nrow(param_stds) == 0 || is.na(concentration)) return(NA_character_)
  
  # Find strictest class whose limit >= concentration
  passed <- param_stds %>% filter(value_conv >= concentration)
  if (nrow(passed) == 0) return("Unclassified")
  
  # Return the strictest (lowest rank) passing class
  passed_classes <- passed$limit[passed$limit %in% CLASS_ORDER]
  CLASS_ORDER[min(match(passed_classes, CLASS_ORDER), na.rm = TRUE)]
}

# cache results
get_bol_class_cache <- memoise::memoise(
  Vectorize(get_bol_class, vectorize.args = c("parameter", "concentration", "unit"))
)

### get bol 1333 standard classifications for samples
classify_water_1333_bulk <- function(df, stds) {
  # Get relevant standards once
  bol_stds <- stds %>%
    filter(regulator == "Bolivian Law 1333", media == "water") %>%
    select(parameter, value, unit, limit)
  
  # Join standards to data by parameter
  df_joined <- df %>%
    left_join(bol_stds, by = "parameter", relationship = "many-to-many")
  
  cat("unique unit pairs:", df_joined %>% distinct(unit.x, unit.y) %>% nrow(), "\n")
  print(df_joined %>% distinct(unit.x, unit.y))
  
  df_joined = df_joined |>
    rowwise() %>%
    mutate(
      conv       = list(compare_units(unit.x, unit.y)),
      value_conv = if (conv$convertible) value * conv$conversion_factor else value
    ) %>%
    ungroup() %>%
    # Keep only standards the concentration passes
    filter(is.na(value_conv) | concentration <= value_conv) %>%
    # Pick strictest passing class per observation
    mutate(limit_rank = match(limit, c("Class A","Class B","Class C","Class D","Unclassified")),
           classification = limit) %>%
    group_by(station, date, parameter, concentration) %>%
    slice_min(limit_rank, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(station, date, parameter, concentration, classification)

  # Join back — unmatched rows get "Unclassified"
  df %>%
    left_join(df_joined, by = c("station", "date", "parameter", "concentration")) %>%
    mutate(classification = ifelse(is.na(classification), "Unclassified", classification))
}

### get usgs classes for sediment samples
classify_sediment_usgs_bulk <- function(df, stds) {
  usgs_stds <- stds %>%
    filter(regulator == "USGS", media == "sediment") %>%
    select(parameter, value, unit, limit)  # limit should be "TEL" or "PEL"
  
  if (nrow(usgs_stds) == 0 || !any(df$parameter %in% usgs_stds$parameter)) {
    return(df %>% mutate(sed_class = "No Standard Available"))
  }
  
  # Build unit conversion lookup on unique pairs only
  unit_pairs <- df %>%
    left_join(usgs_stds, by = "parameter", relationship = "many-to-many") %>%
    distinct(unit.x, unit.y)
  
  unit_pairs$factor <- map2_dbl(
    unit_pairs$unit.x, unit_pairs$unit.y,
    ~ { res <- compare_units(.x, .y); if (res$convertible) res$conversion_factor else NA_real_ }
  )
  
  df_joined <- df %>%
    left_join(usgs_stds, by = "parameter", relationship = "many-to-many") %>%
    left_join(unit_pairs, by = c("unit.x", "unit.y")) %>%
    mutate(value_conv = if_else(!is.na(factor), value * factor, value)) %>%
    mutate(limit_rank = match(limit, c("TEL", "PEL"))) %>%  # TEL stricter than PEL
    # keep standards the concentration exceeds (opposite of water — above = worse)
    filter(concentration > value_conv) %>%
    group_by(station, date, parameter, concentration) %>%
    slice_max(limit_rank, n = 1, with_ties = FALSE) %>%  # worst exceeded threshold
    ungroup() %>%
    select(station, date, parameter, concentration, sed_class = limit)
  
  df %>%
    left_join(df_joined, by = c("station", "date", "parameter", "concentration")) %>%
    mutate(sed_class = case_when(
      sed_class == "PEL" ~ "Above PEL",
      sed_class == "TEL" ~ "Above TEL",
      TRUE               ~ "Below TEL"
    ))
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
      # na_before <- sum(is.na(terra::values(r)))
      na_before <- terra::global(eji_r, fun = "isNA")[[1]] # fast & lean
      if (na_before == 0) break
      r <- terra::focal(r, w = 3, fun = "modal", na.policy = "only", na.rm = TRUE)
      # na_after <- sum(is.na(terra::values(r)))
      na_after <- terra::global(eji_r, fun = "isNA")[[1]] # fast & lean
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
