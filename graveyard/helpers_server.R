#### Graveyard ####


#### Shared Constants ####
### Note: Many of these are declared elsewhere
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

CLASS_MAP <- c("Class A"=0,"Class B"=1,"Class C"=2,"Class D"=3,"Unclassified"=4)

USGS_MAP  <- c("Below TEL"=0,"Above TEL"=1,"Above PEL"=2)

REVERSE_PARAMS <- c("Oxygen Saturation (%)","Dissolved Oxygen (mg/l O2)","pH","Resistivity (Ohm.cm)")

use_scope <- function(scope, all_df, bol_df) if (identical(scope,"bol")) bol_df else all_df


#### take unit out of parameter ####
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


#### Enhanced yearly data loader with flexible cleaning and translation ####
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


#### Get standard threshold values for a parameter ####
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


#### MAP HELPERS ####

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


#### Create a generic download handler ####
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

#### Plot class proportions with overlay (standardized vs raw) ####
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


#### Extract coordinates and drop geometry from sf object ####
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


#### Apply spatial filter to keep only points in Bolivia ####
spatial_filter_bolivia <- function(data, lon_col, lat_col) {
  data |>
    st_as_sf(coords = c(lon_col, lat_col), crs = st_crs(bol_border)) |>
    st_filter(bol_border) |>
    extract_coords_and_drop_geometry()
}

#### Load and combine data from multiple Excel files by year ####
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


## from server.R
# Detect metals from columns ending with " Class"
# metals <- reactive({
#   df <- active_water_1333()
#   class_cols <- names(df)[stringr::str_ends(names(df), " Class")]
#   metals <- stringr::str_remove(class_cols, " Class$")
#   metals
# })



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

## all_water_1333
# # merging occurs elsewhere now
# {
#   water_files <- list.files(water_data_path_clean, pattern = "^water_\\d{4}_clean\\.xlsx$", full.names = TRUE)
#   
#   water_dfs <- lapply(water_files, function(f) {
#     year <- stringr::str_extract(basename(f), "\\d{4}")
#     df <- read_xlsx(f)
#     df$Year <- as.integer(year)
#     df$date <- as.Date(df$date, "%d/%m/%Y")
#     df
#   })
#   
#   all_data <- bind_rows(water_dfs) |> 
#     mutate(station = str_replace(station,
#                                  "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
#                                  "Tacobamba arriba Pilcomayo")) |>
#     mutate(station = str_replace(station,
#                                  "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
#                                  "Pilcomayo arriba Tacobamba")) |>
#     filter(!is.na(`Latitude Decimal`))
#   
#   return(all_data)
# } # the old stuff to combine it all

## all_sed_clean
# {
#   
#   sed_files_clean <- list.files(sed_data_path_clean, pattern = "^sed_\\d{4}_clean\\.xlsx$", full.names = TRUE)
#   
#   sed_dfs_clean <- lapply(sed_files_clean, function(f) {
#     year <- stringr::str_extract(basename(f), "\\d{4}")
#     df <- read_xlsx(f)
#     df$Year <- as.integer(year)
#     df$date <- as.Date(df$date, "%d/%m/%Y")
#     df
#   })
#   
#   df <- bind_rows(sed_dfs_clean) |>
#     mutate(station = str_replace(station,
#                                  "Tacobamba - Agua arriba confluencia Pilcomayo - Tacobamba",
#                                  "Tacobamba arriba Pilcomayo")) |>
#     mutate(station = str_replace(station,
#                                  "Pilcomayo - Agua arriba confluencia Pilcomayo - Tacobamba",
#                                  "Pilcomayo arriba Tacobamba"))
#   
#   return(df)
# } # the old stuff to combine the files

### 

# risk_raster = reactive({
#   req(input$main_tab == "Risk Scores Map")   # do nothing unless Map tab active
#   
#   layers = list()
#   cat("\n[risk_raster reactive] Checkboxes on: ",
#       "\n   Water: ", isTRUE(input$risk_water),
#       "\n   Sediment: ", isTRUE(input$risk_sed),
#       "\n   EJI: ", isTRUE(input$risk_eji),
#       "\n   Pop: ", isTRUE(input$risk_pop))
#   
#   r = load_risk_rasters(debug=TRUE)
#   
#   if (isTRUE(input$risk_hq)) {
#     layers[["hq"]] <- r$hq
#   }
#   if (isTRUE(input$risk_vul)) {
#     layers[["vul"]] <- r$vul
#   }
#   if (isTRUE(input$risk_air)) {
#     layers[["air"]] <- r$air
#   }
#   if (isTRUE(input$risk_mining)) {
#     layers[["mining"]] <- r$mining
#   }
#   if (isTRUE(input$risk_pop)) {
#     layers[["pop"]] <- r$pop
#   }
#   
#   if (length(layers) == 0) {
#     cat("\n[risk_raster reactive] No layers detected")
#     return(NULL)
#   } else {
#     cat("\n", length(layers), " layers included in map. Rastering into one map now.")
#   }
#   
#   r_stack <- terra::rast(layers)
#   r_merge = terra::app(r_stack, fun = sum, na.rm = TRUE)
#   rlist = list(merged = r_merge, individuals = layers)
#   cat("\n[risk_raster] returning r_stack and r_merge. Layers in r_merge: ", names(rlist$individuals))
#   return(rlist)
# })

## pca
# output$pca_static <- renderPlot({
#   req(pca_result())
#   
#   res <- pca_result()
#   req(res)
#   
#   df   <- res$df
#   rpca <- res$pca
#   
#   scores   <- as.data.frame(rpca$ind$coord[, 1:2])
#   loadings <- as.data.frame(rpca$var$coord[, 1:2])
#   
#   scores$station <- df$station
#   scores$date    <- df$date
#   scores$media   <- df$media
#   
#   arrow_scale <- 1 # 1.5   # try between 1 and 3
#   
#   circle_scale <- 1.1
#   theta <- seq(0, 2*pi, length.out = 200)
#   circle <- data.frame(
#     Dim.1 = cos(theta)*circle_scale, 
#     Dim.2 = sin(theta)*circle_scale)
#   
#   ggplot() +
#     geom_path(data = circle, aes(Dim.1, Dim.2), color = "grey50") +   # unit circle
#     geom_segment(
#       data = transform(loadings,
#                        Dim.1 = Dim.1 * arrow_scale,
#                        Dim.2 = Dim.2 * arrow_scale),
#       aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2),
#       arrow  = arrow(length = unit(0.25, "cm")),
#       colour = "steelblue4",
#       linewidth = 0.4
#     ) +
#     # labels offset from arrow tips
#     geom_text(
#       data = transform(loadings,
#                        Dim.1 = Dim.1 * arrow_scale * 1.05,
#                        Dim.2 = Dim.2 * arrow_scale * 1.05),
#       aes(x = Dim.1, y = Dim.2, label = rownames(loadings)),
#       colour = "steelblue4",
#       size = 3
#     ) +
#     labs(x = sprintf("1st Dimension (%d%%)", round(rpca$eig[[1,2]],0)), y = sprintf("2nd Dimension (%d%%)", round(rpca$eig[[2,2]],0)))+ #, colour = "Media") +
#     theme_minimal()+
#     coord_fixed(ratio=1)
#   
# })

## numeric columns
# # Columns to exclude from parameter dropdown
# excluded_columns <- c("Decimal Latitude", "Decimal Longitude",
#                       "Latitude Decimal", "Longitude Decimal", 
#                       "Lat_dd", "Long_dd",
#                       "Distance from Bank", "Distance from Shore",
#                       "Clay (%)", "Silt (%)", "Sand (%)",
#                       "0.032 mm - No. 450 (ASTM) (%)",
#                       "0.063 mm - No. 230 (ASTM) (%)",
#                       "0.125 mm - No. 120 (ASTM) (%)",
#                       "0.250 mm - No. 060 (ASTM) (%)",
#                       "0.500 mm - No. 035 (ASTM) (%)",
#                       "1.00 mm - No. 018 (ASTM) (%)",
#                       "2.00 mm - No. 010 (ASTM) (%)",
#                       "Year", "0.016 mm (%)",
#                       "4.75 mm - No. 004 (ASTM) (%)",
#                       "num_unclass",
#                       "num_class_b",
#                       "num_class_c",
#                       "num_class_d")
# 
# possible_columns <- setdiff(names(df), excluded_columns)
# numeric_columns <- possible_columns[sapply(df[possible_columns], is.numeric)]
# 
# numeric_columns