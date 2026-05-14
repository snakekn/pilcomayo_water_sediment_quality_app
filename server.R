

#Define Server
server <- function(input, output, session) {
  message("Starting server.R")

  # ── Language / i18n setup ────────────────────────────────────────────────────
  lang <- reactiveVal("en")

  # tr() — look up a string in the current language
  tr <- function(key) {
    strings <- if (isolate(lang()) == "es") STRINGS_ES else STRINGS_EN
    strings[[key]] %||% paste0("[", key, "]")
  }

  # Send the full translation dictionary to the browser once on session start
  session$onFlushed(function() {
    session$sendCustomMessage("i18n_dict", list(en = STRINGS_EN, es = STRINGS_ES))
  }, once = TRUE)

  # React to language toggle clicks (from www/i18n.js via Shiny.setInputValue)
  observeEvent(input$lang, {
    lang(input$lang)
  })

  # Re-label all translatable Shiny inputs when language changes
  observeEvent(lang(), {
    l <- lang()
    s <- if (l == "es") STRINGS_ES else STRINGS_EN  # active string table

    # Convenience: build a named choices vector from string-table keys and raw values
    ch <- function(keys, vals) setNames(vals, sapply(keys, function(k) s[[k]]))

    # Data Preparation sidebar
    updateCheckboxInput(session, "filter_bolivia_only", label = s[["filter_bolivia"]])
    updateActionButton(session, "reset_filters",        label = s[["reset_filters_btn"]])

    # Data scope (shared across Time Series / Ranking / PCA tabs)
    updateRadioButtons(session, "plot_data_scope",
                       label   = s[["scope_label"]],
                       choices = ch(c("scope_bol", "scope_all"), c("bol", "all")))

    # Time Series tab
    updateSelectInput(session, "ts_standard_mode",
                      label   = s[["ts_standards"]],
                      choices = ch(c("ts_std_all","ts_std_strict","ts_std_none",
                                     "ts_std_bol","ts_std_epa","ts_std_who",
                                     "ts_std_usgs","ts_std_fao"),
                                   c("all","strict","none","bol","epa","who","usgs","fao")))

    # Ranking Plots — Worst Stations
    updateSelectInput(session, "station_plot_media",
                      label   = s[["select_media"]],
                      choices = ch(c("media_all","media_water","media_sed"),
                                   c("all","water","sediment")))
    updateSelectInput(session, "station_plot_fraction",
                      label   = s[["select_fraction"]],
                      choices = ch(c("fraction_all","fraction_total","fraction_dissolved","fraction_suspended"),
                                   c("any","Total","Dissolved","Suspended")))
    updateSelectInput(session, "station_plot_method_parameter",
                      label   = s[["hq_calc_label"]],
                      choices = ch(c("hq_pct95","hq_median","hq_mean","hq_max"),
                                   c("pct95","median","mean","max")))
    updateSelectInput(session, "station_plot_method_temporal",
                      label   = s[["rank_temp_label"]],
                      choices = ch(c("rank_recent","rank_average","hq_max"),
                                   c("recent","average","max")))

    # Ranking Plots — Worst Parameters
    updateSelectInput(session, "param_plot_media",
                      label   = s[["select_media"]],
                      choices = ch(c("media_all","media_water","media_sed"),
                                   c("all","water","sediment")))
    updateSelectInput(session, "param_plot_fraction",
                      label   = s[["select_fraction"]],
                      choices = ch(c("fraction_all","fraction_total","fraction_dissolved","fraction_suspended"),
                                   c("all","Total","Dissolved","Suspended")))
    updateSelectInput(session, "param_plot_method_temporal",
                      label   = s[["rank_temp_label"]],
                      choices = ch(c("rank_recent","rank_average_time","rank_max_year","rank_weighted"),
                                   c("recent","average","max","weighted")))

    # Ranking Plots — Worst Observations
    updateSelectInput(session, "observation_plot_media",
                      label   = s[["select_media"]],
                      choices = ch(c("media_water","media_sed"), c("water","sediment")))

    # Ranking Plots — Worst Sieve Sizes
    updateSelectInput(session, "sieve_plot_method_spatial",
                      label   = s[["combine_sta_label"]],
                      choices = ch(c("hq_pct95","combine_max","combine_median","combine_mean"),
                                   c("pct95","max","median","mean")))
    updateSelectInput(session, "sieve_plot_method_temporal",
                      label   = s[["rank_temp_label"]],
                      choices = ch(c("rank_recent","rank_average_time","rank_max_year","rank_weighted"),
                                   c("recent","average","max","weighted")))
    updateRadioButtons(session, "sieve_plot_method",
                       label   = s[["rank_by_label"]],
                       choices = ch(c("rank_avg_val","rank_max_val"), c("avg","max")))

    # PCA tab
    updateSelectInput(session, "pca_media",
                      label   = s[["select_media"]],
                      choices = ch(c("media_all","media_water","media_sed"),
                                   c("all","water","sediment")))
    updateActionButton(session, "deselect_all_pca", label = s[["pca_clear_btn"]])
    updateActionButton(session, "run_pca",          label = s[["pca_run_btn"]])

    # Risk Map — temporal/aggregation selects
    updateSelectInput(session, "water_temp_ag",
                      label   = s[["risk_temp_label"]],
                      choices = ch(c("risk_temp_recent","risk_temp_average"), c("recent","mean")))
    updateSelectInput(session, "water_param_ag",
                      label   = s[["risk_final_ag_label"]],
                      choices = ch(c("risk_ag_mean","risk_ag_max","risk_ag_pct95"), c("mean","max","pct95")))
    updateSelectInput(session, "water_fraction",
                      label   = s[["select_fraction"]],
                      choices = ch(c("fraction_all","fraction_dissolved","fraction_suspended"),
                                   c("All","Dissolved","Suspended")))
    updateSelectInput(session, "water_bin_method",
                      label   = s[["risk_bin_method"]],
                      choices = ch(c("risk_bin_quantile","risk_bin_equal_area","risk_bin_equal_int"),
                                   c("quantile","equal_area","equal_interval")))
    updateSelectInput(session, "sed_temp_ag",
                      label   = s[["risk_temp_label"]],
                      choices = ch(c("risk_temp_recent","risk_temp_average"), c("recent","mean")))
    updateSelectInput(session, "sed_param_ag",
                      label   = s[["risk_final_ag_label"]],
                      choices = ch(c("risk_ag_mean","risk_ag_max","risk_ag_pct95"), c("mean","max","pct95")))
    updateSelectInput(session, "sed_bin_method",
                      label   = s[["risk_bin_method"]],
                      choices = ch(c("risk_bin_quantile","risk_bin_equal_area","risk_bin_equal_int"),
                                   c("quantile","equal_area","equal_interval")))
    updateSelectInput(session, "eji_bin_method",
                      label   = s[["risk_bin_method"]],
                      choices = ch(c("risk_bin_quantile","risk_bin_equal_int"),
                                   c("quantile","equal_interval")))
    updateSelectInput(session, "pop_bin_method",
                      label   = s[["risk_bin_method"]],
                      choices = ch(c("risk_bin_equal_int","risk_bin_equal_area","risk_bin_jenks"),
                                   c("equal_interval","equal_area","jenks")))
  }, ignoreInit = TRUE)

  #### Prepare master_data ####
  
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

  # Raw (unfiltered) data — always holds the full loaded/uploaded dataset.
  # filtered_water() / filtered_sed() read from these, so there's no circular dependency
  # when the sync observe writes filtered results back into master_data.
  raw_water <- reactiveVal(NULL)
  raw_sed   <- reactiveVal(NULL)
  
  app_initialized = reactiveVal(FALSE) # check if we tried including master_data from prior data
  
  # to print out the new master_data
  observe({
    message("Loading Master Data...")
    
    raw_water(if (file.exists("data/processed/all_water_scored.qs2")) qs_read("data/processed/all_water_scored.qs2") else { print("no all_water_scored.qs2"); tibble() })
    raw_sed(if (file.exists("data/processed/all_sed_scored.qs2")) qs_read("data/processed/all_sed_scored.qs2") else { print("no all_sed_scored.qs2"); tibble() })
    master_data$water_locyear <- if (file.exists("data/processed/all_water_locyear.qs2")) qs_read("data/processed/all_water_locyear.qs2") else { print("no all_water_locyear.qs2"); tibble() }
    master_data$sed_locyear <- if (file.exists("data/processed/all_sed_locyear.qs2")) qs_read("data/processed/all_sed_locyear.qs2") else { print("no all_sed_locyear.qs2"); tibble() }
    master_data$sed_loctime <<- if(file.exists("data/processed/all_sed_loctime.qs2")) qs_read("data/processed/all_sed_loctime.qs2") else { print("no all_sed_loctime.qs2"); tibble() }
    master_data$water_loctime <<- if(file.exists("data/processed/all_water_loctime.qs2")) qs_read("data/processed/all_water_loctime.qs2") else { print("no all_water_loctime.qs2"); tibble() }
    # all_media_scored is derived from water + sed via the filter sync observe below
    master_data$all_media_locyear <<- if(file.exists("data/processed/all_media_locyear.qs2")) qs_read("data/processed/all_media_locyear.qs2") else { print("no all_media_locyear.qs2"); tibble() }
    master_data$all_media_loctime <<- if(file.exists("data/processed/all_media_loctime.qs2")) qs_read("data/processed/all_media_loctime.qs2") else { print("no all_media_loctime.qs2"); tibble() }

    app_initialized(TRUE) # we tried loading the data in
    message("Master Data Loaded.")
    
  }) |> bindEvent(TRUE) # Only run once on startup
  
  # check if we have data available
  output$data_ready <- reactive({
    water_ready <- !is.null(master_data$water_scored) && nrow(master_data$water_scored) > 0
    sed_ready   <- !is.null(master_data$sed_scored) && nrow(master_data$sed_scored) > 0
    all_media_ready = !is.null(master_data$all_media_scored) && nrow(master_data$all_media_scored) > 0
    water_ready || sed_ready || all_media_ready
  })
  outputOptions(output, "data_ready", suspendWhenHidden = FALSE)

  observe({
    message("[DEBUG] Data Update Trigger")
    message(" - Water Scored Rows: ", nrow(master_data$water_scored))
    message(" - Sed Scored Rows: ", nrow(master_data$sed_scored))
  })

  # one-liner wrapper to check whether we have the data required to run a section
  guard_data <- function(
    df,
    cols = c("station", "media", "parameter", "date"),
    notify = TRUE,
    msg = NULL
  ) {
    if (is.null(msg)) msg <- tr("notif_no_data_loaded")
    has_data(
      df = df,
      cols = cols,
      notify = notify,
      msg = msg,
      master_data = master_data,
      session = session
    )
  }

  # send the user to the data prep tab if they select the overlay button
  observeEvent(input$go_data_prep, {
    updateTabsetPanel(session, "main_tab", selected = "data_prep")
  })

  # ── Language-aware Introduction tab markdown ─────────────────────────────────
  output$intro_md <- renderUI({
    includeMarkdown(if (lang() == "es") "text/introduction.es.md" else "text/introduction.md")
  })
  output$intro_sources_md <- renderUI({
    includeMarkdown(if (lang() == "es") "text/introduction_sources.es.md" else "text/introduction_sources.md")
  })
  output$acknowledgements_md <- renderUI({
    includeMarkdown(if (lang() == "es") "text/acknowledgements.es.md" else "text/acknowledgements.md")
  })

  output$water_data_tab <- renderUI({
    has_water <- !is.null(master_data$water_scored) &&
      is.data.frame(master_data$water_scored) &&
      nrow(master_data$water_scored) > 0

    if (!has_water) {
      return(
        div(
          style = "padding: 20px; color: #666;",
          h4(tr("no_water_title")),
          p(tr("no_water_msg"))
        )
      )
    }

    tagList(
      h4(tr("yearly_summary")),
      DT::DTOutput("data_prep_water_table"),
      br(),
      h4(tr("complete_dataset")),
      DT::DTOutput("full_water_table")
    )
  })

  output$sed_data_tab <- renderUI({
    has_sed <- !is.null(master_data$sed_scored) &&
      is.data.frame(master_data$sed_scored) &&
      nrow(master_data$sed_scored) > 0

    if (!has_sed) {
      return(
        div(
          style = "padding: 20px; color: #666;",
          h4(tr("no_sed_title")),
          p(tr("no_sed_msg"))
        )
      )
    }

    tagList(
      h4(tr("yearly_summary")),
      DT::DTOutput("data_prep_sed_table"),
      br(),
      h4(tr("complete_dataset")),
      DT::DTOutput("full_sed_table")
    )
  })

  output$sed_data_ready <- reactive({
    !is.null(raw_sed()) && nrow(raw_sed()) > 0
  })
  outputOptions(output, "sed_data_ready", suspendWhenHidden = FALSE)

  # Combined data ready check — uses raw data so filtering to zero rows doesn't break the UI
  output$map_data_ready <- reactive({
    water_ready <- !is.null(raw_water()) && nrow(raw_water()) > 0
    sed_ready   <- !is.null(raw_sed())   && nrow(raw_sed())   > 0
    water_ready || sed_ready
  })
  outputOptions(output, "map_data_ready", suspendWhenHidden = FALSE)

  # ── Global Data Filters ─────────────────────────────────────────────────────
  # filtered_water / filtered_sed read from raw_water / raw_sed (not master_data),
  # so the sync observe below that writes back to master_data creates no circularity.

  filtered_water <- reactive({
    df <- raw_water()
    if (is.null(df) || nrow(df) == 0) return(df)
    # Bolivia filter runs first so that station choices are always a Bolivia-subset
    if (isTRUE(input$filter_bolivia_only)) {
      df <- filter_to_border(df, "longitude_decimal", "latitude_decimal", bol_border)
    }
    if (!is.null(input$filter_water_dates) && !anyNA(input$filter_water_dates)) {
      df <- df |> filter(date >= input$filter_water_dates[1],
                         date <= input$filter_water_dates[2])
    }
    # Only apply station filter for stations that still exist after Bolivia clip
    valid_stations <- if (isTRUE(input$filter_bolivia_only)) unique(df$station) else NULL
    sel <- input$filter_water_stations
    if (!is.null(sel) && length(sel) > 0) {
      if (!is.null(valid_stations)) sel <- intersect(sel, valid_stations)
      if (length(sel) > 0) df <- df |> filter(station %in% sel)
    }
    df
  })

  filtered_sed <- reactive({
    df <- raw_sed()
    if (is.null(df) || nrow(df) == 0) return(df)
    if (isTRUE(input$filter_bolivia_only)) {
      df <- filter_to_border(df, "longitude_decimal", "latitude_decimal", bol_border)
    }
    if (!is.null(input$filter_sed_dates) && !anyNA(input$filter_sed_dates)) {
      df <- df |> filter(date >= input$filter_sed_dates[1],
                         date <= input$filter_sed_dates[2])
    }
    valid_stations <- if (isTRUE(input$filter_bolivia_only)) unique(df$station) else NULL
    sel <- input$filter_sed_stations
    if (!is.null(sel) && length(sel) > 0) {
      if (!is.null(valid_stations)) sel <- intersect(sel, valid_stations)
      if (length(sel) > 0) df <- df |> filter(station %in% sel)
    }
    df
  })

  filtered_all_media <- reactive({
    w <- filtered_water() %||% tibble()
    s <- filtered_sed()   %||% tibble()
    # sieve_size is NA (logical) in water data, character in sediment — coerce before binding
    if ("sieve_size" %in% names(w)) w$sieve_size <- as.character(w$sieve_size)
    if ("sieve_size" %in% names(s)) s$sieve_size <- as.character(s$sieve_size)
    bind_rows(w, s)
  })

  # Sync filtered data into master_data so every existing consumer is automatically filtered.
  observe({
    master_data$water_scored    <- filtered_water()
    master_data$sed_scored      <- filtered_sed()
    master_data$all_media_scored <- filtered_all_media()
  })

  # Filter UI for Water
  output$filter_water_ui <- renderUI({
    df <- raw_water()
    req(df, nrow(df) > 0)
    dates    <- df$date[!is.na(df$date)]
    stations <- sort(unique(df$station))
    tagList(
      h5(tr("filter_water_sec"), style = "margin: 10px 0 4px 0; font-weight: 600;"),
      dateRangeInput(
        "filter_water_dates", tr("filter_date_label"),
        start = min(dates), end = max(dates),
        min   = min(dates), max = max(dates),
        format = "yyyy-mm-dd", separator = " to "
      ),
      selectizeInput(
        "filter_water_stations", tr("filter_sta_label"),
        choices  = stations,
        selected = NULL,
        multiple = TRUE,
        options  = list(placeholder = tr("filter_sta_ph"))
      )
    )
  })

  # Filter UI for Sediment
  output$filter_sed_ui <- renderUI({
    df <- raw_sed()
    req(df, nrow(df) > 0)
    dates    <- df$date[!is.na(df$date)]
    stations <- sort(unique(df$station))
    tagList(
      h5(tr("filter_sed_sec"), style = "margin: 10px 0 4px 0; font-weight: 600;"),
      dateRangeInput(
        "filter_sed_dates", tr("filter_date_label"),
        start = min(dates), end = max(dates),
        min   = min(dates), max = max(dates),
        format = "yyyy-mm-dd", separator = " to "
      ),
      selectizeInput(
        "filter_sed_stations", tr("filter_sta_label"),
        choices  = stations,
        selected = NULL,
        multiple = TRUE,
        options  = list(placeholder = tr("filter_sta_ph"))
      )
    )
  })

  # Filter status badge shown under the filter controls
  output$filter_status_ui <- renderUI({
    n_raw_w  <- nrow(raw_water()  %||% tibble())
    n_filt_w <- nrow(filtered_water() %||% tibble())
    n_raw_s  <- nrow(raw_sed()    %||% tibble())
    n_filt_s <- nrow(filtered_sed()   %||% tibble())

    water_active <- n_filt_w < n_raw_w
    sed_active   <- n_filt_s < n_raw_s

    if (!water_active && !sed_active) {
      p(tr("filter_no_active"),
        style = "color: #666; font-size: 12px; margin: 6px 0 0 0;")
    } else {
      div(
        style = "color: #c0392b; font-size: 12px; font-weight: 600; margin: 6px 0 0 0;",
        if (water_active) p(sprintf(tr("filter_water_rows"), n_filt_w, n_raw_w),
                            style = "margin: 2px 0;"),
        if (sed_active)   p(sprintf(tr("filter_sed_rows"), n_filt_s, n_raw_s),
                            style = "margin: 2px 0;")
      )
    }
  })

  # Reset all filters back to full date range + no station selection
  observeEvent(input$reset_filters, {
    updateCheckboxInput(session, "filter_bolivia_only", value = FALSE)
    df_w <- raw_water()
    df_s <- raw_sed()
    if (!is.null(df_w) && nrow(df_w) > 0) {
      dates_w <- df_w$date[!is.na(df_w$date)]
      updateDateRangeInput(session, "filter_water_dates",
                           start = min(dates_w), end = max(dates_w))
      updateSelectizeInput(session, "filter_water_stations", selected = character(0))
    }
    if (!is.null(df_s) && nrow(df_s) > 0) {
      dates_s <- df_s$date[!is.na(df_s$date)]
      updateDateRangeInput(session, "filter_sed_dates",
                           start = min(dates_s), end = max(dates_s))
      updateSelectizeInput(session, "filter_sed_stations", selected = character(0))
    }
  })
  # When Bolivia-only checkbox toggles, restrict (or restore) station picker choices
  observeEvent(input$filter_bolivia_only, {
    df_w <- raw_water()
    df_s <- raw_sed()

    if (isTRUE(input$filter_bolivia_only)) {
      if (!is.null(df_w) && nrow(df_w) > 0) {
        bol_w   <- filter_to_border(df_w, "longitude_decimal", "latitude_decimal", bol_border)
        choices <- sort(unique(bol_w$station))
        updateSelectizeInput(session, "filter_water_stations",
                             choices  = choices,
                             selected = intersect(input$filter_water_stations, choices))
      }
      if (!is.null(df_s) && nrow(df_s) > 0) {
        bol_s   <- filter_to_border(df_s, "longitude_decimal", "latitude_decimal", bol_border)
        choices <- sort(unique(bol_s$station))
        updateSelectizeInput(session, "filter_sed_stations",
                             choices  = choices,
                             selected = intersect(input$filter_sed_stations, choices))
      }
    } else {
      if (!is.null(df_w) && nrow(df_w) > 0) {
        updateSelectizeInput(session, "filter_water_stations",
                             choices  = sort(unique(df_w$station)),
                             selected = input$filter_water_stations)
      }
      if (!is.null(df_s) && nrow(df_s) > 0) {
        updateSelectizeInput(session, "filter_sed_stations",
                             choices  = sort(unique(df_s$station)),
                             selected = input$filter_sed_stations)
      }
    }
  })

  # ── End Global Data Filters ──────────────────────────────────────────────────

  # Unified campaign/date range UI that works for both
  output$map_campaign_ui <- renderUI({
    cat("\n=== DEBUG map_campaign_ui ===\n")
    
    # Get data based on selected media
    df <- if (!is.null(input$plot_media) && input$plot_media == "water") {
      req(guard_data(master_data$water_scored))
      master_data$water_scored
    } else {
      req(guard_data(master_data$sed_scored))
      master_data$sed_scored
    }
    
    req(nrow(df) > 0)
    cat("  Using", input$plot_media, "data with", nrow(df), "rows\n")
    
    # Determine date column
    date_col <- intersect(c("date", "Date"), names(df))[1]
    if (is.na(date_col)) return(p("No date column found"))
    if (is.na(date_col)) return(p("No date column found"))
    
    dates <- df[[date_col]]
    dates <- dates[!is.na(dates)]
    
    if (length(dates) == 0) {
      return(p("No date data available"))
    }
    
    if (!inherits(dates, "date")) {
      dates <- as.Date(dates)
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
  
  #### Managing Data Uploads ####
  
  ### Remove notification that data is required if it's ready
  observe({
    ready <- !is.null(master_data$all_media_scored) &&
      nrow(master_data$all_media_scored) > 0
    
    if (ready) {
      removeNotification("data_status")
    }
  })
  
  ### Add notification for startup on lacking data
  observe({
    if (app_has_data(master_data)) {
      removeNotification("app_data_status")
    } else {
      showNotification(
        tr("notif_no_data_loaded"),
        id = "app_data_status",
        type = "message",
        duration = NULL,
        closeButton = FALSE
      )
    }
  })
  
  # Upload button
  upload_result <- dataUploadServer(
    id        = "upload_data",
    base_data = initial_water,
    master_data = master_data,
    raw_water = raw_water,
    raw_sed   = raw_sed,
    lang      = lang
  )
  
  # save upload to master_data for all downstream app components
  observe({
    result <- upload_result$parsed()
    req(result)
    
    if (result$media == "water") {
      raw_water(result$scored)
      master_data$water_locyear <- result$locyear
    } else { # assuming it's sediment
      raw_sed(result$scored)
      master_data$sed_locyear <- result$locyear
    }
    # all_media_scored is automatically rebuilt by the filter sync observe
    message("Upload applied: ", result$media, " — ", nrow(result$scored), " rows")
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
    req(guard_data(master_data$water_scored))
    return(master_data$water_scored) # skip all the other combining work...
    
  })
  
  
  # Only points in Bolivia
  bol_water_clean <- reactive({
    cat("all_water_clean() in bol_water_clean")
    filter_to_border(master_data$water_scored, "longitude_decimal", "latitude_decimal", bol_border)
  })
  
  
  # Read and combine water data (1333 version)
  all_water_1333 <- reactive({
    ## Nadav's Note: there are a bunch of these data manipulation functions. We want to:
    ### 1. All data (which can be in several formats) goes thru a unified function, where their media is considered and all relevant standards are calculated
    ### 2. Data is all compiled into one dataset that can be loaded in any part of the app
    ### 3. Uploaded data is added to this dataset 
    ### 4. All maps showing data use this single dataset
    ### 5. Users can still download pre-loaded datasets, or filter as needed
    
    req(guard_data(master_data$water_scored))
    return(master_data$water_scored) 
  })
  
  
  # Only points in Bolivia
  bol_water_1333 <- reactive({
    cat("all_water_clean in bol_water_1333")
    filter_to_border(isolate(all_water_clean()), "longitude_decimal", "latitude_decimal", bol_border)
  })
  
  ## Load sediment data ##
  all_sed_clean <- reactive({
    return(master_data$sed_scored)
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
      df$date <- as.Date(df$date, "%d/%m/%Y")
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
  
  
  #### Get list of years in the data ####
  
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
  
  ################# PCA #########################
  
  numeric_columns <- reactive({
    df <- master_data$all_media_scored
    possible = setdiff(names(df), EXCLUDED_COLS)
    possible[sapply(df[possible],is.numeric)]
  })
  
  observe({
    df = master_data$all_media_scored
    req(guard_data(df), input$pca_media)
    
    
    updateSelectizeInput(inputId = "pca_parameters",
                         choices = get_param_list(df, media_type = input$pca_media),
                         selected = NULL)
    updateSelectizeInput(inputId = "pca_station",
                         choices = c("All Stations" = "all", unique(df$station)),
                         selected = "all")
  })
  
  observeEvent(input$deselect_all_pca, {
    updateSelectizeInput(session, "pca_parameters", selected = character(0))
  })
  
  pca_result <- eventReactive(input$run_pca, {
    calc_pca(data = master_data$all_media_scored, 
             params = input$pca_parameters, 
             media_selection = input$pca_media, 
             station_selection = input$pca_station,
             draw_circle = TRUE)
  })
  
  output$pca_plot <- renderPlotly({
    res <- pca_result()
    req(res)
    
    df   <- res$df
    rpca <- res$pca
    
    # message("[pca_plot output] dimensional values")
    # message(rpca$eig)
    
    scores   <- as.data.frame(rpca$ind$coord[, 1:2])
    loadings <- as.data.frame(rpca$var$coord[, 1:2])
    
    scores$station <- df$station
    scores$date    <- df$date
    scores$media   <- df$media
    
    arrow_scale <- 1 # 1.5   # try between 1 and 3
    
    theta  <- seq(0, 2*pi, length.out = 200)
    circle <- data.frame(x = cos(theta), y = sin(theta))
    
    
    g <- ggplot() +
      # points
      # geom_point(
      #   data = scores,
      #   aes(
      #     x = Dim.1, y = Dim.2,
      #     colour = media,
      #     text   = paste(
      #       "Station:", station,
      #       "<br>Date:", date,
      #       "<br>Media:", media
      #     )
      #   ),
      #   alpha = 0.8, size = 2.5
      # ) +
      # arrows (scaled up a bit)
      geom_path(data = circle, aes(x, y), color = "grey50") +   # unit circle
      
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
      labs(x = sprintf("1st Dimension (%d%%)", round(rpca$eig[[1,2]],0)), y = sprintf("2nd Dimension (%d%%)", round(rpca$eig[[2,2]],0)))+ #, colour = "Media") +
      theme_minimal()+
      coord_fixed(ratio=1)
    
    # Convert to plotly – this is what makes the plot appear
    plotly::ggplotly(g, tooltip = "text")
  })
  
  # Scree plot
  output$scree_plot <- renderPlot({
    res <- pca_result()
    req(res)
    
    p <- fviz_screeplot(res$pca, addlabels = TRUE)
    
    p + theme_minimal(base_size = 16) +
      theme(
        text = element_text(size = 16),          # all text
        axis.title = element_text(size = 18),    # axis labels
        axis.text = element_text(size = 14),     # tick numbers
        # axis.text.x = element_text(angle = 45, hjust = 1),  # rotate if crowded
        plot.margin = margin(20, 20, 20, 20)     # bigger margins
      )
  })
  

  ##### Risk Map #####
  
  ### set reactives ###
  
  combined_risk_raster = reactiveVal(NULL)
  
  ## set click location for all to use
  click_location <- reactive({
    req(input$main_tab == "Risk Scores Map")
    
    click  <- input$risk_map_click
    if (is.null(click)) return(list(lat = NA_real_, lng = NA_real_))
    list(lat = click$lat, lng = click$lng)
  })
  
  ## get raster data based on click
  click_data <- reactive({
    req(input$main_tab == "Risk Scores Map")
    
    loc    <- click_location()
    layers <- risk_individuals()   # water, sediment, eji, pop — never combined
    
    default <- list(values = numeric(0), score = NA_real_, layer_keys = character(0))
    if (is.na(loc$lat)) return(default)
    
    point <- terra::vect(matrix(c(loc$lng, loc$lat), ncol = 2), crs = "EPSG:4326")
    
    extract_val <- function(r) {
      if (is.null(r) || !inherits(r, "SpatRaster")) return(NA_real_)
      point_proj <- if (terra::same.crs(r, "EPSG:4326")) point else terra::project(point, terra::crs(r))
      val <- tryCatch(terra::extract(r, point_proj, ID = FALSE)[1, 1], error = function(e) NA_real_)
      if (is.finite(val)) round(val, 1) else NA_real_
    }
    
    # Always extract individual layer values for squares
    values_vec <- setNames(rep(NA_real_, length(layers)), names(layers))
    for (layer_name in names(layers)) {
      values_vec[layer_name] <- extract_val(layers[[layer_name]])
    }
    
    # Composite score: combined raster if active, otherwise NA (no summing)
    score <- if (isTRUE(input$risk_combined)) {
      r_combined <- tryCatch(combined_risk_raster(), error = function(e) NULL)
      extract_val(r_combined)
    } else {
      NA_real_
    }
    
    list(
      values     = unname(values_vec),
      score      = score,
      layer_keys = names(layers)
    )
  })  
  ## get individual layer values
  risk_individuals <- reactive({
    req(input$main_tab == "Risk Scores Map")
    layers <- list()
    
    # Water (checkbox: risk_water)
    if (isTRUE(input$risk_water)) {
      r <- water_raster_display()
      if (!is.null(r)) layers$water <- r
    }
    
    # Sediment (checkbox: risk_sediment)  
    if (isTRUE(input$risk_sediment)) {
      r <- sediment_raster_display()
      if (!is.null(r)) layers$sediment <- r
    }
    
    # EJI (checkbox: risk_eji)
    if (isTRUE(input$risk_eji)) {
      r <- eji_raster_binned()
      if (!is.null(r)) layers$eji <- r
    }
    
    # Population (checkbox: risk_pop_density)
    if (isTRUE(input$risk_pop_density) && exists("pop_raster")) {
      cat("pop_bin_breaks:", pop_bin_breaks(), "\n")
      pop_binned <- bin_raster(pop_raster, pop_bin_breaks())  # your existing logic
      layers$pop <- pop_binned
    }
    
    layers  # returns list of available rasters
  })
  
  # render the output map and specify drawing order
  output$risk_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark,   group = "Dark") %>%
      addProviderTiles(providers$Esri.WorldImagery,   group = "Satellite") %>%
      addMapPane("polygonPane", zIndex = 405) %>% # EJI polygons go below rasters but above basemap
      addMapPane("rasterPane1", zIndex = 410) %>% #  pop raster goes above EJI
      addMapPane("rasterPane2", zIndex = 415) %>% # risk rasters go above pop density
      addMapPane("polygonPane2", zIndex = 420) %>% # tailings are above rasters
      addMapPane("polylinePane", zIndex = 425) %>% # polylines on top of polygons and rasters
      addMapPane("pointPane", zIndex = 430) %>% # points on top of everything
      addLayersControl(
        baseGroups = c("Light", "Dark", "Satellite"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(lat=-20.5, lng=-65.3, zoom=7.2) |>
      addMeasure(
        position = "topright",
        primaryLengthUnit = "kilometers",
        secondaryLengthUnit = "meters",
        primaryAreaUnit = "sqkilometers"
      )
  })
  
  # observe({
  #   req(input$main_tab == "Risk Scores Map")
  #   
  #   # r <- risk_raster()
  #   
  #   # Silently do nothing if no layers selected — don't stop(), just return
  #   if (is.null(r) || is.null(r$merged)) return()
  #   
  #   proxy <- leafletProxy("risk_map") |> clearImages()
  #   
  #   pal <- colorNumeric("viridis", terra::values(r$merged), na.color = "transparent")
  #   
  #   proxy |>
  #     addRasterImage(r$merged, colors = pal, opacity = 0.7, 
  #                    layerId = "risk_merged", project = FALSE)
  # })
  
  # ── Risk map: create-layer buttons ────────────────────────────
  
  output$risk_sidebar <- renderUI({
    loc  <- click_location()   # lat/lng only
    data <- click_data()       # values + score + layer names
    
    layer_display_names <- c(
      "combined" = "Combined Risk",
      "water"    = "Water Risk",
      "sediment" = "Sediment Risk",
      "eji"      = "EJI Vulnerability",
      "pop"      = "Population Density"
    )
    
    tagList(
      # Composite score
      if (!is.na(data$score) && length(data$values) > 0) {
        div(class = "composite-score",
            "Composite Score: ",
            span(style = "font-size: 28px;", data$score)
        )
      },
      
      # One square per active layer
      div(class = "risk-squares",
          lapply(seq_along(data$values), function(i) {
            key   <- data$layer_keys[i]   # e.g. "eji"
            score <- ifelse(is.na(data$values[i]), "-", data$values[i])
            div(class = "risk-square-container",
                div(class = "risk-label", layer_display_names[[key]]), # name of the layer
                div(class = paste0("risk-square risk", i), score) # score
            )
          })
      ),
      
      # Coords
      div(id = "risk-coords",
          strong("Clicked Location:"), br(),
          paste0(
            "Lat: ",  ifelse(is.na(loc$lat), "-", round(loc$lat, 6)),
            ", Lng: ", ifelse(is.na(loc$lng), "-", round(loc$lng, 6))
          )
      )
    )
  })  
  # Initialize empty sidebar
  outputOptions(output, "risk_map", suspendWhenHidden = FALSE)
  
  ################# RANKING PLOTS ############################
  
  # Compute available years
  observe({
    df <- master_data$all_media_scored
    req(guard_data(df))
    
    years <- sort(unique(df$year))  # or year(df$date)
    if (length(years) == 0) return()
    
    # Default: last 5 years excluding most recent
    max_yr <- max(years)
    default_min <- max_yr - 6  # 5 years back from year-before-last
    default_range <- c(max(default_min, min(years)), max_yr - 1)
    
    updateSliderInput(session, "param_plot_recent_years",
                      min = min(years), max = max(years),
                      value = default_range
    )
    updateSliderInput(session, "station_plot_recent_years",
                      min = min(years), max = max(years),
                      value = default_range
    )
    updateSliderInput(session, "sieve_plot_recent_years",
                      min = min(years), max = max(years),
                      value = default_range
    )
    updateSliderInput(session, "observation_plot_recent_years",
                      min = min(years), max = max(years),
                      value = default_range
    )
  })
  
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
    grep(" Class$", colnames(master_data$all_media_scored), value = TRUE)
  })
  
  observe({
    df = master_data$all_media_scored
    req(guard_data(df))
    
    #View(stds)
    param_list = get_param_list(df, need_std = TRUE, need_hq=TRUE) # only show those with HQ
    #View(param_list)
    
    updateSelectInput(inputId = "station_plot_param",
                      choices = c("All", param_list),
                      selected = "All")
    
    updateSelectInput(inputId = "observation_plot_param",
                      choices = param_list,
                      selected = "All")
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
    req(df, guard_data(df))
    
    params_list = get_param_list(df)
    # cat("\n\n", params_list,"\n")

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
    media = input$observation_plot_media
    range = input$observation_plot_recent_years
    req(param, media)
    
    df = master_data$all_media_scored
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    df = df |>
      filter(!is.na(HQ),
             parameter == input$observation_plot_param,
             media == input$observation_plot_media)
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    if (input$observation_plot_media == "water") {
      df = df |> 
        filter(fraction == "Total")
    }
    
    # if(!is.null(input$observation_plot_recent_years)) {
    #   df = df |>
    #     filter(year <= range[2], year >= range[1])
    # }
    
    return(df)
    ###
    
    # 
    # # En
    # # Compute HQ-based water score per observation
    # out <- df %>%
    #   group_by(station, date) %>%
    #   summarise(
    #     water_score = mean(HQ, na.rm = TRUE),   # <- NEW HQ-based score
    #     max_HQ      = max(HQ, na.rm = TRUE),    # optional
    #     n_params    = sum(!is.na(HQ)),
    #     .groups = "drop"
    #   ) %>%
    #   filter(is.finite(water_score))
    # # View(out)
    # return(out)
  })
  
  
  
  output$observation_scores_ui <- renderUI({
    df <- observation_scores()
    
    if (is.null(df) || nrow(df) == 0) {
      return(no_data_callout(input$observation_plot_media))
    }
    
    # otherwise return the plot output placeholder
    plotlyOutput("observation_scores_plot", height = "500px")
  })
  
  
  output$observation_scores_plot <- renderPlotly({
    df = observation_scores()

    req(input$observation_plot_media, 
      input$observation_plot_param, 
      # input$observation_plot_station, 
      df)

    ### for adding a year range filter
    # calculate date range
    nyears = 0 # null case - function won't consider it if we're not using recent
    
    # # calculate date range
    # if(input$observation_plot_method_temporal == "recent") {
    #   req(input$observation_plot_recent_years)
    #   range = input$observation_plot_recent_years
    #   nyears = if(!is.null(input$param_plot_recent_years)) {
    #     range[2] - range[1]
    #   } else {
    #     0
    #   }
    #   
    #   df = df |>
    #     filter(year <= range[2])
    #   
    #   validate(
    #     need(nrow(data) > 0, "No data available for ranking.")
    #   )
    # }
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    p <- df |>
      slice_max(HQ, n = 15, with_ties = FALSE) |>
      mutate(label = paste0(station, " (", date, ")"),
             label = fct_reorder(label, HQ))

    title_label = sprintf("Top %g HQs Observed of %s in %s", nrow(p), input$observation_plot_param, input$observation_plot_media)
    # View(p)
    p = p |>
      ggplot(aes(x = label, y = HQ, 
                 text = paste("Hazard Quotient:", round(HQ, 2)))) +
      geom_col(fill = "darkslateblue") +
      coord_flip() +
      theme_minimal() +
      labs(
        title = title_label, 
        x = NULL, y = "Hazard Quotient"
      )
      quiet_plotly(p, tooltip = "text")
    }) 
  
  # Calculate max date for recency weighting
  max_date <- reactive({
    max(master_data$all_media_scored$date, na.rm = TRUE)
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
  
  ### To get the # of recent years to review
  # Reactive that counts unique years
  all_available_years <- reactive({
    df <- master_data$all_media_scored  # or your data reactive
    req(df, guard_data(df))
    
    year_min = min(df$year, na.rm=TRUE)
    year_max = max(df$year, na.rm=TRUE)
    
    c(year_min, year_max)
  })
  
  # Update slider max when data changes
  observeEvent(all_available_years(), {
    years = all_available_years()
    default_max = ifelse(2024 <= years[2], 2023, years[2])
    default_min = ifelse(default_max - 5 > years[1], default_max-5, years[1])
    
    # observation plot
    updateSliderInput(
      session,
      "observation_plot_recent_years",
      min = years[1], max = years[2],
      value = c(default_min, default_max)
    )
    # param plot
    updateSliderInput(
      session,
      "param_plot_recent_years",
      min = years[1], max = years[2],
      value = c(default_min, default_max)
    )
    # station plot
    updateSliderInput(
      session,
      "station_plot_recent_years",
      min = years[1], max = years[2],
      value = c(default_min, default_max)
    )
    # sieve plot
    updateSliderInput(
      session,
      "sieve_plot_recent_years",
      min = years[1], max = years[2],
      value = c(default_min, default_max)
    )
  })
  
  # Use it in your reactive/plot
  output$filtered_data <- renderTable({
    n_years <- input$years_to_show
    req(n_years)
    
    df <- master_data$all_media_scored
    req(guard_data(df))
    
    recent_years <- tail(sort(unique(year(df$date))), n_years)
    df |> filter(year(date) %in% recent_years)
  })
  
  
  output$station_scores_plot <- renderPlotly({
    req(input$station_plot_param,
        input$station_plot_media,
        input$station_plot_method_parameter, 
        input$station_plot_method_temporal, 
        guard_data(master_data$all_media_scored))
    
    data = master_data$all_media_scored
    nyears = 0 # null case - function won't consider it if we're not using recent
    
    # calculate date range
    if(input$station_plot_method_temporal == "recent") {
      req(input$station_plot_recent_years)
      range = input$station_plot_recent_years
      nyears = if(!is.null(input$station_plot_recent_years)) {
        range[2] - range[1]
      } else {
        0
      }
      
      data = data |>
        filter(year <= range[2])
      
      validate(
        need(nrow(data) > 0, "No data available for ranking.")
      )
    }
    
    # 1/8/2026: Update to utilize new param names
    p = plot_top_hq_stations(data, 
                             media = input$station_plot_media, 
                             param = input$station_plot_param, 
                             fraction = input$station_plot_fraction, 
                             temporal_aggregation = input$station_plot_method_temporal,
                             param_aggregation = input$station_plot_method_parameter,
                             recent_range = nyears)
    quiet_plotly(p, tooltip = "text")
  })
  
  observe({
    df <- master_data$water_scored
    stations <- sort(unique(df$station))
    updateSelectInput(inputId = "param_plot_station",
                      choices = c("All Stations", stations))
  })
  
  active_water_1333_param_plot <- reactive({
    df <- master_data$water_scored
    
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
    req(guard_data(master_data$all_media_scored),
        input$param_plot_station, 
        input$param_plot_media, 
        input$param_plot_method_temporal)
    
    data = master_data$all_media_scored
    nyears = 0 # null case - function won't consider it if we're not using recent
    
    # calculate date range
    if(input$param_plot_method_temporal == "recent") {
      req(input$param_plot_recent_years)
      range = input$param_plot_recent_years
      nyears = if(!is.null(input$param_plot_recent_years)) {
        range[2] - range[1]
      } else {
        0
      }
      
      data = data |>
        filter(year <= range[2])
      
      validate(
        need(nrow(data) > 0, "No data available for ranking.")
      )
    }

    # 1/8/2026: Update to utilize new param names
    p = plot_top_hq_params(data = data, 
                           media_type = input$param_plot_media,
                           fraction = input$param_plot_fraction,
                           station = input$param_plot_station,
                           temporal_aggregation = input$param_plot_method_temporal,
                           # spatial_aggregation = input$param_plot_method_spatial,
                           decay_per_day = input$param_plot_decay,
                           recent_range = nyears)
    
    quiet_plotly(p, tooltip = "text")
  })
  
  
  
  # Nadav's Notes: Next to do. Will want to set up in a separate file like the others
  output$sieve_scores_plot <- renderPlotly({
    req(input$sieve_plot_method, 
        input$sieve_plot_param, 
        input$sieve_plot_station, 
        guard_data(master_data$sed_scored))
    
    data = master_data$sed_scored
    nyears = 0 # null case - function won't consider it if we're not using recent
    
    # calculate date range
    if(input$sieve_plot_method_temporal == "recent") {
      req(input$sieve_plot_recent_years)
      range = input$sieve_plot_recent_years
      nyears = if(!is.null(input$sieve_plot_recent_years)) {
        range[2] - range[1]
      } else {
        0
      }
      
      data = data |>
        filter(year <= range[2])
      
      validate(
        need(nrow(data) > 0, "No data available for ranking.")
      )
    }
    # calculate date range
    range = input$sieve_plot_recent_years
    nyears = if(!is.null(input$sieve_plot_recent_years)) {
      range[2] - range[1] 
    } else {
      0
    }
    
    data = master_data$all_media_scored |>
      filter(year <= range[2])
    
    p = plot_top_hq_sieve(data = data,
                          param_selection = input$sieve_plot_param,
                          param_aggregation = input$sieve_plot_method_spatial,
                          station_selection = input$sieve_plot_station,
                          temporal_aggregation = input$sieve_plot_method_temporal,
                          recent_range = nyears
                          )
    quiet_plotly(p, tooltip = "text")
  })
  
  param_plot_year_range <- year_range_slider_server(
    id = "param_plot_recent_years",
    data = reactive(master_data$all_media_scored),
    year_col = "year"
  )
  
  filtered_param_data <- reactive({
    yrs <- param_plot_year_range()  # returns c(min_year, max_year)
    req(yrs)
    
    master_data$all_media_scored |> 
      filter(year(date) >= yrs[1], year(date) <= yrs[2])  # adjust date/year col
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
    req(guard_data(df))
    
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
  
  # dynamically update choices for time series station & parameters
  observeEvent(input$main_tab, {
    if(input$main_tab != "Time Series") {
      return()
    }
    
    df = master_data$all_media_scored
    req(guard_data(df))
    cat("\nall_media_scored observer triggered")
    
    cat("\nall_media_scored class: ", class(master_data$all_media_scored)[1])
    
    cat("\nall_media_scored nrows: ", nrow(master_data$all_media_scored))
    
    cat("\n=== DEBUG observe ts_tabs ===\n")
    
    ## prepare the station list
    updateSelectInput(session, "ts_station", choices = c("All Stations" = "all", sort(unique(df$station)))) # , selected = "All Stations")
    
    ## prepare the parameter list
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
    
    # cat("\n\nnames in df", names(df), "\n\n\n")
    
    param_cols <- df |>
      filter(!tolower(parameter) %in% tolower(exclude_cols)) |>        # remove metadata rows
      mutate(concentration = suppressWarnings(as.numeric(concentration))) |>
      filter(!is.na(concentration)) |>                         # keep real numeric params
      pull(parameter) |>
      unique() |>
      sort()
    
    updateSelectInput(session, "ts_param", choices = param_cols)
    
    ## prepare the standards list
    stds_selectable = c("No Standards Found")
    param_selected = input$ts_param
    if (!is.null(param_selected)) {
      stds_options = strict_stds |>
        filter(parameter == param_selected,
               media %in% c("water", "drinking water", "sediment")) |>
        pull(regulator) |>
        unique() |>
        sort()
      
      stds_selectable = {
        if(length(stds_options)>0) {
          c("All", "Strict", stds_options)
        } 
      }
      updateSelectInput(session, "ts_standard_mode", choices = stds_selectable)
    }
  })
  
  # observeEvent(input$ts_station, {
  #   cat("\n\nts_station changed to: ", input$ts_station)
  # })
  # 
  # observeEvent(input$ts_param, {
  #   cat("\n\nts_param changed to: ", input$ts_param)
  # })
  # 
  ts_filtered_data_water <- reactive({
    df = master_data$water_scored
    cat("\n\nnrows master_data", nrow(df))
    req(guard_data(df))
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
  
  output$ts_plot_water <- renderUI({
    message("\nIN TS_PLOT_WATER")
    # Unwrap the reactive data with ()
    data_df <- master_data$water_scored
    
    # # DIAGNOSTIC: What exactly is this?
    # message("\n[DIAGNOSTIC] ts_filtered_data_water() output:")
    # message("Class: ", paste(class(data_df), collapse = ", "))
    # message("Is data.frame? ", is.data.frame(data_df))
    # message("Is tibble? ", inherits(data_df, "tbl"))
    # message("Is NULL? ", is.null(data_df))
    # 
    # if (!is.null(data_df)) {
    #   message("Nrows: ", nrow(data_df))
    #   message("Columns: ", paste(colnames(data_df), collapse = ", "))
    #   message("First few rows:")
    #   print(head(data_df))
    # }
    
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
  })|> bindCache(master_data$water_scored, input$ts_station, input$ts_param, input$ts_standard_mode)
  output$ts_plot_sed <- renderUI({
    message("\nIN TS_PLOT_SED")
    # Unwrap the reactive data with ()
    data_df <- master_data$sed_scored
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
    
    return(p)
  })  

  
  ##############  MAPS  #############################

  # Dynamically populate year choices for map
  observe({
    sed_years <- sed_years_usgs()
    updateSelectInput(session, "sed_year", choices = sed_years, selected = max(sed_years))
  })
  
  # observe({
  #   water_years <- water_years_1333()
  #   updateSelectInput(session, "water_year", choices = water_years, selected = max(water_years))
  # })
  # 
  # Load selected dataset for map
  # sed_selected_data <- reactive({
  #   req(master_data$sed_scored, input$sed_date_range)
  #   req(nrow(master_data$sed_scored) > 0)  # Make sure it has data
  #   
  #   master_data$sed_scored |>
  #     filter(date >= input$sed_date_range[1],
  #            date <= input$sed_date_range[2])
  # })
  
  # water_selected_data <- reactive({
  #   req(master_data$water_scored, input$water_date_range)
  #   req(nrow(master_data$water_scored) > 0)  # Make sure it has data
  #   
  #   master_data$sed_scored |>
  #     filter(date >= input$water_date_range[1],
  #            date <= input$water_date_range[2])
  # })
  
  # Populate campaign dropdown for map
  output$sed_campaign_ui <- renderUI({
    # checks that our data works as expected
    req(guard_data(master_data$sed_scored))

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
    df = master_data$water_scored 
    req(guard_data(df))

    date_col <- if ("date" %in% names(df)) "date" else "date"
    dates <- df[[date_col]]
    dates <- dates[!is.na(dates)]
    
    if (!inherits(dates, "date")) {
      dates <- as.Date(dates)
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
    df = master_data$sed_scored
    req(guard_data(df))

    tamiz <- unique(df$sieve_size)
    tamiz <- tamiz[!is.na(tamiz)]
    tamiz <- sort(tamiz)
    
    tamiz_choices = c("All" = "All", setNames(tamiz, tamiz))
    selectInput("tamiz", "Select Sieve Size:", choices = tamiz_choices, selected = "All")
  })
  
  # Populate metal dropdown dynamically for map
  observe({
    df = master_data$sed_scored
    req(guard_data(df))
    
    # Get unique parameters from the long-format data
    sed_params <- unique(df$parameter)
    sed_params <- sed_params[!is.na(sed_params)]
    
    # Filter out parameters that start with a digit (sieve sizes)
    sed_params <- sed_params[!grepl("^\\d", sed_params)]
    sed_params <- sort(sed_params)
    # cat("Available sediment parameters:", paste(sed_params, collapse = ", "), "\n")
    
    updateSelectInput(session, "sed_metal", 
                      choices = sed_params,
                      selected = if(length(sed_params) > 0) sed_params[1] else NULL)
  })
  
  observe({
    # cat("\n=== DEBUG new water param dropdown filler based on existing sed_data ===\n")
    df = master_data$water_scored
    req(guard_data(df))
    
    # Get unique parameters from the long-format data
    water_params <- unique(df$parameter)
    water_params <- water_params[!is.na(water_params)]
    water_params <- sort(water_params)
    
    # cat("Available water parameters:", paste(water_params, collapse = ", "), "\n")
    
    updateSelectInput(session, "water_metal", 
                      choices = water_params,
                      selected = if(length(water_params) > 0) water_params[1] else NULL)
  })
  
  # for updatin the water parameter select option
  # Server
  observe({
    df = master_data$water_scored
    req(guard_data(df))
    
    water_params <- sort(unique(na.omit(df$parameter)))
    
    choices <- c(
      "All Parameters" = "all",
      "All Metals" = "metals",
      "All Nutrients" = "nutrients",
      setNames(water_params, water_params)
    )
    
    updateSelectizeInput(session, "water_params",
                         choices = choices,
                         selected = "all",
                         server = TRUE
    )
  })
  
  observeEvent(input$water_params, {
    selected <- input$water_params
    
    # If nothing selected, revert to "all"
    if (length(selected) == 0) {
      updateSelectizeInput(session, "water_params", selected = "all")
      return()
    }
    
    # Preset options that are mutually exclusive with everything else
    presets <- c("all", "metals", "nutrients")
    
    selected_presets <- intersect(selected, presets)
    selected_individual <- setdiff(selected, presets)
    
    # If user just added a preset, clear everything else and keep only that preset
    if (length(selected_presets) > 0 && length(selected_individual) > 0) {
      # Figure out which preset was most recently added
      # by checking what's new — keep only presets, drop individuals
      updateSelectizeInput(session, "water_params", selected = selected_presets[length(selected_presets)])
      return()
    }
    
    # If multiple presets selected, keep only the most recent one
    if (length(selected_presets) > 1) {
      updateSelectizeInput(session, "water_params", selected = selected_presets[length(selected_presets)])
      return()
    }
  })
  
  # Populate parameter choices for water and sediment risk map creation
  output$water_params_ui <- renderUI({
    df = master_data$water_scored
    req(guard_data(df))
    
    water_params <- sort(unique(na.omit(df$parameter)))
    
    choices <- c(
      "All Parameters" = "all",
      "All Metals" = "metals", 
      "All Nutrients" = "nutrients",
      setNames(water_params, water_params)
    )
    
    selectizeInput("water_params", "Parameter(s):",
                   choices = choices,
                   selected = "all",
                   multiple = TRUE,
                   options = list(
                     optgroups = list(
                       list(value = "presets", label = "— Presets —"),
                       list(value = "individual", label = "— Individual Parameters —")
                     ),
                     optgroupField = "group",
                     options = c(
                       list(list(value = "all",       label = "All Parameters", group = "presets"),
                            list(value = "metals",    label = "All Metals",     group = "presets"),
                            list(value = "nutrients", label = "All Nutrients",  group = "presets")),
                       lapply(water_params, function(p) list(value = p, label = p, group = "individual"))
                     )
                   ))
  })
  
  output$sed_params_ui <- renderUI({
    df = master_data$sed_scored
    req(guard_data(df))
    
    sed_params <- sort(unique(na.omit(df$parameter)))
    
    choices <- c(
      "All Parameters" = "all",
      "All Metals" = "metals",
      setNames(sed_params, sed_params)
    )
    
    selectizeInput("sed_params", "Parameter(s):",
                   choices = choices,
                   selected = "all",
                   multiple = TRUE,
                   options = list(
                     optgroups = list(
                       list(value = "presets", label = "— Presets —"),
                       list(value = "individual", label = "— Individual Parameters —")
                     ),
                     optgroupField = "group",
                     options = c(
                       list(list(value = "all",       label = "All Parameters", group = "presets"),
                            list(value = "metals",    label = "All Metals",     group = "presets")),
                       lapply(sed_params, function(p) list(value = p, label = p, group = "individual"))
                     )
                   ))
  })
  
  # Filtered data based on all inputs
  sed_filtered_data <- reactive({
    df <- master_data$sed_scored
    req(guard_data(df))
    
    req(input$plot_media == "sediment")  # Only run for sediment
    
    cat("  Initial rows:", nrow(df), "\n")
    
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
        df[[date_col]] <- as.Date(df[[date_col]])
      }
      
      start_date <- as.Date(input$map_date_range[1])
      end_date <- as.Date(input$map_date_range[2])
      
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
    
    # filter out some columns for better viewing
    df = df |>
      select(-c(fraction, media, converted_from_mg_kg, CR, WL, std_info, CR_cases_10k, has_HQ, has_CR, has_WL, has_standard)) |>
      relocate(year, .before = station) |>
      relocate(c(latitude_decimal, longitude_decimal), .after=last_col())
    
    return(df)
  })
  
  # Create similar water_filtered_data reactive for water maps
  water_filtered_data <- reactive({
    df <- master_data$water_scored
    req(guard_data(df))
    req(input$plot_media == "water")
    
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
        df[[date_col]] <- as.Date(df[[date_col]])
      }
      
      start_date <- as.Date(input$water_date_range[1])
      end_date <- as.Date(input$water_date_range[2])
      
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
    sed_df <- sed_filtered_data()
  
    req(nrow(sed_df) > 0) # replace with guard_data()?
    req(input$sed_metal)
    req(input$sed_value_type)
    
    if (isTRUE(input$sed_value_type == "sed_class")) {
      sed_df <- classify_sediment_usgs_bulk(sed_df, stds)
    }
    
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
    else if (input$sed_value_type == "sed_class") {
      valid_levels <- c("Below TEL", "Above TEL", "Above PEL")
      sed_df$sed_class <- ifelse(sed_df$sed_class %in% valid_levels, sed_df$sed_class, NA_character_)
      
      pal <- colorFactor(
        palette = c("lightblue", "darkorange", "firebrick"),
        levels  = valid_levels,
        na.color = "gray"
      )
      colors <- pal(sed_df$sed_class)
      
      label_text <- paste0(
        "Station: ", sed_df$station, "<br>",
        "Date: ", sed_df$date, "<br>",
        "Parameter: ", sed_df$parameter, "<br>",
        "Sieve Size: ", sed_df$sieve_size, "<br>",
        "Concentration: ", round(sed_df$concentration, 3), " ", sed_df$unit, "<br>",
        "Classification: ", sed_df$sed_class
      )
    }
    else if (input$sed_value_type == "hq") {
      # cat("  Using HQ palette\n")
      # cat(names(sed_df))
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
    
    # prepare legend
    legend_pal <- switch(input$sed_value_type,
                         sed_value = pal,
                         sed_class = pal,
                         hq        = if (!is.null(pal)) pal else NULL,
                         NULL
    )
    
    legend_values <- switch(input$sed_value_type,
                            sed_value = ~concentration,
                            sed_class = ~sed_class,
                            hq        = if (!is.null(pal)) ~HQ[HQ >= 1] else NULL,
                            NULL
    )
    
    legend_title <- switch(input$sed_value_type,
                           sed_value = paste0(input$sed_metal, "<br>(", sed_df$unit[1], ")"),
                           sed_class = "USGS Standard",
                           hq        = "Hazard Quotient (HQ)",
                           ""
    )
    
    # Use leafletProxy to update only the markers
    leafletProxy("sed_map", data = sed_df) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(
        lng         = ~get(lng_col),
        lat         = ~get(lat_col),
        radius      = 6,
        stroke      = TRUE,
        color       = "black",
        weight      = 1.5,
        fillOpacity = 0.8,
        fillColor   = colors,
        label       = lapply(label_text, htmltools::HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = legend_pal,
        values   = legend_values,
        title    = legend_title,
        opacity  = 1
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
  
  ### worst water observations selectors
  # check if current param has standards
  water_param_has_standard <- reactive({
    req(input$water_metal)
    param_stds <- stds %>%
      filter(
        media == "water",
        .data$parameter == input$water_metal
      )
    list(
      has_bol  = any(param_stds$regulator == "Bolivian Law 1333"),
      has_any  = nrow(param_stds) > 0
    )
  })
  
  # determine if the two radiobuttons for observations should be shown
  observe({
    has <- water_param_has_standard()
    
    choices <- c(
      "Measured Concentration" = "water_value"
    )
    if (has$has_bol) choices <- c(choices, "Compare to Bolivian Standards" = "water_class")
    if (has$has_any) choices <- c(choices, "Hazard Quotient (HQ)" = "hq")
    
    current  <- isolate(input$water_value_type)
    selected <- if (current %in% choices) current else "water_value"
    
    updateRadioButtons(session, "water_value_type",
                       choices  = choices,
                       selected = selected
    )
  })
  
  ### worst water observations selectors
  sed_param_has_standard <- reactive({
    req(input$sed_metal)
    list(
      has_usgs = any(stds$parameter == input$sed_metal & stds$regulator == "USGS" & stds$media == "sediment", na.rm = TRUE),
      has_any  = any(stds$parameter == input$sed_metal & stds$media == "sediment", na.rm = TRUE)
    )
  })
  
  # determine which radiobuttons for observations should be shown
  observe({
    has <- sed_param_has_standard()
    
    choices <- c("Measured Concentration" = "sed_value")
    if (has$has_usgs) choices <- c(choices, "Compare to USGS Guidelines (TEL/PEL)" = "sed_class")
    if (has$has_any)  choices <- c(choices, "Hazard Quotient (HQ)" = "hq")
    
    current  <- isolate(input$sed_value_type)
    selected <- if (current %in% choices) current else "sed_value"
    
    updateRadioButtons(session, "sed_value_type",
                       choices  = choices,
                       selected = selected
    )
  })
  
  # Use observe() with leafletProxy to update water markers
  observe({
    # Check if water is selected
    if (is.null(input$plot_media)) {
      cat("  plot_media is NULL\n")
      return()
    }
    
    if (input$plot_media != "water") {
      cat("  Not water media, skipping\n")
      return()
    }
    
    # Get filtered data
    water_df <- water_filtered_data()
    cat("  water_filtered_data returned:", nrow(water_df), "rows\n")
    
    # throw an error if we shouldn't be allowed to use this feature
    if (input$water_value_type %in% c("water_class", "hq")) {
      has <- water_param_has_standard()
      no_std <- (input$water_value_type == "water_class" && !has$has_bol) ||
        (input$water_value_type == "hq"           && !has$has_any)
      
      if (no_std) {
        leafletProxy("water_map") %>%
          clearControls() %>%
          addControl(
            html = paste0(
              "<div style='background:rgba(255,240,240,0.95); padding:10px 14px;",
              "border-left:4px solid #dc3545; border-radius:4px; font-size:13px;'>",
              "<strong>No standards available</strong><br>",
              "<span style='color:#666;'>", input$water_metal, " has no ",
              if (input$water_value_type == "water_class") "Bolivian 1333" else "HQ",
              " standards.</span></div>"
            ),
            position = "topright"
          )
        return()  # ← stops here, never reaches palette code
      }
    }
    
    
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
      # get bol classification for each sample
      water_df <- classify_water_1333_bulk(water_df, stds)
      # water_df$classification <- get_bol_class_cache(
      #   water_df$parameter, water_df$concentration, water_df$unit, stds
      # )
      
      pal <- colorFactor(
        palette  = c("lightblue", "lightgreen", "gold", "darkorange", "darkred"),
        levels   = CLASS_ORDER,
        na.color = "gray"
      )
      colors <- pal(water_df$classification)
      
      label_text <- paste0(
        "Station: ", water_df$station, "<br>",
        "Date: ", water_df$date, "<br>",
        "Parameter: ", water_df$parameter, "<br>",
        "Concentration: ", round(water_df$concentration, 3), " ", water_df$unit, "<br>",
        "Class: ", ifelse(is.na(water_df$classification), "No standard", water_df$classification)
      )
    } else if (input$water_value_type == "hq") {
      hq_values <- water_df$HQ[is.finite(water_df$HQ)]
      if (length(hq_values) == 0) { cat("ERROR: No valid HQ values\n"); return() }
      
      pal    <- colorNumeric(palette = "Reds", domain = hq_values, na.color = "gray")
      colors <- pal(water_df$HQ)
      
      label_text <- paste0(
        "Station: ", water_df$station, "<br>",
        "Date: ", water_df$date, "<br>",
        "Parameter: ", water_df$parameter, "<br>",
        "Hazard Quotient: ", round(water_df$HQ, 3)
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
        values = if (input$water_value_type == "water_value") {
          water_df$concentration
        } else if (input$water_value_type == "water_class") {
          water_df$classification
        } else if (input$water_value_type == "hq") {
          water_df$HQ
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
    
    if (input$sed_value_type == "sed_class") {
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
      filter(regulator == "Bolivian Law 1333") |>
      select(regulator, parameter, abbr, value, unit, limit)
  })
  
  output$stds_usgs_table_ts <- renderDT({
    stds |>
      filter(regulator == "USGS")
  })
  
  output$stds_all <- renderDT({
    stds
  })
  
  output$stds_strict = renderDT({
    strict_stds |>
      select(-c(hqcr)) |>
      filter(str_detect(media,input$plot_media))
  })


  
  # In server.R - populate water_params choices
  observe({
    df = master_data$water_scored
    req(guard_data(df))
    
    water_params <- unique(df$parameter)
    water_params <- water_params[!is.na(water_params)]
    water_params <- sort(water_params)
    
    updateSelectInput(session, "water_params",
                      choices = c("All Parameters" = "all", water_params),
                      selected = "all")
  })
  
  observe({
    if (isTRUE(input$risk_river)) {
      if (!exists("river_network")) {
        river_network <<- st_read("data/shp/River_Network.shp")
      }
      if (!exists("pilco_line")) {
        pilco_line <<- st_read("data/geojson/pilco_line.geojson")
      }
      
      leafletProxy("risk_map") |>
        clearGroup("river_network") |>
        addPolylines(
          data    = river_network,
          color   = "darkcyan",
          weight  = 1.5,
          opacity = 1,
          group   = "river_network",
          options = pathOptions(pane = "polylinePane")
        ) |>
        addPolylines(
          data    = pilco_line,
          color   = "darkblue",
          weight  = 3,
          opacity = 1,
          group   = "river_network",
          options = pathOptions(pane = "polylinePane")
        )
    } else {
      leafletProxy("risk_map") |>
        clearGroup("river_network")
    }
  })
  
  observe({
    if (isTRUE(input$risk_basin)) {
      
      leafletProxy("risk_map") |>
        clearGroup("borders") |>
        addPolygons(
          data    = pilco_basin,
          color   = "black",
          weight  = 1.5,
          opacity = 1,
          fillOpacity = 0,
          group   = "borders",
          options = pathOptions(pane = "polygonPane")
        ) 
    } else {
      leafletProxy("risk_map") |>
        clearGroup("borders")
    }
  })
  
  water_stations_data <- eventReactive(input$score_water, {
    params   <- if (is.null(input$water_params))   "all"    else input$water_params
    temp_ag  <- if (is.null(input$water_temp_ag))  "recent" else input$water_temp_ag
    param_ag <- if (is.null(input$water_param_ag)) "pct95"  else input$water_param_ag
    nyears   <- if (is.null(input$water_nyears) || is.na(input$water_nyears)) 5 else input$water_nyears
    fraction <- if (is.null(input$water_fraction) || input$water_fraction == "All") NULL else input$water_fraction
    
    withProgress(message = tr("notif_rendering_water"), {
      prepare_water_quality_data(
        data                 = master_data$water_scored,
        params               = params,
        param_aggregation    = param_ag,
        temporal_aggregation = temp_ag,
        nyears               = nyears,
        fraction             = fraction,
        date                 = Sys.Date()
      )
    })
  })
  
  water_bin_breaks <- reactiveVal(NULL)
  sed_bin_breaks   <- reactiveVal(NULL)
  eji_bin_breaks <- reactiveVal(NULL)
  pop_bin_breaks <- reactiveVal(NULL)
  
  bin_raster <- function(r, breaks) {
    if (is.null(breaks) || length(breaks) < 2) {
      warning("bin_raster: need at least 2 breaks, got ", length(breaks))
      return(r)
    }
    
    vals <- terra::values(r, na.rm = TRUE)
    
    # extend breaks to cover full raster range
    breaks[1] <- min(breaks[1], min(vals))
    breaks[length(breaks)] <- max(breaks[length(breaks)], max(vals))
    
    rcl <- cbind(
      from    = breaks[-length(breaks)],
      to      = breaks[-1],
      becomes = seq_along(breaks[-1])
    )
    
    result <- terra::classify(r, rcl = rcl, include.lowest = TRUE, right = TRUE)
    
    valid_bins <- seq_along(breaks[-1])
    result_vals <- terra::values(result)
    result_vals[!is.na(result_vals) & !result_vals %in% valid_bins] <- NA
    terra::values(result) <- result_vals
    
    result
  }
  
  # water station palette helper
  water_station_pal <- function(hq_vals, binned = FALSE, n = 5, breaks = NULL) {
    if (binned && !is.null(breaks)) {
      bins <- cut(hq_vals, breaks = breaks, labels = FALSE, include.lowest = TRUE)
      n_bins <- length(breaks) - 1
      colorFactor(
        rev(RColorBrewer::brewer.pal(min(n_bins, 9), "RdYlBu")),
        domain = as.character(seq_len(n_bins))
      )
    } else {
      colorNumeric("RdYlBu", domain = hq_vals, reverse = TRUE, na.color = "grey")
    }
  }
  
  
  # Add to map on creation
  observeEvent(water_stations_data(), {
    df <- water_stations_data()
    req(guard_data(df, cols = c("HQ")))

    hq_vals   <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
    bin_colors <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fc8d59", "#d73027", "#67001f")
    
    df$popup_text <- sapply(1:nrow(df), function(i) {
      hq      <- df$HQ[i]
      station <- df$station[i]
      date    <- df$date[i]
      
      if (input$water_temp_ag == "recent") {
        date_display <- paste0("<b>Date:</b> ", date)
      } else {
        min_date <- if ("min_date" %in% names(df)) df$min_date[i] else date
        date_display <- paste0("<b>Date Range:</b> ", min_date, " to ", date)
      }
      
      popup <- paste0(
        "<b>Station:</b> ", station, "<br>",
        "<b>Aggregated HQ:</b> ", round(hq, 3), "<br>",
        date_display
      )
      
      if ("parameter_hqs" %in% names(df) && "parameter_names" %in% names(df)) {
        param_hqs   <- df$parameter_hqs[[i]]
        param_names <- df$parameter_names[[i]]
        
        if (!is.null(param_hqs) && length(param_hqs) > 1) {
          
          param_df <- data.frame(
            parameter = param_names,
            HQ        = param_hqs,
            stringsAsFactors = FALSE
          )
          
          param_aggregated <- param_df %>%
            group_by(parameter) %>%
            summarise(HQ = max(HQ, na.rm = TRUE), .groups = "drop")
          
          param_hqs_unique   <- param_aggregated$HQ
          param_names_unique <- param_aggregated$parameter
          
          breaks     <- c(0, 0.5, 1, 2, 5, 10, Inf)
          bin_labels <- c("0-0.5", "0.5-1", "1-2", "2-5", "5-10", "10+")
          
          bin_counts       <- table(cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE))
          max_count        <- max(bin_counts)
          max_bar_height   <- 80
          container_height <- max_bar_height + 30
          pixels_per_count <- max_bar_height / max_count
          
          hist_html <- paste0(
            "<div style='margin-top: 8px; border-top: 1px solid #ccc; padding-top: 8px;'>",
            "<small><b>Parameter Distribution (n=", length(param_hqs_unique), " unique parameters):</b></small><br>",
            "<div style='display: flex; align-items: flex-end; height: ", container_height,
            "px; margin-top: 4px; gap: 2px; padding-top: 10px; overflow: hidden;'>"
          )
          
          for (j in seq_along(bin_labels)) {
            count    <- as.numeric(bin_counts[j])
            bin_mask <- cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE) == bin_labels[j]
            
            if (count > 0) {
              params_in_bin <- param_names_unique[bin_mask]
              hqs_in_bin    <- param_hqs_unique[bin_mask]
              param_details <- paste0(params_in_bin, " (", round(hqs_in_bin, 2), ")", collapse = "&#10;")
              tooltip_text  <- paste0("HQ by parameter (", bin_labels[j], " HQ):&#10;", param_details)
            } else {
              tooltip_text <- paste0(bin_labels[j], ": no parameters")
            }
            
            bar_height <- count * pixels_per_count
            
            hist_html <- paste0(hist_html,
                                "<div style='flex: 1; display: flex; flex-direction: column; align-items: center;'>",
                                "<div style='width: 100%; background-color: ", bin_colors[j],
                                "; height: ", bar_height, "px;' ",
                                "title='", tooltip_text, "'></div>",
                                "<small style='font-size: 9px; margin-top: 2px; font-weight: bold;'>", count, "</small>",
                                "</div>"
            )
          }
          
          hist_html <- paste0(hist_html,
                              "</div>",
                              "<div style='display: flex; justify-content: space-between;'>",
                              "<small style='font-size: 8px; color: #666;'>0</small>",
                              "<small style='font-size: 8px; color: #666;'>0.5</small>",
                              "<small style='font-size: 8px; color: #666;'>1</small>",
                              "<small style='font-size: 8px; color: #666;'>2</small>",
                              "<small style='font-size: 8px; color: #666;'>5</small>",
                              "<small style='font-size: 8px; color: #666;'>10+</small>",
                              "</div>",
                              "<small style='color: #666;'>Min: ", round(min(param_hqs_unique), 2),
                              " | Max: ", round(max(param_hqs_unique), 2), "</small>",
                              "</div>"
          )
          
          popup <- paste0(popup, hist_html)
        }
      }
      
      return(popup)
    })
    
    water_stations_df(df)
    
    # Use water_bin_breaks() directly — no dependency on raster display
    is_binned <- isTRUE(input$bin_water) && input$apply_water_bins > 0 && !is.null(water_bin_breaks())
    
    if (is_binned) {
      breaks      <- water_bin_breaks()
      n           <- length(breaks) - 1
      bin_vals    <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
      pal         <- colorFactor(
        rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlBu")),
        domain = as.character(seq_len(n))
      )
      fill_colors <- pal(bin_vals)
    } else {
      pal         <- colorNumeric("RdYlBu", domain = hq_vals, reverse = TRUE, na.color = "grey")
      fill_colors <- pal(df$HQ)
    }
    
    leafletProxy("risk_map") |>
      clearGroup("water_stations") |>
      addCircleMarkers(
        data        = df,
        lng         = ~longitude_decimal,
        lat         = ~latitude_decimal,
        color       = "black",
        weight      = 1.5,
        fillColor   = fill_colors,
        fillOpacity = 1,
        radius      = 8,
        group       = "water_stations",
        popup       = ~popup_text,
        options     = pathOptions(pane = "pointPane")
      )
  })
  
  # Toggle visibility
  observe({
    if (isTRUE(input$risk_water_stations)) {
      leafletProxy("risk_map") |> showGroup("water_stations")
    } else {
      leafletProxy("risk_map") |> hideGroup("water_stations")
    }
  })
  
  sed_stations_data <- eventReactive(input$score_sediment, {
    params   <- if (is.null(input$sed_params))   "all"    else input$sed_params
    temp_ag  <- if (is.null(input$sed_temp_ag))  "recent" else input$sed_temp_ag
    param_ag <- if (is.null(input$sed_param_ag)) "pct95"  else input$sed_param_ag
    nyears   <- if (is.null(input$sed_nyears) || is.na(input$sed_nyears)) 5 else input$sed_nyears
    
    withProgress(message = tr("notif_rendering_sed"), {
      prepare_water_quality_data(
        data                 = master_data$sed_scored,
        params               = params,
        param_aggregation    = param_ag,
        temporal_aggregation = temp_ag,
        nyears               = nyears,
        fraction             = NULL,
        date                 = Sys.Date()
      )
    })
  })
  
  # Add to map on creation
  observeEvent(sed_stations_data(), {
    df <- sed_stations_data()
    req(guard_data(df, cols = c("HQ")))
    
    hq_vals    <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
    bin_colors <- c("#1a9850", "#d9ef8b", "#ffffbf", "#fc8d59", "#d73027", "#67001f")
    
    df$popup_text <- sapply(1:nrow(df), function(i) {
      hq      <- df$HQ[i]
      station <- df$station[i]
      date    <- df$date[i]
      
      if (input$sed_temp_ag == "recent") {
        date_display <- paste0("<b>Date:</b> ", date)
      } else {
        min_date <- if ("min_date" %in% names(df)) df$min_date[i] else date
        date_display <- paste0("<b>Date Range:</b> ", min_date, " to ", date)
      }
      
      popup <- paste0(
        "<b>Station:</b> ", station, "<br>",
        "<b>Aggregated HQ:</b> ", round(hq, 3), "<br>",
        date_display
      )
      
      if ("parameter_hqs" %in% names(df) && "parameter_names" %in% names(df)) {
        param_hqs   <- df$parameter_hqs[[i]]
        param_names <- df$parameter_names[[i]]
        
        if (!is.null(param_hqs) && length(param_hqs) > 1) {
          
          param_df <- data.frame(
            parameter = param_names,
            HQ        = param_hqs,
            stringsAsFactors = FALSE
          )
          
          param_aggregated <- param_df %>%
            group_by(parameter) %>%
            summarise(HQ = max(HQ, na.rm = TRUE), .groups = "drop")
          
          param_hqs_unique   <- param_aggregated$HQ
          param_names_unique <- param_aggregated$parameter
          
          breaks     <- c(0, 0.5, 1, 2, 5, 10, Inf)
          bin_labels <- c("0-0.5", "0.5-1", "1-2", "2-5", "5-10", "10+")
          
          bin_counts       <- table(cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE))
          max_count        <- max(bin_counts)
          max_bar_height   <- 80
          container_height <- max_bar_height + 30
          pixels_per_count <- max_bar_height / max_count
          
          hist_html <- paste0(
            "<div style='margin-top: 8px; border-top: 1px solid #ccc; padding-top: 8px;'>",
            "<small><b>Parameter Distribution (n=", length(param_hqs_unique), " unique parameters):</b></small><br>",
            "<div style='display: flex; align-items: flex-end; height: ", container_height,
            "px; margin-top: 4px; gap: 2px; padding-top: 10px; overflow: hidden;'>"
          )
          
          for (j in seq_along(bin_labels)) {
            count    <- as.numeric(bin_counts[j])
            bin_mask <- cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE) == bin_labels[j]
            
            if (count > 0) {
              params_in_bin <- param_names_unique[bin_mask]
              hqs_in_bin    <- param_hqs_unique[bin_mask]
              param_details <- paste0(params_in_bin, " (", round(hqs_in_bin, 2), ")", collapse = "&#10;")
              tooltip_text  <- paste0("HQ by parameter (", bin_labels[j], " HQ):&#10;", param_details)
            } else {
              tooltip_text <- paste0(bin_labels[j], ": no parameters")
            }
            
            bar_height <- count * pixels_per_count
            
            hist_html <- paste0(hist_html,
                                "<div style='flex: 1; display: flex; flex-direction: column; align-items: center;'>",
                                "<div style='width: 100%; background-color: ", bin_colors[j],
                                "; height: ", bar_height, "px;' ",
                                "title='", tooltip_text, "'></div>",
                                "<small style='font-size: 9px; margin-top: 2px; font-weight: bold;'>", count, "</small>",
                                "</div>"
            )
          }
          
          hist_html <- paste0(hist_html,
                              "</div>",
                              "<div style='display: flex; justify-content: space-between;'>",
                              "<small style='font-size: 8px; color: #666;'>0</small>",
                              "<small style='font-size: 8px; color: #666;'>0.5</small>",
                              "<small style='font-size: 8px; color: #666;'>1</small>",
                              "<small style='font-size: 8px; color: #666;'>2</small>",
                              "<small style='font-size: 8px; color: #666;'>5</small>",
                              "<small style='font-size: 8px; color: #666;'>10+</small>",
                              "</div>",
                              "<small style='color: #666;'>Min: ", round(min(param_hqs_unique), 2),
                              " | Max: ", round(max(param_hqs_unique), 2), "</small>",
                              "</div>"
          )
          
          popup <- paste0(popup, hist_html)
        }
      }
      
      return(popup)
    })
    
    sed_stations_df(df)
    
    # Use sed_bin_breaks() directly — no dependency on raster display
    is_binned <- isTRUE(input$bin_sediment) && input$apply_sed_bins > 0 && !is.null(sed_bin_breaks())
    
    if (is_binned) {
      breaks      <- sed_bin_breaks()
      n           <- length(breaks) - 1
      bin_vals    <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
      pal         <- colorFactor(
        rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlGn")),
        domain = as.character(seq_len(n))
      )
      fill_colors <- pal(bin_vals)
    } else {
      pal         <- colorNumeric("RdYlGn", domain = hq_vals, reverse = TRUE, na.color = "grey")
      fill_colors <- pal(df$HQ)
    }
    
    leafletProxy("risk_map") |>
      clearGroup("sed_stations") |>
      addCircleMarkers(
        data        = df,
        lng         = ~longitude_decimal,
        lat         = ~latitude_decimal,
        color       = "black",
        weight      = 1.5,
        fillColor   = fill_colors,
        fillOpacity = 1,
        radius      = 8,
        group       = "sed_stations",
        popup       = ~popup_text,
        options     = pathOptions(pane = "pointPane")
      )
  })
  
  # Toggle visibility
  observe({
    if (isTRUE(input$risk_sed_stations)) {
      leafletProxy("risk_map") |> showGroup("sed_stations")
    } else {
      leafletProxy("risk_map") |> hideGroup("sed_stations")
    }
  })
  
  observe({
    if (isTRUE(input$risk_eji)) {
      if (!exists("eji_data")) {
        withProgress(message = "Loading EJI data...", {
          eji_data <<- st_read("data/census/shp/Bolivia_Mun_EJI_Shape_updated.shp", quiet = TRUE) %>%
            st_transform(4326)
        })
      }
      
      is_binned <- isTRUE(input$bin_eji) && 
        !is.null(input$apply_eji_bins) && 
        input$apply_eji_bins > 0 && 
        !is.null(eji_bin_breaks()) &&
        !is.null(eji_raster_binned())
      
      if (is_binned) {
        r    <- eji_raster_binned()
        
        # Project to Web Mercator to match leaflet's internal CRS
        r_3857 <- terra::project(r, "EPSG:3857", method = "near")
        
        vals <- sort(unique(terra::values(r_3857, na.rm = TRUE)))
        n    <- length(vals)
        pal  <- colorFactor(
          colorRampPalette(RColorBrewer::brewer.pal(9, "Purples"))(n),
          domain   = as.character(vals),
          levels   = as.character(vals),
          na.color = "transparent"
        )
        leafletProxy("risk_map") %>%
          clearGroup("eji") %>%
          addRasterImage(
            r_3857,
            colors  = pal,
            opacity = 0.7,
            group   = "eji",
            options = leafletOptions(pane = "polygonPane")
          )
      } else {
        pal         <- colorNumeric("Purples", domain = eji_data$eji, na.color = "transparent")
        fill_colors <- pal(eji_data$eji)
        leafletProxy("risk_map") %>%
          clearGroup("eji") %>%
          addPolygons(
            data        = eji_data,
            fillColor   = fill_colors,
            fillOpacity = 0.5,
            color       = "white",
            weight      = 0.5,
            opacity     = 0.8,
            group       = "eji",
            options     = pathOptions(pane = "polygonPane"),
            label       = ~paste0("Municipality: ", adm3_name, "<br>EJI Score: ", round(eji, 3)) %>% 
              lapply(htmltools::HTML)
          )
      }
    } else {
      leafletProxy("risk_map") %>% clearGroup("eji")
    }
  })
  
  
  observeEvent(input$apply_eji_bins, {
    req(exists("eji_data"))
    
    n      <- if (is.null(input$eji_nbins) || is.na(input$eji_nbins)) 5 else input$eji_nbins
    method <- if (is.null(input$eji_bin_method)) "quantile" else input$eji_bin_method
    
    hq_vals <- eji_data$eji[!is.na(eji_data$eji)]
    
    breaks <- switch(method,
                     quantile       = unique(quantile(hq_vals, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE)),
                     equal_interval = seq(min(hq_vals), max(hq_vals), length.out = n + 1)
    )
    breaks[1]              <- min(breaks[1], min(hq_vals))
    breaks[length(breaks)] <- max(breaks[length(breaks)], max(hq_vals))
    
    eji_bin_breaks(breaks)
    
    withProgress(message = "Rasterizing EJI layer...", {
      
      # Assign bin values directly to polygons
      eji_vect <- terra::vect(eji_data)
      bin_nums <- as.integer(cut(eji_data$eji, breaks = breaks, labels = FALSE, include.lowest = TRUE))
      eji_vect$bin_val <- bin_nums
      
      # Rasterize at 0.01 degree resolution using max to handle boundaries
      tmpl_01 <- terra::rast(
        terra::ext(eji_vect),
        resolution = 0.01,
        crs = "EPSG:4326"
      )
      
      eji_r <- terra::rasterize(eji_vect, tmpl_01, field = "bin_val", fun = "max")
      message("NAs after rasterize: ", sum(is.na(terra::values(eji_r))))
      
      # Fill boundary NAs - only 1-2 cells wide so small window sufficient
      repeat {
        # na_before <- sum(is.na(terra::values(eji_r))) # costly
        na_before <- terra::global(eji_r, fun = "isNA")[[1]] # fast & lean
        eji_r <- terra::focal(eji_r, w = 3, fun = "modal", na.policy = "only", na.rm = TRUE)
        # na_after <- sum(is.na(terra::values(eji_r))) # costly
        na_after <- terra::global(eji_r, fun = "isNA")[[1]] # fast & lean
        
        message("NAs remaining: ", na_after)
        if (na_after == na_before || na_after == 0) break
      }
      
      vals <- terra::values(eji_r)
      message("Unique values: ", paste(sort(unique(vals[!is.na(vals)])), collapse = ", "))
      message("NA count: ", sum(is.na(vals)))
      message("Zero count: ", sum(vals == 0, na.rm = TRUE))
      
      # Crop and mask to basin boundary
      eji_r <- terra::crop(eji_r, terra::vect(pilco_basin))
      eji_r <- terra::mask(eji_r, terra::vect(pilco_basin))
      
      eji_raster_binned(eji_r)
    })
  })
  
  observeEvent(input$bin_eji, {
    if (!isTRUE(input$bin_eji)) eji_bin_breaks(NULL)
  })
  
  # Population Raster
  observe({
    if (isTRUE(input$risk_pop_density)) {
      if (!exists("pop_raster")) {
        withProgress(message = "Loading population density...", {
          pop_raster <<- rast("data/population_raster/pop_2024.tif") %>%
            project("EPSG:4326") %>%
            crop(pilco_basin) %>%
            mask(pilco_basin) %>%
            sqrt()
        })
      }
      
      pop_vals <- terra::values(pop_raster, na.rm = TRUE)
      pop_vals <- pop_vals[pop_vals > 0]
      
      is_binned <- isTRUE(input$bin_pop) && !is.null(input$apply_pop_bins) && input$apply_pop_bins > 0 && !is.null(pop_bin_breaks())
      
      if (is_binned) {
        breaks    <- pop_bin_breaks()
        r_binned  <- bin_raster(pop_raster, breaks)
        r_3857    <- terra::project(r_binned, "EPSG:3857", method = "near")
        leafletProxy("risk_map") %>%
          clearGroup("pop_density") %>%
          addRasterImage(
            r_3857,
            colors  = viridisLite::magma(length(breaks) - 1),
            opacity = 0.9,
            group   = "pop_density",
            options = leafletOptions(pane = "rasterPane1")
          )
      } else {
        pal    <- colorNumeric("magma", domain = pop_vals, na.color = "transparent")
        r_3857 <- terra::project(pop_raster, "EPSG:3857", method = "near")
        leafletProxy("risk_map") %>%
          clearGroup("pop_density") %>%
          addRasterImage(
            r_3857,
            colors  = pal,
            opacity = 0.9,
            group   = "pop_density",
            options = leafletOptions(pane = "rasterPane1")
          )
      }
    } else {
      leafletProxy("risk_map") %>% clearGroup("pop_density")
    }
  })
  
  # Pop density binning observer
  observeEvent(input$apply_pop_bins, {
    req(exists("pop_raster"))
    
    n      <- if (is.null(input$pop_nbins) || is.na(input$pop_nbins)) 5 else input$pop_nbins
    method <- if (is.null(input$pop_bin_method)) "equal_area" else input$pop_bin_method
    
    hq_source <- terra::values(pop_raster, na.rm = TRUE)
    hq_source <- hq_source[hq_source > 0]
    
    breaks <- switch(method,
                     equal_interval = seq(min(hq_source), max(hq_source), length.out = n + 1),
                     equal_area     = {
                       sorted_vals <- sort(hq_source)
                       n_cells     <- length(sorted_vals)
                       indices     <- round(seq(0, n_cells, length.out = n + 1))
                       unique(sorted_vals[pmax(indices, 1)])
                     },
                     jenks          = {
                       cls <- classInt::classIntervals(hq_source, n = n, style = "fisher")
                       cls$brks
                     }
    )
    
    breaks[1]              <- min(breaks[1], min(hq_source))
    breaks[length(breaks)] <- max(breaks[length(breaks)], max(hq_source))
    
    pop_bin_breaks(breaks)
  })
  
  observeEvent(input$bin_pop, {
    if (!isTRUE(input$bin_pop)) pop_bin_breaks(NULL)
  })
  

 

  
  # ── Reactive data ─────────────────────────────────────────────
  unique_mines_data <- reactive({
    req(input$risk_mines)
    
    if (!exists("unique_mines")) {
      withProgress(message = "Loading mining activity data...", {
        unique_mines <<- read_csv("data/mining_exposure/unique_mines.csv") %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326)
      })
    }
    
    if (isTRUE(input$clip_mines)) st_filter(unique_mines, pilco_basin) else unique_mines
  })
  
  settlements_data <- reactive({
    req(input$risk_settlements)
    
    if (!exists("settlements")) {
      withProgress(message = "Loading settlement data...", {
        settlements <<- st_read("data/settlements/poblaciones.shp") |>
          st_transform(crs = 4326)
      })
    }
    
    if (isTRUE(input$clip_settlements)) st_filter(settlements, pilco_basin) else settlements
  })
  
  tailings_data <- reactive({
    req(input$risk_tailings)
    
    if (!exists("tailings")) {
      withProgress(message = "Loading tailings data...", {
        tailings <<- st_read("data/air_quality/tailings/tailing_minefac.shp") |>
          st_transform(crs = 4326) |>
          st_zm(drop = TRUE, what = "ZM")
      })
    }
    
    tailings
  })
  
  # ── Observers ─────────────────────────────────────────────────
  
  # Add Mines
  observe({
    if (isTRUE(input$risk_mines)) {
      data <- unique_mines_data()
      
      data$popup_text <- paste0("Mine Name: ", data$mine_name_clean,
                                "<br>Minerals: ", data$minerals,
                                "<br>Source: ", data$source)
      
      icons <- awesomeIcons(
        icon = "mountain",
        library = "fa",
        markerColor = "orange",
        iconColor = "white"
      )
      
      leafletProxy("risk_map") %>%
        clearGroup("mines") %>%
        addAwesomeMarkers(
          data = data,
          icon = icons,
          group = "mines",
          popup = ~popup_text,
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS("
      function(cluster) {
        return new L.DivIcon({
          html: '<div style=\"background-color: #F69730; opacity: 0.5; border-radius: 50%; width: 35px; height: 35px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold;\">' + cluster.getChildCount() + '</div>',
          className: '',
          iconSize: [35, 35]
        });
      }
    ")
          )
        )
    } else {
      leafletProxy("risk_map") %>%
        clearGroup("mines")
    }
  })
  
  # Add Settlements 
  observe({
    if (isTRUE(input$risk_settlements)) {
      data <- settlements_data()
      
      data$popup_text <- paste0("Settlement Name: ", data$nombre_c_2,
                                "<br>Population: ", data$poblacion_)
      
      icons <- awesomeIcons(
        icon = "home",
        library = "fa",
        markerColor = "blue",
        iconColor = "white"
      )
      
      leafletProxy("risk_map") %>%
        clearGroup("settlements") %>%
        addAwesomeMarkers(
          data = data,
          icon = icons,
          group = "settlements",
          popup = ~popup_text,
          clusterOptions = markerClusterOptions(
            iconCreateFunction = JS("
      function(cluster) {
        return new L.DivIcon({
          html: '<div style=\"background-color: #38AADD; opacity: 0.5; border-radius: 50%; width: 35px; height: 35px; display: flex; align-items: center; justify-content: center; color: white; font-weight: bold;\">' + cluster.getChildCount() + '</div>',
          className: '',
          iconSize: [35, 35]
        });
      }
    ")
          )
        )
    } else {
      leafletProxy("risk_map") %>%
        clearGroup("settlements")
    }
  })
  
  # Add Tailings
  observe({
    if (isTRUE(input$risk_tailings)) {
      data <- tailings_data()
      
      data$popup_text <- paste0("Type: ", str_to_title(gsub("_", " ", data$Category_F)))
      
      tailing_pal <- colorFactor(
        palette = c(
          "tailing"       = "#E41A1C",
          "reception"     = "#377EB8",
          "storage"       = "#FF7F00",
          "refinery"      = "#984EA3",
          "heap"          = "#A65628",
          "mine_facility" = "#4DAF4A"
        ),
        domain = c("tailing", "reception", "storage", "refinery", "heap", "mine_facility")
      )
      
      leafletProxy("risk_map") |>
        clearGroup("tailings") |>
        addPolygons(
          data = data,
          group = "tailings",
          popup = ~popup_text,
          fillColor = ~tailing_pal(Category_F),
          fillOpacity = 0.7,
          color = "black",
          weight = 1,
          options = pathOptions(pane = "polygonPane2")
        )
    } else {
      leafletProxy("risk_map") |> clearGroup("tailings")
    }
  })
  
  # Water hazard layer creation
  water_risk_result <- eventReactive(input$create_water, {
    params   <- if (is.null(input$water_params))   "all"    else input$water_params
    temp_ag  <- if (is.null(input$water_temp_ag))  "recent" else input$water_temp_ag
    param_ag <- if (is.null(input$water_param_ag)) "pct95"  else input$water_param_ag
    nyears   <- if (is.null(input$water_nyears) || is.na(input$water_nyears)) 5 else input$water_nyears
    fraction <- if (is.null(input$water_fraction) || input$water_fraction == "All") NULL else input$water_fraction
    
    withProgress(message = "Creating water hazard layer...", {
      create_risk_map(
        data                 = master_data$water_scored,
        params               = params,
        param_aggregation    = param_ag,
        temporal_aggregation = temp_ag,
        nyears               = nyears,
        fraction             = fraction,
        resolution           = if (is.null(input$water_resolution) || is.na(input$water_resolution)) 1000 else input$water_resolution,
        max_risk_distance    = as.numeric(if (is.null(input$water_max_distance) || is.na(input$water_max_distance)) 2000 else input$water_max_distance)
      )
    })
  })
  
  water_raster         <- reactive({ water_risk_result()$risk_raster })
  water_station_scores <- reactive({ water_risk_result()$snapped_points$HQ })
  
  # Sediment hazard layer creation
  sediment_risk_result <- eventReactive(input$create_sediment, {
    params   <- if (is.null(input$sed_params))   "all"    else input$sed_params
    temp_ag  <- if (is.null(input$sed_temp_ag))  "recent" else input$sed_temp_ag
    param_ag <- if (is.null(input$sed_param_ag)) "pct95"  else input$sed_param_ag
    nyears   <- if (is.null(input$sed_nyears) || is.na(input$sed_nyears)) 5 else input$sed_nyears
    
    withProgress(message = "Creating sediment hazard layer...", {
      create_risk_map(
        data                 = master_data$sed_scored,
        params               = params,
        param_aggregation    = param_ag,
        temporal_aggregation = temp_ag,
        nyears               = nyears,
        fraction             = NULL,
        resolution           = if (is.null(input$sed_resolution) || is.na(input$sed_resolution)) 1000 else input$sed_resolution,
        max_risk_distance    = as.numeric(if (is.null(input$sed_max_distance) || is.na(input$sed_max_distance)) 2000 else input$sed_max_distance)
      )
    })
  })
  
  sediment_raster         <- reactive({ sediment_risk_result()$risk_raster })
  sediment_station_scores <- reactive({ sediment_risk_result()$snapped_points$HQ })
  
  
  # Water Raster Display
  water_raster_display <- eventReactive(
    c(input$apply_water_bins, input$bin_water, water_raster()), {
      r <- water_raster()
      req(r)
      if (!isTRUE(input$bin_water)) return(r)
      if (input$apply_water_bins == 0) return(r)
      isolate({
        breaks <- water_bin_breaks()
        req(breaks)
        withProgress(message = "Binning water hazard layer...", {
          bin_raster(r, breaks)
        })
      })
    }, ignoreNULL = TRUE)
  
  # Render observer — only fires when raster or bin state changes
  observeEvent(water_raster_display(), {
    r <- water_raster_display()
    req(r)
    if (isTRUE(input$bin_water) && input$apply_water_bins == 0) return()
    
    is_binned <- length(unique(terra::values(r, na.rm = TRUE))) <= 20
    pal <- if (is_binned) {
      n <- length(unique(terra::values(r, na.rm = TRUE)))
      colorFactor(
        rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlBu")),
        domain = as.character(sort(unique(terra::values(r, na.rm = TRUE)))),
        na.color = "transparent"
      )
    } else {
      colorNumeric("RdYlBu", domain = terra::values(r), reverse = TRUE, na.color = "transparent")
    }
    
    id <- showNotification(tr("notif_rendering_water"), duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id))
    
    r_3857 <- terra::project(r, "EPSG:3857", method = "near")
    
    leafletProxy("risk_map") |>
      addRasterImage(r_3857, colors = pal, opacity = 0.8,
                     layerId = "water_hazard", group = "water_hazard",
                     options = leafletOptions(pane = "rasterPane2"))
  }, ignoreInit = FALSE)
  
  # Show/hide observer — only fires when checkbox changes, no re-render
  observeEvent(input$risk_water, {
    if (!isTRUE(input$risk_water)) {
      leafletProxy("risk_map") |> hideGroup("water_hazard")
    } else {
      leafletProxy("risk_map") |> showGroup("water_hazard")
    }
  }, ignoreInit = TRUE)

  sediment_raster_display <- eventReactive(
    c(input$apply_sed_bins, input$bin_sediment, sediment_raster()), {
      r <- sediment_raster()
      req(r)
      if (!isTRUE(input$bin_sediment)) return(r)
      if (input$apply_sed_bins == 0) return(r)
      isolate({
        breaks <- sed_bin_breaks()
        req(breaks)
        withProgress(message = "Binning sediment hazard layer...", {
          bin_raster(r, breaks)
        })
      })
    }, ignoreNULL = TRUE)
  
  # Render observer sediment
  observeEvent(sediment_raster_display(), {
    r <- sediment_raster_display()
    req(r)
    if (isTRUE(input$bin_sediment) && input$apply_sed_bins == 0) return()
    
    is_binned <- length(unique(terra::values(r, na.rm = TRUE))) <= 20
    pal <- if (is_binned) {
      n <- length(unique(terra::values(r, na.rm = TRUE)))
      colorFactor(
        rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlGn")),
        domain = as.character(sort(unique(terra::values(r, na.rm = TRUE)))),
        na.color = "transparent"
      )
    } else {
      colorNumeric("RdYlGn", domain = terra::values(r), reverse = TRUE, na.color = "transparent")
    }
    
    id <- showNotification(tr("notif_rendering_sed"), duration = NULL, closeButton = FALSE, type = "message")
    on.exit(removeNotification(id))
    
    r_3857 <- terra::project(r, "EPSG:3857", method = "near")
    
    leafletProxy("risk_map") |>
      addRasterImage(r_3857, colors = pal, opacity = 0.8,  # r_3857 not r
                     layerId = "sed_hazard", group = "sed_hazard",
                     options = leafletOptions(pane = "rasterPane2"))
  }, ignoreInit = FALSE)
  
  # Show/hide observer sediment
  observeEvent(input$risk_sediment, {
    if (!isTRUE(input$risk_sediment)) {
      leafletProxy("risk_map") |> hideGroup("sed_hazard")
    } else {
      leafletProxy("risk_map") |> showGroup("sed_hazard")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$apply_water_bins, input$bin_water), {
  is_binned <- isTRUE(input$bin_water) && input$apply_water_bins > 0
  
  if (is_binned) {
    df <- water_stations_df()
    r  <- tryCatch(water_raster(), error = function(e) NULL)
    
    if (is.null(df) || nrow(df) == 0) {
      showNotification(tr("notif_score_water_bin"), type = "warning", duration = 4)
      return()
    }
    if (is.null(r)) {
      showNotification(tr("notif_create_water_bin"), type = "warning", duration = 4)
      return()
    }
    
    n      <- if (is.null(input$water_nbins) || is.na(input$water_nbins)) 5 else input$water_nbins
    method <- if (is.null(input$water_bin_method)) "quantile" else input$water_bin_method
    
    hq_source <- terra::values(r, na.rm = TRUE)
    hq_source <- hq_source[!is.na(hq_source)]
    
    breaks <- switch(method,
                     quantile       = {
                       station_hqs <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
                       unique(quantile(station_hqs, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
                     },
                     equal_interval = seq(min(hq_source), max(hq_source), length.out = n + 1),
                     equal_area     = unique(quantile(hq_source, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
    )
    breaks[1]              <- min(breaks[1], min(hq_source))
    breaks[length(breaks)] <- max(breaks[length(breaks)], max(hq_source))
    
    water_bin_breaks(breaks)
    
    n        <- length(breaks) - 1
    bin_vals <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
    pal      <- colorFactor(
      rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlBu")),
      domain = as.character(seq_len(n))
    )
    fill_colors <- pal(bin_vals)
    
    leafletProxy("risk_map") |>
      clearGroup("water_stations") |>
      addCircleMarkers(
        data        = df,
        lng         = ~longitude_decimal,
        lat         = ~latitude_decimal,
        color       = "black",
        weight      = 1.5,
        fillColor   = fill_colors,
        fillOpacity = 1,
        radius      = 8,
        group       = "water_stations",
        popup       = df$popup_text,
        options     = pathOptions(pane = "pointPane")
      )
  } else {
    water_bin_breaks(NULL)
    
    df <- water_stations_df()
    if (!is.null(df) && nrow(df) > 0) {
      hq_vals     <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
      pal         <- colorNumeric("RdYlBu", domain = hq_vals, reverse = TRUE, na.color = "grey")
      fill_colors <- pal(df$HQ)
      
      leafletProxy("risk_map") |>
        clearGroup("water_stations") |>
        addCircleMarkers(
          data        = df,
          lng         = ~longitude_decimal,
          lat         = ~latitude_decimal,
          color       = "black",
          weight      = 1.5,
          fillColor   = fill_colors,
          fillOpacity = 1,
          radius      = 8,
          group       = "water_stations",
          popup       = df$popup_text,
          options     = pathOptions(pane = "pointPane")
        )
    }
  }
}, ignoreInit = TRUE)
  
  observeEvent(c(input$apply_sed_bins, input$bin_sediment), {
    is_binned <- isTRUE(input$bin_sediment) && input$apply_sed_bins > 0
    
    if (is_binned) {
      df <- sed_stations_df()
      r  <- tryCatch(sediment_raster(), error = function(e) NULL)
      
      if (is.null(df) || nrow(df) == 0) {
        showNotification(tr("notif_score_sed_bin"), type = "warning", duration = 4)
        return()
      }
      if (is.null(r)) {
        showNotification(tr("notif_create_sed_bin"), type = "warning", duration = 4)
        return()
      }
      
      n      <- if (is.null(input$sed_nbins) || is.na(input$sed_nbins)) 5 else input$sed_nbins
      method <- if (is.null(input$sed_bin_method)) "quantile" else input$sed_bin_method
      
      hq_source <- terra::values(r, na.rm = TRUE)
      hq_source <- hq_source[!is.na(hq_source)]
      
      breaks <- switch(method,
                       quantile       = {
                         station_hqs <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
                         unique(quantile(station_hqs, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
                       },
                       equal_interval = seq(min(hq_source), max(hq_source), length.out = n + 1),
                       equal_area     = unique(quantile(hq_source, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
      )
      breaks[1]              <- min(breaks[1], min(hq_source))
      breaks[length(breaks)] <- max(breaks[length(breaks)], max(hq_source))
      
      sed_bin_breaks(breaks)
      
      n        <- length(breaks) - 1
      bin_vals <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
      pal      <- colorFactor(
        rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlGn")),
        domain = as.character(seq_len(n))
      )
      fill_colors <- pal(bin_vals)
      
      leafletProxy("risk_map") |>
        clearGroup("sed_stations") |>
        addCircleMarkers(
          data        = df,
          lng         = ~longitude_decimal,
          lat         = ~latitude_decimal,
          color       = "black",
          weight      = 1.5,
          fillColor   = fill_colors,
          fillOpacity = 1,
          radius      = 8,
          group       = "sed_stations",
          popup       = df$popup_text,
          options     = pathOptions(pane = "pointPane")
        )
    } else {
      sed_bin_breaks(NULL)
      
      df <- sed_stations_df()
      if (!is.null(df) && nrow(df) > 0) {
        hq_vals     <- df$HQ[!is.na(df$HQ) & is.finite(df$HQ)]
        pal         <- colorNumeric("RdYlGn", domain = hq_vals, reverse = TRUE, na.color = "grey")
        fill_colors <- pal(df$HQ)
        
        leafletProxy("risk_map") |>
          clearGroup("sed_stations") |>
          addCircleMarkers(
            data        = df,
            lng         = ~longitude_decimal,
            lat         = ~latitude_decimal,
            color       = "black",
            weight      = 1.5,
            fillColor   = fill_colors,
            fillOpacity = 1,
            radius      = 8,
            group       = "sed_stations",
            popup       = df$popup_text,
            options     = pathOptions(pane = "pointPane")
          )
      }
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$bin_water, {
    if (!isTRUE(input$bin_water)) water_bin_breaks(NULL)
  })
  
  observeEvent(input$bin_sediment, {
    if (!isTRUE(input$bin_sediment)) sed_bin_breaks(NULL)
  })
  
  
  ######## LEGEND ###########
  
  water_scored_stations <- reactiveVal(NULL)
  sed_scored_stations <- reactiveVal(NULL)
  water_stations_df <- reactiveVal(NULL)
  sed_stations_df   <- reactiveVal(NULL)
  
  water_hq_range <- reactive({
    pts <- water_scored_stations()
    req(pts)
    range(pts$HQ, na.rm = TRUE)
  })
  
  sed_hq_range <- reactive({
    pts <- sed_scored_stations()
    req(pts)
    range(pts$HQ, na.rm = TRUE)
  })
  
  observe({
    leafletProxy("risk_map") |> removeControl("map_legend")
    
    html <- ""
    
    # Water (stations or raster)
    if (isTRUE(input$risk_water) || isTRUE(input$risk_water_stations)) {
      is_binned <- isTRUE(input$bin_water) && !is.null(input$apply_water_bins) && input$apply_water_bins > 0
      if (is_binned) {
        breaks <- water_bin_breaks()
        if (!is.null(breaks)) {
          n    <- length(breaks) - 1
          bins <- as.character(seq_len(n))
          pal  <- colorFactor(rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlBu")), domain = bins)
          html <- paste0(html, legend_color_bar(pal, "Water Score (binned)", NULL, NULL, bins = bins))
        } else {
          pal <- colorNumeric("RdYlBu", domain = water_hq_range(), reverse = TRUE)
          html <- paste0(html, legend_color_bar(pal, "Water Score", water_hq_range()[1], water_hq_range()[2]))
        }
      } else {
        pal <- colorNumeric("RdYlBu", domain = water_hq_range(), reverse = TRUE)
        html <- paste0(html, legend_color_bar(pal, "Water Score", water_hq_range()[1], water_hq_range()[2]))
      }
    }
    
    # Sediment (stations or raster)
    if (isTRUE(input$risk_sediment) || isTRUE(input$risk_sed_stations)) {
      is_binned <- isTRUE(input$bin_sediment) && !is.null(input$apply_sed_bins) && input$apply_sed_bins > 0
      if (is_binned) {
        breaks <- sed_bin_breaks()
        if (!is.null(breaks)) {
          n    <- length(breaks) - 1
          bins <- as.character(seq_len(n))
          pal  <- colorFactor(rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlGn")), domain = bins)
          html <- paste0(html, legend_color_bar(pal, "Sediment Score (binned)", NULL, NULL, bins = bins))
        } else {
          pal <- colorNumeric("RdYlGn", domain = sed_hq_range(), reverse = TRUE)
          html <- paste0(html, legend_color_bar(pal, "Sediment Score", sed_hq_range()[1], sed_hq_range()[2]))
        }
      } else {
        pal <- colorNumeric("RdYlGn", domain = sed_hq_range(), reverse = TRUE)
        html <- paste0(html, legend_color_bar(pal, "Sediment Score", sed_hq_range()[1], sed_hq_range()[2]))
      }
    }
    
    # EJI
    if (isTRUE(input$risk_eji)) {
      if (exists("eji_data")) {
        is_binned <- isTRUE(input$bin_eji) && !is.null(eji_bin_breaks())
        if (is_binned) {
          breaks <- eji_bin_breaks()
          n      <- length(breaks) - 1
          bins   <- as.character(seq_len(n))
          pal    <- colorFactor(RColorBrewer::brewer.pal(min(n, 9), "Purples"), domain = bins)
          html   <- paste0(html, legend_color_bar(pal, "EJI Score (binned)", NULL, NULL, bins = bins))
        } else {
          eji_min <- min(eji_data$eji, na.rm = TRUE)
          eji_max <- max(eji_data$eji, na.rm = TRUE)
          pal     <- colorNumeric("Purples", domain = c(eji_min, eji_max))
          html    <- paste0(html, legend_color_bar(pal, "EJI Score", eji_min, eji_max))
        }
      }
    }
    
    # Population density
    if (isTRUE(input$risk_pop_density)) {
      if (exists("pop_raster")) {
        pop_vals <- terra::values(pop_raster, na.rm = TRUE)
        pop_vals <- pop_vals[pop_vals > 0]
        is_binned <- isTRUE(input$bin_pop) && !is.null(pop_bin_breaks())
        if (is_binned) {
          breaks <- pop_bin_breaks()
          n      <- length(breaks) - 1
          bins   <- as.character(seq_len(n))
          pal <- colorFactor(viridisLite::magma(min(n, 9)), domain = bins)
          html   <- paste0(html, legend_color_bar(pal, "Population Density (binned)", NULL, NULL, bins = bins))
        } else {
          pal  <- colorNumeric("magma", domain = pop_vals, na.color = "transparent")
          html <- paste0(html, legend_color_bar(pal, "Population Density (√pop/km²)", min(pop_vals), max(pop_vals)))
        }
      }
    }
    
    # Tailings
    if (isTRUE(input$risk_tailings)) {
      html <- paste0(html, legend_categorical(
        "Tailings/Facilities",
        colors = c("#E41A1C", "#377EB8", "#FF7F00", "#984EA3", "#A65628", "#4DAF4A"),
        labels = c("Tailing", "Reception", "Storage", "Refinery", "Heap", "Mine Facility")
      ))
    }
    
    if (isTRUE(input$risk_combined)) {
      if (!is.null(combined_risk_raster())) {
        r    <- combined_risk_raster()
        vals <- sort(unique(terra::values(r, na.rm = TRUE)))
        n    <- length(vals)
        bins <- as.character(vals)
        pal <- colorFactor(
          colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(n),
          domain   = as.character(vals),
          levels   = as.character(vals)
        )
        html <- paste0(html, legend_color_bar(pal, "Combined Risk Score", NULL, NULL, bins = bins))
      }
    }
    
    if (nchar(html) > 0) {
      leafletProxy("risk_map") |>
        addControl(html = legend_wrapper(html), position = "bottomleft", layerId = "map_legend")
    }
  })
  
  
  # When station layer is created
  observeEvent(water_stations_data(), {
    water_scored_stations(water_stations_data())
  })
  
  # When risk raster is created
  observeEvent(water_risk_result(), {
    water_scored_stations(water_risk_result()$snapped_points)
  })
  
  observeEvent(sed_stations_data(), {
    sed_scored_stations(sed_stations_data())
  })
  
  observeEvent(sediment_risk_result(), {
    sed_scored_stations(sediment_risk_result()$snapped_points)
  })
  
  # ── Reactive to store binned EJI raster (created at EJI bin time) ──────────
  eji_raster_binned <- reactiveVal(NULL)
  
  
  # ── Combined risk layer ─────────────────────────────────────────────────────
  
observeEvent(input$create_combined, {
  
  water_r <- tryCatch(water_raster(), error = function(e) NULL)
  sed_r   <- tryCatch(sediment_raster(), error = function(e) NULL)
  if (!is.null(water_r)) message("Water res (UTM meters): ", paste(round(terra::res(water_r)), collapse = ", "))
  if (!is.null(sed_r))   message("Sed res (UTM meters): ",   paste(round(terra::res(sed_r)),   collapse = ", "))
  if (exists("pop_raster")) message("Pop res (degrees): ",   paste(terra::res(pop_raster),      collapse = ", "))
  if (!is.null(eji_raster_binned())) message("EJI res (degrees): ", paste(terra::res(eji_raster_binned()), collapse = ", "))
  
  selected <- c(
    water = isTRUE(input$combined_water),
    sed   = isTRUE(input$combined_sed),
    eji   = isTRUE(input$combined_eji),
    pop   = isTRUE(input$combined_pop)
  )
  
  message("[create_combined] Selected layers have been updated: ")
  active <- risk_individuals()
  message(names(active))
  
  
  if (!any(selected)) {
    showNotification(tr("notif_select_layers_combine"), type = "warning")
    return()
  }
  
  # ── Validate that each selected layer is binned and ready ─────────────────
  not_ready <- c()
  
  if (selected["water"]) {
    water_binned_ok <- isTRUE(input$bin_water) &&
                       !is.null(input$apply_water_bins) &&
                       input$apply_water_bins > 0 &&
                       !is.null(water_bin_breaks()) &&
                       !is.null(water_raster())
    if (!water_binned_ok) not_ready <- c(not_ready, "Water Risk")
  }
  
  if (selected["sed"]) {
    sed_binned_ok <- isTRUE(input$bin_sediment) &&
                     !is.null(input$apply_sed_bins) &&
                     input$apply_sed_bins > 0 &&
                     !is.null(sed_bin_breaks()) &&
                     !is.null(sediment_raster())
    if (!sed_binned_ok) not_ready <- c(not_ready, "Sediment Risk")
  }
  
  if (selected["eji"]) {
    if (is.null(eji_raster_binned())) not_ready <- c(not_ready, "EJI Vulnerability")
  }
  
  if (selected["pop"]) {
    pop_binned_ok <- exists("pop_raster") &&
                     isTRUE(input$bin_pop) &&
                     !is.null(input$apply_pop_bins) &&
                     input$apply_pop_bins > 0 &&
                     !is.null(pop_bin_breaks())
    if (!pop_binned_ok) not_ready <- c(not_ready, "Population Density")
  }
  
  if (length(not_ready) > 0) {
    showNotification(
      paste0("The following layer(s) must be created and binned before combining: ",
             paste(not_ready, collapse = ", "), "."),
      type     = "warning",
      duration = 8
    )
    return()
  }
  
  withProgress(message = tr("notif_rendering_combined"), {
    tryCatch({
    
      cat("=== COMBINE DEBUG ===\n")
      active = risk_individuals()
      cat("Selected layers:", paste(names(active), collapse = ", "), "\n")
      
      water_r <- tryCatch(water_raster(), error = function(e) NULL)
      sed_r   <- tryCatch(sediment_raster(), error = function(e) NULL)
      cat("water_raster() is null:", is.null(water_r), "\n")
      cat("sediment_raster() is null:", is.null(sed_r), "\n")
      cat("pop_raster exists:", exists("pop_raster"), "\n")
      cat("eji_raster_binned() is null:", is.null(eji_raster_binned()), "\n")
      
      base_utm <- if (!is.null(water_r)) water_r else if (!is.null(sed_r)) sed_r else NULL
      cat("base_utm is null:", is.null(base_utm), "\n")
      
      # Determine target resolution
      if (isTRUE(input$combined_custom_res) && !is.null(input$combined_resolution) && !is.na(input$combined_resolution)) {
        target_res <- as.numeric(input$combined_resolution)
        cat("Using custom resolution:", target_res, "\n")
      } else {
        target_res <- if (!is.null(base_utm) && (selected["eji"] || selected["pop"])) {
          if (exists("pop_raster")) {
            terra::res(pop_raster)[1]
          } else {
            terra::res(terra::project(base_utm, "EPSG:4326", method = "near"))[1]
          }
        } else if (!is.null(base_utm)) {
          terra::res(terra::project(base_utm, "EPSG:4326", method = "near"))[1]
        } else if (exists("pop_raster")) {
          terra::res(pop_raster)[1]
        } else {
          showNotification("No template available.", type = "warning")
          return()
        }
        cat("Using auto resolution:", target_res, "\n")
      }
      
      if (isTRUE(input$combined_custom_res) && target_res < 0.008 && (selected["eji"] || selected["pop"])) {
        showNotification(
          "Fine resolution with EJI or population layers may be slow to compute.",
          type = "warning", duration = 6
        )
      }
      
      # Build template at target resolution
      base_extent <- if (!is.null(base_utm)) {
        terra::ext(terra::project(base_utm, "EPSG:4326", method = "near"))
      } else if (exists("pop_raster")) {
        terra::ext(pop_raster)
      } else {
        showNotification("No template available.", type = "warning")
        return()
      }
      
      template_4326 <- terra::rast(base_extent, resolution = target_res, crs = "EPSG:4326")
      cat("template_4326 res:", paste(terra::res(template_4326), collapse = ", "), "\n")
      cat("template_4326 crs:", terra::crs(template_4326, describe = TRUE)$code, "\n")
      
      wd <- tryCatch(water_raster_display(), error = function(e) { message("water_display error: ", e$message); NULL })
      sd <- tryCatch(sediment_raster_display(), error = function(e) { message("sed_display error: ", e$message); NULL })
      message("water_display is null: ", is.null(wd))
      message("sed_display is null: ", is.null(sd))
      cat("template_4326 res:", paste(terra::res(template_4326), collapse = ", "), "\n")
      cat("template_4326 crs:", terra::crs(template_4326, describe = TRUE)$code, "\n")
      
      layers <- list()
      
      if (selected["water"]) {
        cat("Standardizing water...\n")
        wd <- tryCatch(water_raster_display(), error = function(e) { message("water_display error: ", e$message); NULL })
        if (is.null(wd)) { showNotification("Water display not ready.", type = "warning"); return() }
        layers[["water"]] <- standardize_raster(wd, template_4326, fill_nas = FALSE)
        cat("Water done, ncell:", terra::ncell(layers[["water"]]), "\n")
      }
      
      if (selected["sed"]) {
        cat("Standardizing sed...\n")
        sd <- tryCatch(sediment_raster_display(), error = function(e) { message("sed_display error: ", e$message); NULL })
        if (is.null(sd)) { showNotification("Sediment display not ready.", type = "warning"); return() }
        layers[["sed"]] <- standardize_raster(sd, template_4326, fill_nas = FALSE)
        cat("Sed done, ncell:", terra::ncell(layers[["sed"]]), "\n")
      }
      
      if (selected["eji"]) {
        cat("Standardizing EJI...\n")
        layers[["eji"]] <- standardize_raster(eji_raster_binned(), template_4326, fill_nas = TRUE)
        cat("EJI done, ncell:", terra::ncell(layers[["eji"]]), "\n")
      }
      
      if (selected["pop"]) {
        pop_binned <- bin_raster(pop_raster, pop_bin_breaks())
        message("Pop NAs before replacement: ", sum(is.na(terra::values(pop_binned))))
        pop_vals <- terra::values(pop_binned)
        pop_vals[is.na(pop_vals)] <- 1L
        terra::values(pop_binned) <- pop_vals
        message("Pop NAs after replacement: ", sum(is.na(terra::values(pop_binned))))
        layers[["pop"]] <- standardize_raster(pop_binned, template_4326, fill_nas = FALSE)
        message("Pop NAs after standardize: ", sum(is.na(terra::values(layers[["pop"]]))))
      }
      
      cat("All layers built:", paste(names(layers), collapse = ", "), "\n")
      cat("Reducing...\n")
      combined <- Reduce("+", layers)
      cat("Combined ncell:", terra::ncell(combined), "\n")
      cat("Combined NA count:", sum(is.na(terra::values(combined))), "\n")
      
      # Clip to basin boundary
      combined <- terra::crop(combined, terra::vect(pilco_basin))
      combined <- terra::mask(combined, terra::vect(pilco_basin))
      
      combined_risk_raster(combined)
      showNotification(tr("notif_combined_created"), type = "message", duration = 4)
    
      risk_individuals(list(
        water = if("water" %in% active) layers[["water"]] else NULL,
        sed = if("sed" %in% active) layers[["sed"]] else NULL,
        eji = if("eji" %in% active) layers[["eji"]] else NULL,
        pop = if("pop" %in% active) layers[["pop"]] else NULL,
      ))
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      message("Combined layer error: ", e$message)
    })
  })

})
  
  # combined risk raster render observer (fired when created)
observeEvent(combined_risk_raster(), {
  r <- combined_risk_raster()
  req(r)
  
  # Project to Web Mercator to match leaflet's internal CRS
  r_3857 <- terra::project(r, "EPSG:3857", method = "near")
  
  vals <- sort(unique(terra::values(r_3857, na.rm = TRUE)))
  n    <- length(vals)
  pal  <- colorFactor(
    colorRampPalette(RColorBrewer::brewer.pal(9, "YlOrRd"))(n),
    domain   = as.character(vals),
    levels   = as.character(vals),
    na.color = "transparent"
  )
  
  id <- showNotification(tr("notif_rendering_combined"), duration = NULL, closeButton = FALSE, type = "message")
  on.exit(removeNotification(id))
  
  leafletProxy("risk_map") |>
    addRasterImage(r_3857, colors = pal, opacity = 1,
                   layerId = "combined_risk", group = "combined_risk",
                   options = leafletOptions(pane = "rasterPane2")) |>
    showGroup("combined_risk")
  
  updateCheckboxInput(session, "risk_combined", value = TRUE)
})
  
  # combined raster show/hide observer
  observeEvent(input$risk_combined, {
    if (!isTRUE(input$risk_combined)) {
      leafletProxy("risk_map") |> hideGroup("combined_risk")
    } else {
      if (!is.null(combined_risk_raster())) {
        leafletProxy("risk_map") |> showGroup("combined_risk")
      }
    }
  }, ignoreInit = TRUE)
  
  
  ######### WATERSHEDS #############
  
  water_watersheds <- reactiveVal(NULL)
  sed_watersheds   <- reactiveVal(NULL)
  
  observeEvent(input$delineate_water_watersheds, {
    df <- water_stations_df()
    if (is.null(df) || nrow(df) == 0) {
      showNotification(tr("notif_score_water_ws"),
                       type = "warning", duration = 5)
      return()
    }
    
    withProgress(message = "Delineating water station subcatchments...", {
      tryCatch({
        result <- delineate_subcatchments(
          station_df    = df,
          flow_dir_path = "data/dem/flow_direction_wbt2.tif",
          flow_acc_path = "data/dem/flow_accumulation_wbt2.tif",
          snap_dist     = 10000
        )
        water_watersheds(result)
        showNotification(tr("notif_water_ws_done"), type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        message("Watershed error: ", e$message)
      })
    })
  })
  
  observeEvent(input$delineate_sed_watersheds, {
    df <- sed_stations_df()
    if (is.null(df) || nrow(df) == 0) {
      showNotification(tr("notif_score_sed_ws"),
                       type = "warning", duration = 5)
      return()
    }
    
    withProgress(message = "Delineating sediment station subcatchments...", {
      tryCatch({
        result <- delineate_subcatchments(
          station_df    = df,
          flow_dir_path = "data/dem/flow_direction_wbt2.tif",
          flow_acc_path = "data/dem/flow_accumulation_wbt2.tif",
          snap_dist     = 10000
        )
        sed_watersheds(result)
        showNotification(tr("notif_sed_ws_done"), type = "message", duration = 4)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 10)
        message("Watershed error: ", e$message)
      })
    })
  })
  
  # Display observer
  observe({
    water_bin_breaks()
    sed_bin_breaks()
    water_watersheds()
    sed_watersheds()
    
    # Water watersheds
    if (isTRUE(input$risk_watersheds_water) && !is.null(water_watersheds())) {
      df <- water_watersheds()
      is_binned <- isTRUE(input$bin_water) && !is.null(water_bin_breaks())
      
      if (is_binned) {
        breaks    <- water_bin_breaks()
        n         <- length(breaks) - 1
        bin_vals  <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
        pal       <- colorFactor(rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlBu")), domain = as.character(seq_len(n)))
        fill_colors  <- pal(bin_vals)
        border_colors <- pal(bin_vals)
      } else {
        pal          <- colorNumeric("RdYlBu", domain = df$HQ, reverse = TRUE, na.color = "transparent")
        fill_colors  <- pal(df$HQ)
        border_colors <- pal(df$HQ)
      }
      
      leafletProxy("risk_map") %>%
        clearGroup("water_watersheds") %>%
        addPolygons(
          data        = df,
          fillColor   = fill_colors,
          fillOpacity = 0.4,
          color       = border_colors,
          weight      = 1.5,
          opacity     = 0.8,
          group       = "water_watersheds",
          label       = ~paste0(station, " | HQ: ", round(HQ, 2)) %>% lapply(htmltools::HTML),
          options     = pathOptions(pane = "polygonPane")
        )
    } else {
      leafletProxy("risk_map") %>% clearGroup("water_watersheds")
    }
    
    # Sediment watersheds
    if (isTRUE(input$risk_watersheds_sed) && !is.null(sed_watersheds())) {
      df <- sed_watersheds()
      is_binned <- isTRUE(input$bin_sediment) && !is.null(sed_bin_breaks())
      
      if (is_binned) {
        breaks    <- sed_bin_breaks()
        n         <- length(breaks) - 1
        bin_vals  <- as.character(cut(df$HQ, breaks = breaks, labels = FALSE, include.lowest = TRUE))
        pal       <- colorFactor(rev(RColorBrewer::brewer.pal(min(n, 9), "RdYlGn")), domain = as.character(seq_len(n)))
        fill_colors  <- pal(bin_vals)
        border_colors <- pal(bin_vals)
      } else {
        pal          <- colorNumeric("RdYlGn", domain = df$HQ, reverse = TRUE, na.color = "transparent")
        fill_colors  <- pal(df$HQ)
        border_colors <- pal(df$HQ)
      }
      
      leafletProxy("risk_map") %>%
        clearGroup("sed_watersheds") %>%
        addPolygons(
          data        = df,
          fillColor   = fill_colors,
          fillOpacity = 0.4,
          color       = border_colors,
          weight      = 1.5,
          opacity     = 0.8,
          group       = "sed_watersheds",
          label       = ~paste0(station, " | HQ: ", round(HQ, 2)) %>% lapply(htmltools::HTML),
          options     = pathOptions(pane = "polygonPane")
        )
    } else {
      leafletProxy("risk_map") %>% clearGroup("sed_watersheds")
    }
  })
  
  # Jackson TO DO:
  # - implement ability to click on the map and retrieve the different scores at that point
  # - revisit UI
  # - Data Preparation tab
  
  data_prep_water <- reactive({
    req(guard_data(master_data$water_scored))
    
    df <- master_data$water_scored |>
      group_by(year) |>
      summarise(n_params = length(unique(parameter)),
                n_stations = length(unique(station)),
                n_obs = n(),
                HQ_range = paste0(round(min(HQ, na.rm = TRUE), digits = 2), " - ", round(max(HQ, na.rm = TRUE), digits = 2)))
    
    return(df)
  })
  
  output$data_prep_water_table <- renderDT({
    data_prep_water()
  })
  
  data_prep_sed <- reactive({
    req(guard_data(master_data$sed_scored))
    
    df <- master_data$sed_scored |>
      group_by(year) |>
      summarise(n_params = length(unique(parameter)),
                n_stations = length(unique(station)),
                n_obs = n(),
                HQ_range = paste0(round(min(HQ, na.rm = TRUE), digits = 2), " - ", round(max(HQ, na.rm = TRUE), digits = 2)))
    
    return(df)
  })
  
  output$data_prep_sed_table <- renderDT({
    data_prep_sed()
  })
  
  output$full_water_table <- renderDT({
    master_data$water_scored |>
      mutate(
        value = round(concentration, 2),
        HQ    = round(HQ, 2)
      ) |>
      select(station, date, parameter, fraction, value, unit, HQ, data_source)
  })
  
  output$full_sed_table <- renderDT({
    master_data$sed_scored |>
      mutate(
        value = round(concentration, 2),
        HQ    = round(HQ, 2)
      ) |>
      select(station, date, parameter, sieve_size, value, unit, HQ, data_source)
  })
  
} # End Server