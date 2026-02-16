plot_pilcomayo_ts <- function(
    data,
    media,
    param,
    station,
    fraction = "any",
    standard_mode = "all"
) {
  message("\n[plot_pilcomayo_ts] START")
  message("Param: ", param, " | Media: ", media, " | Station: ", station,
          " | Mode: ", standard_mode)

  library(ggiraph) # me being a bit lazy here...
  # -------------------------------------------------------
  # DEFINE MEDIA LABEL (FIX FOR YOUR ERROR)
  # -------------------------------------------------------
  media_label <- ifelse(media == "water", "Water", "Sediment")

  # -------------------------------------------------------
  # 1. FILTER INPUT DATA
  # -------------------------------------------------------
  
  df <- data %>%
    filter(
      .data$media == .env$media,
      .data$parameter == .env$param,
    )
  
  if (!station %in% c("all", "All Stations")) {
    df = df |> filter(.data$station ==.env$station)
  }

  if (nrow(df) == 0) {
    message("No data found for this selection.")
    no_data_callout
    return(no_data_callout(media_label))
  }

  # fraction filter
  if (fraction != "any" && param != "pH" && any(df$fraction == fraction)) {
    df <- df %>% filter(fraction == fraction)
  }

  if (param == "pH") df <- df %>% filter(unit == "u")

  df$date <- as.Date(df$date)
  
  # Standardize units within the filtered dataset
  reference_unit <- df$unit[1]
  
  df <- df %>%
    rowwise() %>%
    mutate(
      conv_obj = list(compare_units(reference_unit, unit)),
      concentration = ifelse(
        conv_obj$convertible, 
        concentration * conv_obj$conversion_factor, 
        concentration
      ),
      unit = reference_unit  # Update unit to reference unit
    ) %>%
    ungroup()

  # -------------------------------------------------------
  # 2. LOAD + FILTER STANDARDS
  # -------------------------------------------------------
  regulator_map <- c(
    "EPA" = "epa",
    "WHO" = "who",
    "USGS" = "usgs",
    "FAO" = "fao"
  )

  if (standard_mode == "none") {
    param_stds <- NULL
    has_standard <- FALSE

  } else {
    if (standard_mode == "strict") {
      param_stds <- stds %>% filter(
        .data$media == .env$media,
        str_detect(.data$parameter, fixed(.env$param, ignore_case = TRUE))
      )

    } else if (standard_mode == "bol") {
      param_stds <- stds %>% filter(
        regulator == "Bolivian Law 1333",
        .data$media == .env$media,
        str_detect(.data$parameter, fixed(.env$param, ignore_case = TRUE))
      )
      cat("\n\nbol selected", nrow(param_stds))

    } else if (standard_mode == "all") {
      param_stds <- stds %>%
        filter(
          .data$media == .env$media,
          .data$parameter == .env$param
        )

    } else if (standard_mode %in% names(regulator_map)) {
      regulator_code <- regulator_map[[standard_mode]]
      param_stds <- stds %>% filter(
        tolower(.data$regulator) == .env$regulator_code,
        .data$media == .env$media,
        str_detect(.data$parameter, fixed(.env$param, ignore_case = TRUE))
      )

    } else {
      param_stds <- NULL
    }

    has_standard <- !is.null(param_stds) && nrow(param_stds) > 0
  }

  if (!has_standard) param_stds <- NULL

  # -------------------------------------------------------
  # NO-STANDARDS CALLOUT OVERLAY — FIX ADDED
  # -------------------------------------------------------
  no_standards_banner <- ""
  if (!has_standard) {
    no_standards_banner <- sprintf("
      <div style='
        position:absolute;
        top:20px; 
        left:50%%; 
        transform:translateX(-50%%);
        padding:12px 20px;
        background:rgba(255,230,230,0.9);
        border-left:4px solid #dc3545;
        border-radius:4px;
        font-size:14px;
        z-index:9999;
        text-align:center;
      '>
        <strong>No standards found</strong><br>
        No regulatory thresholds exist for %s (%s).
      </div>",
      param, media_label
    )
  }

  # -------------------------------------------------------
  # 3. PROCESS STANDARDS
  # -------------------------------------------------------
  if (has_standard) {
    sample_unit <- df$unit[1]

    param_stds <- param_stds %>%
      rowwise() %>%
      mutate(
        conv_obj = list(compare_units(sample_unit, unit)),
        convertible = conv_obj$convertible,
        conv_factor = ifelse(convertible, conv_obj$conversion_factor, NA_real_),
        value_converted = ifelse(convertible, value * conv_factor, value),
        display_unit  = ifelse(convertible, sample_unit, unit)
      ) %>%
      ungroup()
  }

  # -------------------------------------------------------
  # 4. DAILY AVERAGES
  # -------------------------------------------------------
  df_avg <- df %>%
    group_by(date) %>%
    summarise(avg_concentration = mean(concentration, na.rm = TRUE), .groups = "drop")

  # -------------------------------------------------------
  # 5. DEFINE ALPHA VALUE SAFELY (ALWAYS CREATE alpha_value)
  # -------------------------------------------------------
  if (media == "sediment") {
    
    if (!all(is.na(df$distance_from_bank))) {
      
      min_dist <- min(df$distance_from_bank, na.rm = TRUE)
      max_dist <- max(df$distance_from_bank, na.rm = TRUE)
      
      df$alpha_value <- scales::rescale(
        1 - (df$distance_from_bank - min_dist) / (max_dist - min_dist),
        to = c(0.3, 1)
      )
      
    } else {
      df$alpha_value <- 1
    }
    
  } else {
    # Water ALWAYS gets alpha = 1
    df$alpha_value <- 1
  }
  
  
  # -------------------------------------------------------
  # 6. HOVER TEXT
  # (unchanged except that HQ logic still works)
  # -------------------------------------------------------
  df <- df %>%
    rowwise() %>%
    mutate(
      hq_text = if_else(
        has_HQ,
        paste0("HQ: ", HQ, " (", std_info$HQ$std_reg, ": ", std_info$HQ$std_val, std_info$HQ$std_unit, ")"),
        ""
      ),
      fraction_text = if_else(
        !is.na(fraction) & fraction != "",
        paste0("Fraction: ", fraction, "<br>"),
        ""
      ),
      sed_text = if (media == "sediment") {
        paste0(
          "Sieve: ", sieve_size %||% "N/A", "<br>",
          "Dist. from bank: ", distance_from_bank %||% "N/A", "<br>"
        )
      } else "",
      hover_text = paste0(
        "Station: ", station, "<br>",
        "Date: ", format(date, "%Y-%m-%d"), "<br>",
        str_to_title(parameter), ": ", round(concentration, 3), " ", unit, "<br>",
        ifelse(nchar(hq_text) > 0, paste0(hq_text, "<br>"), ""),
        ifelse(nchar(fraction_text) > 0, fraction_text, ""),
        sed_text
      )
    ) %>% ungroup()

  # -------------------------------------------------------
  # 7. BASE PLOT
  # -------------------------------------------------------
  station_label = if_else(station %in% c("all", "All Stations"),"All Stations", station)
  
  p <- ggplot() +
    geom_line(
      data = df_avg,
      aes(x = date, y = avg_concentration),
      color = "black",
      linewidth = 0.8
    ) +
    ggiraph::geom_point_interactive(
      data = df,
      aes(x = date, y = concentration, alpha = alpha_value, tooltip = hover_text),
      color = "black",
      size = 2
    ) +
    scale_alpha_identity() +
    labs(
      x = "Date",
      y = paste0(param, " (", df$unit[1], ")"),
      title = paste0("Time Series of ", param, " at ", station_label, " (", str_to_title(media), ")")
    ) +
    theme_minimal()+ 
    scale_y_log10(
      breaks = scales::breaks_log(10),
      labels = scales::label_number(accuracy = 1, big.mark = ",")
    )
    

  # -------------------------------------------------------
  # 8. ADD STANDARD LINES (unchanged)
  # -------------------------------------------------------

  if (has_standard) {
    std_dates <- seq(min(df$date), max(df$date), length.out = 200)

    for (i in seq_len(nrow(param_stds))) {
      hover_text <- paste0(
        param_stds$regulator[i], "<br>",
        "Standard: ",
        round(param_stds$value_converted[i], 3), " ",
        param_stds$display_unit[i]
      )

      std_df <- data.frame(
        date = std_dates,
        std_value = param_stds$value_converted[i],
        hover_label = hover_text,
        stringsAsFactors = FALSE
      )

      p <- p + ggiraph::geom_line_interactive(
        data = std_df,
        aes(x = date, y = std_value, tooltip = hover_label, data_id = hover_label),
        color = "red",
        linetype = "dashed",
        linewidth = 0.8
      )
    }
  }

  # -------------------------------------------------------
  # 9. RETURN: PLOT + NO-STANDARDS OVERLAY
  # -------------------------------------------------------
  ggiraph::girafe(
    ggobj = p,
    options = list(
      opts_hover(css = "stroke:orange;stroke-width:2px;"),
      opts_toolbar(saveaspng = FALSE)
    )
  )
  # return(
  #   htmltools::tagList(
  #     HTML(no_standards_banner),
  #     girafe(
  #       ggobj = p,
  #       options = list(
  #         opts_hover(css = "stroke:orange;stroke-width:2px;"),
  #         opts_toolbar(saveaspng = FALSE)
  #       )
  #     )
  #   )
  # )
}


