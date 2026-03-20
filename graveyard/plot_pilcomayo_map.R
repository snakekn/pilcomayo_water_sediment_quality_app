plot_pilcomayo_map <- function(data, media, param, date, fraction = "Total") {
  date <- as.Date(date)
  
  # Reverse color scheme for parameters where lower values are worse
  reverse_params <- c(
    "pH",
    "Oxygen",
    "Dissolved Oxygen",
    "Oxygen Saturation"
  )
  
  # Filter for media and parameter
  df <- data |>
    filter(media == !!media) |>
    filter(parameter == param)
  
  # Only apply fraction filter for parameters that actually have fractions
  # Skip for pH and other field parameters
  # Only do this step for water data (sediment is not broken into fractions for any parameters)
  if (media == "water") {
    if (param != "pH" && any(data$fraction == fraction)) {
      df <- df |>
        filter(fraction == !!fraction)
    }
  }
  
  # Special handling for pH - filter to pH units only
  if (param == "pH") {
    df <- df |>
      filter(unit == "u")
  }
  
  # Get data for the selected date
  date_data <- df |>
    filter(date == !!date)
  
  # Get stations with data on the selected date
  stations_with_data <- unique(date_data$station)
  
  # For stations without data on the selected date, get most recent prior data
  missing_stations <- df |>
    filter(
      !station %in% stations_with_data,
      date < !!date
    ) |>
    group_by(station) |>
    slice_max(date, n = 1, with_ties = FALSE) |>
    ungroup()
  
  # Combine current date data with most recent prior data
  df <- bind_rows(date_data, missing_stations)
  
  if (!"HQ" %in% names(df)) {
    print("scoring data")
    df <- score_data(df)
  }
  
  print("data scored")
  
  # Check if has_standard column exists, if not create it
  if (!"has_standard" %in% names(df)) {
    message("has_standard column missing, checking std_info...")
    # Try to determine if standards exist from std_info
    if ("std_info" %in% names(df)) {
      df <- df |>
        mutate(has_standard = !is.na(HQ) | !is.na(CR) | !is.na(WL))
    } else {
      # Default to FALSE if we can't determine
      df <- df |>
        mutate(has_standard = FALSE)
    }
  }
  
  df <- df |>
    mutate(
      marker_radius = 6,
      stroke_color = "black",
      stroke_weight = 1.5
    )
  
  # Choose color palette based on parameter type
  if (param %in% reverse_params) {
    pal <- colorNumeric(
      palette = c("firebrick", "lightsalmon", "grey99", "steelblue", "royalblue4"),  
      domain = df$concentration,
      na.color = "gray",
      reverse = FALSE
    )
  } else {
    pal <- colorNumeric(
      palette = "Reds",  
      domain = df$concentration,
      na.color = "gray",
      reverse = FALSE
    )
  }
  
  colors <- pal(df$concentration)
  
  # leaflet map
  m <- leaflet(df) |>
    addTiles() |>
    setView(lat=-23, lng=-61, zoom=6) |>
    addPolylines(data = pilco_line, 
                 color = "darkcyan", 
                 weight = 3, 
                 opacity = 0.8) |>
    addPolygons(data = bol_border,
                color = "black",
                weight = 3,
                fill = FALSE)
  
  # Add circle markers - check if has_standard exists and is not NULL/NA
  has_std <- if (nrow(df) > 0 && !is.null(df$has_standard)) {
    first(df$has_standard, default = FALSE)
  } else {
    FALSE
  }
  
  if (has_std) {
    # Determine which points are out of compliance
    if ("is_range_param" %in% names(df) && first(df$is_range_param, default = FALSE)) {
      df_compliant <- df |> filter(concentration >= param_std_low & concentration <= param_std_high)
      df_violation <- df |> filter(concentration < param_std_low | concentration > param_std_high)
    } else {
      df_compliant <- df |> filter(HQ < 1)
      df_violation <- df |> filter(HQ >= 1)
    }
    
    # Add compliant points
    if (nrow(df_compliant) > 0) {
      m <- m |>
        addCircleMarkers(
          data = df_compliant,
          lng = ~longitude_decimal,
          lat = ~latitude_decimal,
          radius = ~marker_radius,
          stroke = TRUE,
          color = ~stroke_color,
          weight = ~stroke_weight,
          fillOpacity = 0.8,
          fillColor = pal(df_compliant$concentration),
          label = lapply(seq_len(nrow(df_compliant)), function(i) {
            htmltools::HTML(paste0(
              "Site: ", df_compliant$station[i], "<br>",
              "Lat: ", df_compliant$latitude_decimal[i], "<br>",
              "Long: ", df_compliant$longitude_decimal[i], "<br>",
              "Date: ", df_compliant$date[i], "<br>",
              param, ": ", df_compliant$concentration[i], " ", df_compliant$unit[i], "<br>",
              "HQ: ", round(df_compliant$HQ[i], 3)
            ))
          })
        )
    }
    
    # Add violation points with double outline
    if (nrow(df_violation) > 0) {
      m <- m |>
        addCircleMarkers(
          data = df_violation,
          lng = ~longitude_decimal,
          lat = ~latitude_decimal,
          radius = 8.5,
          stroke = TRUE,
          color = "black",
          weight = 1.5,
          fillOpacity = 1,
          fillColor = "yellow"
        ) |>
        addCircleMarkers(
          data = df_violation,
          lng = ~longitude_decimal,
          lat = ~latitude_decimal,
          radius = 5.5,
          stroke = TRUE,
          color = "black",
          weight = 1.5,
          fillOpacity = 1,
          fillColor = pal(df_violation$concentration),
          label = lapply(seq_len(nrow(df_violation)), function(i) {
            htmltools::HTML(paste0(
              "Site: ", df_violation$station[i], "<br>",
              "Lat: ", df_violation$latitude_decimal[i], "<br>",
              "Long: ", df_violation$longitude_decimal[i], "<br>",
              "Date: ", df_violation$date[i], "<br>",
              param, ": ", df_violation$concentration[i], " ", df_violation$unit[i], "<br>",
              "HQ: ", round(df_violation$HQ[i], 3)
            ))
          })
        )
    }
  } else {
    # No standard exists, add all points with default styling
    m <- m |>
      addCircleMarkers(
        lng = ~longitude_decimal,
        lat = ~latitude_decimal,
        radius = ~marker_radius,
        stroke = TRUE,
        color = ~stroke_color,
        weight = ~stroke_weight,
        fillOpacity = 0.8,
        fillColor = colors,
        label = lapply(seq_len(nrow(df)), function(i) {
          htmltools::HTML(paste0(
            "Site: ", df$station[i], "<br>",
            "Lat: ", df$latitude_decimal[i], "<br>",
            "Long: ", df$longitude_decimal[i], "<br>",
            "Date: ", df$date[i], "<br>",
            param, ": ", df$concentration[i], " ", df$unit[i]
          ))
        })
      )
  }
  
  # Add legend and standard info
  # Add legend and standard info
  if (has_std) {
    m <- m |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df$concentration,
        title = paste0(param, "<br>(", df$unit[1], ")"),
        opacity = 0.8
      )
    
    # Extract standard info from std_info column
    if ("std_info" %in% names(df) && nrow(df) > 0) {
      std_info <- first(df$std_info)
      
      # Check if this is a range parameter
      if ("is_range_param" %in% names(df) && first(df$is_range_param, default = FALSE)) {
        # Extract range values (if they exist in std_info)
        if ("param_std_low" %in% names(df) && "param_std_high" %in% names(df)) {
          std_low <- first(df$param_std_low)
          std_high <- first(df$param_std_high)
          display_unit <- first(df$unit)
          std_source <- "Standard"
          
          if (!is.na(std_low) && !is.na(std_high)) {
            m <- m |>
              htmlwidgets::onRender(
                paste0(
                  "function(el, x) {",
                  "  var legend = document.querySelector('.leaflet-bottom.leaflet-right');",
                  "  if (legend) {",
                  "    var standardDiv = document.createElement('div');",
                  "    standardDiv.className = 'leaflet-control';",
                  "    standardDiv.style.cssText = 'background: white; padding: 8px; border: 2px solid rgba(0,0,0,0.2); border-radius: 4px; margin-top: 10px; margin-bottom: 10px;';",
                  "    standardDiv.innerHTML = '<strong>Acceptable Range:</strong><br>", 
                  round(std_low, 3), " - ", round(std_high, 3), " ", display_unit, "<br>(",
                  std_source, ")';",
                  "    legend.appendChild(standardDiv);",
                  "  }",
                  "}"
                )
              )
          }
        }
      } else if (!is.null(std_info) && length(std_info) > 0) {
        # Try to extract from HQ field in std_info
        if (!is.null(std_info$HQ) && !is.na(std_info$HQ$std_val)) {
          std_val <- std_info$HQ$std_val
          std_unit <- std_info$HQ$std_unit
          std_source <- std_info$HQ$std_reg
          
          m <- m |>
            htmlwidgets::onRender(
              paste0(
                "function(el, x) {",
                "  var legend = document.querySelector('.leaflet-bottom.leaflet-right');",
                "  if (legend) {",
                "    var standardDiv = document.createElement('div');",
                "    standardDiv.className = 'leaflet-control';",
                "    standardDiv.style.cssText = 'background: white; padding: 8px; border: 2px solid rgba(0,0,0,0.2); border-radius: 4px; margin-top: 10px; margin-bottom: 10px;';",
                "    standardDiv.innerHTML = '<strong>Standard:</strong><br>", 
                round(std_val, 3), " ", std_unit, "<br>(",
                std_source, ")';",
                "    legend.appendChild(standardDiv);",
                "  }",
                "}"
              )
            )
        }
      }
    }
  } else {
    m <- m |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df$concentration,
        title = paste0(param, "<br>(", df$unit[1], ")"),
        opacity = 0.8
      )
  }
  
  return(m)
}
