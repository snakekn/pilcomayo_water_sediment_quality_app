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
  if (media == "drinking water") {
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
  
  df <- df |>
    calc_hq_in_df(param = param,
                  media = media,
                  plot_type = "map")
  
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
  
  # Add circle markers
  if (first(df$has_standard)) {
    # Determine which points are out of compliance
    if (exists("is_range_param") && first(df$is_range_param)) {
      # For range parameters, out of compliance if outside range
      df_compliant <- df |> filter(concentration >= param_std_low & concentration <= param_std_high)
      df_violation <- df |> filter(concentration < param_std_low | concentration > param_std_high)
    } else {
      # For single threshold, out of compliance if HQ >= 1
      df_compliant <- df |> filter(hq < 1)
      df_violation <- df |> filter(hq >= 1)
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
              "HQ: ", round(df_compliant$hq[i], 3)
            ))
          })
        )
    }
    
    # Add violation points with double outline
    if (nrow(df_violation) > 0) {
      # First layer: black outer ring
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
        )
      
      # Second layer: yellow middle ring + colored fill
      m <- m |>
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
              "HQ: ", round(df_violation$hq[i], 3)
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
  if (first(df$has_standard)) {
    m <- m |>
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df$concentration,
        title = paste0(param, "<br>(", df$unit[1], ")"),
        opacity = 0.8
      )
    
    # Add standard info box
    if (exists("is_range_param") && first(df$is_range_param)) {
      # Range-based standard
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
            round(first(df$param_std_low), 3), " - ", round(first(df$param_std_high), 3), " ", first(df$display_unit), "<br>(",
            first(df$std_source), ")';",
            "    legend.appendChild(standardDiv);",
            "  }",
            "}"
          )
        )
    } else {
      # Single threshold standard
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
            round(first(df$param_std), 3), " ", first(df$display_unit), "<br>(",
            first(df$std_source), ")';",
            "    legend.appendChild(standardDiv);",
            "  }",
            "}"
          )
        )
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
