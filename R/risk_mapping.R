
###### HOW TO USE: ###########
# Step 1: Ensure you have scored data (tidy format with HQ scores)
# Step 2: Decide which parameters, fraction, date range, and aggregation method(s) you want to use
# Step 3: Use decisions made in step 2 to determine inputs for the create_risk_map function and run it
# 
# 
## Example: 
# risk_map <- create_risk_map(data = all_water_scored,
#                             params = c("metals", "nutrients"),
#                             fraction = "Dissolved",
#                             date = "2023-01-31",
#                             temporal_aggregation = "weighted",
#                             param_aggregation = "mean"
#                             )
# 
# In this example, we are:
# - using water quality data 
# - including only metals and nutrients
# - if a parameter is split into fractions, include only the dissolved fraction (keeping parameters that are not split into fractions)
# - using only data up to Jan 31, 2023 (removing data that is more recent than that)
# - for each station-parameter, aggregate HQ across time using a weighted mean (more recent is weighted higher)
# - for each station, aggregate across parameters by taking the mean HQ
#
# 
# Step 4: use the output of the create_risk_map function as the input to the plot_risk_map function
## Optional: add a title 
# 
# Final output is a leaflet map with stations (points), river network (lines), and interpolated risk (raster).
# Clicking on a station will give a popup window with more information and a histogram of parameter HQs 


# Main function 
# example: create_risk_map(all_media_scored, "all", param_aggregation = "pct95")
create_risk_map <- function(data, 
                            params = "all",
                            fraction = NULL,
                            date = Sys.Date(),
                            temporal_aggregation = "recent",  # how to aggregate across time for each station-parameter combination
                            param_aggregation = "pct95", # how to aggregate across parameters for each station (after temporal aggregation)
                            decay_per_day = NULL,      # for weighted method
                            snap_distance = 1000,
                            max_risk_distance = 5000,
                            resolution = 100,
                            crs = "EPSG:4326",
                            border_sf = NULL,
                            river_network_sf = NULL,
                            nyears = 5) {
  
  interpolation_method <- "distance_weighted"
  
  valid_interpolation_methods <- c("default", "distance_weighted", "max_upstream", "nearest_upstream")
  if (!interpolation_method %in% valid_interpolation_methods) {
    stop(paste("Invalid interpolation_method. Choose from:", paste(valid_interpolation_methods, collapse = ", ")))
  }
  
  valid_param_aggregations <- c("mean", "median", "max", "sum", "pct95", "nemerow")
  if (!param_aggregation %in% valid_param_aggregations) {
    stop(paste("Invalid param_aggregation. Choose from:", paste(valid_param_aggregations, collapse = ", ")))
  }
  
  # if borders are null, load them. Fixes issue if bol_border isn't loaded in environment
  if(exists("bol_border") && is.null(border_sf)) { # load bol_border
    border_sf = bol_border
  } else if(is.null(border_sf)) { # load from geojson file
    bol_border <- st_read("data/geojson/bol_borders.geojson", quiet = TRUE)
    border_sf = bol_border
  } 
  
  if (is.null(border_sf)) { # still doesn't exist
    stop(paste("No Bolivian Border found. Please specify a border"))
  }
  
  # if river_network is null, load them. Fixes issue if river_network isn't loaded in environment
  if(exists("river_network") && is.null(river_network_sf)) { # load river_network
    river_network_sf = river_network
  } else if(is.null(river_network_sf)) { # load from geojson file
    river_network <- st_read("data/shp/River_Network.shp", quiet = TRUE)
    river_network_sf = river_network
  } 
  
  if (is.null(river_network_sf)) { # still doesn't exist
    stop(paste("No river network found. Please specify a network"))
  }
  
  message(paste("Starting risk map creation using interpolation method:", interpolation_method))
  message(paste("Parameter aggregation:", param_aggregation))
  message(paste("Temporal aggregation:", temporal_aggregation))
  
  # Filter and prepare data (now with param_aggregation parameter)
  wq_param <- prepare_water_quality_data(data, params, fraction, date, param_aggregation, temporal_aggregation, decay_per_day, nyears)
  
  # Convert to spatial and snap to river - returns BOTH snapped points AND transformed river network
  snap_result <- snap_points_to_river(wq_param, river_network_sf, border_sf, snap_distance)
  snapped_points <- snap_result$points
  river_network_sf <- snap_result$river_network  # Use the transformed network!
  
  # Build network topology
  network_topology <- build_network_topology(river_network_sf)
  
  # Find adjacent station pairs on the network
  station_pairs <- find_adjacent_stations(snapped_points, river_network_sf, network_topology)
  
    risk_result <- create_gradient_risk_raster(
      river_network_sf, snapped_points, station_pairs, 
      resolution, max_risk_distance
    )
  
  message(paste("Created risk map for", 
                ifelse(length(params) == 1 && tolower(params) == "all", "all parameters", 
                       paste(length(params), "parameter(s)")), 
                "using", interpolation_method, "interpolation method with", param_aggregation, "parameter aggregation."))
  
  return(list(
    risk_raster = risk_result$risk_raster,
    downstream_network = risk_result$river_network,
    snapped_points = snapped_points,
    segment_hq = risk_result$segment_hq,
    station_pairs = if(interpolation_method == "distance_weighted") station_pairs else NULL,
    date_used = attr(snapped_points, "date_used"),
    interpolation_method_used = interpolation_method,
    param_aggregation_used = param_aggregation,
    parameters_used = attr(snapped_points, "parameters_used")
  ))
}

plot_risk_map <- function(risk_map_result, 
                          title = NULL,
                          show_stations = TRUE,
                          show_river = TRUE,
                          opacity = 0.7,
                          high_risk_threshold = 10) {
  
  library(htmltools)
  library(leaflet)
  library(leaflet.extras)
  
  # Extract components
  risk_raster <- risk_map_result$risk_raster
  snapped_points <- risk_map_result$snapped_points
  river_network <- risk_map_result$downstream_network
  interpolation_method_used <- risk_map_result$interpolation_method_used
  date_used <- risk_map_result$date_used
  
  n_params <- length(unique(unlist(snapped_points$parameter_names)))
  
  # Confirm if pilco_line exists or make it
  # if pilco_line is null, load them
  if(!exists("pilco_line")) { # load pilco_line
    pilco_line <- st_read("data/geojson/pilco_line.geojson", quiet = TRUE)
  } 
  
  if (is.null(pilco_line)) {
    stop(paste("No Pilco line found. Please specify a line"))
  } 
  
  # Extract metadata about parameters and param_aggregation and temporal_aggregation
  parameters_used <- attr(snapped_points, "parameters_used")
  param_aggregation_method <- attr(snapped_points, "param_aggregation_method")
  temporal_aggregation_method <- attr(snapped_points, "temporal_aggregation_method")
  
  single_hq <- FALSE
  
  # Determine HQ label based on parameters used
  if (!is.null(parameters_used) && !is.null(param_aggregation_method)) {
    
    if (length(parameters_used) == 1 && temporal_aggregation_method == "recent") {
      single_hq <- TRUE
    }
    
    if (length(parameters_used) == 1) {
      if (temporal_aggregation_method == "recent") {
        hq_label <- paste0(parameters_used, " HQ<br>Using most recent data<br>")
      } else if (temporal_aggregation_method == "mean") {
        hq_label <- paste0(parameters_used, " HQ<br>Averaged over time<br>")
      } else if (temporal_aggregation_method == "weighted") {
        hq_label <- paste0(parameters_used, " HQ<br>Averaged over time<br>(weighted for recency)<br>")
      }
    } else {
      if (param_aggregation_method == "sum") {
        if (temporal_aggregation_method == "recent") {
          hq_label <- paste0("Total HI<br>Sum of HQ across ", n_params, " parameters<br>using most recent data<br>")
        } else if (temporal_aggregation_method == "mean") {
          hq_label <- paste0("Aggregated HI =<br>Sum of HQ across ", n_params, " parameters<br>averaged over time<br>")
        } else if (temporal_aggregation_method == "weighted") {
          hq_label <- paste0("Aggregated HI =<br>Sum of HQ across ", n_params, " parameters<br>averaged over time<br>(weighted for recency)<br>")
        }
      } else {
        if (temporal_aggregation_method == "recent") {
          hq_label <- paste0("Aggregated HQ =<br>", tools::toTitleCase(param_aggregation_method), " HQ across ", n_params, " parameters<br>using most recent data<br>")
        } else if(temporal_aggregation_method == "mean") {
          hq_label <- paste0("Aggregated HQ =<br>", tools::toTitleCase(param_aggregation_method), " HQ across ", n_params, " parameters<br>averaged over time<br>")
        } else if (temporal_aggregation_method == "weighted") {
          hq_label <- paste0("Aggregated HQ =<br>", tools::toTitleCase(param_aggregation_method), " HQ across ", n_params, " parameters<br>averaged over time<br>(weighted for recency)<br>")
        }
      }
    }
    
    # if (length(parameters_used) == 1 && temporal_aggregation_method == "recent") {
    #   single_hq <- TRUE
    #   hq_label <- paste0(params, " HQ<br>Using most recent data<br>")
    # } else if (temporal_aggregation_method == "recent") {
    #   hq_label <- paste0("Aggregated HQ<br>", tools::toTitleCase(param_aggregation_method), " across ", n_params, " parameters<br>",
    #                      "Using most recent data<br>")
    # } else if (temporal_aggregation_method == "weighted") {
    #   hq_label <- paste0("Aggregated HQ<br>", tools::toTitleCase(param_aggregation_method), " across ", n_params, " parameters<br>",
    #                      "Mean across time<br>(Weighted for recency)<br>")
    # } else if (temporal_aggregation_method == "mean") {
    #   hq_label <- paste0("Aggregated HQ<br>", tools::toTitleCase(param_aggregation_method), " across ", n_params, " parameters<br>",
    #                      "Mean across time<br>")
    # }
  } else {
    hq_label <- "HQ"
  }
  
  if (single_hq) {
    if(param_aggregation_method == "sum" && length(parameters_used) > 1) {
      hq_label_short <- paste0(parameters_used, " HI")
    } else {
      hq_label_short <- paste0(parameters_used, " HQ")
    }
  } else {
    if (param_aggregation_method == "sum") {
      if (temporal_aggregation_method == "recent") {
        hq_label_short <- "Total HI"
      } else {
        hq_label_short <- "Aggregated HI"
      }
    } else {
      hq_label_short <- "Aggregated HQ" 
    }
  }

  message(paste("Using HQ label:", hq_label))
  
  # Transform raster to WGS84 for leaflet
  message("Preparing raster for visualization...")
  risk_raster_wgs84 <- project(risk_raster, "EPSG:4326", method = "bilinear")
  
  # Get raster values for diagnostics
  risk_values <- values(risk_raster_wgs84)
  risk_values <- risk_values[!is.na(risk_values) & risk_values > 0]
  
  if (length(risk_values) == 0) {
    stop("No risk values to display")
  }
  
  actual_max <- max(risk_values, na.rm = TRUE)
  message(sprintf("Raster value range: %.3f to %.3f", min(risk_values), actual_max))
  
  # Clamp raster values to 50 (everything above 50 is black)
  risk_raster_wgs84_clamped <- risk_raster_wgs84
  raster_vals <- values(risk_raster_wgs84_clamped)
  raster_vals[!is.na(raster_vals) & raster_vals > 50] <- 50
  values(risk_raster_wgs84_clamped) <- raster_vals
  
  # FIXED COLOR SCALE - hardcoded breaks at 1 and 10
  # Create 300 total colors distributed across the three zones
  zone1_colors <- colorRampPalette(c("#1a9850", "#91cf60", "#d9ef8b", "#ffffbf"))(100)
  zone2_colors <- colorRampPalette(c("#ffffbf", "#fee08b", "#fc8d59", "#e34a33", "#d73027"))(100)
  zone3_colors <- colorRampPalette(c("#d73027", "#a50026", "#67001f", "#000000"))(100)
  
  all_colors <- c(zone1_colors, zone2_colors, zone3_colors)
  
  # Create explicit bins with breaks at 0, 1, 10, 50
  # Generate fine-grained breaks within each zone for smooth gradients
  bins <- c(
    seq(0, 1, length.out = 101),      # 100 bins in zone 1
    seq(1.01, 10, length.out = 100),  # 99 bins in zone 2 (avoid duplicate at 1)
    seq(10.01, 50, length.out = 100)  # 99 bins in zone 3 (avoid duplicate at 10)
  )
  
  # Use colorBin with explicit bins
  pal <- colorBin(
    palette = all_colors,
    domain = c(0, 50),
    bins = bins,
    na.color = "transparent"
  )
  
  # Function to get color for any HQ value (used for station points)
  get_color_for_hq <- function(hq) {
    hq_clamped <- pmin(hq, 50)
    
    sapply(hq_clamped, function(val) {
      if (val <= 1) {
        # 0-1: green to yellow
        idx <- round((val / 1) * 99) + 1
        idx <- max(1, min(100, idx))
        zone1_colors[idx]
      } else if (val <= 10) {
        # 1-10: yellow to red
        idx <- round(((val - 1) / 9) * 99) + 1
        idx <- max(1, min(100, idx))
        zone2_colors[idx]
      } else {
        # 10-50: red to black
        idx <- round(((val - 10) / 40) * 99) + 1
        idx <- max(1, min(100, idx))
        zone3_colors[idx]
      }
    })
  }
  
  # Create base map
  message("Building map...")
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Light") %>%
    addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite")
  
  # Add risk raster
  map <- map %>%
    addRasterImage(risk_raster_wgs84_clamped, 
                   colors = pal, 
                   opacity = opacity,
                   group = "Risk Map")
  
  # Add river network if requested
  if (show_river && !is.null(river_network)) {
    rivers_wgs84 <- st_transform(river_network, 4326)
    map <- map %>%
      addPolylines(data = rivers_wgs84,
                   color = "steelblue",
                   weight = 1,
                   opacity = 0.5,
                   group = "River Network") %>%
      addPolylines(data = pilco_line,
                   color = "darkblue",
                   weight = 2,
                   opacity = 0.8,
                   group = "River Network")
  }
  
  # Add monitoring stations if requested
  if (show_stations && !is.null(snapped_points)) {
    stations_wgs84 <- st_transform(snapped_points, 4326)
    
    max_station_hq <- max(stations_wgs84$HQ, na.rm = TRUE)
    message(sprintf("Station HQ range: %.3f to %.3f", 
                    min(stations_wgs84$HQ), max_station_hq))
    
    # Apply the SAME color function to stations
    stations_wgs84$station_color <- get_color_for_hq(stations_wgs84$HQ)
    
    # Create popup text
    stations_wgs84$popup_text <- sapply(1:nrow(stations_wgs84), function(i) {
      hq <- stations_wgs84$HQ[i]
      station <- stations_wgs84$station[i]
      date <- stations_wgs84$date[i]
      min_date <- if("min_date" %in% names(stations_wgs84)) stations_wgs84$min_date[i] else date
      
      # Bins set up on 1/19/26 in meeting
      hq_lims = c("Below Regulatory Limits" = 1, 
                  "Low Risk" = 1.5,
                  "Moderate Risk" = 2.5, 
                  "High Risk" = 5,
                  "Very High Risk" = 12.5, 
                  "Extreme Risk" = 35000, 
                  "Higher than Limits Support" = 1e10)
      lim_values = as.numeric(hq_lims)
      names_vec = names(hq_lims)
      
      warning_text = names_vec[findInterval(hq, lim_values, rightmost.closed=TRUE)+1]
      
      # 
      # if (hq > high_risk_threshold) {
      #   warning_text <- paste0(" <span style='color:darkred; font-weight:bold;'>(EXTREME RISK)</span>")
      # } else if (hq > 1) {
      #   warning_text <- paste0(" <span style='color:red;'>(High Risk)</span>")
      # } else {
      #   warning_text <- " <span style='color:green;'>(Safe)</span>"
      # }
      
      # Format date range
      if (temporal_aggregation_method == "recent") {
        date_display <- paste0("<b>Date:</b> ", date)
      } else {
        date_display <- paste0("<b>Date Range:</b> ", min_date, " to ", date)
      }
      
      popup <- paste0(
        "<b>Station:</b> ", station, "<br>",
        "<b>", hq_label_short, ":</b> ", round(hq, 3), warning_text, "<br>",
        date_display
      )
      
      # Add histogram if we have parameter-level HQ data
      if ("parameter_hqs" %in% names(stations_wgs84)) {
        param_hqs <- stations_wgs84$parameter_hqs[[i]]
        param_names <- stations_wgs84$parameter_names[[i]]
        
        if (!is.null(param_hqs) && length(param_hqs) > 1) {
          
          # Aggregate to one value per unique parameter
          # Create a dataframe to work with
          param_df <- data.frame(
            parameter = param_names,
            HQ = param_hqs,
            stringsAsFactors = FALSE
          )
          
          # Aggregate by parameter using the same method as param_aggregation
          # You'll need to pass param_aggregation through to this point, or use max as default
          param_aggregated <- param_df %>%
            group_by(parameter) %>%
            summarise(
              HQ = max(HQ, na.rm = TRUE),  # Or use the actual param_aggregation method
              .groups = "drop"
            )
          
          # Use aggregated values for histogram
          param_hqs_unique <- param_aggregated$HQ
          param_names_unique <- param_aggregated$parameter
          
          breaks <- c(0, 0.5, 1, 2, 5, 10, Inf)
          bin_labels <- c("0-0.5", "0.5-1", "1-2", "2-5", "5-10", "10+")
          bin_colors <- c("#1a9850", "#d9ef8b", "#ffffbf", "#fc8d59", "#d73027", "#67001f")
          
          bin_counts <- table(cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE))
          max_count <- max(bin_counts)
          
          # Fixed height calculations
          max_bar_height <- 80
          container_height <- max_bar_height + 30
          pixels_per_count <- max_bar_height / max_count
          
          hist_html <- paste0(
            "<div style='margin-top: 8px; border-top: 1px solid #ccc; padding-top: 8px;'>",
            "<small><b>Parameter Distribution (n=", length(param_hqs_unique), " unique parameters):</b></small><br>",
            "<div style='display: flex; align-items: flex-end; height: ", container_height, "px; margin-top: 4px; gap: 2px; padding-top: 10px; overflow: hidden;'>"
          )
          
          for (j in seq_along(bin_labels)) {
            count <- as.numeric(bin_counts[j])
            bin_mask <- cut(param_hqs_unique, breaks = breaks, labels = bin_labels, include.lowest = TRUE) == bin_labels[j]
            
            if (count > 0) {
              params_in_bin <- param_names_unique[bin_mask]
              hqs_in_bin <- param_hqs_unique[bin_mask]
              param_details <- paste0(params_in_bin, " (", round(hqs_in_bin, 2), ")", collapse = "&#10;")
              tooltip_text <- paste0(hq_label_short, " by parameter (", bin_labels[j], " HQ):&#10;", param_details)
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
                              "<div style='display: flex; justify-content: space-between; margin-top: 2px;'>",
                              "<small style='font-size: 9px; color: #666;'>HQ Range</small>",
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
    
    unsnapped_points <- all_water_data |>
      filter(!is.na(`Decimal latitude`) & !is.na(`Decimal longitude`)) |>
      st_as_sf(coords = c("Decimal longitude", "Decimal latitude"),
               crs = 4326)
    
    # Create popup text for unsnapped points
    unsnapped_points$popup_text <- paste0(
      "<b>Station:</b> ", unsnapped_points$Station, "<br>",
      "<b>Date:</b> ", unsnapped_points$Date, "<br>",
      "<small style='color: purple;'>(Original location - unsnapped)</small>"
    )
    
    map <- map |>
      addCircleMarkers(
        data = unsnapped_points,
        radius = 4,
        color = "purple",
        weight = 1,
        fillColor = "white",
        fillOpacity = 0.3,
        popup = ~popup_text,
        group = "Original Locations"
      )
    
    map <- map %>%
      addCircleMarkers(
        data = stations_wgs84,
        radius = 8,
        color = "black",
        weight = 1.5,
        fillColor = ~station_color,
        fillOpacity = 1,
        popup = ~popup_text,
        group = "Monitoring Stations<br>(snapped to river network)"
      )
  }
  
  # Add legend
  # Add legend with clean gradient display
  map <- map %>%
    addLegend(
      position = "bottomright",
      colors = c(
        zone1_colors[1],    # Dark green at 0
        zone1_colors[50],   # Mid green at 0.5
        zone1_colors[100],  # Yellow at 1
        zone2_colors[20],   # Light orange at ~2-3
        zone2_colors[50],   # Orange at ~5
        zone2_colors[100],  # Red at 10
        zone3_colors[25],   # Dark red at ~20
        zone3_colors[50],   # Very dark red at ~30
        zone3_colors[75],   # Almost black at ~40
        zone3_colors[100]   # Black at 50
      ),
      labels = c("0", "0.5", "1", "3", "5", "10", "20", "30", "40", "50+"),
      title = HTML(paste0("Risk Score:<br><small style='color: #888;'>", hq_label, "</small><br>",
                          "<small style='color: #888;'>0-1: Safe</small><br>",
                          "<small style='color: #888;'>1-10: Risk</small><br>",
                          "<small style='color: #888;'>10-50+: Extreme</small><br>")),
      opacity = 1
    )
  
  # Add layer controls
  map <- map %>%
    addLayersControl(
      baseGroups = c("Light", "Dark", "Satellite"),
      overlayGroups = c("Risk Map", "River Network", "Monitoring Stations<br>(snapped to river network)", "Original Locations"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  # Add title if provided
  if (!is.null(title)) {
    map <- map %>%
      addControl(
        html = paste0("<div style='background: white; padding: 10px; border-radius: 5px; border-left: 4px solid #d73027;'>",
                      "<h4 style='margin: 0;'>", title, "</h4>",
                      "<p style='margin: 5px 0 0 0; font-size: 12px;'>Method: ", interpolation_method_used, 
                      " | Date: ", date_used, "</p>",
                      "<p style='margin: 5px 0 0 0; font-size: 11px; color: #666;'>",
                      "Fixed scale: 0-1 safe, 1-10 risk, 10-50 extreme, 50+ black</p>",
                      "</div>"),
        position = "topright"
      )
  }
  
  message("Map complete!")
  return(map)
}
