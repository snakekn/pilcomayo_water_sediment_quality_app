#### Risk Mapping Helper Functions

# Helper: Prepare water quality data - WITH TEMPORAL AGGREGATION
# example: prepare_water_quality_data(all_media_scored, unique(all_media_scored$parameter), fraction = NULL, date = 2024)
prepare_water_quality_data <- function(data, params, fraction, date, 
                                       param_aggregation = "pct95",
                                       temporal_aggregation = "recent",  # "recent", "mean", or "weighted"
                                       decay_per_day = NULL,
                                       nyears = 5) {
  message("Not replacing HQ < 1 with HQ = 0 since we're taking 95th percentile data, typically. Others can deal with it.")
  
  ### Not important to do :) using pct95
  # data <- data |>
  #   mutate(HQ = ifelse(HQ < 1, 0 , HQ))
  
  message("Filtering data for parameter(s)...")
  
  # Validate param_aggregation method
  valid_param_aggregations <- c("mean", "median", "max", "sum", "pct95", "nemerow")
  if (!param_aggregation %in% valid_param_aggregations) {
    stop(paste("Invalid param_aggregation. Choose from:", paste(valid_param_aggregations, collapse = ", ")))
  }
  
  # Validate temporal method
  valid_temporal_aggregations <- c("recent", "mean", "weighted")
  if (!temporal_aggregation %in% valid_temporal_aggregations) {
    stop(paste("Invalid temporal_aggregation. Choose from:", paste(valid_temporal_aggregations, collapse = ", ")))
  }
  
  # Handle "all" parameter - use all parameters in dataset
  if (length(params) == 1 && tolower(params) == "all") {
    message(paste("Using ALL parameters in dataset with", param_aggregation, "parameter aggregation..."))
    
    # Filter base data
    wq_param <- data %>%
      filter(!is.na(HQ) & !is.na(longitude_decimal) & !is.na(latitude_decimal))
    
    # Get list of unique parameters for reporting
    all_parameters <- unique(wq_param$parameter)
    message(paste("Found", length(all_parameters), "unique parameters in dataset"))
    
  } else {
    # Handle specific parameter(s) - can be single or multiple
    message(paste("Using", length(params), "specified parameter(s) with", param_aggregation, "parameter aggregation..."))
    message(paste("Parameters:", paste(params, collapse = ", ")))
    
    # Define your parameter groups EXPAND THIS!!!!!!!!!!!!!
    param_groups <- list(
      # Heavy metals and metalloids
      metals = c("Arsenic", "Cadmium", "Lead", "Mercury", "Chromium", 
                 "Copper", "Zinc", "Nickel", "Silver", "Iron", "Manganese",
                 "Bismuth", "Thallium", "Boron", "Selenium"),
      
      # Nutrients
      nutrients = c("Nitrogen", "Nitrate", "Nitrite", "Phosphates", 
                    "Ammonia"),
      
      # Major ions/minerals
      ions = c("Calcium", "Magnesium", "Sodium", "Potassium", 
               "Chlorides", "Sulfate", "Fluoruros"),
      
      # Oxygen-related
      oxygen = c("Oxygen", "Oxygen Saturation", "BOD", "COD"),
      
      # Physical parameters
      physical = c("Temperature", "pH", "Turbidity", "Color", 
                   "Specific conductivity", "Resistivity", "Salinity"),
      
      # Solids
      solids = c("Solids", "TDS", "TSS", "Settlable solids"),
      
      # Alkalinity/hardness
      alkalinity_hardness = c("alkalinity", "Phenolphthalein alkalinity", 
                              "hardness"),
      
      # Organic/chemical contaminants
      organics = c("Phenols", "Hydrocarbons", "Organic carbon", 
                   "Cyanides", "Free cyanides", "Sulphides"),
      
      # Biological
      biological = c("coliforms", "Fecal coliforms", "Mesophilic aerobes"),
      
      # Flow/location
      hydrology = c("Flow", "Average Velocity", "Partial Pressure"),
      
      # Coordinates
      coordinates = c("Decimal latitude", "Decimal longitude")
    )
    
    # Check which params are group names vs actual parameter names
    actual_params <- unlist(lapply(params, function(p) {
      if (p %in% names(param_groups)) {
        param_groups[[p]]  # Return the group's values
      } else {
        p  # Return the parameter as-is
      }
    }))
    
    wq_param <- data |> filter(parameter %in% actual_params)
    
    if (nrow(wq_param) == 0) {
      stop(paste("No data found for parameter(s):", paste(params, collapse = ", ")))
    }
    
    all_parameters <- actual_params
  }
  
  # Filter by fraction if specified
  if (!is.null(fraction) && "fraction" %in% names(wq_param)) {
    # Only remove rows where fraction is NOT NA AND fraction doesn't match the specified fraction
    # This keeps: (1) rows where fraction matches, AND (2) rows where fraction is NA
    wq_param <- wq_param %>% 
      filter(is.na(fraction) | fraction == !!fraction)
    
    message(paste("Filtered to fraction:", fraction, "(keeping NA fractions)"))
  }
  
  # Ensure date column exists and is in proper format
  if (!"date" %in% names(wq_param)) {
    stop("Data must contain a 'date' column")
  }
  
  if (!inherits(wq_param$date, "Date")) {
    wq_param$date <- as.Date(wq_param$date)
  }
  
  # Filter by date
  if (!is.null(date)) {
    target_date <- as.Date(date)
    wq_param <- wq_param %>% filter(date <= target_date)
  } else {
    target_date <- max(wq_param$date, na.rm = TRUE)
  }
  
  if (nrow(wq_param) == 0) {
    stop("No data found after date filtering")
  }
  
  # TEMPORAL AGGREGATION STEP
  message(paste("Temporal aggregation method:", temporal_aggregation))
  
  if (temporal_aggregation == "recent") {
    
    wq_temporal = temporal_filtering(wq_param, nyears=nyears) # simple filtering out
    # 
    # if (is.null(nyears)) {
    #   # ORIGINAL BEHAVIOR: Most recent single measurement
    #   message("Getting most recent measurement for each station-parameter combination...")
    #   
    #   most_recent_dates <- wq_param %>%
    #     group_by(station, parameter) %>%
    #     summarise(max_date = max(date, na.rm = TRUE), .groups = "drop")
    #   
    #   group_vars <- c("station", "parameter")
    #   if ("fraction" %in% names(wq_param)) {
    #     group_vars <- c(group_vars, "fraction")
    #     message("Note: Grouping by fraction")
    #   }
    #   if ("sieve_size" %in% names(wq_param)) {
    #     group_vars <- c(group_vars, "sieve_size")
    #     message("Note: Multiple sieve sizes detected")
    #   }
    #   
    #   wq_temporal <- wq_param %>%
    #     left_join(most_recent_dates, by = c("station", "parameter")) %>%
    #     filter(date == max_date) %>%
    #     group_by(across(all_of(group_vars))) %>%
    #     slice(1) %>%
    #     ungroup() %>%
    #     select(-max_date)
    #   
    #   message(paste("Date range:", min(wq_temporal$date), "-", max(wq_temporal$date)))
    #   
    # 
    #   } else {
    #   # NEW BEHAVIOR: All data from last n years
    #   message(paste("Getting all data from last", nyears, "years for each station-parameter combination..."))
    #   
    #   # Find the most recent date in the filtered dataset
    #   max_date_in_data <- max(wq_param$date, na.rm = TRUE)
    #   
    #   # Calculate cutoff date (nyears before the most recent date)
    #   cutoff_date <- max_date_in_data - (nyears * 365.25)  # Account for leap years
    #   
    #   message(paste("Date range for analysis:", cutoff_date, "to", max_date_in_data))
    #   
    #   # Filter to only include data from last nyears
    #   wq_temporal <- wq_param %>%
    #     filter(date >= cutoff_date)
    #   
    #   if (nrow(wq_temporal) == 0) {
    #     stop(paste("No data found in the last", nyears, "years"))
    #   }
    #   
    #   message(paste("Found", nrow(wq_temporal), "measurements in the last", nyears, "years"))
    # }
    
  } else if (temporal_aggregation == "mean") {
    message("Averaging all measurements across time for each station-parameter combination...")
    
    # Build grouping variables conditionally
    group_vars <- c("station", "parameter")
    if ("fraction" %in% names(wq_param)) {
      group_vars <- c(group_vars, "fraction")
    }
    if ("sieve_size" %in% names(wq_param)) {
      group_vars <- c(group_vars, "sieve_size")
    }
    
    wq_temporal <- wq_param %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(
        HQ = mean(HQ, na.rm = TRUE),
        date = max(date),  # Most recent date
        min_date = min(date),
        longitude_decimal = first(longitude_decimal),
        latitude_decimal = first(latitude_decimal),
        n_observations = n(),
        .groups = "drop"
      )
    
    message(paste("Averaged", sum(wq_temporal$n_observations), "total observations"))
    
  
    } else if (temporal_aggregation == "weighted") {
    # Weighted average with more recent observations weighted higher
    message("Calculating weighted average with recency weighting...")
    
    # Set default decay if not provided
    if (is.null(decay_per_day)) {
      decay_per_day <- 0.001  # gentle decay
      message("Using default decay: 0.001 per day")
    }
    
    # Build grouping variables conditionally
    group_vars <- c("station", "parameter")
    if ("fraction" %in% names(wq_param)) {
      group_vars <- c(group_vars, "fraction")
    }
    if ("sieve_size" %in% names(wq_param)) {
      group_vars <- c(group_vars, "sieve_size")
    }
    
    wq_temporal <- wq_param %>%
      group_by(across(all_of(group_vars))) %>%
      mutate(
        days_ago = as.numeric(target_date - date),
        # Exponential decay: weight = exp(-decay * days_ago)
        weight = exp(-decay_per_day * days_ago)
      ) %>%
      summarise(
        HQ = weighted.mean(HQ, w = weight, na.rm = TRUE),
        date = max(date),  # Use most recent date for reference
        longitude_decimal = first(longitude_decimal),
        latitude_decimal = first(latitude_decimal),
        n_observations = n(),
        effective_n = sum(weight),  # Effective sample size
        .groups = "drop"
      )
    
    message(paste("Weighted average across", sum(wq_temporal$n_observations), 
                  "observations with decay =", decay_per_day))
  }
  
  message(paste("Found", nrow(wq_temporal), "station-parameter combinations"))
  
  # AGGREGATE ACROSS SIEVE SIZES (if present)
  if ("sieve_size" %in% names(wq_temporal)) {
    message("Aggregating across sieve sizes using max HQ...")
    
    # Preserve fraction in grouping if it exists
    sieve_group_vars <- c("station", "parameter")
    if ("fraction" %in% names(wq_temporal)) {
      sieve_group_vars <- c(sieve_group_vars, "fraction")
    }
    
    wq_temporal <- wq_temporal %>%
      group_by(across(all_of(sieve_group_vars))) %>%
      summarise(
        HQ = max(HQ, na.rm = TRUE), # note: using max for sieve data, rather than 95th percentile?
        date = max(date),
        longitude_decimal = first(longitude_decimal),
        latitude_decimal = first(latitude_decimal),
        .groups = "drop"
      )
    
    message(paste("After sieve_size aggregation:", nrow(wq_temporal), "station-parameter combinations"))
  }
  
  
  # Now aggregate across parameters for each station (PARAMETER AGGREGATION)
  message("Aggregating across parameters for each station...")
  
  # Calculate mean coordinates for each station across ALL measurements
  station_coords <- wq_param %>%
    group_by(station) %>%
    summarise(
      mean_longitude = mean(longitude_decimal, na.rm = TRUE),
      mean_latitude = mean(latitude_decimal, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate min_date from original wq_param (which has all the date info)
  station_date_ranges <- wq_param %>%
    group_by(station) %>%
    summarise(
      min_date = min(date, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Then use these mean coordinates in the final aggregation
  wq_final <- wq_temporal %>%
    group_by(station) %>%
    summarise(
      parameter_hqs = list(HQ),
      parameter_names = list(parameter),
      HQ = switch(param_aggregation,
                  "mean" = mean(HQ, na.rm = TRUE),
                  "median" = median(HQ, na.rm = TRUE),
                  "max" = max(HQ, na.rm = TRUE),
                  "sum" = sum(HQ, na.rm = TRUE),
                  "pct95" = quantile(HQ, probs = 0.95, na.rm=TRUE),
                  "nemerow" = sqrt((max(HQ, na.rm = TRUE)^2+mean(HQ, na.rm = TRUE)^2)/2)),
      date = max(date),
      n_parameters = n(),
      .groups = "drop"
    ) %>%
    left_join(station_coords, by = "station") %>%
    left_join(station_date_ranges, by = "station") %>%
    rename(longitude_decimal = mean_longitude,
           latitude_decimal = mean_latitude)
  
  message(paste("Calculated", param_aggregation, "HQ across", 
                ifelse(length(all_parameters) == 1, 
                       paste("parameter:", all_parameters), 
                       paste(length(all_parameters), "parameters")),
                "for", nrow(wq_final), "stations"))
  
  # Store metadata as attributes
  attr(wq_final, "date_used") <- target_date
  attr(wq_final, "parameters_used") <- all_parameters
  attr(wq_final, "param_aggregation_method") <- param_aggregation
  attr(wq_final, "temporal_aggregation_method") <- temporal_aggregation
  attr(wq_final, "decay_per_day") <- decay_per_day
  
  message(paste("Using", nrow(wq_final), "stations with measurements up to", target_date))
  
  return(wq_final)
}




# Helper: Snap points to river network - MODIFIED to preserve all metadata
snap_points_to_river <- function(wq_param, river_network_sf, border_sf, snap_distance) {
  library(sf)
  
  message("Converting to spatial points and snapping to river...")
  
  wq_points <- sf::st_as_sf(wq_param, 
                        coords = c("longitude_decimal", "latitude_decimal"),
                        crs = 4326)
  
  if (!is.null(border_sf)) {
    border_sf <- st_transform(border_sf, st_crs(wq_points))
    wq_points <- st_filter(wq_points, border_sf, .predicate = st_within)
  }
  
  # Transform to projected CRS
  if (st_is_longlat(river_network_sf)) {
    bbox <- st_bbox(river_network_sf)
    lon_center <- mean(c(bbox["xmin"], bbox["xmax"]))
    utm_zone <- floor((lon_center + 180) / 6) + 1
    hemisphere <- ifelse(mean(c(bbox["ymin"], bbox["ymax"])) >= 0, "north", "south")
    utm_crs <- ifelse(hemisphere == "north",
                      paste0("EPSG:326", sprintf("%02d", utm_zone)),
                      paste0("EPSG:327", sprintf("%02d", utm_zone)))
    river_network_sf <- st_transform(river_network_sf, utm_crs)
  }
  
  wq_points <- st_transform(wq_points, st_crs(river_network_sf))
  
  if (!"line_id" %in% names(river_network_sf)) {
    river_network_sf$line_id <- 1:nrow(river_network_sf)
  }
  
  # Snap to river
  nearest_segments <- st_nearest_feature(wq_points, river_network_sf)
  distances_to_river <- sapply(1:nrow(wq_points), function(i) {
    st_distance(wq_points[i, ], river_network_sf[nearest_segments[i], ])[1,1]
  })
  
  keep <- distances_to_river <= snap_distance
  wq_points <- wq_points[keep, ]
  nearest_segments <- nearest_segments[keep]
  
  if (nrow(wq_points) == 0) {
    stop("No points within snap distance of river network")
  }
  
  # Snap geometries
  snapped_geoms <- vector("list", nrow(wq_points))
  for (i in 1:nrow(wq_points)) {
    line <- river_network_sf[nearest_segments[i], ]
    pt <- wq_points[i, ]
    snapped_geoms[[i]] <- st_nearest_points(pt, line) %>% 
      st_cast("POINT") %>% 
      st_geometry() %>% 
      .[[2]]
  }
  
  st_geometry(wq_points) <- st_sfc(snapped_geoms, crs = st_crs(wq_points))
  wq_points$nearest_segment <- nearest_segments
  wq_points$line_id <- river_network_sf$line_id[nearest_segments]
  
  # Preserve all metadata from wq_param
  attr(wq_points, "date_used") <- attr(wq_param, "date_used")
  attr(wq_points, "parameters_used") <- attr(wq_param, "parameters_used")
  attr(wq_points, "param_aggregation_method") <- attr(wq_param, "param_aggregation_method")
  attr(wq_points, "temporal_aggregation_method") <- attr(wq_param, "temporal_aggregation_method")
  attr(wq_points, "decay_per_day") <- attr(wq_param, "decay_per_day")
  
  message(paste("Snapped", nrow(wq_points), "points to river network"))
  
  # Return both points and the transformed river network
  return(list(
    points = wq_points,
    river_network = river_network_sf
  ))
}




# HELPER: Build network topology 
build_network_topology <- function(river_network_sf, connection_tolerance = 50) {
  message("Building network topology...")
  
  # CRITICAL: Ensure river network is in projected CRS
  if (st_is_longlat(river_network_sf)) {
    stop("River network must be in projected CRS (meters) before building topology. This should have been done in snap_points_to_river().")
  }
  
  # Ensure line_id exists and is unique
  if (!"line_id" %in% names(river_network_sf)) {
    river_network_sf$line_id <- 1:nrow(river_network_sf)
  }
  
  n_segs <- nrow(river_network_sf)
  message(paste("Processing", n_segs, "river segments..."))
  
  # Use spatial binning to reduce search space
  bin_size <- connection_tolerance * 2
  message(sprintf("Using bin size: %.1f meters", bin_size))
  
  # Extract endpoints efficiently
  start_coords <- matrix(NA, nrow = n_segs, ncol = 2)
  end_coords <- matrix(NA, nrow = n_segs, ncol = 2)
  
  for (i in 1:n_segs) {
    coords <- st_coordinates(river_network_sf[i, ])[, 1:2]
    start_coords[i, ] <- coords[1, ]
    end_coords[i, ] <- coords[nrow(coords), ]
  }
  
  # Diagnostic: check coordinate ranges
  message(sprintf("Start coords: X range [%.1f, %.1f], Y range [%.1f, %.1f]",
                  min(start_coords[,1]), max(start_coords[,1]),
                  min(start_coords[,2]), max(start_coords[,2])))
  message(sprintf("End coords: X range [%.1f, %.1f], Y range [%.1f, %.1f]",
                  min(end_coords[,1]), max(end_coords[,1]),
                  min(end_coords[,2]), max(end_coords[,2])))
  
  # Assign start points to bins
  start_bins_x <- floor(start_coords[, 1] / bin_size)
  start_bins_y <- floor(start_coords[, 2] / bin_size)
  start_bin_key <- paste(start_bins_x, start_bins_y, sep = "_")
  
  # Create lookup: bin -> list of segment indices with start points in that bin
  bin_lookup <- split(1:n_segs, start_bin_key)
  
  # Diagnostic: check for duplicate endpoints
  n_unique_starts <- length(unique(start_bin_key))
  n_unique_ends <- length(unique(paste(floor(end_coords[,1]/bin_size), floor(end_coords[,2]/bin_size), sep="_")))
  message(sprintf("Network has %d segments with %d unique start bins and %d unique end bins", 
                  n_segs, n_unique_starts, n_unique_ends))
  
  # Check average bin occupancy
  avg_per_bin <- mean(sapply(bin_lookup, length))
  max_per_bin <- max(sapply(bin_lookup, length))
  message(sprintf("Average segments per bin: %.1f, max: %d", avg_per_bin, max_per_bin))
  
  if (max_per_bin > 100) {
    warning(paste("Some bins have", max_per_bin, "segments! This suggests overlapping/duplicate geometry.",
                  "Consider cleaning the river network or reducing connection_tolerance."))
  }
  
  message("Finding connections...")
  
  # Store connections as lists of pairs, then convert to adjacency lists
  downstream_pairs <- list()
  n_connections <- 0
  start_time <- Sys.time()
  
  # For each end point, only check start points in nearby bins
  for (i in 1:n_segs) {
    end_x <- end_coords[i, 1]
    end_y <- end_coords[i, 2]
    
    # Determine which bins to check (this end point + 8 surrounding bins)
    end_bin_x <- floor(end_x / bin_size)
    end_bin_y <- floor(end_y / bin_size)
    
    # Check 3x3 grid of bins
    candidates <- c()
    for (dx in -1:1) {
      for (dy in -1:1) {
        bin_key <- paste(end_bin_x + dx, end_bin_y + dy, sep = "_")
        if (bin_key %in% names(bin_lookup)) {
          candidates <- c(candidates, bin_lookup[[bin_key]])
        }
      }
    }
    
    if (length(candidates) > 0) {
      # Calculate distances only to candidate start points
      dx <- start_coords[candidates, 1] - end_x
      dy <- start_coords[candidates, 2] - end_y
      dists <- sqrt(dx^2 + dy^2)
      
      # Find connections (exclude self)
      connected_idx <- which(dists < connection_tolerance & candidates != i)
      
      if (length(connected_idx) > 0) {
        connected <- candidates[connected_idx]
        # Store pairs for later processing
        for (j in connected) {
          n_connections <- n_connections + 1
          downstream_pairs[[n_connections]] <- c(i, j)  # i connects to j
        }
      }
    }
    
    if (i %% 500 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      rate <- i / elapsed
      remaining <- (n_segs - i) / rate
      message(sprintf("Processed %d/%d segments (%.1f%%) | %d connections | %.1f seg/sec | ~%.0f sec remaining", 
                      i, n_segs, 100*i/n_segs, n_connections, rate, remaining))
    }
  }
  
  elapsed_total <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  message(sprintf("Found %d connections in %.1f seconds", n_connections, elapsed_total))
  
  # Now build adjacency lists efficiently
  message("Building adjacency lists...")
  downstream_adj <- vector("list", n_segs)
  names(downstream_adj) <- as.character(river_network_sf$line_id)
  upstream_adj <- vector("list", n_segs)
  names(upstream_adj) <- as.character(river_network_sf$line_id)
  
  if (length(downstream_pairs) > 0) {
    message(paste("Processing", length(downstream_pairs), "connection pairs..."))
    
    # Convert pairs to matrix for efficient processing
    pairs_matrix <- matrix(unlist(downstream_pairs), ncol = 2, byrow = TRUE)
    
    # Build downstream adjacency using tapply
    message("Building downstream adjacency...")
    downstream_list <- tapply(pairs_matrix[,2], pairs_matrix[,1], function(x) {
      river_network_sf$line_id[x]
    }, simplify = FALSE)
    
    for (i in names(downstream_list)) {
      idx <- as.numeric(i)
      line_id <- as.character(river_network_sf$line_id[idx])
      downstream_adj[[line_id]] <- downstream_list[[i]]
    }
    
    # Build upstream adjacency
    message("Building upstream adjacency...")
    upstream_list <- tapply(pairs_matrix[,1], pairs_matrix[,2], function(x) {
      river_network_sf$line_id[x]
    }, simplify = FALSE)
    
    for (i in names(upstream_list)) {
      idx <- as.numeric(i)
      line_id <- as.character(river_network_sf$line_id[idx])
      upstream_adj[[line_id]] <- upstream_list[[i]]
    }
  }
  
  # Diagnostic: show connection distribution
  n_downstream <- sapply(downstream_adj, length)
  message(sprintf("Connections per segment: mean=%.1f, median=%.0f, max=%d", 
                  mean(n_downstream), median(n_downstream), max(n_downstream)))
  
  message("Topology complete!")
  
  return(list(
    downstream = downstream_adj,
    upstream = upstream_adj
  ))
}




# HELPER: Find adjacent station pairs
find_adjacent_stations <- function(snapped_points, river_network_sf, network_topology) {
  message("Finding adjacent station pairs...")
  
  pairs <- list()
  
  for (i in 1:nrow(snapped_points)) {
    station_i <- snapped_points[i, ]
    seg_i <- station_i$line_id
    
    # Find immediately downstream stations
    downstream_stations <- find_next_downstream_station(
      station_i, seg_i, snapped_points, river_network_sf, network_topology
    )
    
    # Create pairs with each downstream neighbor
    for (ds in downstream_stations) {
      pairs[[length(pairs) + 1]] <- list(
        upstream_id = i,
        downstream_id = ds$station_idx,
        upstream_station = station_i$station,
        downstream_station = ds$station,
        upstream_HQ = station_i$HQ,
        downstream_HQ = ds$HQ,
        path_segments = ds$path_segments,
        total_distance = ds$total_distance
      )
    }
  }
  
  message(paste("Found", length(pairs), "adjacent station pairs"))
  return(pairs)
}




# Helper: Find next downstream station(s)
find_next_downstream_station <- function(current_station, current_seg, all_stations, 
                                         river_network_sf, network_topology) {
  visited_segments <- c()
  queue <- list(list(seg = current_seg, dist = 0, path = c(current_seg)))
  results <- list()
  
  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]
    
    if (current$seg %in% visited_segments) next
    visited_segments <- c(visited_segments, current$seg)
    
    # Check if there's a station on this segment (other than current station)
    stations_on_seg <- which(all_stations$line_id == current$seg)
    stations_on_seg <- stations_on_seg[stations_on_seg != which(all_stations$station == current_station$station)]
    
    if (length(stations_on_seg) > 0) {
      # Found adjacent station(s) - don't search further from this branch
      for (idx in stations_on_seg) {
        seg_length <- as.numeric(st_length(river_network_sf[river_network_sf$line_id == current$seg, ]))
        results[[length(results) + 1]] <- list(
          station_idx = idx,
          station = all_stations$station[idx],
          HQ = all_stations$HQ[idx],
          path_segments = current$path,
          total_distance = current$dist + seg_length
        )
      }
    } else {
      # No station on this segment, continue downstream
      next_segs <- network_topology$downstream[[as.character(current$seg)]]
      if (length(next_segs) > 0) {
        for (next_seg in next_segs) {
          seg_length <- as.numeric(st_length(river_network_sf[river_network_sf$line_id == current$seg, ]))
          queue[[length(queue) + 1]] <- list(
            seg = next_seg,
            dist = current$dist + seg_length,
            path = c(current$path, next_seg)
          )
        }
      }
    }
  }
  
  return(results)
}





# NEW: Create gradient-based risk raster
create_gradient_risk_raster <- function(river_network_sf, snapped_points, station_pairs, 
                                        resolution, max_risk_distance) {
  
  library(terra)
  
  message("Creating gradient-based risk raster...")
  
  # Sample points densely along river with interpolated HQ values
  sample_points <- sample_gradient_points(river_network_sf, station_pairs, resolution)
  
  # Create raster extent
  bbox <- st_bbox(river_network_sf)
  bbox[1] <- bbox[1] - max_risk_distance
  bbox[2] <- bbox[2] - max_risk_distance
  bbox[3] <- bbox[3] + max_risk_distance
  bbox[4] <- bbox[4] + max_risk_distance
  
  ext <- terra::ext(bbox[1], bbox[3], bbox[2], bbox[4])
  risk_raster <- terra::rast(ext, resolution = resolution, crs = st_crs(river_network_sf)$wkt)
  
  # Rasterize sample points
  if (nrow(sample_points) > 0) {
    river_raster <- terra::rasterize(vect(sample_points), risk_raster, 
                              field = "HQ", fun = "mean", background = NA)
  } else {
    river_raster <- risk_raster
    values(river_raster) <- NA
  }
  
  # Apply distance decay
  river_binary <- !is.na(river_raster)
  distance_raster <- distance(river_binary)
  decay_factor <- 1 - (distance_raster / max_risk_distance)
  decay_factor <- clamp(decay_factor, lower = 0, upper = 1)
  
  # Interpolate outward from river
  river_hq_extended <- focal(river_raster, w = 3, fun = "mean", na.policy = "only", na.rm = TRUE)
  for (i in 1:10) {
    river_hq_extended <- focal(river_hq_extended, w = 5, fun = "mean", na.policy = "only", na.rm = TRUE)
  }
  
  risk_score <- river_hq_extended * decay_factor
  risk_score[distance_raster > max_risk_distance] <- 0
  
  return(list(
    risk_raster = risk_score,
    river_network = river_network_sf,
    segment_hq = NULL
  ))
}




# NEW: Sample points with gradient interpolation
sample_gradient_points <- function(river_network_sf, station_pairs, resolution) {
  message("Sampling points along river with gradient interpolation...")
  
  all_points <- list()
  
  # For each station pair, sample points along the path
  for (pair in station_pairs) {
    path_segs <- pair$path_segments
    cumulative_dist <- 0
    
    for (seg_id in path_segs) {
      river_seg <- river_network_sf[river_network_sf$line_id == seg_id, ]
      if (nrow(river_seg) == 0) next
      
      seg_length <- as.numeric(st_length(river_seg))
      line_coords <- st_coordinates(river_seg)[, 1:2]
      
      # Sample points along segment
      n_samples <- max(3, ceiling(seg_length / (resolution / 2)))
      
      for (j in 1:n_samples) {
        t <- (j - 1) / (n_samples - 1)  # 0 to 1
        point_coords <- interpolate_along_line(line_coords, t)
        
        # Distance from upstream station to this point
        dist_from_upstream <- cumulative_dist + (t * seg_length)
        
        # Linear interpolation of HQ
        if (pair$total_distance > 0) {
          proportion <- dist_from_upstream / pair$total_distance
          proportion <- max(0, min(1, proportion))
          hq_value <- pair$upstream_HQ + proportion * (pair$downstream_HQ - pair$upstream_HQ)
        } else {
          hq_value <- pair$upstream_HQ
        }
        
        all_points[[length(all_points) + 1]] <- data.frame(
          x = point_coords[1],
          y = point_coords[2],
          HQ = hq_value,
          pair_id = length(station_pairs)
        )
      }
      
      cumulative_dist <- cumulative_dist + seg_length
    }
  }
  
  if (length(all_points) == 0) {
    return(st_sf(geometry = st_sfc(crs = st_crs(river_network_sf))))
  }
  
  points_df <- do.call(rbind, all_points)
  points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = st_crs(river_network_sf))
  
  message(paste("Created", nrow(points_sf), "sample points"))
  return(points_sf)
}

# Helper: Interpolate point along line
interpolate_along_line <- function(line_coords, t) {
  # t is from 0 to 1
  total_length <- 0
  segment_lengths <- numeric(nrow(line_coords) - 1)
  
  for (i in 1:(nrow(line_coords) - 1)) {
    seg_len <- sqrt(sum((line_coords[i + 1, ] - line_coords[i, ])^2))
    segment_lengths[i] <- seg_len
    total_length <- total_length + seg_len
  }
  
  target_dist <- t * total_length
  cumulative <- 0
  
  for (i in 1:(nrow(line_coords) - 1)) {
    if (cumulative + segment_lengths[i] >= target_dist) {
      # Point is on this segment
      remaining <- target_dist - cumulative
      seg_t <- remaining / segment_lengths[i]
      return(line_coords[i, ] + seg_t * (line_coords[i + 1, ] - line_coords[i, ]))
    }
    cumulative <- cumulative + segment_lengths[i]
  }
  
  # Fallback to end point
  return(line_coords[nrow(line_coords), ])
}
