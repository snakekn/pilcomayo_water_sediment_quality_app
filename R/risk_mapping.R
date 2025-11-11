create_risk_map <- function(data, 
                            param,
                            fraction = NULL,
                            date = NULL,
                            method = "default",
                            snap_distance = 1000,
                            max_risk_distance = 5000,
                            resolution = 100,
                            crs = "EPSG:4326",
                            border_sf = bol_border,
                            river_network_sf = river_network) {
  
  # Validate method parameter
  valid_methods <- c("default", "distance_weighted", "max_upstream", "nearest_upstream")
  if (!method %in% valid_methods) {
    stop(paste("Invalid method. Choose from:", paste(valid_methods, collapse = ", ")))
  }
  
  message(paste("Starting risk map creation using method:", method))
  
  # Filter data for the specified parameter
  message("Filtering data for parameter...")
  wq_param <- data %>%
    filter(parameter == param) %>%
    filter(!is.na(HQ) & !is.na(longitude_decimal) & !is.na(latitude_decimal))
  
  # Filter by fraction if specified
  if (!is.null(fraction) && "fraction" %in% names(wq_param)) {
    wq_param <- wq_param %>%
      filter(fraction == !!fraction)
    message(paste("Filtered to fraction:", fraction))
  }
  
  # Ensure date column exists and is in proper format
  if (!"date" %in% names(wq_param)) {
    stop("Data must contain a 'date' column")
  }
  
  # Convert date column to Date type if not already
  if (!inherits(wq_param$date, "Date")) {
    wq_param$date <- as.Date(wq_param$date)
  }
  
  # Filter up to specified date
  if (!is.null(date)) {
    target_date <- as.Date(date)
    wq_param <- wq_param %>%
      filter(date <= target_date)
    message(paste("Filtered to measurements up to:", target_date))
  } else {
    target_date <- max(wq_param$date, na.rm = TRUE)
    message(paste("Using all measurements up to:", target_date))
  }
  
  if (nrow(wq_param) == 0) {
    stop(paste("No data found for parameter:", param, 
               ifelse(!is.null(fraction), paste("and fraction:", fraction), ""),
               ifelse(!is.null(date), paste("up to date:", date), "")))
  }
  
  message(paste("Starting with", nrow(wq_param), "measurements from", 
                length(unique(wq_param$station)), "station names"))
  
  # Convert water quality points to sf object
  message("Converting water quality data to spatial points...")
  wq_points <- st_as_sf(wq_param, 
                        coords = c("longitude_decimal", "latitude_decimal"),
                        crs = 4326)
  
  # Filter points by border if provided
  if (!is.null(border_sf)) {
    message("Filtering points by border...")
    border_sf <- st_transform(border_sf, st_crs(wq_points))
    points_before <- nrow(wq_points)
    wq_points <- st_filter(wq_points, border_sf, .predicate = st_within)
    points_after <- nrow(wq_points)
    message(paste("Filtered to", points_after, "points within border (removed", 
                  points_before - points_after, "points)"))
    if (nrow(wq_points) == 0) {
      stop("No points remain after filtering by border")
    }
  }
  
  # For each station, keep only the most recent measurement
  message("Selecting most recent measurement per station...")
  wq_points <- wq_points %>%
    group_by(station) %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    ungroup()
  
  message(paste("Reduced to", nrow(wq_points), "unique stations with most recent measurements"))
  message(paste("Date range of selected measurements:", 
                min(wq_points$date), "to", max(wq_points$date)))
  
  if (nrow(wq_points) == 0) {
    stop(paste("No data found for parameter:", param))
  }
  
  # Ensure river network is in projected CRS for accurate distance calculations
  message("Checking coordinate reference systems...")
  river_is_geographic <- st_is_longlat(river_network_sf)
  
  if (river_is_geographic) {
    bbox <- st_bbox(river_network_sf)
    lon_center <- mean(c(bbox["xmin"], bbox["xmax"]))
    utm_zone <- floor((lon_center + 180) / 6) + 1
    hemisphere <- ifelse(mean(c(bbox["ymin"], bbox["ymax"])) >= 0, "north", "south")
    utm_crs <- ifelse(hemisphere == "north",
                      paste0("EPSG:326", sprintf("%02d", utm_zone)),
                      paste0("EPSG:327", sprintf("%02d", utm_zone)))
    message(paste("Transforming to", utm_crs, "for accurate distance calculations"))
    river_network_sf <- st_transform(river_network_sf, utm_crs)
  }
  
  wq_points <- st_transform(wq_points, st_crs(river_network_sf))
  
  # Add line_id to river network if not present
  if (!"line_id" %in% names(river_network_sf)) {
    river_network_sf$line_id <- 1:nrow(river_network_sf)
  }
  
  # Find nearest river segment for each point
  message("Finding nearest river segments...")
  nearest_segments <- st_nearest_feature(wq_points, river_network_sf)
  
  # Calculate actual distances to nearest segments
  distances_to_river <- sapply(1:nrow(wq_points), function(i) {
    pt <- wq_points[i, ]
    line <- river_network_sf[nearest_segments[i], ]
    st_distance(pt, line)[1,1]
  })
  
  # Filter out points that are too far from river network
  points_within_snap <- distances_to_river <= snap_distance
  if (sum(!points_within_snap) > 0) {
    message(paste("Removed", sum(!points_within_snap), 
                  "points beyond snap distance of", snap_distance, "meters"))
  }
  
  wq_points_filtered <- wq_points[points_within_snap, ]
  nearest_segments <- nearest_segments[points_within_snap]
  
  if (nrow(wq_points_filtered) == 0) {
    stop("No points within snap distance of river network")
  }
  
  # Snap each point to its nearest position on the nearest line segment
  snapped_points <- wq_points_filtered
  snapped_geometries <- vector("list", nrow(wq_points_filtered))
  position_on_line <- numeric(nrow(wq_points_filtered))
  
  message("Snapping points to river network...")
  for (i in 1:nrow(wq_points_filtered)) {
    pt <- wq_points_filtered[i, ]
    line <- river_network_sf[nearest_segments[i], ]
    line_coords <- st_coordinates(line)[, 1:2]
    pt_coords <- st_coordinates(pt)
    min_dist <- Inf
    closest_point <- NULL
    closest_position <- 0
    total_length <- 0
    
    for (j in 1:(nrow(line_coords) - 1)) {
      x1 <- line_coords[j, 1]
      y1 <- line_coords[j, 2]
      x2 <- line_coords[j + 1, 1]
      y2 <- line_coords[j + 1, 2]
      dx <- x2 - x1
      dy <- y2 - y1
      px <- pt_coords[1] - x1
      py <- pt_coords[2] - y1
      seg_length_sq <- dx^2 + dy^2
      
      if (seg_length_sq == 0) {
        proj_x <- x1
        proj_y <- y1
        t <- 0
      } else {
        t <- max(0, min(1, (px * dx + py * dy) / seg_length_sq))
        proj_x <- x1 + t * dx
        proj_y <- y1 + t * dy
      }
      
      dist <- sqrt((pt_coords[1] - proj_x)^2 + (pt_coords[2] - proj_y)^2)
      
      if (dist < min_dist) {
        min_dist <- dist
        closest_point <- c(proj_x, proj_y)
        segment_length <- sqrt(dx^2 + dy^2)
        closest_position <- total_length + t * segment_length
      }
      
      total_length <- total_length + sqrt(dx^2 + dy^2)
    }
    
    snapped_geometries[[i]] <- st_point(closest_point)
    position_on_line[i] <- closest_position / total_length
  }
  
  st_geometry(snapped_points) <- st_sfc(snapped_geometries, crs = st_crs(wq_points_filtered))
  snapped_points$nearest_segment <- nearest_segments
  snapped_points$position_on_line <- position_on_line
  snapped_points$snap_distance_m <- distances_to_river[points_within_snap]
  
  message(paste("Successfully snapped", nrow(snapped_points), "points to river network"))
  message(paste("Mean snap distance:", round(mean(snapped_points$snap_distance_m), 1), "meters"))
  
  # Trace downstream from each point
  message("Tracing downstream segments...")
  downstream_segments <- find_downstream_segments(river_network_sf, snapped_points)
  
  # Create a list of all downstream segments with their HQ values using selected method
  message(paste("Interpolating HQ values on network using", method, "method..."))
  segment_hq <- interpolate_hq_on_network(river_network_sf, 
                                          snapped_points, 
                                          downstream_segments,
                                          method = method)
  
  # Filter to only downstream segments
  message("Filtering to downstream river segments...")
  downstream_river <- river_network_sf %>%
    filter(line_id %in% unique(downstream_segments$line_id))
  
  if (nrow(downstream_river) == 0) {
    warning("No downstream segments found. Using all river segments.")
    downstream_river <- river_network_sf
  }
  
  # Create extent for raster
  message("Creating risk raster...")
  bbox <- st_bbox(downstream_river)
  buffer_dist <- max_risk_distance
  bbox[1] <- bbox[1] - buffer_dist
  bbox[2] <- bbox[2] - buffer_dist
  bbox[3] <- bbox[3] + buffer_dist
  bbox[4] <- bbox[4] + buffer_dist
  
  # Create empty raster
  ext <- ext(bbox[1], bbox[3], bbox[2], bbox[4])
  risk_raster <- rast(ext, resolution = resolution, crs = st_crs(river_network_sf)$wkt)
  
  # Rasterize downstream river segments with HQ values
  message("Rasterizing river network with HQ values...")
  if (nrow(segment_hq) > 0) {
    # For distance_weighted method, create dense point samples along segments
    if (method == "distance_weighted") {
      message("Creating dense point samples along river for smooth gradients...")
      river_points <- create_gradient_points(river_network_sf, segment_hq, snapped_points, 
                                             downstream_segments, resolution)
      
      if (nrow(river_points) > 0) {
        river_raster <- rasterize(vect(river_points), risk_raster, 
                                  field = "interpolated_hq", 
                                  fun = "mean", 
                                  background = NA)
      } else {
        river_raster <- rast(ext, resolution = resolution, crs = st_crs(river_network_sf)$wkt)
        values(river_raster) <- NA
      }
    } else {
      # Original approach for other methods
      river_hq_sf <- river_network_sf %>%
        left_join(segment_hq, by = "line_id") %>%
        filter(!is.na(interpolated_hq))
      
      river_raster <- rasterize(vect(river_hq_sf), risk_raster, 
                                field = "interpolated_hq", 
                                fun = "max", 
                                background = NA)
    }
  } else {
    river_raster <- rast(ext, resolution = resolution, crs = st_crs(river_network_sf)$wkt)
    values(river_raster) <- NA
  }
  
  # Calculate distance to river
  message("Calculating distance decay...")
  river_binary <- !is.na(river_raster)
  distance_raster <- distance(river_binary)
  
  # Create distance decay factor
  decay_factor <- 1 - (distance_raster / max_risk_distance)
  decay_factor <- clamp(decay_factor, lower = 0, upper = 1)
  
  # Apply distance decay to HQ values
  message("Applying spatial interpolation...")
  river_hq_extended <- focal(river_raster, w = 3, fun = "mean", na.policy = "only", na.rm = TRUE)
  
  # Fill remaining NA values with nearest neighbor approach
  for (i in 1:10) {
    river_hq_extended <- focal(river_hq_extended, w = 5, fun = "mean", 
                               na.policy = "only", na.rm = TRUE)
  }
  
  # Calculate final risk score
  message("Calculating final risk scores...")
  risk_score <- river_hq_extended * decay_factor
  risk_score[distance_raster > max_risk_distance] <- 0
  names(risk_score) <- paste0("Risk_", param)
  
  message(paste("Created risk map for", param, "using", method, "method. Plot using plot_risk_map()."))
  
  return(list(
    risk_raster = risk_score,
    downstream_network = downstream_river,
    snapped_points = snapped_points,
    segment_hq = segment_hq,
    date_used = target_date,
    method_used = method
  ))
}

interpolate_hq_on_network <- function(river_network, points, downstream_segments, method = "default") {
  point_hq <- data.frame(
    point_id = 1:nrow(points),
    HQ = points$HQ,
    station = points$station
  )
  
  # Join with downstream segments to get distance information
  segment_point_data <- downstream_segments %>%
    left_join(point_hq, by = "point_id")
  
  # Apply different aggregation methods based on the method parameter
  if (method == "default") {
    # Original method: simple mean of all upstream HQ values
    segment_hq <- segment_point_data %>%
      group_by(line_id) %>%
      summarise(
        interpolated_hq = mean(HQ, na.rm = TRUE),
        max_hq = max(HQ, na.rm = TRUE),
        min_hq = min(HQ, na.rm = TRUE),
        n_points = n(),
        stations = paste(unique(station), collapse = ", "),
        .groups = "drop"
      )
    
  } else if (method == "distance_weighted") {
    # Network-aware gradient interpolation with confluence averaging
    message("Calculating network-aware gradients between stations...")
    segment_hq <- calculate_network_gradients(river_network, points, downstream_segments)
    
  } else if (method == "max_upstream") {
    # Conservative approach: take maximum HQ from all upstream points
    segment_hq <- segment_point_data %>%
      group_by(line_id) %>%
      summarise(
        interpolated_hq = max(HQ, na.rm = TRUE),
        max_hq = max(HQ, na.rm = TRUE),
        min_hq = min(HQ, na.rm = TRUE),
        n_points = n(),
        stations = paste(unique(station), collapse = ", "),
        .groups = "drop"
      )
    
  } else if (method == "nearest_upstream") {
    # Only use the nearest upstream point for each segment
    segment_hq <- segment_point_data %>%
      group_by(line_id) %>%
      arrange(distance_downstream) %>%
      slice(1) %>%
      summarise(
        interpolated_hq = first(HQ),
        max_hq = first(HQ),
        min_hq = first(HQ),
        n_points = 1,
        stations = first(station),
        nearest_distance = first(distance_downstream),
        .groups = "drop"
      )
    
  } else {
    stop(paste("Unknown method:", method))
  }
  
  # Clean up infinite/NaN values
  segment_hq$interpolated_hq[is.nan(segment_hq$interpolated_hq)] <- NA
  segment_hq$interpolated_hq[is.infinite(segment_hq$interpolated_hq)] <- NA
  segment_hq$max_hq[is.infinite(segment_hq$max_hq)] <- NA
  segment_hq$min_hq[is.infinite(segment_hq$min_hq)] <- NA
  
  message(paste("Interpolated HQ for", sum(!is.na(segment_hq$interpolated_hq)), 
                "of", nrow(segment_hq), "segments"))
  
  return(segment_hq)
}

calculate_network_gradients <- function(river_network, points, downstream_segments) {
  # Create mapping of segments to points
  point_segments <- data.frame(
    point_id = 1:nrow(points),
    line_id = river_network$line_id[points$nearest_segment],
    position = points$position_on_line,
    HQ = points$HQ,
    station = points$station
  )
  
  # Get segment lengths
  segment_lengths <- as.numeric(st_length(river_network))
  names(segment_lengths) <- river_network$line_id
  
  # For each segment, we'll store gradient pairs
  message("Identifying station pairs for gradient calculation...")
  all_segments <- unique(downstream_segments$line_id)
  segment_gradient_info <- list()
  
  for (seg_id in all_segments) {
    segment_gradient_info[[as.character(seg_id)]] <- list(gradient_pairs = list())
  }
  
  # For each point, find downstream points and mark segments between them
  for (i in 1:nrow(points)) {
    upstream_point <- points[i, ]
    upstream_hq <- upstream_point$HQ
    upstream_seg <- river_network$line_id[upstream_point$nearest_segment]
    upstream_position <- upstream_point$position_on_line
    upstream_seg_length <- segment_lengths[as.character(upstream_seg)]
    
    # Distance from start of upstream segment to the station
    upstream_station_offset <- upstream_position * upstream_seg_length
    
    # Find all downstream points from this point
    downstream_from_i <- downstream_segments %>% filter(point_id == i)
    
    # Check if any other points are downstream
    for (j in 1:nrow(points)) {
      if (i == j) next
      
      downstream_point <- points[j, ]
      downstream_seg <- river_network$line_id[downstream_point$nearest_segment]
      downstream_position <- downstream_point$position_on_line
      downstream_seg_length <- segment_lengths[as.character(downstream_seg)]
      
      # Check if point j is downstream of point i
      path_to_j <- downstream_from_i %>% filter(line_id == downstream_seg)
      
      if (nrow(path_to_j) > 0) {
        downstream_hq <- downstream_point$HQ
        
        # Get all segments on path from i to j, ordered by distance
        path_segments <- downstream_from_i %>%
          filter(distance_downstream <= path_to_j$distance_downstream[1]) %>%
          arrange(distance_downstream)
        
        # Calculate total distance from upstream station to downstream station
        # This needs to account for positions within segments
        total_distance <- path_to_j$distance_downstream[1] - upstream_station_offset + 
          (downstream_position * downstream_seg_length)
        
        # Calculate cumulative distance along the path from upstream STATION (not segment start)
        cumulative_dist <- 0
        
        for (k in 1:nrow(path_segments)) {
          seg_id <- path_segments$line_id[k]
          seg_length <- segment_lengths[as.character(seg_id)]
          
          # For the first segment (contains upstream station)
          if (k == 1) {
            # Distance from upstream STATION to start of segment (negative if before station)
            dist_to_seg_start <- -upstream_station_offset
            # Distance from upstream STATION to end of segment  
            dist_to_seg_end <- seg_length - upstream_station_offset
          } else {
            # For other segments, distances from upstream station
            dist_to_seg_start <- cumulative_dist
            dist_to_seg_end <- cumulative_dist + seg_length
          }
          
          seg_key <- as.character(seg_id)
          if (!is.null(segment_gradient_info[[seg_key]])) {
            segment_gradient_info[[seg_key]]$gradient_pairs[[length(segment_gradient_info[[seg_key]]$gradient_pairs) + 1]] <- list(
              upstream_hq = upstream_hq,
              downstream_hq = downstream_hq,
              total_path_distance = total_distance,
              dist_to_seg_start = dist_to_seg_start,
              dist_to_seg_end = dist_to_seg_end,
              is_first_seg = (k == 1),
              is_last_seg = (k == nrow(path_segments)),
              upstream_position = upstream_position,
              downstream_position = downstream_position
            )
          }
          
          # Update cumulative distance for next iteration
          if (k == 1) {
            cumulative_dist <- seg_length - upstream_station_offset
          } else {
            cumulative_dist <- dist_to_seg_end
          }
        }
      }
    }
  }
  
  # Calculate average gradient value for each segment (for summary)
  message("Calculating segment-level summaries...")
  segment_hq <- data.frame(
    line_id = all_segments,
    interpolated_hq = NA_real_,
    n_gradients = 0,
    max_hq = NA_real_,
    min_hq = NA_real_
  )
  
  for (i in 1:nrow(segment_hq)) {
    seg_id <- segment_hq$line_id[i]
    seg_key <- as.character(seg_id)
    
    if (!is.null(segment_gradient_info[[seg_key]]) && 
        length(segment_gradient_info[[seg_key]]$gradient_pairs) > 0) {
      
      gradient_values <- numeric(0)
      for (pair in segment_gradient_info[[seg_key]]$gradient_pairs) {
        # Use midpoint of segment for summary
        mid_dist <- (pair$dist_to_seg_start + pair$dist_to_seg_end) / 2
        if (pair$total_path_distance > 0) {
          proportion <- mid_dist / pair$total_path_distance
          proportion <- max(0, min(1, proportion))
          gradient_val <- pair$upstream_hq + proportion * (pair$downstream_hq - pair$upstream_hq)
          gradient_values <- c(gradient_values, gradient_val)
        }
      }
      
      if (length(gradient_values) > 0) {
        segment_hq$interpolated_hq[i] <- mean(gradient_values, na.rm = TRUE)
        segment_hq$n_gradients[i] <- length(gradient_values)
        segment_hq$max_hq[i] <- max(gradient_values, na.rm = TRUE)
        segment_hq$min_hq[i] <- min(gradient_values, na.rm = TRUE)
      }
    }
  }
  
  # Store the detailed gradient info as an attribute for later use
  attr(segment_hq, "gradient_info") <- segment_gradient_info
  
  # Fill in any remaining segments with nearest upstream value
  missing_segments <- which(is.na(segment_hq$interpolated_hq))
  if (length(missing_segments) > 0) {
    message(paste("Filling", length(missing_segments), "segments with nearest upstream values..."))
    for (idx in missing_segments) {
      seg_id <- segment_hq$line_id[idx]
      upstream_info <- downstream_segments %>%
        filter(line_id == seg_id) %>%
        left_join(point_segments, by = "point_id") %>%
        filter(!is.na(HQ))
      
      if (nrow(upstream_info) > 0) {
        segment_hq$interpolated_hq[idx] <- mean(upstream_info$HQ, na.rm = TRUE)
        segment_hq$n_gradients[idx] <- nrow(upstream_info)
        segment_hq$max_hq[idx] <- max(upstream_info$HQ, na.rm = TRUE)
        segment_hq$min_hq[idx] <- min(upstream_info$HQ, na.rm = TRUE)
      }
    }
  }
  
  return(segment_hq)
}

create_gradient_points <- function(river_network, segment_hq, points, downstream_segments, resolution) {
  gradient_info <- attr(segment_hq, "gradient_info")
  
  if (is.null(gradient_info)) {
    warning("No gradient info found. Using segment-level values.")
    return(st_sf(geometry = st_sfc(crs = st_crs(river_network))))
  }
  
  # Get segment lengths
  segment_lengths <- as.numeric(st_length(river_network))
  names(segment_lengths) <- river_network$line_id
  
  all_points <- list()
  point_counter <- 1
  
  message("Sampling points along river segments...")
  
  for (i in 1:nrow(segment_hq)) {
    seg_id <- segment_hq$line_id[i]
    seg_key <- as.character(seg_id)
    
    # Get the river segment geometry
    river_seg <- river_network[river_network$line_id == seg_id, ]
    if (nrow(river_seg) == 0) next
    
    seg_length <- segment_lengths[as.character(seg_id)]
    if (is.na(seg_length) || seg_length == 0) next
    
    # Sample points along the segment (every ~resolution/2 meters)
    n_samples <- max(3, ceiling(seg_length / (resolution / 2)))
    sample_positions <- seq(0, 1, length.out = n_samples)
    
    line_coords <- st_coordinates(river_seg)[, 1:2]
    
    for (pos_idx in 1:length(sample_positions)) {
      pos <- sample_positions[pos_idx]
      
      # Get point coordinates at this position
      point_coords <- interpolate_point_on_line(line_coords, pos)
      
      # Calculate distance from start of segment to this sample point
      distance_from_seg_start <- pos * seg_length
      
      # Calculate HQ value at this specific position along the segment
      if (!is.null(gradient_info[[seg_key]]) && 
          length(gradient_info[[seg_key]]$gradient_pairs) > 0) {
        
        # Calculate value from each gradient pair
        gradient_values <- numeric(0)
        
        for (pair in gradient_info[[seg_key]]$gradient_pairs) {
          if (pair$total_path_distance > 0) {
            # Calculate absolute distance from upstream station to THIS sample point
            actual_dist <- pair$dist_to_seg_start + distance_from_seg_start
            
            # CRITICAL FIX: Only use this gradient if the sample point is actually
            # between the upstream and downstream stations
            # Skip if we're before the upstream station (actual_dist < 0)
            # Skip if we're after the downstream station (actual_dist > total_path_distance)
            if (actual_dist < 0 || actual_dist > pair$total_path_distance) {
              next
            }
            
            # Calculate the proportion along the total path
            proportion <- actual_dist / pair$total_path_distance
            proportion <- max(0, min(1, proportion))
            
            gradient_val <- pair$upstream_hq + proportion * (pair$downstream_hq - pair$upstream_hq)
            gradient_values <- c(gradient_values, gradient_val)
          }
        }
        
        if (length(gradient_values) > 0) {
          hq_value <- mean(gradient_values, na.rm = TRUE)
        } else {
          # If no valid gradients, fall back to segment-level value
          hq_value <- segment_hq$interpolated_hq[i]
        }
      } else {
        hq_value <- segment_hq$interpolated_hq[i]
      }
      
      if (!is.na(hq_value)) {
        all_points[[point_counter]] <- data.frame(
          line_id = seg_id,
          position = pos,
          interpolated_hq = hq_value,
          x = point_coords[1],
          y = point_coords[2]
        )
        point_counter <- point_counter + 1
      }
    }
  }
  
  if (length(all_points) == 0) {
    return(st_sf(geometry = st_sfc(crs = st_crs(river_network))))
  }
  
  # Combine all points
  points_df <- do.call(rbind, all_points)
  points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = st_crs(river_network))
  
  message(paste("Created", nrow(points_sf), "sample points along river network"))
  return(points_sf)
}

interpolate_point_on_line <- function(line_coords, position) {
  if (position <= 0) return(line_coords[1, ])
  if (position >= 1) return(line_coords[nrow(line_coords), ])
  
  # Calculate cumulative distances
  n_points <- nrow(line_coords)
  segment_lengths <- numeric(n_points - 1)
  for (i in 1:(n_points - 1)) {
    dx <- line_coords[i + 1, 1] - line_coords[i, 1]
    dy <- line_coords[i + 1, 2] - line_coords[i, 2]
    segment_lengths[i] <- sqrt(dx^2 + dy^2)
  }
  
  total_length <- sum(segment_lengths)
  cumulative_lengths <- c(0, cumsum(segment_lengths))
  target_length <- position * total_length
  
  # Find which segment contains the target position
  seg_idx <- which(cumulative_lengths <= target_length)
  seg_idx <- max(seg_idx)
  
  if (seg_idx >= n_points) {
    return(line_coords[n_points, ])
  }
  
  # Interpolate within the segment
  seg_start_dist <- cumulative_lengths[seg_idx]
  seg_length <- segment_lengths[seg_idx]
  
  if (seg_length == 0) {
    return(line_coords[seg_idx, ])
  }
  
  t <- (target_length - seg_start_dist) / seg_length
  t <- max(0, min(1, t))
  
  x <- line_coords[seg_idx, 1] + t * (line_coords[seg_idx + 1, 1] - line_coords[seg_idx, 1])
  y <- line_coords[seg_idx, 2] + t * (line_coords[seg_idx + 1, 2] - line_coords[seg_idx, 2])
  
  return(c(x, y))
}


# Helper function to find downstream segments
find_downstream_segments <- function(river_network, points, connection_tolerance = 50) {
  message("Building network topology...")
  # Extract endpoints of all segments
  endpoints <- lapply(1:nrow(river_network), function(i) {
    coords <- st_coordinates(river_network[i, ])[, 1:2]
    list(
      line_id = river_network$line_id[i],
      start = coords[1, ],
      end = coords[nrow(coords), ]
    )
  })
  
  # Build adjacency list
  adjacency <- vector("list", nrow(river_network))
  names(adjacency) <- river_network$line_id
  message("Finding segment connections...")
  
  for (i in 1:length(endpoints)) {
    end_pt <- endpoints[[i]]$end
    connected <- c()
    for (j in 1:length(endpoints)) {
      if (i == j) next
      start_pt <- endpoints[[j]]$start
      dist <- sqrt(sum((end_pt - start_pt)^2))
      if (dist < connection_tolerance) {
        connected <- c(connected, endpoints[[j]]$line_id)
      }
    }
    adjacency[[as.character(endpoints[[i]]$line_id)]] <- connected
  }
  
  # Trace downstream from each point
  downstream_list <- list()
  message(paste("Tracing downstream from", nrow(points), "points..."))
  
  for (i in 1:nrow(points)) {
    segment_id <- points$nearest_segment[i]
    start_line_id <- river_network$line_id[segment_id]
    visited <- c()
    queue <- start_line_id
    distance_from_source <- list()
    distance_from_source[[as.character(start_line_id)]] <- 0
    
    while (length(queue) > 0) {
      current_id <- queue[1]
      queue <- queue[-1]
      if (current_id %in% visited) next
      visited <- c(visited, current_id)
      connected <- adjacency[[as.character(current_id)]]
      
      if (length(connected) > 0) {
        for (next_id in connected) {
          if (!(next_id %in% visited) && !(next_id %in% queue)) {
            queue <- c(queue, next_id)
            current_dist <- distance_from_source[[as.character(current_id)]]
            segment_length <- as.numeric(st_length(river_network[river_network$line_id == current_id, ]))
            distance_from_source[[as.character(next_id)]] <- current_dist + segment_length
          }
        }
      }
    }
    
    if (length(visited) > 0) {
      downstream <- data.frame(
        line_id = visited,
        point_id = i,
        distance_downstream = sapply(visited, function(lid) {
          dist <- distance_from_source[[as.character(lid)]]
          if (is.null(dist)) 0 else dist
        }),
        stringsAsFactors = FALSE
      )
      downstream_list[[i]] <- downstream
    }
  }
  
  if (length(downstream_list) > 0) {
    result <- do.call(rbind, downstream_list)
    message(paste("Found", length(unique(result$line_id)), "unique downstream segments"))
    return(result)
  } else {
    return(data.frame(line_id = integer(), point_id = integer(), 
                      distance_downstream = numeric()))
  }
}

# Interactive plotting with leaflet
plot_risk_map <- function(result, param_name = NULL) {
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for interactive maps. Install with: install.packages('leaflet')")
  }
  
  library(leaflet)
  message("Starting interactive map creation...")
  
  if (is.null(param_name)) {
    param_name <- names(result$risk_raster)
  }
  
  message("Processing risk raster values...")
  raster_values <- values(result$risk_raster, mat = FALSE)
  valid_values <- raster_values[!is.na(raster_values) & raster_values > 0]
  
  message("Transforming spatial data to WGS84...")
  downstream_network_wgs84 <- st_transform(result$downstream_network, 4326)
  snapped_points_wgs84 <- st_transform(result$snapped_points, 4326)
  hq_values <- snapped_points_wgs84$HQ
  hq_range <- range(hq_values, na.rm = TRUE)
  
  message("Initializing leaflet map...")
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addScaleBar(position = "bottomleft")
  
  # Add method information to title if available
  map_title <- if (!is.null(result$method_used)) {
    paste0("Risk Map (Method: ", result$method_used, ")")
  } else {
    "Risk Map"
  }
  
  # Add risk raster if valid values exist
  if (length(valid_values) > 0) {
    message("Adding risk raster layer...")
    risk_raster_wgs84 <- project(result$risk_raster, "EPSG:4326", method = "bilinear")
    pal_raster <- colorNumeric(
      palette = "YlOrRd",
      domain = range(valid_values),
      na.color = "transparent"
    )
    risk_stars <- stars::st_as_stars(risk_raster_wgs84)
    
    map <- map %>%
      addRasterImage(
        as(risk_stars, "Raster"),
        colors = pal_raster,
        opacity = 0.7,
        group = "Risk Score"
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal_raster,
        values = valid_values,
        title = "Risk Score",
        group = "Risk Score"
      )
    message("Risk raster layer added successfully")
  } else {
    warning("Risk raster contains no valid values. Showing river network and points only.")
  }
  
  message("Adding river network layer...")
  map <- map |>
    addPolylines(
      data = river_network,
      color = "steelblue",
      weight = 1.5,
      opacity = 0.2,
      group = "River Network"
    )
  map <- map %>%
    addPolylines(
      data = downstream_network_wgs84,
      color = "blue",
      weight = 2,
      opacity = 0.8,
      group = "River Network",
      popup = ~paste0("<b>Line ID:</b> ", line_id)
    )
  
  map <- map |>
    addPolygons(
      data = bol_border,
      color = "black",
      weight = 2.5,
      fillOpacity = 0,
      group = "Bolivia Border"
    )
  
  # Add monitoring points if they exist
  if (!is.null(result$snapped_points) && nrow(result$snapped_points) > 0) {
    message("Adding monitoring stations layer...")
    hq_values_clean <- hq_values[!is.na(hq_values)]
    
    if (length(hq_values_clean) > 0) {
      pal_points <- colorNumeric(
        palette = c("green", "yellow", "red"),
        domain = hq_range,
        na.color = "gray"
      )
      
      popup_content <- paste0(
        "<b>Station:</b> ", snapped_points_wgs84$station, "<br>",
        "<b>Date:</b> ", snapped_points_wgs84$date, "<br>",
        "<b>HQ:</b> ", round(snapped_points_wgs84$HQ, 3)
      )
      
      map <- map %>%
        addCircleMarkers(
          data = snapped_points_wgs84,
          radius = 8,
          color = "black",
          weight = 2,
          fillColor = ~pal_points(HQ),
          fillOpacity = 0.8,
          group = "Monitoring Stations",
          popup = popup_content,
          label = ~station
        ) %>%
        addLegend(
          position = "topright",
          pal = pal_points,
          values = hq_values,
          title = "Hazard Quotient",
          group = "Monitoring Stations"
        )
      message("Monitoring stations layer added successfully")
    }
  }
  
  message("Adding layer controls...")
  map <- map %>%
    addLayersControl(
      overlayGroups = c("Risk Score", "River Network", "Monitoring Stations"),
      options = layersControlOptions(collapsed = FALSE),
      position = "topleft"
    )
  
  message("Interactive map created successfully!")
  return(map)
}