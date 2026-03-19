# True Upstream Mining Exposure Analysis in R
# Handles points snapped to river segments (not nodes) by splitting segments

library(sf)           # For spatial data handling
library(dplyr)        # For data manipulation
library(igraph)       # For network analysis
library(ggplot2)      # For plotting
library(lwgeom)       # For advanced geometric operations

# =============================================================================
# SEGMENT-SPLITTING APPROACH FOR POINTS ON RIVER SEGMENTS
# =============================================================================


# Read your exported data
read_spatial_data <- function() {
  cat("Reading spatial data...\n")
  
  ######## CHANGE THESE FILE PATHS TO WHEREVER YOUR DATA IS STORED ##############
  rivers <- st_read("data/river_network/River_Network.shp")  # or .gpkg
  settlements <- st_read("data/mining_exposure/stations_scored_snapped.shp")  # or .gpkg  
  mines <- st_read("data/mining_exposure/mines_2km.shp")  # or .gpkg
  
  # Check what the actual field names are after export
  cat("River fields:", names(rivers), "\n")
  cat("Settlement fields:", names(settlements), "\n") 
  cat("Mine fields:", names(mines), "\n")
  
  return(list(rivers = rivers, settlements = settlements, mines = mines))
}

# Find which river segment each point lies on
find_point_segments <- function(points, rivers, point_type = "points") {
  cat(sprintf("Finding river segments for %d %s...\n", nrow(points), point_type))
  
  point_segments <- data.frame(
    point_id = 1:nrow(points),
    segment_id = NA,
    position_along_segment = NA,
    distance_to_segment = NA
  )
  
  for (i in 1:nrow(points)) {
    if (i %% 500 == 0 || i <= 10) {
      cat(sprintf("  Processing %s %d/%d\n", point_type, i, nrow(points)))
    }
    
    point <- points[i,]
    
    # Find nearest river segment
    distances <- st_distance(point, rivers)
    nearest_idx <- which.min(distances)[1]
    
    if (!is.na(nearest_idx)) {
      # Calculate position along the segment (0 = start, 1 = end)
      river_geom <- st_geometry(rivers[nearest_idx,])[[1]]
      position <- calculate_position_along_linestring(point, river_geom)
      
      point_segments$segment_id[i] <- nearest_idx
      point_segments$position_along_segment[i] <- position
      point_segments$distance_to_segment[i] <- as.numeric(distances[nearest_idx])
    }
  }
  
  valid_assignments <- !is.na(point_segments$segment_id)
  cat(sprintf("Successfully assigned %d/%d %s to river segments\n", 
              sum(valid_assignments), nrow(points), point_type))
  
  return(point_segments[valid_assignments,])
}

# Calculate position along linestring (0 = start, 1 = end)
calculate_position_along_linestring <- function(point, linestring) {
  if (!inherits(linestring, "LINESTRING")) {
    return(0.5)  # Default to middle
  }
  
  # Get coordinates
  line_coords <- st_coordinates(linestring)
  point_coords <- st_coordinates(point)[1, 1:2]
  
  # Find closest point on line and calculate position
  min_dist <- Inf
  best_position <- 0.5
  cumulative_length <- 0
  
  # Check each segment of the linestring
  for (i in 1:(nrow(line_coords) - 1)) {
    seg_start <- line_coords[i, 1:2]
    seg_end <- line_coords[i + 1, 1:2]
    
    # Get closest point on this segment
    result <- point_to_segment_projection(point_coords, seg_start, seg_end)
    
    if (result$distance < min_dist) {
      min_dist <- result$distance
      segment_length <- sqrt(sum((seg_end - seg_start)^2))
      position_in_segment <- result$t * segment_length
      
      # Calculate position along entire linestring
      total_length <- calculate_linestring_length(line_coords)
      if (total_length > 0) {
        best_position <- (cumulative_length + position_in_segment) / total_length
      }
    }
    
    # Add this segment's length to cumulative
    cumulative_length <- cumulative_length + sqrt(sum((seg_end - seg_start)^2))
  }
  
  return(pmax(0, pmin(1, best_position)))  # Clamp to [0,1]
}

# Project point onto line segment
point_to_segment_projection <- function(point, seg_start, seg_end) {
  seg_vec <- seg_end - seg_start
  point_vec <- point - seg_start
  
  seg_length_sq <- sum(seg_vec^2)
  if (seg_length_sq == 0) {
    return(list(t = 0, distance = sqrt(sum(point_vec^2))))
  }
  
  t <- sum(point_vec * seg_vec) / seg_length_sq
  t <- pmax(0, pmin(1, t))  # Clamp to segment
  
  closest_point <- seg_start + t * seg_vec
  distance <- sqrt(sum((point - closest_point)^2))
  
  return(list(t = t, distance = distance))
}

# Calculate total length of linestring
calculate_linestring_length <- function(coords) {
  if (nrow(coords) < 2) return(0)
  
  total <- 0
  for (i in 1:(nrow(coords) - 1)) {
    segment_length <- sqrt(sum((coords[i + 1, 1:2] - coords[i, 1:2])^2))
    total <- total + segment_length
  }
  return(total)
}

# Create split river network with intermediate nodes
create_split_river_network <- function(rivers, settlements, mines, from_field, to_field) {
  cat("Creating split river network with intermediate nodes...\n")
  
  # Find which segments contain points
  settlement_segments <- find_point_segments(settlements, rivers, "settlements")
  mine_segments <- find_point_segments(mines, rivers, "mines")
  
  # Combine all point locations
  all_point_segments <- rbind(
    data.frame(settlement_segments, type = "settlement"),
    data.frame(mine_segments, type = "mine")
  )
  
  # Sort by segment_id and position for processing
  all_point_segments <- all_point_segments[order(all_point_segments$segment_id, 
                                                 all_point_segments$position_along_segment),]
  
  cat(sprintf("Need to split %d segments containing %d points\n", 
              length(unique(all_point_segments$segment_id)), nrow(all_point_segments)))
  
  # Build new network with split segments
  new_edges <- data.frame()
  point_to_node_mapping <- data.frame()
  next_node_id <- max(c(rivers[[from_field]], rivers[[to_field]]), na.rm = TRUE) + 1
  
  # Process each original river segment
  for (seg_id in 1:nrow(rivers)) {
    segment <- rivers[seg_id,]
    from_node <- segment[[from_field]]
    to_node <- segment[[to_field]]
    
    # Find points on this segment
    points_on_segment <- all_point_segments[all_point_segments$segment_id == seg_id,]
    
    if (nrow(points_on_segment) == 0) {
      # No points on this segment - keep original
      new_edges <- rbind(new_edges, data.frame(
        from = from_node,
        to = to_node,
        original_segment_id = seg_id
      ))
    } else {
      # Split this segment at point locations
      points_on_segment <- points_on_segment[order(points_on_segment$position_along_segment),]
      
      # Create nodes for each point position
      split_positions <- c(0, points_on_segment$position_along_segment, 1)
      split_nodes <- c(from_node, next_node_id:(next_node_id + nrow(points_on_segment) - 1), to_node)
      
      # Record point-to-node mappings
      for (i in 1:nrow(points_on_segment)) {
        point_to_node_mapping <- rbind(point_to_node_mapping, data.frame(
          original_point_id = points_on_segment$point_id[i],
          point_type = points_on_segment$type[i],
          assigned_node = split_nodes[i + 1],
          segment_id = seg_id,
          position = points_on_segment$position_along_segment[i]
        ))
      }
      
      # Create edges between consecutive nodes
      for (i in 1:(length(split_nodes) - 1)) {
        new_edges <- rbind(new_edges, data.frame(
          from = split_nodes[i],
          to = split_nodes[i + 1],
          original_segment_id = seg_id
        ))
      }
      
      next_node_id <- next_node_id + nrow(points_on_segment)
    }
    
    if (seg_id %% 1000 == 0) {
      cat(sprintf("  Processed segment %d/%d\n", seg_id, nrow(rivers)))
    }
  }
  
  # Create graph
  river_graph <- graph_from_data_frame(new_edges, directed = TRUE)
  
  cat(sprintf("Split network created: %d nodes, %d edges\n", vcount(river_graph), ecount(river_graph)))
  cat(sprintf("Point mappings: %d settlements, %d mines\n",
              sum(point_to_node_mapping$point_type == "settlement"),
              sum(point_to_node_mapping$point_type == "mine")))
  
  return(list(
    graph = river_graph,
    point_mappings = point_to_node_mapping,
    settlement_segments = settlement_segments,
    mine_segments = mine_segments
  ))
}

# Build river network with segment splitting
build_river_network_split <- function(rivers, settlements, mines) {
  cat("Building river network with segment splitting...\n")
  
  # Identify node field names
  river_fields <- names(rivers)
  cat("Available river fields:", paste(river_fields, collapse = ", "), "\n")
  
  # Find from/to node fields
  from_candidates <- river_fields[grepl("from|FROM|From|fr_|FR_|f_node|F_NODE", river_fields, ignore.case = TRUE)]
  to_candidates <- river_fields[grepl("to|TO|To|to_|TO_|t_node|T_NODE", river_fields, ignore.case = TRUE)]
  
  if (length(from_candidates) == 0) {
    stop("Could not find 'from_node' field. Available fields: ", paste(river_fields, collapse = ", "))
  }
  if (length(to_candidates) == 0) {
    stop("Could not find 'to_node' field. Available fields: ", paste(river_fields, collapse = ", "))
  }
  
  from_field <- from_candidates[1]
  to_field <- to_candidates[1]
  
  cat(sprintf("Using fields: %s -> %s\n", from_field, to_field))
  
  # Create split network
  split_network <- create_split_river_network(rivers, settlements, mines, from_field, to_field)
  
  return(split_network)
}

# Find all nodes upstream from a given node
find_upstream_nodes <- function(graph, start_node, max_distance = Inf) {
  # Convert node to character if it's numeric
  start_node <- as.character(start_node)
  
  # Check if node exists in graph
  if (!start_node %in% V(graph)$name) {
    return(character(0))
  }
  
  # Reverse graph to trace upstream
  reversed_graph <- reverse_edges(graph)
  
  # Find all reachable nodes
  reachable <- distances(reversed_graph, v = start_node, mode = "out")
  
  # Get nodes within max_distance
  upstream_nodes <- names(reachable[1, ])[reachable[1, ] <= max_distance & reachable[1, ] < Inf]
  upstream_nodes <- upstream_nodes[upstream_nodes != start_node]  # Exclude self
  
  return(upstream_nodes)
}

# Calculate upstream mining exposure with split network
calculate_upstream_mining_exposure_split <- function(settlements, mines, network_data, max_upstream_km = 200) {
  cat("Calculating upstream mining exposure with split network...\n")
  cat(sprintf("Maximum upstream search distance: %d km\n", max_upstream_km))
  
  graph <- network_data$graph
  point_mappings <- network_data$point_mappings
  
  # Get settlement and mine node mappings
  settlement_mappings <- point_mappings[point_mappings$point_type == "settlement",]
  mine_mappings <- point_mappings[point_mappings$point_type == "mine",]
  
  cat(sprintf("Processing %d settlements with %d mines\n", 
              nrow(settlement_mappings), nrow(mine_mappings)))
  
  # Initialize results
  settlements$upstream_mine_count <- 0
  settlements$mining_exposure_score <- 0.0
  settlements$nearest_upstream_mine_km <- NA
  settlements$farthest_upstream_mine_km <- NA
  
  # Convert max distance to network units
  max_network_distance <- max_upstream_km
  
  # Process each settlement
  for (i in 1:nrow(settlement_mappings)) {
    if (i %% 50 == 0 || i <= 10) {
      cat(sprintf("Processing settlement %d/%d\n", i, nrow(settlement_mappings)))
    }
    
    settlement_node <- as.character(settlement_mappings$assigned_node[i])
    settlement_idx <- settlement_mappings$original_point_id[i]
    
    # Find all upstream nodes within distance limit
    upstream_nodes <- find_upstream_nodes(graph, settlement_node, max_network_distance)
    
    if (length(upstream_nodes) > 0) {
      # Find mines on upstream nodes
      upstream_mine_indices <- mine_mappings$original_point_id[mine_mappings$assigned_node %in% upstream_nodes]
      
      if (length(upstream_mine_indices) > 0) {
        # Get actual mine locations for distance calculations
        upstream_mine_locations <- mines[upstream_mine_indices,]
        settlement_location <- settlements[settlement_idx,]
        
        # Calculate actual distances
        distances_m <- as.numeric(st_distance(settlement_location, upstream_mine_locations))
        distances_km <- distances_m / 1000
        
        # Calculate exposure score
        exposure_score <- sum(1 / (1 + distances_km))
        
        # Update results
        settlements$upstream_mine_count[settlement_idx] <- length(distances_km)
        settlements$mining_exposure_score[settlement_idx] <- exposure_score
        settlements$nearest_upstream_mine_km[settlement_idx] <- min(distances_km)
        settlements$farthest_upstream_mine_km[settlement_idx] <- max(distances_km)
      }
    }
  }
  
  return(settlements)
}

# =============================================================================
# DIAGNOSTIC FUNCTIONS
# =============================================================================

diagnose_split_network <- function(network_data, settlements, mines) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("SPLIT NETWORK DIAGNOSTICS\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  graph <- network_data$graph
  point_mappings <- network_data$point_mappings
  
  cat(sprintf("Split network statistics:\n"))
  cat(sprintf("  Total nodes: %d\n", vcount(graph)))
  cat(sprintf("  Total edges: %d\n", ecount(graph)))
  cat(sprintf("  Connected components: %d\n", components(graph)$no))
  
  settlement_mappings <- point_mappings[point_mappings$point_type == "settlement",]
  mine_mappings <- point_mappings[point_mappings$point_type == "mine",]
  
  cat(sprintf("\nPoint mapping results:\n"))
  cat(sprintf("  Settlements mapped: %d/%d (%.1f%%)\n", 
              nrow(settlement_mappings), nrow(settlements),
              100 * nrow(settlement_mappings) / nrow(settlements)))
  cat(sprintf("  Mines mapped: %d/%d (%.1f%%)\n", 
              nrow(mine_mappings), nrow(mines),
              100 * nrow(mine_mappings) / nrow(mines)))
  
  # Test upstream connectivity
  cat(sprintf("\nTesting upstream connectivity (first 3 settlements):\n"))
  for (i in 1:min(3, nrow(settlement_mappings))) {
    settlement_node <- as.character(settlement_mappings$assigned_node[i])
    upstream_nodes <- find_upstream_nodes(graph, settlement_node, max_distance = 50)
    cat(sprintf("  Settlement %d (node %s): %d upstream nodes within 50 steps\n", 
                i, settlement_node, length(upstream_nodes)))
    
    # Check for upstream mines
    upstream_mine_count <- sum(mine_mappings$assigned_node %in% upstream_nodes)
    cat(sprintf("    Upstream mines found: %d\n", upstream_mine_count))
  }
}

# =============================================================================
# RESULTS ANALYSIS
# =============================================================================

analyze_upstream_results <- function(settlements) {
  cat("\n", paste(rep("=", 70), collapse = ""), "\n")
  cat("UPSTREAM MINING EXPOSURE RESULTS (SPLIT NETWORK)\n")
  cat(paste(rep("=", 70), collapse = ""), "\n")
  
  total_settlements <- nrow(settlements)
  settlements_with_mines <- sum(settlements$upstream_mine_count > 0, na.rm = TRUE)
  
  cat(sprintf("Total settlements: %d\n", total_settlements))
  cat(sprintf("Settlements with upstream mines: %d (%.1f%%)\n", 
              settlements_with_mines, 
              100 * settlements_with_mines / total_settlements))
  
  if (settlements_with_mines > 0) {
    exposed_settlements <- settlements[settlements$upstream_mine_count > 0,]
    
    cat(sprintf("\nUpstream Exposure Statistics:\n"))
    cat(sprintf("  Mean exposure score: %.3f\n", mean(exposed_settlements$mining_exposure_score, na.rm = TRUE)))
    cat(sprintf("  Median exposure score: %.3f\n", median(exposed_settlements$mining_exposure_score, na.rm = TRUE)))
    cat(sprintf("  Max exposure score: %.3f\n", max(exposed_settlements$mining_exposure_score, na.rm = TRUE)))
    
    cat(sprintf("\nUpstream Mine Count Statistics:\n"))
    cat(sprintf("  Mean upstream mines: %.1f\n", mean(exposed_settlements$upstream_mine_count, na.rm = TRUE)))
    cat(sprintf("  Median upstream mines: %.0f\n", median(exposed_settlements$upstream_mine_count, na.rm = TRUE)))
    cat(sprintf("  Max upstream mines: %d\n", max(exposed_settlements$upstream_mine_count, na.rm = TRUE)))
    
    cat(sprintf("\nDistance to Nearest Upstream Mine:\n"))
    cat(sprintf("  Mean distance: %.1f km\n", mean(exposed_settlements$nearest_upstream_mine_km, na.rm = TRUE)))
    cat(sprintf("  Median distance: %.1f km\n", median(exposed_settlements$nearest_upstream_mine_km, na.rm = TRUE)))
    
    # Distribution of upstream mine counts
    mine_count_table <- table(exposed_settlements$upstream_mine_count)
    cat(sprintf("\nDistribution of Upstream Mine Counts:\n"))
    for (count in names(mine_count_table)) {
      settlements_count <- mine_count_table[count]
      cat(sprintf("  %s upstream mines: %d settlements\n", count, settlements_count))
    }
    
    # Top 15 most exposed settlements
    cat(sprintf("\nTop 15 Most Exposed Settlements:\n"))
    top_settlements <- exposed_settlements[order(-exposed_settlements$upstream_mine_count, -exposed_settlements$mining_exposure_score),]
    
    settlement_id_field <- names(settlements)[1]
    
    for (i in 1:min(15, nrow(top_settlements))) {
      row <- top_settlements[i,]
      settlement_id <- row[[settlement_id_field]]
      mine_count <- row$upstream_mine_count
      exposure_score <- row$mining_exposure_score  
      nearest_dist <- row$nearest_upstream_mine_km
      
      cat(sprintf("  Settlement %s: %d upstream mines, score: %.3f, nearest: %.1f km\n",
                  settlement_id, mine_count, exposure_score, 
                  ifelse(is.na(nearest_dist), 0, nearest_dist)))
    }
  } else {
    cat("\nNo settlements found with upstream mines.\n")
    cat("Check the diagnostics for potential issues.\n")
  }
  
  return(settlements)
}

# =============================================================================
# VISUALIZATION
# =============================================================================

create_upstream_exposure_map <- function(settlements, mines, rivers = NULL) {
  cat("Creating upstream exposure map...\n")
  
  p <- ggplot()
  
  if (!is.null(rivers)) {
    p <- p + geom_sf(data = rivers, color = "lightblue", size = 0.2, alpha = 0.6)
  }
  
  p <- p + geom_sf(data = mines, color = "red", size = 1, alpha = 0.8, shape = 17)
  
  settlements_with_exposure <- settlements[settlements$upstream_mine_count > 0,]
  settlements_no_exposure <- settlements[settlements$upstream_mine_count == 0,]
  
  if (nrow(settlements_no_exposure) > 0) {
    p <- p + geom_sf(data = settlements_no_exposure, color = "gray60", size = 0.8, alpha = 0.4)
  }
  
  if (nrow(settlements_with_exposure) > 0) {
    p <- p + 
      geom_sf(data = settlements_with_exposure, 
              aes(color = upstream_mine_count, size = mining_exposure_score)) +
      scale_color_viridis_c(name = "Upstream\nMines", option = "plasma") +
      scale_size_continuous(name = "Exposure\nScore", range = c(1, 4))
  }
  
  p <- p +
    theme_minimal() +
    labs(title = "Upstream Mining Exposure Analysis (Split Network)",
         subtitle = "Red triangles = mines, Colored circles = settlements with upstream mining exposure") +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  print(p)
  ggsave("upstream_mining_exposure_map_split.png", p, width = 14, height = 10, dpi = 300)
  cat("Map saved as 'upstream_mining_exposure_map_split.png'\n")
  
  return(p)
}

# =============================================================================
# MAIN EXECUTION
# =============================================================================

run_upstream_mining_analysis_split <- function(max_upstream_km = 200, run_diagnostics = TRUE) {
  cat(paste(rep("=", 70), collapse = ""), "\n")
  cat("UPSTREAM MINING ANALYSIS - SEGMENT SPLITTING APPROACH\n")
  cat(paste(rep("=", 70), collapse = ""), "\n\n")
  
  start_time <- Sys.time()
  
  # Read data
  cat("Step 1: Reading spatial data...\n")
  spatial_data <- read_spatial_data()
  rivers <- spatial_data$rivers
  settlements <- spatial_data$settlements
  mines <- spatial_data$mines
  
  cat(sprintf("Loaded: %d river segments, %d settlements, %d mines\n", 
              nrow(rivers), nrow(settlements), nrow(mines)))
  
  # Build split network
  cat("\nStep 2: Building split river network...\n")
  network_data <- build_river_network_split(rivers, settlements, mines)
  
  # Run diagnostics
  if (run_diagnostics) {
    cat("\nStep 2.5: Running diagnostics...\n")
    diagnose_split_network(network_data, settlements, mines)
  }
  
  # Calculate upstream exposure
  cat(sprintf("\nStep 3: Calculating upstream exposure...\n"))
  settlements <- calculate_upstream_mining_exposure_split(settlements, mines, network_data, max_upstream_km)
  
  # Analyze results
  cat("\nStep 4: Analyzing results...\n")
  settlements <- analyze_upstream_results(settlements)
  
  # Create map
  cat("\nStep 5: Creating visualization...\n")
  create_upstream_exposure_map(settlements, mines, rivers)
  
  # Export results
  cat("\nStep 6: Exporting results...\n")
  st_write(settlements, "upstream_mining_exposure_results_split.shp", delete_dsn = TRUE)
  
  settlements_df <- settlements %>%
    st_drop_geometry() %>%
    select(1, upstream_mine_count, mining_exposure_score, 
           nearest_upstream_mine_km, farthest_upstream_mine_km)
  
  write.csv(settlements_df, "upstream_mining_exposure_results_split.csv", row.names = FALSE)
  
  end_time <- Sys.time()
  total_time <- end_time - start_time
  
  cat(sprintf("\n✅ SPLIT NETWORK ANALYSIS COMPLETE!\n"))
  cat(sprintf("Total processing time: %.1f minutes\n", 
              as.numeric(total_time, units = "mins")))
  cat(sprintf("Results exported as 'upstream_mining_exposure_results_split.*'\n\n"))
  
  return(list(settlements = settlements, network_data = network_data))
}

# =============================================================================
# USAGE
# =============================================================================

cat("UPSTREAM MINING ANALYSIS - SEGMENT SPLITTING APPROACH\n")
cat("====================================================\n\n")
cat("This version handles points snapped to river segments by:\n")
cat("1. Finding which segment each point lies on\n") 
cat("2. Calculating the position along that segment (0-1)\n")
cat("3. Splitting segments at point locations\n")
cat("4. Creating new intermediate nodes where points are located\n")
cat("5. Building a new network graph with these split segments\n")
cat("6. Tracing upstream through the split network\n\n")
cat("RUN ANALYSIS:\n")
cat("results <- run_upstream_mining_analysis_split()\n\n")
cat("This should accurately identify upstream mines!\n")