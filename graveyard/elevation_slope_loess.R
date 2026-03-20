library(readxl)

downstream_slope <- function(station) {
  
  elev_path <- paste0("misc/downstream_points_", station, "_elev.xls")
  elev <- read_excel(elev_path)
  
  
  # elev$slope <- rep(0, nrow(elev))
  #  
  # for (i in 1:nrow(elev)) {
  #   
  #   if (i < 8 | i > (nrow(elev) - 7)) {
  #     
  #     elev$slope[i] = NA
  #     
  #   } else {
  #     
  #     dist = 1400
  #     
  #     elev_loss = elev$elev[i-7] - elev$elev[i+7]
  #     
  #     elev$slope[i] = elev_loss / dist
  #   }
  # 
  # }
  # 
  # ggplot(elev, aes(x = index, y = elev)) +
  #   geom_point() +
  #   geom_smooth()
  # 
  # ggplot(elev, aes(x = index, y = slope)) +
  #   geom_point() +
  #   geom_smooth()
  # 
  # summary(elev$slope)
  
  span <- switch(station,
                 "potosi" = 0.2,
                 "yocalla" = 0.65,
                 "la_angostura" = 0.25,
                 "cotagaita" = 0.5,
                 "colavi" = 0.2,
                 "tumusla" = 0.4)
  
  # Fit the same model that geom_smooth uses
  model <- loess(elev ~ index, data = elev, span = span)  # span = 0.75 is the default
  
  
  # Predict smoothed values at your original points
  elev$smoothed_elev <- predict(model, newdata = elev)
  
  # Calculate derivatives using finite differences
  # Convert index to actual distance in meters
  elev$distance_m <- elev$index * 100
  
  # Calculate slope as rise/run between smoothed points
  elev$smoothed_slope <- c(NA, diff(elev$smoothed_elev) / diff(elev$distance_m))
  
  # # Plot elevation with station markers
  # ggplot(elev, aes(x = distance_m/1000)) +
  #   geom_point(aes(y = elev), alpha = 0.1, size = 2) +  # original points
  #   geom_line(aes(y = smoothed_elev), color = "cyan", size = 1) +  # smoothed line
  #   labs(y = "Elevation (m)", x = "Distance along river (km)",
  #        title = "River Elevation Profile with Monitoring Stations") +
  #   theme_minimal()
  # 
  # 
  # # Plot slope with stations (flipped y-axis)
  # ggplot(elev, aes(x = distance_m/1000, y = smoothed_slope)) +
  #   geom_hline(yintercept = 0, color = "gray") +
  #   geom_line(linewidth = 1.2) +
  #   scale_y_reverse() +  # This flips the axis
  #   labs(y = "Slope (m/m)", x = "Distance along river (km)",
  #        title = "River Slope with Monitoring Stations") +
  #   theme_minimal()
  
  
  # write_csv(elev, "misc/potosi_smoothed_slope.csv")
  
  # Create a data frame with station information
  ##### CHANGE THIS FOR DIFFERENT STATIONS/SEGMENTS OF RIVER
  stations <- switch(station,
                     "potosi" = data.frame(
                       name = c("San Antonio - Potosí", "Tarapaya", "Pilcomayo arriba Tacobamba", "Talula - Automática",
                                "Puente Mendez", "Viña Quemada", "Puente Aruma - Automática", "Villamontes"),
                       index = c(1, 155, 834, 1012, 1460, 1939, 4506, 5686)
                     ),
                     "yocalla" = data.frame(
                       name = c("Yocalla"),
                       index = c(1)
                     ),
                     "la_angostura" = data.frame(
                       name = c("La Angostura", "Chuquiago", "El Puente"),
                       index = c(1, 116, 1663)
                     ), 
                     "cotagaita" = data.frame(
                       name = c("Cotagaita"),
                       index = c(23)
                     ),
                     "tumusla" = data.frame(
                       name = c("Tumusla", "Palca Grande", "San Josecito"),
                       index = c(9, 727, 2874)
                     ),
                     "colavi" = data.frame(
                       name = c("Colavi - Canutillos", "Tacobamba arriba Pilcomayo"),
                       index = c(1, 238)
                     )
  )
  
  
  
  # Add distance in meters
  stations$distance_m <- stations$index * 100
  
  # Join with elevation data to get smoothed elevations at those points
  stations <- stations %>%
    left_join(elev %>% select(index, smoothed_elev), by = "index")
  
  # Plot elevation with station markers
  elev_plot <- ggplot(elev, aes(x = distance_m/1000)) +
    geom_point(aes(y = elev), alpha = 0.1, size = 2) +  # original points
    geom_line(aes(y = smoothed_elev), color = "cyan", size = 1) +  # smoothed line
    geom_point(data = stations, aes(x = distance_m/1000, y = smoothed_elev),
               color = "red", size = 5) +  # station points
    geom_text(data = stations, aes(x = distance_m/1000, y = smoothed_elev, label = name),
              angle = 0, vjust = -1.5, hjust = -.2, size = 4) +  # station labels
    labs(y = "Elevation (m)", x = "Distance along river (km)",
         title = "River Elevation Profile with Monitoring Stations") +
    theme_minimal()
  
  # Plot slope with stations (flipped y-axis)
  slope_plot <- ggplot(elev, aes(x = distance_m/1000, y = smoothed_slope)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line(linewidth = 1.2) +
    geom_vline(data = stations, aes(xintercept = distance_m/1000),
               linetype = "dashed", color = "black", alpha = 0.4, linewidth = .8) +
    geom_text(data = stations, aes(x = distance_m/1000,
                                   y = 0.8*(max(elev$smoothed_slope, na.rm = TRUE)),
                                   label = name),
              angle = 90, vjust = -0.5, hjust = 0, size = 4) +
    scale_y_reverse() +  # This flips the axis
    labs(y = "Slope (m/m)", x = "Distance along river (km)",
         title = "River Slope with Monitoring Stations") +
    theme_minimal()
  
  station_coords <- all_media_scored |>
    select(station, latitude_decimal, longitude_decimal) |>
    filter(!is.na(latitude_decimal), !is.na(longitude_decimal)) |>
    group_by(station) |>
    summarise(lat = mean(latitude_decimal),
              lon = mean(longitude_decimal)) |>
    ungroup()
  
  # Add the coordinates back to your stations dataframe for ordering
  stations <- stations %>%
    left_join(station_coords, by = c("name" = "station"))
  
  # Filter out stations without coordinates before creating sf object
  stations_with_coords <- stations %>%
    filter(!is.na(lon), !is.na(lat))
  
  # Create sf points in the correct order (only for stations with coordinates)
  stations_sf_ordered <- st_as_sf(stations_with_coords, 
                                  coords = c("lon", "lat"),
                                  crs = 4326)
  
  segment_path <- paste0("misc/", station, "_segments_dissolve.shp")
  segment <- st_read(segment_path)
  
  # Map plot
  library(ggrepel)
  
  library(ggrepel)
  
  map_plot <- ggplot() +
    geom_sf(data = river_network, color = "lightgrey", linewidth = 0.5) +  # full network faded
    geom_sf(data = segment) +
    geom_sf(data = stations_sf_ordered, size = 4, aes(color = smoothed_elev)) +  # station points
    scale_color_continuous(palette = "viridis") +
    geom_text_repel(
      data = stations_sf_ordered, 
      aes(label = name, geometry = geometry),
      stat = "sf_coordinates",
      size = 3,
      box.padding = 0.5,      # padding around labels
      point.padding = 0.3,    # padding around points
      segment.color = "black", # color of connecting lines
      segment.size = 0.3,     # thickness of connecting lines
      min.segment.length = 0, # always draw segment lines
      max.overlaps = Inf      # allow all labels
    ) +
    labs(title = "Monitoring Station Locations Along River",
         color = "Elevation (m)") +
    theme_minimal()
  
  map_plot
  
  library(patchwork)
  # Combine with patchwork
  combined_plot <- elev_plot / (slope_plot + map_plot)
  
  return(combined_plot)
  
}



