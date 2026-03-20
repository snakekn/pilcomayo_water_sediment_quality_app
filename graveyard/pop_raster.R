library(terra)
library(leaflet)

load_pop_rasters = function() {
  
  # Load raster
  my_raster <- rast(here::here("data/population_raster/pop_2024.tif"))
  
  # View info
  my_raster
  print(my_raster)
  
  # Plot
  plot(my_raster)
  
  # Get values as matrix
  values_matrix <- as.matrix(my_raster)
  
  # Get extent
  terra::ext(my_raster)
  
  # Get CRS
  crs(my_raster)
  
  # Convert to data frame (for ggplot)
  df <- as.data.frame(my_raster, xy = TRUE)
  
  # Create color palette
  pal <- colorNumeric(
    palette = colorRamp(c("#000000", "#Ff0FFF")),
    domain = values(my_raster),
    na.color = "transparent"
  )
  
  # Add to leaflet with base map
  leaflet() %>%
    addTiles() %>%  # Default OpenStreetMap
    addRasterImage(my_raster, colors = pal, opacity = 0.6) %>%
    addLegend(pal = pal, values = values(my_raster), title = "Values")
  
  # Or use different base maps:
  # leaflet() %>%
  #   addProviderTiles(providers$CartoDB.Positron) %>%  # Light background
  #   addRasterImage(my_raster, colors = pal, opacity = 0.6)
  # 
  # leaflet() %>%
  #   addProviderTiles(providers$Esri.WorldImagery) %>%  # Satellite
  #   addRasterImage(my_raster, colors = pal, opacity = 0.6)
  # 
  # leaflet() %>%
  #   addProviderTiles(providers$Stamen.Terrain) %>%  # Terrain
  #   addRasterImage(my_raster, colors = pal, opacity = 0.6)
}