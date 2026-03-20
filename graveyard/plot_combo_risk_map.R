#unused. we require shiny to make tooltips work, supposedly
plot_combo_risk_map = function(layers_list,
                               center_lng = -65.7,
                               center_lat = -19.5,
                               zoom = 7) {
  # layers_list: named list of SpatRaster, e.g. list(HQ = rhq, Vuln = rvul, ...)
  # center_lng/lat: initial view
  
  stopifnot(length(layers_list) > 0)
  
  # check all are SpatRaster, drop invalid
  ok_idx <- vapply(layers_list, inherits, logical(1), "SpatRaster")
  layers_list <- layers_list[ok_idx]
  if (length(layers_list) == 0) stop("No valid SpatRaster layers provided.")
  
  # merged extent for view
  r_stack <- terra::rast(layers_list)
  
  # build base leaflet map with all rasters
  m <- leaflet() |>
    addTiles() |>
    setView(lng = center_lng, lat = center_lat, zoom = zoom)
  
  # add each raster as its own layer with a consistent palette
  for (nm in names(layers_list)) {
    r <- layers_list[[nm]]
    pal <- colorNumeric("viridis", terra::values(r), na.color = "transparent")
    
    m <- m |>
      addRasterImage(
        r,
        colors  = pal,
        opacity = 0.7,
        project = FALSE,
        group   = nm
      )
  }
  
  # store layer names for JS
  layer_names <- names(layers_list)
  
  # add click handler via JS: on map click, send lat/lng back to Shiny
  # and show a popup with values for each layer
  # Note: extraction itself happens in R using terra::extract
  m <- htmlwidgets::onRender(
    m,
    sprintf(
      "
      function(el, x) {
        var map = this;
        map.on('click', function(e) {
          // send click lat/lng to Shiny
          if (window.Shiny) {
            Shiny.setInputValue('risk_map_click', {
              lat: e.latlng.lat,
              lng: e.latlng.lng,
              nonce: Math.random()
            }, {priority: 'event'});
          }
        });
      }
      "
    )
  )
  
  # attach the terra stack as attribute so server code can use it
  attr(m, "risk_layers") <- layers_list
  invisible(m)
}
