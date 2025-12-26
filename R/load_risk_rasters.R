load_risk_rasters = function(debug = FALSE) {

  # identify where the data is held
  data_path = "data/"
  
  hq_path = NULL
  vul_path = "census/census_summ.shp"
  air_path = "air_quality/hotspot_merged.tif"
  mining_path = NULL
  pop = "population_raster/pop_2024.tif"
  
  # prepare a list with nulls if we don't have the data
  r = list(hq = NULL,
           vul = NULL,
           air = NULL,
           mining = NULL,
           pop = NULL)
  
  # update the list
  if(debug) {
    r$hq = make_dummy_raster()
    r$vul = make_dummy_raster()
    r$air = make_dummy_raster()
    r$mining = make_dummy_raster()
    r$pop = make_dummy_raster()
  } else {
    r$hq = rast(here(data_path, hq_path))
    r$vul = rast(here(data_path, vul_path))
    r$air = rast(here(data_path, air_path))
    r$mining = rast(here(data_path, mining_path))
    r$pop = rast(here(data_path, pop_path))
  }
  
  return(r)
}

make_dummy_raster = function() {
  r_dummy <<- rast(nrows = 100, ncols = 100,
                   xmin = -66.3, xmax = -65.2,
                   ymin =  -28.8,  ymax =  -27.2)
  # note: will need to reproject later if they're not all the same
  
  values(r_dummy) <- runif(ncell(r_dummy), 0, 1)  # random 0–1
  
  return(r_dummy)
}


# library(terra)
# r <- rast(nrows = 2, ncols = 2, xmin = -1, xmax = 1, ymin = -1, ymax = 1,
#           crs = "EPSG:4326")
# values(r) <- 1:4
# 
# crs(r)            # should show EPSG:4326, no warnings
# r2 <- project(r, "EPSG:3857")
# Sys.getenv("PROJ_LIB")
# Sys.getenv("GDAL_DATA")
# Sys.getenv("PATH")
