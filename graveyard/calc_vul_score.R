### Purpose: take census data and make it into a raster of pop. vul values
# we'll likely create it once and keep it to load in

# Will Jackson just put this tract-level data into GIS & raster?
# Anything else to do?

## Inputs ##
# - % age vulnerable
# - P31: Not registered to insurance
# - % vulnerable workers (ag, mining)
# - Filter: P36J (people in Potosi)


## Output ##
# - Raster with the score

## Process ##
# - take in data per-census tract
# - get our fav columns and get scores
# - normalize into 0-100 score for Vulnerability
# - rasterize vector data & returnf