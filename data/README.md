# Data Intake
Various types of data were included in this analysis, and are available to include in continued reporting of the Pilcomayo Basin. Data analyses are maintained in [Zenodo](https://zenodo.org/communities/river-remedy-brenucsb).

## Regulatory Standards
The project pulled regulatory standards from a variety of sources, including the Bolivian Law 1333, WHO, USEPA, USGS, FAO, and others. Those standards are compiled into all_standards.csv. Additional standards can be included in this document and loaded into the application using the `load_base_data` and/or `set_strict_stds` functions. 

## Reported Concentrations Data
All pollutant concentration data (in both sediment and water media) was collected from the [Trinational Commission for the Development of the Pilcomayo River basin](https://www.pilcomayo.net/). The agency did not offer this project permission to host their data, but the data can be freely accessed online from their website: [https://www.pilcomayo.net/calidaddeaguas](https://www.pilcomayo.net/calidaddeaguas).

The data can be added to the `sed` or `water` folders in two formats:
1) Trinational Commission's Downloaded Format: Place this into the `raw` folder.
2) Clean Format: This is a processed format that utilizes tidy data, where each line is a single sample-pollutant data point. You can view an example of this format by reviewing the `example_format.csv` document in both the `sed` and `water` folders. 

## Bolivian Community Data

### Census

Population vulnerability was characterized using demographic, socioeconomic, housing, and mortality data from the [2024 Bolivian Population and Housing Census](https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/). Selected indicators, such as education level and health insurance coverage, were expressed as proportions of each municipality’s vulnerable population and percentile-ranked to represent relative vulnerability across the basin.

### Mining Activity
Mining activity data was compiled from [mindat.org](https://www.mindat.org), the [USGS](https://pubs.usgs.gov/publication/ofr20171079), and [GeoBolivia](https://geo.gob.bo). The data was filtered to include only locations within the Pilcomayo Basin, and to remove duplicates across sources.

### Population Raster
The population raster (people per grid-cell, 30 arc seconds) was collected from [WorldPop](https://hub.worldpop.org/geodata/summary?id=76454) (2015) to determine the number of people impacted by environmental conditions in an area along the Pilcomayo River. This can be updated to show a new version of the data on the combined risk map within the application.

## Modeled Air Quality Data
The air quality data provides was developed using the [NOAA HYSPLIT model](https://www.ready.noaa.gov/HYSPLIT_hytrial.php) and NCEP Global Data Assimilation System (GDAS) data from May 2024 to October 2024. Further information on the process can be viewed in the associated report.

