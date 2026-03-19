# Data Intake
Various types of data were included in this analysis, and are available to include in continued reporting of the Pilcomayo Basin.

## Regulatory Standards
The project pulled regulatory standards from a variety of sources, including the Bolivian Law 1333, WHO, USEPA, USGS, FAO, and others. Those standards are compiled into all_standards.csv. Additional standards can be included in this document and loaded into the application using the `load_base_data` and/or `set_strict_stds` functions. 

## Reported Concentrations Data
All pollutant concentration data (in both sediment and water media) was collected from the [Trinational Commission for the Development of the Pilcomayo River basin](https://www.pilcomayo.net/). The agency did not offer this project permission to host their data, but the data can be freely accessed online from their website: [https://www.pilcomayo.net/calidaddeaguas](https://www.pilcomayo.net/calidaddeaguas).

The data can be added to the `sed` or `water` folders in two formats:
1) Trinational Commission's Downloaded Format: Place this into the `raw` folder.
2) Clean Format: This is a processed format that utilizes tidy data, where each line is a single sample-pollutant data point. You can view an example of this format by reviewing the `example_format.csv` document in both the `sed` and `water` folders. 

## Bolivian Community Data

### Census

### Mining Exposure


### Population Raster
The population raster was collected from **add source**. This can be updated to show a new version of the data on the combined risk map within the application.

## Modeled Air Quality Data
The air quality data provided was developed using the NOAA HYSPLIT model as described in the **resultant** paper. If one was interested in re-running these analyses using a different case (e.g., varied pollutants, locations, or environmental conditions), they would need to utilize the NOAA HYSPLIT model to update that analysis. 

