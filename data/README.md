# Data Intake

Various types of data were included in this analysis, and are available to include in continued reporting of the Pilcomayo Basin.

## Regulatory Standards

The project pulled regulatory standards from a variety of sources, including the Bolivian Law 1333, WHO, USEPA, USGS, FAO, and others. Those standards are compiled into all_standards.csv. Additional standards can be included in this document and loaded into the application using the `set_strict_stds` function. 

## Reported Concentrations Data

All pollutant concentration data (in both sediment and water media) was collected from the [Trinational Commission for the Development of the Pilcomayo River basin](https://www.pilcomayo.net/). The agency did not offer this project permission to host their data, but the data can be freely accessed online from their website: [https://www.pilcomayo.net/calidaddeaguas](https://www.pilcomayo.net/calidaddeaguas).

The data can be added to the `sed` or `water` folders in two formats:

1) Trinational Commission's Downloaded Format: Place this into the `raw` folder.
2) Clean Format: This is a processed format that utilizes tidy data, where each line is a single sample-pollutant data point. You can view an example of this format by reviewing the `example_format.csv` document in both the `sed` and `water` folders. 

## Bolivian Community Data

### Census

Data was sourced from the [2024 Bolivian Population and Housing Census](https://www.ine.gob.bo/index.php/censos-y-banco-de-datos/censos/). This includes individual-level demographic and socioeconomic records. It also covers household-level housing and infrastructure characteristics, as well as registered mortality rates. From these characteristics, a subset was selected and expressed as proportions of each municipality’s total population. Any missing or invalid values were omitted from calculations for corresponding indicators. More information can be found in the final report. 

### Mining Exposure


### Population Raster

The population raster was collected from **add source**. This can be updated to show a new version of the data on the combined risk map within the application.

## Modeled Air Quality Data

The air quality data provided was developed using the NOAA HYSPLIT model as described in the **resultant** paper. If one was interested in re-running these analyses using a different case (e.g., varied pollutants, locations, or environmental conditions), they would need to utilize the NOAA HYSPLIT model to update that analysis. 

The model selected for air pollution analysis was the [HYSPLIT model](https://www.arl.noaa.gov/hysplit/) developed by the National Oceanic and Atmospheric Administration (NOAA) Air Resources Laboratory. HYSPLIT uses meteorological data to predict how pollutants from a point source travel through air and how they accumulate over time. The model’s calculations use a hybrid of the Langrangian approach, which uses a moving frame of reference to calculate advection and dispersion, and the Eulerian method, which uses a fixed 3D grid as a frame of reference to calculate concentrations. For this analysis, particle concentrations were modeled using the particle dispersion model. In the particle dispersion model, a fixed number of particles are advected within the study grid based on the mean wind field and are dispersed using a turbulence component.

The meteorological data inputted into HYSPLIT was [NOAA’s 2024 Global Data Assimilation System (GDAS) data](https://www.ncei.noaa.gov/products/weather-climate-models/global-data-assimilation). GDAS interpolates meteorological data recorded from observation stations around the globe into a 3-dimensional grid. This data is provided in a 1 degree by 1 degree resolution grid and is reported in 3 hr time intervals. 
