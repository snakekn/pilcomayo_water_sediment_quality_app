# About The Project
The Water and Sediment Quality Explorer application allows users to upload water and sediment quality data to generate dynamic data visualizations and maps of the Pilcomayo Basin. 

Additional information about this project can be found on the [Bren School of the Environment's Masters Project website](https://bren.ucsb.edu/projects/addressing-centuries-heavy-metal-pollution-understanding-human-health-crisis-pilcomayo). 

## About the App
### Available Data
The application utilizes data from the Trinational Commission from 2016-2023, with the option to include data from 2024 (See 4.1 Data Limitations). Users may filter the data by analyte, sampled station, and time ranges, and visualize results based on raw values, comparisons to standards, or risk-based factors such as hazard quotient. Users can upload new environmental data sampled in the region utilizing a provided formatting guide or the format utilized by the Trinational Commission, supporting the analysis of future environmental surveys conducted by AMTSK, local community organizations, government agencies, and other interested parties. 
### Ranking Plots
A series of plots rank the reported concentrations of water or sediment media by:
- ranking individual reported samples by concentration or HQ,
- ranking stations by HQ,
- ranking hazardous parameters, and
- ranking grain sizes by HQ
Each plot can be filtered to focus on specific parameters, media, fractions, or stations. Sediment parameters were grouped by sieve size (grain-size fraction) using the Wentworth grain size classification system (Wentworth, 1922). Additional information about these plots can be found in Appendix A.2.1 Data Analysis.
### Time Series Plots
Time series plots were generated to inspect trends in water and sediment quality over time. Users may select a parameter and a location to generate a time series plot showing how measured values of that parameter have changed over time. Users may also display relevant reference limits to compare with measured values.
### Combined Risk Maps
Layers that combine hazards from water and sediment quality data with municipality-level EJI scores and population density can be generated. Water and sediment risk layers are created by scoring stations (see 3.3.1 Hazard Quotient Aggregation), and interpolating scores across the river network (see 3.3.1 Interpolation Across River Network). Users may adjust the method for scoring stations. EJI scores and population density are included within the app. 

Once layers are generated, and a score binning method is selected for each layer (e.g. Equal-Interval, Quantiles), layers can be included into a final prioritization map to provide priority scores along impacted areas along the basin. The locations of mines and settlements, rivers, tailings, ingenious and processing facilities, and subcatchments can also be shown. Subcatchments may be delineated using the water and sediment stations as outlet points, and are assigned the scores of their corresponding stations.
