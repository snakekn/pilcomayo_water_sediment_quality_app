### Introduction

The Pilcomayo River Basin in southern Bolivia has faced centuries of pollution from mining activity, largely dating back to the discovery of the world's largest silver deposit near Potosi in 1545. Heavy metal contamination of water and sediments, along with acid mine drainage, pose significant risks to the environment, and to the ~1.5 million people of the Pilcomayo basin.

This tool allows users to explore sediment and water quality data collected in the basin between 2016 and 2024. Key water and sediment quality parameters are compared to standards from Bolivian Ley No. 1333, and USGS Sediment Quality Guidelines when applicable.

Use the tabs above to:

- *Filter* data by time, location, water/sediment parameters, and more.
- *Visualize* results on interactive maps using raw measurements and comparisons to standards.
- *Explore* time series trends for individual sampling stations and parameters across multiple years.
- *Rank* observations, stations, and parameters by raw measurements and comparisons to standards.
- *Conduct* principal component analyses (PCA) to find correlation between parameters.
- *Review* applicable environmental standards.

------

#### Standards Used:
Sediment Quality (USGS SQGs for aquatic life):

- ***Below TEL*** - Adverse effects unlikely/infrequent
- ***Above TEL*** - Adverse effects possible
- ***Above PEL*** - Adverse effects likely/frequent

Water Quality (Bolivian Ley No. 1333):

- ***Class A*** - Natural waters of the highest quality, which qualify as potable water for human consumption without any prior treatment, or with simple bacteriological disinfection in necessary cases verified by a laboratory.
- ***Class B*** - Waters of general use, which for human consumption require physical treatment and bacteriological disinfection.
- ***Class C*** - Waters of general use, which to be suitable for human consumption require complete physical-chemical treatment and bacteriological disinfection.
- ***Class D*** - Waters of minimum quality, which for human consumption, in extreme cases of public need, require an initial pre-sedimentation process, as they may have high turbidity due to a high content of suspended solids, followed by complete physical-chemical treatment and special bacteriological disinfection against eggs and intestinal parasites.
- ***Unclassified*** - Exceeds all other standard limits.

---

#### Notes & Caveats:
- Values above or below detection thresholds were converted to half the detection threshold if below (i.e. '<0.5' --> '0.25'), or 1.5 x the detection threshold if above (i.e. '>0.5' --> '0.75'). Therefore, not all values represent exact measurements
- Some parameters in the water and sediment datasets do not have corresponding standards in USGS SQGs or Bolivian law. As such, some features in this app may not include the full range of parameters from the original data, particularly when comparing to standards.
- USGS SQGs are based on effects on sediment-dwelling aquatic organisms, whereas the Bolivian standards from Ley No. 1333 are based on safe levels for human consumption/use.
- The PCA method used in this app fills in missing data by guessing based on patterns in the existing data. It works best when most data are present and the data follow clear trends, but the filled-in values are only estimates and might affect the results.