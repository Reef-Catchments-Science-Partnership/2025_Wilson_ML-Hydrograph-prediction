# 2025_Wilson_ML-Hydrograph-prediction
Abstract for paper:

The timing of sample collection in water quality monitoring programs substantially affects the accuracy of estimated pollutant loads. To minimise error, concentrations should be sampled across the hydrograph, capturing the rising limb, peak, and falling limb, to reflect the dynamic nature of pollutant transport. However, in river systems with variable flow regimes, predicting the timing and duration of events in real time is challenging. Monitoring programs often resort to oversampling to ensure that critical periods are represented, but this approach increases both effort and cost. In this study, we apply gradient boosting decision trees (CatBoost) to forecast river height in a tropical, fast-responding catchment characterised by high flow variability, using hourly rainfall, discharge, and river height data collected over a 15-year period. We evaluate model performance across forecast horizons ranging from 1 to 48 hours and show that hydrograph shape and duration can be reliably predicted under both extreme flow and low-flow conditions, with forecast accuracy declining as horizon length increases. Our findings demonstrate that machine learning forecasts can reduce reliance on oversampling while maintaining pollutant load accuracy, offering a scalable tool to improve the efficiency of water quality monitoring programs. 


# Data Availablity 
Data used in this study was accessed from the Queensland Government's Water Monitoring Information Portal using this link https://water-monitoring.information.qld.gov.au/
Sites that were used in this study were as follows: 
## 113006A Tully River at Euramo
- Discharge
- River Height
- Rainfall

## 113015A Tully River at Tully Gorge National Park
- Discharge
- River Level

## 113004A Cochable Creek at Powerline
- Discharge
- River Level

## 114001A Murray River at Upper Murray
- Rainfall 
