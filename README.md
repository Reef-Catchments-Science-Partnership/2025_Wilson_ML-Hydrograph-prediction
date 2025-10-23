# 2025_Wilson_ML-Hydrograph-prediction
Abstract for paper:

Hydrology and water quality are innately linked, as flow dynamics control the transport of pollutants within river systems. Consequently, the timing of sample collection in water quality monitoring programs strongly influences the accuracy of estimated pollutant loads. To minimise error, concentrations should be sampled across the hydrograph, capturing the rising limb, peak, and falling limb, to reflect the dynamic nature of pollutant transport. However, in river systems with variable flow regimes, predicting the timing and duration of events in real time is challenging. Monitoring programs often resort to oversampling to ensure that critical periods are represented, but this approach increases both effort and cost. In this study, we apply probabilistic gradient boosting decision tree regression (CatBoost) to forecast river height in a tropical, fast-response catchment characterised by high flow variability, using hourly rainfall, discharge, and river height data collected over a 15-year period. Model performance was evaluated across forecast horizons ranging from 1-hour to 48-hour. The models reproduced hydrograph magnitude, shape, and timing with high accuracy at short horizons (1-hour to 12-hour), while forecast confidence and accuracy declined progressively at longer horizons (24-hour to 48-hour). Forecast performance also varied across flow regimes: low flows were predicted accurately across all horizons, moderate flows reliably up to the 24-hour horizon, and high flows with strong skill up to the 12-hour horizon. Predictive skill declined for extreme events; however, forecasts remained operationally valuable up to the 12-hour horizon. These findings highlight the potential for short-term forecasts to support adaptive, resource-efficient sampling programs and reduce reliance on oversampling while maintaining pollutant-load accuracy.

# Data Availablity 
Data used in this study was accessed from the Queensland Government's Water Monitoring Information Portal https://water-monitoring.information.qld.gov.au/

Sites and variables that were used in this study are as follows: 
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
