# TL;DR

This repo includes crowdsourced precipitation phase data, R functions to mimic the precipitation phase partitioning methods used in the National Water Model and NextGen formulations, and a comprehensive analysis thereof.

I presented this analysis at the 2025 CIROH DevCon in Burlington, Vermont. You can view the poster [here](poster/jennings_nwm_ppt_phase_2025_resized.pdf).

If you want to play with the code, take a look at the [`analysis`](analysis/) folder. You can view the raw and summarized data in the [`data`](data/) folder.

# A little background

Precipitation phase partitioning is the process of using ancillary data—most commonly near surface meteorological information such as air temperature and relative humidity-to determine what type of precipitation (e.g., rain, snow, mixed) is falling. Because reports of precipitation phase are rare relative to other meteorological observations, we often deploy precipitation phase partitioning methods in hydrologic models and time series analysis.

## Precipitation Phase Partitioning Methods

Two of the most common methods are *thresholds* and *ranges*. The former uses a single value whereby all temperatures (air, wet bulb, or dew point) above the value are rain and all below are snow. The latter deploys two values, with all snow below the lower end of the range, all rain above the upper end, and a mix of precipitation in between. Oftentimes, the proportion of solid and liquid precipitation in the mixed range is determined as a linear function of the observed temperature's difference from the upper and lower values.

Some models may also deploy a *sigmoidal curve* or another type of empirical function, such as *binary logistic regression*. In both these cases the probability or proportion of solid (or liquid) precipitation follows an s-shaped curve based on temperature and sometimes additional meteorological information. 

## The National Water Model

The NOAA-NWS Office of Water Prediction runs the National Water Model (NWM) in the US to predict streamflow and other hydrologic variables. In cold and temperate regions it is critical that the model accurately simulate the proportion of rain and snow falling at the land surface. To do this, the NWM employs the Jordan (1991) phase partitioning method, which relies on a dual-threshold range with a conditional step function to determine mixed precipitation. This is one of the methods we analyzed.

## NextGen Formulations

The current NWM uses the Noah-MP land surface model embedded within the WRF-Hydro modeling system to simulate the majority of hydrologic processes (it also has terrain and streamflow routing modules). The Next Generation Water Resources Modeling Framework, slated to support the next and future NWMs, in contrast, allows the user to select from a variety of models and modules that can be configured into unique formulations. In practice this means there are several more phase partitioning methods, including thresholds, ranges, and statistical methods, available for use in operations. We evaluate these, as well, and ways to optimize them.
